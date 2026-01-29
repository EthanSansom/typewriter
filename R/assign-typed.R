# todos ------------------------------------------------------------------------

# Documentation:
# - `%<~%`
# - `assign_typed()`
# - `stop_constant_assignment()` (dependancy)

# Testing:
# - `is_typed_object()`

# assignment -------------------------------------------------------------------

# TODO: I've updated the `.assign_` functions to return their `VALUE` instead
# of `NULL`.

#' @export
`%<~%` <- function(sym, call) {
  call <- rlang::enexpr(call)
  sym <- rlang::enexpr(sym)
  env <- rlang::caller_env()

  if (rlang::is_call(call, "function", ns = "")) {
    .assign_typed_function(
      sym = sym,
      call = call,
      env = env
    )
  } else if (rlang::is_call(call, "const", ns = c("", "typewriter"))) {
    .assign_const(
      sym = sym,
      const_call = call,
      env = env
    )
  } else {
    .assign_typed(
      sym = sym,
      call = call,
      env = env
    )
  }
}

#' @export
assign_typed <- function(sym, call, env = rlang::caller_env()) {
  call <- rlang::enexpr(call)
  if (rlang::is_call(call, "const", ns = c("", "typewriter"))) {
    .assign_const(
      sym = rlang::enexpr(sym),
      const_call = call,
      env = env
    )
  } else {
    .assign_typed(
      sym = rlang::enexpr(sym),
      call = call,
      env = env
    )
  }
}

.assign_const <- function(sym, const_call, env) {

  error_call <- rlang::caller_env()
  check_is_symbol(sym, call = error_call)

  args <- rlang::call_args(const_call)
  sym_name <- rlang::as_name(sym)

  if (length(args) != 1L) {
    typewriter_abort_invalid_input(
      message = c(
        sprintf("Can't declare constant object `%s`.", sym_name),
        i = sprintf("`const()` must be provided 1 argument to initialize `%s`.", sym_name),
        x = sprintf("Provided %i arguments to `const()`.", length(args))
      ),
      call = error_call
    )
  }

  value_label <- rlang::as_label(args[[1]])
  VALUE <- check_is_evaluable(
    args[[1]],
    env = env,
    message = c(
      sprintf("Can't declare constant object `%s`.", sym_name),
      x = sprintf("Attempted assignment `%s <- %s`.", sym_name, value_label),
      x = sprintf("Can't evaluate `%s` in `env = %s`.", value_label, env_desc(env))
    ),
    call = error_call
  )

  active_binding_body <- rlang::expr({
    if (missing(object)) {
      return(VALUE)
    }
    typewriter::stop_constant_assignment(
      name = !!sym_name,
      error_call = !!env
    )
  })
  active_binding_fn_env <- new.env(parent = env)
  active_binding_fn_env$VALUE <- VALUE

  active_binding_function <- rlang::new_function(
    args = rlang::pairlist2(object = ),
    body = active_binding_body,
    env = active_binding_fn_env
  )
  class(active_binding_function) <- c(
    "typewriter_active_binding_function",
    "typewriter_active_binding_constant_function",
    "function"
  )

  rlang::env_unbind(env, sym_name)
  makeActiveBinding(sym = sym, fun = active_binding_function, env = env)
  invisible(VALUE)
}

.assign_typed_function <- function(sym, call, env) {

  error_call <- rlang::caller_env()
  check_is_symbol(sym, call = error_call)
  sym_name <- rlang::as_name(sym)

  typed_fun <- rlang::try_fetch(
    typed(!!call, env = env),
    typewriter_error = function(cnd) {
      typewriter_abort(
        message = sprintf("Can't assign typed function to object `%s`.", sym_name),
        call = error_call,
        parent = cnd
      )
    }
  )

  rlang::env_bind(env, !!sym_name := typed_fun)
  invisible(typed_fun)
}

utils::globalVariables(":=")

.assign_typed <- function(sym, call, env) {

  error_call <- rlang::caller_env()
  check_is_symbol(sym, call = error_call)

  # An <alias> may be provided by name, e.g. `call = ns::a_int` or `call = a_int`
  # In this case we convert it into a call, e.g. `a_int()`. Otherwise, `call`
  # must be a simple call to a function which exists in `env`.
  if (is_named_symbol(call)) {
    maybe_alias <- check_is_evaluable(
      x = call,
      env = env,
      message = c(
        "`call` must be an <alias> provided by name or a simple call.",
        i = sprintf("`call` is the object `%s`.", rlang::as_label(call)),
        x = sprintf("Can't evaluate `call` in `env = %s`.", env_desc(env))
      ),
      call = error_call
    )
    if (!is_type_alias(maybe_alias)) {
      typewriter_abort_invalid_input(
        sprintf(
          "`call` must be an <alias> or a simple call, not %s.",
          obj_type_friendly(maybe_alias)
        ),
        call = error_call
      )
    }
    call_fun <- maybe_alias
    call_fun_sym <- call
    call <- rlang::call2(call)
  } else if (!rlang::is_call(call)) {
    typewriter_abort_invalid_input(
      sprintf(
        "`call` must be an <alias> or a simple call, not %s.",
        obj_type_friendly(call)
      ),
      call = error_call
    )
  } else {
    check_is_simple_call(
      x = call,
      message = c(
        "`call` must be an <alias> or a simple call.",
        x = sprintf("`call = %s` is a complex call.", rlang::as_label(call)),
        i = "A simple call has the form `foo()` or `ns::foo()`, not e.g. bar$foo()."
      ),
      call = error_call
    )
    maybe_function <- check_is_evaluable(
      x = call[[1]],
      env = env,
      message = c(
        "`call` must be an <alias> or a simple call.",
        i = sprintf("`call` is a simple call to `%s`.", rlang::as_label(call[[1]])),
        x = sprintf("Can't evaluate `%s` in `env = %s`.", rlang::as_label(call[[1]]), env_desc(env))
      ),
      call = error_call
    )
    fun <- check_is_function(
      maybe_function,
      message = c(
        "`call` must be an <alias> or a simple call.",
        x = sprintf("`call` is a malformed call to `%s`.", rlang::as_label(call[[1]])),
        x = sprintf(
          "`%s` is %s in `env = %s`, not a function.",
          rlang::as_label(call[[1]]), obj_type_friendly(maybe_function), env_desc(env)
        )
      ),
      call = error_call
    )
    if (typeof(fun) %in% c("builtin", "special")) {
      typewriter_abort_invalid_input(
        message = c(
          '`call` must be an <alias> or a simple call to a function of type "closure".',
          i = sprintf("`call` is a simple call to function `%s`.", rlang::as_label(call[[1]])),
          x = sprintf('The function `%s` is of type "%s".', rlang::as_label(call[[1]]), typeof(fun))
        ),
        call = error_call
      )
    }
    call_fun_sym <- call[[1]]
    call_fun <- fun
  }

  call_args <- rlang::call_args(call)
  call_args_names <- rlang::names2(call_args)

  args <- mapply(
    arg = call_args,
    arg_name = call_args_names,
    arg_pos = seq_along(call_args),
    FUN = \(arg, arg_name, arg_pos) {
      check_is_evaluable(
        arg,
        env = env,
        call = error_call,
        message = if (arg_name == "") {
          sprintf("Can't evaluate argument `%s` at postion %i of `call`.", rlang::as_label(arg), arg_pos)
        } else {
          sprintf("Can't evaluate argument `%s = %s` of `call`.", arg_name, rlang::as_label(arg))
        }
      )
    },
    SIMPLIFY = FALSE
  )

  # Take the first argument of `call` to be the initialization value of the
  # assigned object if the argument is unnamed.
  if (length(call_args) != 0 && call_args_names[[1]] == "") {
    VALUE <- args[[1]]
    args <- args[-1]

    # Ideally we'd allow any assignment possible with `<-`, but initialization
    # with a missing value introduces too much downstream messiness.
    if (rlang::is_missing(VALUE)) {
      sym_name <- rlang::as_name(sym)
      typewriter_abort_invalid_input(
        message = c(
          sprintf("Can't declare typed object `%s`.", sym_name),
          x = sprintf("Attempted to initialize typed object `%s` with a missing value.", sym_name)
        ),
        call = error_call
      )
    }

    # Ensure that the initialization value is of the correct type by attempting
    # to evaluate the provided type-checking `call` with `VALUE` as it's first
    # argument.
    rlang::try_fetch(
      expr = eval(call, env),
      error = function(cnd) {
        typewriter_abort(
          message = c(
            sprintf("Can't declare typed object `%s`.", rlang::as_name(sym)),
            i = sprintf("Attempted assignment `%s <- %s`.", rlang::as_name(sym), rlang::as_label(call_args[[1]]))
          ),
          class = "typewriter_error_invalid_assignment",
          call = error_call,
          parent = cnd
        )
      }
    )
  } else {
    if (is_type_alias(call_fun)) {
      VALUE <- new_uninitialized(
        name = attr(call_fun, "name"),
        desc = attr(call_fun, "desc"),
        bullets = attr(call_fun, "bullets")
      )
    } else {
      VALUE <- new_uninitialized(
        name = rlang::as_label(call_fun_sym),
        desc = sprintf("An object checked using `%s`.", rlang::as_label(call)),
        bullets = NULL
      )
    }
  }

  # We in-line the definition of `call_fun` into the active binding function
  # and assign that definition to `call_fun_sym`. This allows error messages
  # to reference a call of the correct name. However, we don't want to overwrite
  # a namespaced function during assignment, so we take the `foo` from `ns::foo`
  # if `call_fun_sym` is a namespaced symbol `ns::foo`.
  if (rlang::is_call(call_fun_sym, c("::", ":::"))) {
    call_fun_sym <- as.symbol(call_fun_sym[[3]])
  }

  new_call <- rlang::call_modify(rlang::call2(call_fun_sym), sym, !!!args)
  sym_name <- rlang::as_name(sym)

  active_binding_args <- symbol_to_pairlist(sym)
  active_binding_body <- rlang::expr({
    if (missing(!!sym)) {
      return(VALUE)
    }

    # Explicitly prevent missing values (e.g. via `quote(expr = )`)
    typewriter::check_missing_assignment(
      expr = !!sym,
      name = !!sym_name,
      error_call = !!env
    )

    # In-lined definition of the function called in `call`
    !!call_fun_sym <- !!call_fun

    # Attempt to call the in-lined version of `call` (i.e. `new_call`)
    typewriter::check_typed_assignment(
      expr = !!new_call,
      name = !!sym_name,
      error_call = !!env
    )

    # Since we passed the check above, update the value of the typed object which
    # we store in the parent environment of this function.
    VALUE <<- !!sym
    VALUE
  })

  # Using instead of `rlang::new_environment()` to handle the case where `VALUE`
  # is `rlang::zap()`.
  active_binding_fn_env <- new.env(parent = env)
  active_binding_fn_env$VALUE <- VALUE

  active_binding_function <- rlang::new_function(
    args = active_binding_args,
    body = active_binding_body,
    env = active_binding_fn_env
  )
  class(active_binding_function) <- c("typewriter_active_binding_function", "function")

  rlang::env_unbind(env, sym_name)
  makeActiveBinding(sym = sym, fun = active_binding_function, env = env)
  invisible(VALUE)
}

utils::globalVariables("!<-")

#' Test if the object is typed
#'
#' @description
#'
#' This function returns `TRUE` if `x` is an object typed by [%<~%] or [assign_typed()]
#' in `env` and `FALSE` otherwise. The object to test must be provided by name.
#'
#' @param x `[object]`
#'
#' An object provided by name.
#'
#' @param env `[environment]`
#'
#' The environment in which to check whether `x` is a typed object. Defaults to
#' the calling environment of `is_typed_object()`.
#'
#' @return
#'
#' `TRUE` if `x` is a typed object, `FALSE` otherwise.
#'
#' @examplesIf requireNamespace("chk", quietly = TRUE)
#' x %<~% chk::chk_integer(10L)
#' y <- 10L
#' identical(x, y)
#' is_typed_object(x)
#' is_typed_object(y)
#'
#' @export
is_typed_object <- function(x, env = rlang::caller_env()) {
  x <- check_is_symbol(rlang::enexpr(x), message = "`x` must be an object provided by name.")
  check_is_environment(env)
  bindingIsActive(x, env) && inherits(activeBindingFunction(x, env), "typewriter_active_binding_function")
}

# TODO: Document

#' @export
is_constant <- function(x, env = rlang::caller_env()) {

}

# helpers ----------------------------------------------------------------------

symbol_to_pairlist <- function(sym) {
  rlang::pairlist2(!!sym := rlang::missing_arg())
}

env_desc <- function(env) {
  sprintf("<environment: %s>", rlang::env_label(env))
}

is_named_symbol <- function(x) {
  is.symbol(x) || rlang::is_call(x, c("::", ":::"))
}

# dependencies -----------------------------------------------------------------

# These are not functions intended for external use, but are functions which are
# used within the active binding function of a typed object.

# TODO: Document

#' @export
stop_constant_assignment <- function(name, error_call) {
  typewriter_abort(
    message = sprintf("Can't assign a value to constant `%s`.", name),
    class = "typewriter_error_invalid_assignment",
    call = error_call
  )
}

#' Check that an object can be evaluated
#'
#' @description
#'
#' This function is used internally by the active binding functions of objects
#' typed using [%<~%] or [assign_typed()]. It is not meant for use outside of
#' this context.
#'
#' @param expr `[expression]`
#'
#' An expression.
#'
#' @param name `[character(1)]`
#'
#' An object name.
#'
#' @param error_call `[environment]`
#'
#' An environment.
#'
#' @returns
#'
#' An error if `expr` signals an error and `NULL` otherwise.
#'
#' @examples
#' foo <- function(x) {
#'   check_typed_assignment(
#'     expr = x,
#'     name = "x",
#'     error_call = rlang::caller_env()
#'   )
#' }
#' try(foo(stop("An Error")))
#' @export
check_typed_assignment <- function(expr, name, error_call) {
  rlang::try_fetch(
    expr = expr,
    error = function(cnd) {
      typewriter_abort(
        message = sprintf("Attempted to assign an invalid value to typed object `%s`.", name),
        class = "typewriter_error_invalid_assignment",
        call = error_call,
        parent = cnd
      )
    }
  )
}

#' Check that an object is not missing
#'
#' @description
#'
#' This function is used internally by the active binding functions of objects
#' typed using [%<~%] or [assign_typed()]. It is not meant for use outside of
#' this context.
#'
#' @param expr `[expression]`
#'
#' An expression.
#'
#' @param name `[character(1)]`
#'
#' An object name.
#'
#' @param error_call `[environment]`
#'
#' An environment.
#'
#' @returns
#'
#' An error if `expr` is missing and `NULL` otherwise.
#'
#' @examples
#' foo <- function(x) {
#'   check_missing_assignment(
#'     expr = x,
#'     name = "x",
#'     error_call = rlang::caller_env()
#'   )
#' }
#' try(foo())
#' @export
check_missing_assignment <- function(expr, name, error_call) {
  if (rlang::is_missing(expr)) {
    typewriter_abort(
      message = sprintf("Attempted to assign a missing value to typed object `%s`.", name),
      class = "typewriter_error_invalid_assignment",
      call = error_call
    )
  }
}
