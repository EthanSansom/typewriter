# assignment -------------------------------------------------------------------

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
  .assign_typed(
    sym = rlang::enexpr(sym),
    call = rlang::enexpr(call),
    env = env
  )
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

  active_binding_function <- rlang::new_function(
    args = active_binding_args,
    body = active_binding_body,
    env = rlang::new_environment(data = list(VALUE = VALUE), parent = env)
  )

  rlang::env_unbind(env, sym_name)
  makeActiveBinding(sym = sym, fun = active_binding_function, env = env)
}

utils::globalVariables("!<-")

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
