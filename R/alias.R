# todos ------------------------------------------------------------------------

# Documentation:
# - type_alias()
# - alias_caller()
# - is_type_alias()

# aliasing ---------------------------------------------------------------------

#' Create a type alias function
#'
#' @description
#'
#' TODO: Description
#'
#' @param call `[call]`
#'
#' A call to a type-checking function. The first formal argument of the type-checking
#' function should be the object checked by the function and this argument should not
#' be supplied to `call`.
#'
#' For example [chk::chk_integer()] checks that it's first argument `x` is an
#' integer. An integer type alias function can be declared like so:
#'
#' ```r
#' a_integer <- type_alias(chk::chk_integer())
#' ```
#'
#' This is roughly equivalent to:
#' ```r
#' a_integer <- function(...) {
#'   chk::chk_integer(..1)
#' }
#' ```
#'
#' `type_alias()` supports NSE type-checking functions which use `substitute()`
#' on their arguments. To do so, the generated type alias functions must forward
#' their arguments via dots.
#'
#' Additional arguments to the type-checking function may be provided to
#' the type-checking call. For example, the function `[chk::check_values()]`
#' checks that it's first argument `x` is a subset of `values`. We can create a
#' type alias for a subset of options like so:
#'
#' ```r
#' opts <- c("print", "save", "log")
#' a_valid_options <- type_alias(chk::check_values(values = opts))
#' ```
#'
#' This is roughly equivalent to:
#' ```r
#' a_valid_options <- function(...) {
#'   chk::check_values(..1, values = c("print", "save", "log"))
#' }
#' ```
#'
#' Additional arguments provided to the type-checking call are evaluated
#' immediately by `type_alias()` in the calling environment.
#'
#' @param name `[character(1) / NULL]`
#'
#' What the type alias should be called (e.g. `"integer"`). This is displayed
#' when a `type_alias()` is printed or formatted. By default, `name` is inferred
#' from the `call` argument.
#'
#' @param desc `[character(1) / NULL]`
#'
#' A description of the type alias (e.g. `"An integer vector."`). This is
#' displayed when a `type_alias()` is printed and used to describe `typed()`
#' function arguments when an alias is used for argument typing. By default,
#' the `desc` is generated using the `call` argument.
#'
#' @param bullets `[character / NULL]`
#'
#' Additional lines shown when the type alias is printed. If installed, the
#' `bullets` are formatted using the [cli] package.
#'
#' @param return_call `[TRUE / FALSE]`
#'
#' Should the output of the type-checking `call` be returned by the type alias?
#' This is useful for type-checking functions which coerce a checked object to
#' the correct type.
#'
#' If `return_call` is `FALSE` (the default), the generated type alias function
#' will return it's first argument.
#'
#' @returns
#'
#' A type alias function which takes one unnamed `...` argument.
#'
#' @examples
#' # TODO: Examples
#' a_integer <- type_alias(chk::chk_integer())
#' @export
type_alias <- function(call, name = NULL, desc = NULL, bullets = NULL, return_call = FALSE) {
  call <- check_is_simple_call(x = rlang::enexpr(call), x_name = "call")
  check_is_string(name, null_ok = TRUE)
  check_is_string(desc, null_ok = TRUE)
  check_is_character(bullets, null_ok = TRUE)
  check_is_bool(return_call)

  error_call <- rlang::current_env()
  env <- rlang::caller_env()

  call_fun_sym <- call[[1]]
  call_args <- as.list(call[-1])

  maybe_function <- check_is_evaluable(
    call_fun_sym,
    env = env,
    message = c(
      "`call` must be a simple call.",
      i = sprintf("`call` is a simple call to `%s`.", rlang::as_label(call_fun_sym)),
      x = sprintf("Can't evaluate `%s` in `env = %s`.", rlang::as_label(call_fun_sym), env_desc(env))
    ),
    call = error_call
  )
  fun <- check_is_function(
    maybe_function,
    message = c(
      "`call` must be a simple call.",
      x = sprintf("`call` is a malformed call to `%s`.", rlang::as_label(call_fun_sym)),
      x = sprintf(
        "`%s` is %s in `env = %s`, not a function.",
        rlang::as_label(call_fun_sym), obj_type_friendly(maybe_function), env_desc(env)
      )
    ),
    call = error_call
  )
  if (typeof(fun) %in% c("builtin", "special")) {
    typewriter_abort_invalid_input(
      message = c(
        '`call` must be a simple call to a function of type "closure".',
        i = sprintf("`call` is a simple call to function `%s`.", rlang::as_label(call_fun_sym)),
        x = sprintf('The function `%s` is of type "%s".', rlang::as_label(call_fun_sym), typeof(fun))
      ),
      call = error_call
    )
  }
  args <- mapply(
    arg = call_args,
    arg_name = rlang::names2(call_args),
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

  if (rlang::is_call(call_fun_sym, c("::", ":::"))) {
    call_fun_sym <- as.symbol(call_fun_sym[[3]])
  }
  fun_call <- rlang::call_modify(rlang::call2(call_fun_sym), ... = , !!!args)

  # As with {purrr} <partialised> functions, an alias needs to receive arguments
  # via `...` to support NSE check functions that use `substitute()` on their
  # arguments (e.g. via `rlang::caller_arg()`). We ensure that `...` contains
  # only one unnamed argument using `check_type_alias_dots()`. This dot is then
  # forwarded to the first argument of the type check `fun_call()`.
  body <- rlang::expr({
    # `sym("...")` prevents package note "... may be used in an incorrect context"
    typewriter::check_type_alias_dots(!!rlang::sym("..."))
    !!call_fun_sym <- !!fun
    !!fun_call
    ...elt(1L)
  })

  # Modify the body to return `fun_call()` instead of `...elt(1L)`
  if (return_call) {
    body[[length(body)]] <- NULL
  }

  out <- rlang::new_function(
    args = rlang::pairlist2(... = ),
    body = body,
    env = env
  )

  new_type_alias(
    fun = out,
    name = name %||% rlang::as_name(call_fun_sym),
    desc = desc %||% sprintf("An object checked using `%s`.", rlang::as_label(call)),
    bullets = bullets
  )
}

utils::globalVariables("!<-")

new_type_alias <- function(fun, name, desc, bullets) {
  structure(
    fun,
    class = c("typewriter_type_alias", "function"),
    name = name,
    desc = desc,
    bullets = bullets
  )
}

#' @export
is_type_alias <- function(x) {
  inherits(x, "typewriter_type_alias")
}

#' @export
format.typewriter_type_alias <- function(x, ...) {
  sprintf("<alias<%s>>", attr(x, "name"))
}

#' @export
print.typewriter_type_alias <- function(x, ...) {
  cat(format(x), "\n")
  cat(attr(x, "desc"), "\n")
  cat_bullets(attr(x, "bullets"))
}

# helpers ----------------------------------------------------------------------

# Allows `check_integer(error_call = rlang::caller_env())` to get the
# correct env when the check is wrapped in an alias.
#' @export
alias_caller <- function() {
  quoted_parent_frame
}
quoted_parent_frame <- quote(parent.frame(1L))

# dependencies -----------------------------------------------------------------

# These are functions used within a generated `type_alias()` and are not meant
# for external use.

#' Check that one unnamed argument is supplied to dots
#'
#' @description
#'
#' This function is used internally by functions generated via `type_alias()`.
#' It is not meant to be used outside of this context.
#'
#' @param ...
#'
#' An unnamed argument.
#'
#' @returns
#'
#' An error if too many dots, or an unnamed dot, are provided. `NULL` otherwise.
#'
#' @examples
#' foo <- function(...) { check_type_alias_dots(...) }
#' try(foo(10, 11))
#' @export
check_type_alias_dots <- function(...) {
  if (...length() != 1) {
    typewriter_abort(
      message = c(
        "Must supply exactly one argument to `...`.",
        x = sprintf("Supplied %i arguments to `...`.", ...length())
      ),
      call = rlang::caller_env(),
      class = "typewriter_error_type_alias_invalid_input"
    )
  }
  if (!is.null(...names())) {
    typewriter_abort(
      message = c(
        "Arguments to `...` must be unnamed.",
        x = sprintf("Argument `..1` is named %s.", encodeString(...names(), quote = '"'))
      ),
      call = rlang::caller_env(),
      class = "typewriter_error_type_alias_invalid_input"
    )
  }
}
