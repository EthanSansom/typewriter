# aliasing ---------------------------------------------------------------------

#' @export
type_alias <- function(call, name = NULL, desc = NULL, bullets = NULL) {
  call <- check_is_simple_call(x = rlang::enexpr(call), x_name = "call")
  check_is_string(name, null_ok = TRUE)
  check_is_string(desc, null_ok = TRUE)
  check_is_character(bullets, null_ok = TRUE)

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
  # forwarded to the first argument of the type check `call`.
  body <- rlang::expr({
    # `sym("...")` prevents package note "... may be used in an incorrect context"
    typewriter::check_type_alias_dots(!!rlang::sym("..."))
    !!call_fun_sym <- !!fun
    !!fun_call
    ...elt(1L)
  })

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
  quote(parent.frame(1L))
}

# dependencies -----------------------------------------------------------------

# These are functions used within a generated `type_alias()` and are not meant
# for external use.

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
