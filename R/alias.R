# todos ------------------------------------------------------------------------

# TODO: Rename `alias` to `type_alias`, since `alias` is a function in {stats}

# TODO: What do we do with the alias when the first argument is unnamed?
# - This is normally the initialization value, but not in our context
# - I feel like we should throw an error, since we're expecting the same
#   kind of function as in `%<~%`, in which the first argument should be
#   an initialization value...

# TODO: Implement the `bullets` argument of the `alias`, which we'll need for implementing
#       list_of and other aliases.

# functions --------------------------------------------------------------------

#' @export
alias <- function(call, name = NULL, desc = NULL, bullets = NULL) {
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

  body <- rlang::expr({
    if (...length() != 1) {
      typewriter::stop_alias_invalid_input(c(
        "Must supply exactly one argument to `...`.",
        x = sprintf("Supplied %i arguments to `...`.", ...length())
      ))
    }
    if (!is.null(...names())) {
      typewriter::stop_alias_invalid_input(c(
        "Arguments to `...` must be unnamed.",
        x = sprintf("Argument `..1` is named %s.", encodeString(...names(), quote = '"'))
      ))
    }
    !!call_fun_sym <- !!fun
    !!fun_call
    ...elt(1L)
  })

  out <- rlang::new_function(
    args = rlang::pairlist2(... = ),
    body = body,
    env = env
  )

  new_alias(
    fun = out,
    name = name %||% rlang::as_name(call_fun_sym),
    desc = desc %||% sprintf("An object checked using `%s`.", rlang::as_label(call)),
    bullets = bullets
  )
}

utils::globalVariables("!<-")

#' @export
is_alias <- function(x) {
  inherits(x, "typewriter_alias")
}

#' @export
print.typewriter_alias <- function(x, ...) {
  cat(sprintf("<alias<%s>>\n", attr(x, "name")))
  cat(paste0(attr(x, "desc"), "\n"))
  cat_alias_bullets(x)
}

cat_alias_bullets <- function(x) {
  bullets <- attr(x, "bullets")
  if (rlang::is_installed("cli")) {
    writeLines(cli::cli_fmt(cli::cli_bullets(bullets)))
  } else {
    # {cli} interprets names in `cli_marks` as bullet marks and ignores all
    # other names, so we do the same here to match.
    cli_marks <- c(" ", "i", "x", "v", "!", "*", ">")
    bullet_names <- rlang::names2(bullets)
    cli_bullet <- bullet_names %in% cli_marks
    bullets[cli_bullet] <- paste(bullet_names[cli_bullet], bullets[cli_bullet])
    writeLines(bullets)
  }
}

new_alias <- function(fun, name, desc, bullets) {
  structure(
    fun,
    class = c("typewriter_alias", "function"),
    name = name,
    desc = desc,
    bullets = bullets
  )
}

# helpers ----------------------------------------------------------------------

# NOTE: Allows `check_integer(error_call = rlang::caller_env())` to get the
# correct env when the check is wrapped in an alias.
#' @export
alias_caller <- function() {
  quote(parent.frame(1L))
}

# Reminder: `alias_caller()` works because the evaluation of a default argument
# is different from that of a supplied argument. Supplied argument promises
# reference the environment of the caller, which a default argument is evaluated
# within the function.
if (FALSE) {
  test_eval <- function(call = parent.frame(1L)) {
    print(call)
    print(parent.frame(1L))
  }
  foo_1 <- function() test_eval()
  foo_2 <- function() test_eval(call = parent.frame(1L))

  foo_1() # Default `call = parent.frame(1L)` evaluated within it's function `test_eval()`
  foo_2() # Default `call = parent.frame(1L)` evaluated within it's caller `foo_2()`
}

# dependencies -----------------------------------------------------------------

# These are functions used within a generated `alias()` and are not meant for
# external use.

#' @export
stop_alias_invalid_input <- function(message) {
  typewriter_abort(
    message = message,
    call = rlang::caller_env(),
    class = "typewriter_error_alias_invalid_input"
  )
}
