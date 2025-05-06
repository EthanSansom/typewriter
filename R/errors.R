# todos ------------------------------------------------------------------------

# TODO: Check is string and check is character for `alias(name, desc, bullets)`

# check ------------------------------------------------------------------------

check_is_symbol <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {
  if (rlang::is_symbol(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    sprintf("`%s` must be a symbol, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

check_is_environment <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  if (is.environment(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    sprintf("`%s` must be an environment, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

check_is_simple_call <- function(
    x,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {
  if (rlang::is_call_simple(x)) {
    return(x)
  }
  if (rlang::is_call(x)) {
    typewriter_abort_invalid_input(
      message %||% c(
        sprintf("`%s` must be a simple call (e.g. `foo()` or `ns::foo()`, not `bar$foo()`).", x_name),
        x = sprintf("`%s = %s` is a complex call.", x_name, rlang::as_label(x))
      ),
      call = call
    )
  }
  typewriter_abort_invalid_input(
    message %||% sprintf("`%s` must be a call, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

check_is_evaluable <- function(
    x,
    env,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {
  if (rlang::is_quosure(x)) {
    env <- rlang::quo_get_env(x)
    x <- rlang::quo_get_expr(x)
  }

  rlang::try_fetch(
    eval(x, envir = env),
    error = function(cnd) {
      typewriter_abort_invalid_input(
        message = message %||% sprintf("Can't evaluate object `%s`.", x_name),
        call = call,
        parent = cnd
      )
    }
  )
}

check_is_function <- function(
    x,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {
  if (is.function(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    message = message %||% sprintf("`%s` must be a function, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

assert_dots_named <- function(..., message = NULL, call = rlang::caller_env()) {

  named_dots_at <- rlang::have_name(rlang::enexprs(...))
  if (all(named_dots_at)) {
    return(invisible())
  }
  unnamed_dots <- paste0("..", which(!named_dots_at))
  n_unnamed_dots <- length(unnamed_dots)

  if (n_unnamed_dots > 5) {
    unnamed_dots <- paste0("c(", commas(unnamed_dots[1:4]), ", ..., ", unnamed_dots[n_unnamed_dots], ")")
  } else if (n_unnamed_dots > 1) {
    unnamed_dots <- paste0("c(", commas(unnamed_dots), ")")
  }

  typewriter_abort_invalid_input(
    message = c(
      "Arguments supplied to `...` must be named.",
      x = sprintf("Argument%s `%s` are unnanmed.", ngettext(n_unnamed_dots, "", "s"), unnamed_dots)
    ),
    call = call
  )
}

# stop -------------------------------------------------------------------------

stop_malformed_call <- function(
    x,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {
  unamed_at <- !rlang::have_name(rlang::call_args(x))
  n_unnamed <- sum(unamed_at)

  typewriter_abort(
    message = message %||% c(
      sprintf("All arguments to `%s` must be named (except potentially the first).", x_name),
      x = sprintf(
        "`%s = %s` has %i unnamed argument%s %s.",
        x_name, rlang::as_label(x), n_unnamed, ngettext(n_unnamed, "", "s"),
        at_positions(unamed_at)
      )
    ),
    call = call,
    class = "typewriter_error_invalid_input"
  )
}

# abort ------------------------------------------------------------------------

typewriter_abort_invalid_input <- function(
    message,
    call = rlang::caller_env(),
    parent = NULL
) {
  rlang::abort(
    message = message,
    call = call,
    parent = parent,
    class = c("typewriter_error", "typewriter_error_invalid_input")
  )
}

typewriter_abort <- function(
    message,
    class = character(),
    call = rlang::caller_env(),
    parent = NULL,
    internal = FALSE
  ) {
  rlang::abort(
    message = message,
    class = c("typewriter_error", class),
    call = call,
    parent = parent,
    internal = internal
  )
}

# messaging --------------------------------------------------------------------

at_positions <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at postion ", "at positions ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

commas <- function(x) {
  paste(x, collapse = ", ")
}
