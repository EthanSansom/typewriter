# todos ------------------------------------------------------------------------

# Unify names (no `_is_`) and use `stop_input_type()` where applicable.

# check ------------------------------------------------------------------------

check_quo_evaluable <- function(quo, msg, error_call = caller_env()) {
  try_fetch(
    eval_tidy(quo),
    error = function(cnd) abort(msg, call = error_call, parent = cnd)
  )
}

check_is_character <- function(
    x,
    allow_null = FALSE,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  if ((allow_null && is.null(x)) || is.character(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    sprintf("`%s` must be a character, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

check_string <- function(
    x,
    allow_null = FALSE,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  if ((allow_null && is.null(x)) || rlang::is_string(x)) {
    return(x)
  }
  if (!is.character(x)) {
    typewriter_abort_invalid_input(
      sprintf("`%s` must be a string, not %s.", x_name, obj_type_friendly(x)),
      call = call
    )
  }
  what <- if (length(x) == 1L) "character `NA`" else sprintf("length %i character", length(x))
  typewriter_abort_invalid_input(
    c(
      sprintf("`%s` must be a string.", x_name),
      x = sprintf("`%s` is a %s.", x_name, what)
    ),
    call = call
  )
}

check_is_bool <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  if (rlang::is_bool(x)) {
    return(x)
  }
  if (!is.logical(x)) {
    typewriter_abort_invalid_input(
      sprintf("`%s` must be `TRUE` or `FALSE`, not %s.", x_name, obj_type_friendly(x)),
      call = call
    )
  }
  what <- if (length(x) == 1L) "`NA`" else sprintf("length %i logical vector", length(x))
  typewriter_abort_invalid_input(
    c(
      sprintf("`%s` must be `TRUE` or `FALSE`.", x_name),
      x = sprintf("`%s` is a %s.", x_name, what)
    ),
    call = call
  )
}

check_is_symbol <- function(
    x,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
  ) {
  if (rlang::is_symbol(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    message %||% sprintf("`%s` must be a symbol, not %s.", x_name, obj_type_friendly(x)),
    call = call
  )
}

check_environment <- function(
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

check_is_call_simple <- function(
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
  rlang::try_fetch(
    rlang::eval_tidy(x, env = env),
    error = function(cnd) {
      typewriter_abort_invalid_input(
        message = message %||% sprintf("Can't evaluate object `%s`.", x_name),
        call = call,
        parent = cnd
      )
    }
  )
}

assert_dots_named <- function(..., .message = NULL, .call = rlang::caller_env()) {

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
    message = .message %||% c(
      "Arguments supplied to `...` must be named.",
      x = sprintf(
        "Argument%s `%s` %s unnanmed.",
        ngettext(n_unnamed_dots, "", "s"), unnamed_dots, ngettext(n_unnamed_dots, "is", "are")
      )
    ),
    call = .call
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
