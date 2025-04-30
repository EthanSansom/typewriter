# todos ------------------------------------------------------------------------

## Get standalone-obj-type from {rlang} for `obj_type_friendly()`
# - alter the error messages in this script

## Update `check_quo_is_callish()` to handle cases where the call/function
#  doesn't exist in the quosure's environment.

# check ------------------------------------------------------------------------

#' Check that an object is a symbol
#'
#' @param x An object to check.
#' @param x_name Name used in error message.
#' @param call Call used in error condition.
#'
#' @returns A symbol.
#' @keywords internal
#' @noRd
check_is_symbol <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {

  if (rlang::is_symbol(x)) {
    return(x)
  }
  typewriter_abort_invalid_input(
    # TODO: Add "not {obj_type_friendly(x)}"
    sprintf("`%s` must be a symbol.", x_name),
    call = call
  )
}

#' Check that a quosure is a call or a function provided by name
#'
#' @param quosure A quosure.
#' @param quosure_name Name used in error message.
#' @param call Call used in error condition.
#'
#' @returns
#'
#' A call expression (not a quosure!) If `quosure` is a quoted function, then
#' it is converted to a call with `rlang::call2()` prior to returning.
#'
#' @keywords internal
#' @noRd
check_quo_is_callish <- function(
    quosure,
    quosure_name = rlang::caller_arg(quosure),
    call = rlang::caller_env()
  ) {

  if (!rlang::is_quosure(quosure)) {
    typewriter_abort("`quosure` must be a quosure.", internal = TRUE)
  }

  # TODO: Need to check two things:
  # 1. For a named function like `ns::foo` or `foo`, does `foo` exist in the quosure env?
  # 2. For a call like `ns::bar()` and `bar()`, does `bar` exist in the quosure env?

  expr <- rlang::quo_get_expr(quosure)
  if (quo_is_named_function(quosure)) {
    return(rlang::call2(expr))
  }
  if (rlang::is_call_simple(expr)) {
    return(expr)
  }
  if (rlang::is_call(expr)) {
    typewriter_abort_invalid_input(
      # TODO: Add more examples of what a simple call is...
      sprintf("`%s` must be a simple call.", quosure_name),
      call = call
    )
  }
  typewriter_abort_invalid_input(
    # TODO: Add second line, x = `quosure_name = rlang::as_label(expr)` is a BLANK.
    sprintf("`%s` must be a simple call or function provided by name.", quosure_name),
    call = call
  )
}

check_is_simple_call <- function(
    x,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {
  # TODO: `foo[[1]]` and `foo$bar` are simple calls, so we don't catch them here,
  #       but I think we CAN prevent special and primitive calls to fix this issue.
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
