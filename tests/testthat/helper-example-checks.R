# checks -----------------------------------------------------------------------

# {typewriter} relies on user/package-created checks for typing, so we define
# a few examples here, used in tests.

check_integer <- function(x, len = NULL) {
  if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
    rlang::abort(
      message = sprintf("Invalid object %s", rlang::caller_arg(x)),
      class = "invalid_input",
      call = rlang::caller_env()
    )
  }
}

check_character <- function(x, len = NULL) {
  if (!(is.character(x) && (is.null(len) || length(x) == len))) {
    rlang::abort(
      message = sprintf("Invalid object %s", rlang::caller_arg(x)),
      class = "invalid_input",
      call = rlang::caller_env()
    )
  }
}

check_funish <- function(x) {
  if (is.function(x) || rlang::is_formula(x, lhs = FALSE)) {
    return(rlang::as_function(x))
  }
  rlang::abort(
    message = sprintf("Invalid object %s", rlang::caller_arg(x)),
    class = "invalid_input",
    call = rlang::caller_env()
  )
}

# aliases ----------------------------------------------------------------------

check_integer_alias <- type_alias(check_integer())
