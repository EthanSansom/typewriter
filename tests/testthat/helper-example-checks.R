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

# aliases ----------------------------------------------------------------------

check_integer_alias <- alias(check_integer())
