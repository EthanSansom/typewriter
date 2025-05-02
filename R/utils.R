is_try_error <- function(x) {
  class(x)[[1]] == "try-error"
}

is_empty <- function(x) {
  length(x) == 0L
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(1L), ...)
}

expr_type <- function(expr) {
  if (is.symbol(expr)) {
    "symbol"
  } else if (rlang::is_call(expr, c("::", ":::"))) {
    "namespaced"
  } else if (rlang::is_syntactic_literal(expr)) {
    "literal"
  } else if (rlang::is_call_simple(expr)) {
    "simple_call"
  } else if (rlang::is_call(expr)) {
    "call"
  } else {
    typeof(expr)
  }
}
