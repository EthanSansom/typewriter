unattr <- function(x) {
  attributes(x) <- NULL
  x
}

first <- function(x) {
  x[[1]]
}

last <- function(x) {
  x[[length(x)]]
}

first_is_named <- function(x) {
  have_name(x)[[1]]
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

map_chr <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1L), ...)
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
