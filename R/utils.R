is_try_error <- function(x) {
  class(x)[[1]] == "try-error"
}

quo_is_named_function <- function(quosure) {
  expr <- rlang::quo_get_expr(quosure)
  (is.symbol(expr) || rlang::is_call(expr, c("::", ":::"))) && is.function(rlang::eval_tidy(quosure))
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}
