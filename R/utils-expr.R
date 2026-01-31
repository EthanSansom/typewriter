# call -------------------------------------------------------------------------

call_replace_args <- function(call, ...) {
  call2(call[[1]], ...)
}

call_prepend_args <- function(call, ...) {
  call_modify(call[1], ..., !!!call_args(call), .homonyms = "first")
}

# quo --------------------------------------------------------------------------

quo_is_namespaced_symbol <- function(x) {
  is_named_symbol(quo_get_expr(x))
}

is_namespaced_symbol <- function(x) {
  is_call(x, c("::", ":::"))
}

quo_is_modified_typer <- function(x) {
  is_modified_typer(quo_get_expr(x))
}

# `multi()` is currently the only implemented type-call modifier
is_modified_typer <- function(x) {
  is_call(x, "multi", ns = c("", "typewriter"))
}

quo_is_call_simple <- function(x, ns = NULL) {
  is_call_simple(quo_get_expr(x), ns = ns)
}
