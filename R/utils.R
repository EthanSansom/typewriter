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

cat_bullets <- function(bullets) {
  if (is_empty(bullets)) {
    return(invisible())
  } else if (rlang::is_installed("cli")) {
    writeLines(cli::format_bullets_raw(bullets))
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
