new_uninitialized <- function(name = NULL, desc = NULL, bullets = NULL) {
  structure(
    list(),
    name = name,
    desc = desc,
    bullets = bullets,
    class = "typewriter_uninitialized"
  )
}

#' @export
is_uninitialized <- function(x) {
  inherits(x, "typewriter_uninitialized")
}

#' @export
format.typewriter_uninitialized <- function(x, ...) { # nocov start
  name <- attr(x, "name")
  if (is.null(name)) {
    "<uninitialized>"
  } else {
    sprintf("<uninitialized<%s>>", name)
  }
} # nocov end

#' @export
print.typewriter_uninitialized <- function(x, ...) {
  desc <- attr(x, "desc")
  cat(format(x), "\n")
  if (!is.null(desc)) { cat(desc, "\n") }
  cat_bullets(attr(x, "bullets"))
}
