new_uninitialized <- function() {
  structure(list(), class = "typewriter_uninitialized")
}

#' @export
is_uninitialized <- function(x) {
  inherits(x, "typewriter_uninitialized")
}

#' @method print typewriter_uninitialized
#' @keywords internal
#' @export
print.typewriter_uninitialized <- function(x, ...) {
  cat("<uninitialized>", "\n", append = TRUE)
}
