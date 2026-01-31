as_labels <- function(x) {
  map_chr(x, as_label)
}

args_labels <- function(x) {
  locs <- seq_along(x)
  names <- names2(x)
  labels <- as_labels(x)

  unnamed <- names == ""
  out <- sprintf("`%s = %s`", names, labels)
  out[unnamed] <- sprintf("%i (`%s`)", locs[unnamed], labels[unnamed])
  out
}

backtick <- function(x) {
  paste0("`", x, "`")
}
