# todos ------------------------------------------------------------------------

# Document: `const()`

# modifiers --------------------------------------------------------------------

#' @export
const <- function(x) {
  typewriter_stop_invalid_context(
    "Must only be used in a typing context (e.g. in `%<~%`, `typewriter::assign_typed()`, `typewriter::typed()`)."
  )
}

#' Mark a function argument as untyped
#'
#' @description
#'
#' Indicates that a typed function argument should be considered untyped.
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()]. This is useful for preventing the default value of an
#' argument from being considered a type-checking call.
#'
#' @param x `[expression]`
#'
#' An expression to use as the default argument value.
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being an untyped argument.
#'
#' @examples
#' # `typed()` and `%<~%` interpret any call as a type call. Here,
#' # `foo()` attempts to type-check `x` using `as.Date(x)`.
#' foo <- typed(function(x = as.Date("2020-01-01")) { x })
#' print(foo)
#'
#' # To use `as.Date("2020-01-01")` as the default value of `x`,
#' # you can explicitly mark it as untyped.
#' foo <- typed(function(x = untyped(as.Date("2020-01-01"))) { x })
#' print(foo)
#' @export
untyped <- function(x) {
  typewriter_stop_invalid_context()
}

#' Mark a function argument as being persistently typed
#'
#' @description
#'
#' Indicates that a function argument should not be assigned to a new
#' type within the body of a typed function. When used in a typed function
#' declaration, this modifier assigns it's argument using [assign_typed()]
#' within the body of the declared function.
#'
#' As a function author, this is useful to ensure that a given argument's
#' type is never inadvertently changed within the body of a function (e.g.
#' an argument is coerced from an integer to a double).
#'
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()].
#'
#' @details
#'
#' When an argument is `static()` calls to [assign_typed()] and
#' [rethrow_parent_assignment_error()] are inserted into the typed function's
#' body.
#'
#' ```r
#' # Typed function declaration
#' foo <- typed(function(x = static(chk::chk_integer())) {
#'   return(x)
#' })
#'
#' # Roughly equivalent to
#' foo <- function(x) {
#'   typewriter::rethrow_parent_assignment_error(
#'     typewriter::assign_typed(x, chk::chk_integer())
#'   )
#'   return(x)
#' }
#' ```
#'
#' @param x `[call]`
#'
#' A call to either:
#' - A type-checking function
#' - Other argument modifiers (e.g. [required()], [optional()], [maybe()])
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being a persistently typed argument.
#'
#' @examplesIf requireNamespace("chk", quietly = TRUE)
#' # This function inadvertently coerces `x` to a numeric
#' # vector, even though `x` began as an integer vector.
#' replace_na_bad <- typed(function(x = chk::chk_integer()) {
#'   x[is.na(x)] <- 0
#'   x
#' })
#' replace_na_bad(1L)
#' class(replace_na_bad(1L))
#'
#' # We can prevent this accident using `static()`, which
#' # will check that `x` remains an integer for the duration
#' # of `replace_na_bad()`.
#' replace_na_bad <- typed(function(x = static(chk::chk_integer())) {
#'   x[is.na(x)] <- 0
#'   x
#' })
#' try(replace_na_bad(1L))
#'
#' # If `x` is never coerced to another type within the function,
#' # then it proceeds without error.
#' replace_na_good <- typed(function(x = static(chk::chk_integer())) {
#'   x[is.na(x)] <- 0L
#'   x
#' })
#' replace_na_good(1L)
#' replace_na_good(c(5L, NA_integer_))
#' class(replace_na_good(c(5L, NA_integer_)))
#' @export
static <- function(x) {
  typewriter_stop_invalid_context()
}

#' Mark a function argument as being required
#'
#' @description
#'
#' Indicates that a function argument must be supplied. This modifier adds
#' an additional check which raises an informative error if an argument
#' with no default value is not supplied when the function is called.
#'
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()].
#'
#' @details
#'
#' When an argument is `required()` a call to [check_required_arg()] is inserted
#' into the typed function's body before the argument's type is checked.
#'
#' ```r
#' # Typed function declaration
#' foo <- typed(function(x = required(chk::chk_integer())) {
#'   return(x)
#' })
#'
#' # Roughly equivalent to
#' foo <- function(x) {
#'   typewriter::check_required_arg(x)
#'   chk::chk_integer(x)
#'   return(x)
#' }
#' ```
#'
#' @param x `[call]`
#'
#' A call to either:
#' - A type-checking function
#' - Other argument modifiers (e.g. [static()], [assigned()], [maybe()])
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being a required argument.
#'
#' @examples
#' # Define an assertion for an argument being numeric
#' assert_numeric <- function(x, x_name = rlang::caller_arg(x)) {
#'   if (!is.numeric(x)) {
#'     rlang::abort(sprintf("`%s` must be numeric.", x_name))
#'   }
#' }
#'
#' # Define a function with numeric-typed argument `x`
#' double <- typed(function(x = assert_numeric()) {
#'   x * 2
#' })
#'
#' # If `x` is un-supplied, we get an error from `assert_numeric()`,
#' # not `double()`
#' try(double())
#'
#' # We can correct this with `required()`
#' double2 <- typed(function(x = required(assert_numeric())) {
#'   x * 2
#' })
#' try(double2())
#' @export
required <- function(x) {
  typewriter_stop_invalid_context()
}

#' Mark a function argument as being optional
#'
#' @description
#'
#' Indicates that a function argument may be unsupplied. An optional argument
#' is only type-checked if it is supplied. If an optional argument is missing,
#' then the type check is skipped.
#'
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()].
#'
#' @param x `[call]`
#'
#' A call to either:
#' - A type-checking function
#' - Other argument modifiers (e.g. [static()], [assigned()], [maybe()])
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being an optional argument.
#'
#' @examples
#' # Define an assertion for an argument being a string
#' assert_string <- function(x, x_name = rlang::caller_arg(x)) {
#'   if (!rlang::is_string(x)) {
#'     rlang::abort(sprintf("`%s` must be a string.", x_name))
#'   }
#' }
#'
#' # Define a message printing function with a default message
#' cat_message <- typed(function(msg = optional(assert_string())) {
#'   if (missing(msg)) {
#'     cat("~No Message Here~\n")
#'   } else {
#'     cat(msg, "\n")
#'   }
#' })
#' cat_message("Hi!")
#' cat_message()
#'
#' # Without `optional()` we'd fail the `assert_string()` type-check
#' cat_message_bad <- typed(function(msg = assert_string()) {
#'   if (missing(msg)) {
#'     cat("~No Message Here~\n")
#'   } else {
#'     cat(msg, "\n")
#'   }
#' })
#' cat_message_bad("Hi!")
#' try(cat_message_bad())
#' @export
optional <- function(x) {
  typewriter_stop_invalid_context()
}

#' Mark a function argument as being potentially NULL
#'
#' @description
#'
#' Indicates that a function argument may be `NULL` A maybe argument
#' is only type-checked if it is not `NULL`. If a maybe argument is `NULL`,
#' then the type check is skipped.
#'
#' This is useful to accommodate the common pattern of using a `NULL`
#' argument value to indicate that the argument is missing.
#'
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()].
#'
#' @param x `[call]`
#'
#' A call to either:
#' - A type-checking function
#' - Other argument modifiers (e.g. [static()], [assigned()], [required()], [optional()])
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being a potentially `NULL` argument.
#'
#' @examplesIf requireNamespace("chk", quietly = TRUE)
#' # The `collapse` argument of `base::paste0()` is either
#' # `NULL` (the default) or a character string. We can use
#' # `maybe()` to maintain this requirement in a strictly
#' # typed version of `base::paste0()`.
#' my_paste0 <- typed(function(
    #'   ... = chk::chk_character(),
#'   collapse = maybe(chk::chk_string(NULL)),
#'   recycle0 = chk::chk_flag(FALSE)
#' ) {
#'   base::paste0(..., collapse = collapse, recycle0 = recycle0)
#' })
#' my_paste0(c("A", "B", "C"))
#' my_paste0(c("A", "B", "C"), collapse = "-")
#'
#' # Without `maybe()` the default use-case of `my_paste0()` would
#' # cause an error.
#' my_paste0_bad <- typed(function(
    #'   ... = chk::chk_character(),
#'   collapse = chk::chk_string(NULL),
#'   recycle0 = chk::chk_flag(FALSE)
#' ) {
#'   base::paste0(..., collapse = collapse, recycle0 = recycle0)
#' })
#' try(my_paste0_bad(c("A", "B", "C")))
#' @export
maybe <- function(x) {
  typewriter_stop_invalid_context()
}

#' Mark a function argument as potentially requiring type coercion
#'
#' @description
#'
#' Indicates that a function argument should be assigned to the value returned
#' by it's type-checking function. This is useful for when a type-checking
#' function coerces it's input to the correct type.
#'
#' This function may only be used when declaring a function with \code{\link{\%<~\%}}
#' or [typed()].
#'
#' @param x `[call]`
#'
#' A call to either:
#' - A type-checking function
#' - Other argument modifiers (e.g. [required()], [optional()], [maybe()])
#'
#' @returns
#'
#' If used outside of a function typing context (e.g. \code{\link{\%<~\%}} or [typed()])
#' this function raises an error. In a function typing context, returns an
#' expression marked as being an argument to re-assign during type-checking.
#'
#' @examples
#' # Define a check for whether an argument is factor-like
#' check_factorish <- function(x, x_name = rlang::caller_arg(x)) {
#'   if (is.factor(x) || is.character(x)) {
#'     return(as.factor(x))
#'   }
#'   rlang::abort(sprintf("`%s` must be a factor or a character.", x_name))
#' }
#'
#' # Retrieve the unique levels of a factor-like vector
#' levels2 <- typed(function(x = assigned(check_factorish())) {
#'   base::levels(x)
#' })
#' levels2(factor(c("A", "B", "C")))
#' levels2(c("A", "B", "C"))
#'
#' # `levels2()` relies on `check_factorish()` to coerce `x` into
#' # a factor variable. If we don't assign the result of `check_factorish(x)`
#' # to the argument `x`, then we get an unexpected result.
#' levels2_bad <- typed(function(x = check_factorish()) {
#'   base::levels(x)
#' })
#' levels2_bad(c("A", "B", "C"))
#' @export
assigned <- function(x) {
  typewriter_stop_invalid_context()
}

# helpers ----------------------------------------------------------------------

is_untyped_call <- function(x) {
  rlang::is_call(x, name = "untyped", ns = c("", "typewriter"))
}

is_modified_call <- function(x) {
  # `const()` and `untyped()` are handled separately
  modifiers <- c("required", "optional", "maybe", "static", "assigned")
  rlang::is_call(x, name = modifiers, ns = c("", "typewriter"))
}

get_modifier_name <- function(modifier_call) {
  modifier <- modifier_call[[1]]
  switch(
    expr_type(modifier),
    symbol = rlang::as_name(modifier),
    namespaced = rlang::as_name(modifier[[3]]),
    typewriter_abort(
      sprintf("Unexpected `modifier_call = %s`.", rlang::as_label(modifier_call)),
      internal = TRUE
    )
  )
}

typewriter_stop_invalid_context <- function(message = NULL) {
  typewriter_abort(
    message %||% "Must only be used in a function typing context (e.g. when calling `%<~%` or `typewriter::typed()`).",
    class = "typewriter_error_invalid_context",
    call = rlang::caller_env()
  )
}
