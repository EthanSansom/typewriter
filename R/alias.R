# todos ------------------------------------------------------------------------

# TODO: New rules
# - I use `error_call` since `call` is so often the name of an object in this package

# TODO: `pillar_type_sum`, maybe rlang:::rlang_type_sum has a standalone version
# - I want something to summarize the `ptype` of an `alias()`
# - See `rlang_type_sum()` https://github.com/r-lib/rlang/blob/bae1fe18e81dd1b4280a2dabae4ac94c9aeac190/R/types.R#L478

# alias ------------------------------------------------------------------------

#' Create a type alias
#'
#' @param call A call
#' @param name A string
#' @param env An environment
#' @param returns A result
#'
#' @returns An alias
#'
#' @examples
#' type_alias(foo)
#'
#' @export
type_alias <- function(
    call,
    name = NULL,
    env = caller_env(),
    returns = c("input", "result")
) {
  typer <- check_typer(enquo(call), x_name = "call")
  name <- check_string(name, allow_null = TRUE)
  env <- check_environment(env)
  returns <- arg_match0(returns, c("input", "result"))

  name <- name %||% "object" # TODO: What should default name be?

  # TODO: Document this, following `purrr::partial()` documentation
  #
  # We supply `...` to the check functions to support NSE evaluation
  # (e.g. substitute()) within those functions. `check_type_alias_dots()`
  # checks that only one unnamed argument is supplied to `...`.
  # See: https://github.com/tidyverse/purrr/blob/90e74842bcd76d310e578351b27501408c5ed1f9/R/adverb-partial.R#L4
  result <- if (returns == "input") list(sym("..1")) else list()
  body <- expr({
    typewriter::check_type_alias_dots(...)
    !!typer_expr(typer, sym("..."))
    !!!result
  })

  fun <- function(...) {}
  body(fun) <- body
  environment(fun) <- env

  new_type_alias(fun, name)
}

# misc -------------------------------------------------------------------------

utils::globalVariables("!<-")

new_type_alias <- function(fun, name) {
  structure(
    fun,
    name = name,
    class = c("typewriter_type_alias", "function")
  )
}

#' @export
is_type_alias <- function(x) {
  inherits(x, "typewriter_type_alias")
}

#' @export
format.typewriter_type_alias <- function(x, ...) {
  sprintf("<alias<%s>>", attr(x, "name"))
}

#' @export
print.typewriter_type_alias <- function(x, ...) {
  cat(format(x), "\n")
  print(unattr(unclass(x))) # TODO: We won't print the function, probably pillar::type_sum
}

# helpers ----------------------------------------------------------------------

# Allows `check_integer(error_call = rlang::caller_env())` to get the
# correct env when the check is wrapped in an alias.
#' @export
alias_caller <- function() {
  quoted_parent_frame
}
quoted_parent_frame <- quote(parent.frame(1L))

# dependencies -----------------------------------------------------------------

# These are functions used within a generated `type_alias()` and are not meant
# for external use.

#' Check that one unnamed argument is supplied to dots
#'
#' @description
#'
#' This function is used internally by functions generated via `type_alias()`.
#' It is not meant to be used outside of this context.
#'
#' @param ...
#'
#' An unnamed argument.
#'
#' @returns
#'
#' An error if too many dots, or an unnamed dot, are provided. `NULL` otherwise.
#'
#' @examples
#' foo <- function(...) { check_type_alias_dots(...) }
#' try(foo(10, 11))
#' @export
check_type_alias_dots <- function(...) {
  if (...length() != 1) {
    typewriter_abort(
      message = c(
        "Must supply exactly one argument to `...`.",
        x = sprintf("Supplied %i arguments to `...`.", ...length())
      ),
      call = rlang::caller_env(),
      class = "typewriter_error_type_alias_invalid_input"
    )
  }
  if (!is.null(...names())) {
    typewriter_abort(
      message = c(
        "Arguments to `...` must be unnamed.",
        x = sprintf("Argument `..1` is named %s.", encodeString(...names(), quote = '"'))
      ),
      call = rlang::caller_env(),
      class = "typewriter_error_type_alias_invalid_input"
    )
  }
}
