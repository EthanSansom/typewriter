# typer ------------------------------------------------------------------------

# A "typer" is a symbol, call, or a modified call (e.g. `multi(...)`) which is
# used to type an object or argument. For calls, we validate the following:
#
# - Must be a simple call (e.g. not `foo$bar()`)
# - Every argument must be evaluable (we allow `quote(<arg>)` to circumvent)
#
# The "typewriter_typer" class data is a list with:
# - `value = <obj>` : The default value used for the typed argument or function
# - `call = <call>/listof<call>` : The call(s) used to type-check the object

typer_expr <- function(typer, arg) {
  UseMethod("typer_expr")
}

#' @export
typer_expr.typewriter_typer <- function(typer, arg) {
  call_prepend_args(typer$call, arg)
}

#' @export
typer_expr.typewriter_multi <- function(typer, arg) {
  call2("{", !!!map(typer$call, call_prepend_args, arg))
}

typer_value <- function(typer) {
  typer$value
}

typer_has_value <- function(typer) {
  !is_zap(typer$value)
}

# multi ------------------------------------------------------------------------

# Package philosophy:
# - We check that an object is a wrapper/modifier using is_call(name, ns = c("", "typewriter"))
# - We let the wrapper/modifier evaluate it's own arguments and return a useful argument
multi <- function(...) {
  error_call <- current_env()
  dots <- enquos(...)

  init <- map_lgl(dots, quo_is_call, name = "init", ns = c("", "typewriter"))
  if (any(init[-1])) {
    stop("`init()` may only be suppled to `..1`") # TODO
  }

  multi <- map_lgl(dots, is_call, name = "multi", ns = c("", "typewriter"))
  if (any(multi)) {
    stop("`multi(...)` may not contain calls to `multi()`.") # TODO
  }

  if (first(init)) {
    value <- eval_tidy(first(dots))
    dots <- dots[-1]
    dots_names <- paste0("..", 1 + seq_along(dots))
  } else {
    value <- zap()
    dots_names <- paste0("..", seq_along(dots))
  }

  typers <- map2(
    dots,
    dots_names,
    \(dot, dot_name) check_typer(dot, dot_name, error_call = error_call)
  )

  default_values <- map_lgl(typers, \(typer) !is_zap(typer$value))
  if (any(default_values)) {
    stop("Calls in `multi(...)` may not have an unnamed first argument.")
    # TODO: Use `init()` to provide a default argument
  }

  structure(
    list(value = value, call = map(typers, `[[`, "call")),
    class = c("typewriter_multi", "typewriter_typer")
  )
}

# TODO: Document: Just used as a wrapper with `multi()`,
#       e.g. `multi(init(10), check_scalar, check_numeric)`
#' @export
init <- function(x) {
  x_label <- as_label(enexpr(x))
  try_fetch(
    x,
    error = function(cnd) {
      abort(sprintf("Can't evaluate `x = %s`.", x_label), parent = cnd)
    }
  )
}

# check ------------------------------------------------------------------------

check_typer <- function(
    x,
    x_name = caller_arg(x),
    error_call = caller_env()
) {
  if (quo_is_symbol(x) || quo_is_namespaced_symbol(x)) {
    return(symbol_to_typer(x))
  } else if (quo_is_modified_typer(x)) {
    return(eval_tidy(x))
  } else if (quo_is_call_simple(x)) {
    return(call_to_typer(x, error_call = error_call))
  }

  what <- "a named function or simple call"
  if (quo_is_call(x)) {
    abort(
      c(
        sprintf("`%s` must be %s, not a complex call.", x_name, what),
        i = "See `?rlang::is_call_simple` for a description of complex calls."
      ),
      call = error_call,
      class = "typewriter_error_input_mistyped"
    )
  }

  stop_input_type(
    x = quo_get_expr(x),
    what = what,
    arg = x_name,
    call = error_call,
    class = "typewriter_error_input_mistyped"
  )
}

symbol_to_typer <- function(x) {
  structure(
    list(value = zap(), call = call2(quo_get_expr(x))),
    class = "typewriter_typer"
  )
}

call_to_typer <- function(
    x,
    x_name = caller_arg(x),
    error_call = caller_env()
) {
  call <- quo_get_expr(x)
  args <- call_args(call)
  call_name <- call_name(call)

  if (length(args) && !first_is_named(args)) {
    value <- check_quo_evaluable(
      quo = first(args),
      msg = sprintf(
        "Can't evaluate the first argument (`%s`) of `%s(...)`.",
        as_label(first(args)), call_name
      ),
      error_call = error_call
    )
    args <- args[-1]
  } else {
    value <- zap()
  }

  args <- map2(
    args,
    sprintf(
      "Can't evaluate argument %s of `%s(...)`.",
      args_labels(args), call_name
    ),
    \(quo, msg) check_quo_evaluable(quo, msg, error_call = error_call)
  )

  structure(
    list(value = value, call = call_replace_args(call, !!!args)),
    class = "typewriter_typer"
  )
}
