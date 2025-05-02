# todos ------------------------------------------------------------------------

# TODO: Polish `as_typed_function()`, with will be a very useful function to
#       have for post-hoc function typing so we do need it.

# function typing --------------------------------------------------------------

#' @export
typed <- function(call, env = rlang::caller_env()) {
  call <- rlang::enexpr(call)
  if (!rlang::is_call(call, "function", ns = "")) {
    if (rlang::is_call(call)) {
      typewriter_abort_invalid_input(
        sprintf("`call` must be a defused call to `function`, not a call to `%s`.", rlang::as_label(call[[1]]))
      )
    }
    typewriter_abort_invalid_input(
      sprintf("`call` must be a defused call to `function`, not %s.", obj_type_friendly(call))
    )
  }
  new_typed_function(
    args = call[[2]],
    body = call[[3]],
    env = env
  )
}

# TODO: This needs work and testing!
#' @export
as_typed_function <- function(.fun, ...) {
  check_is_function(.fun)
  assert_dots_named(...)
  if (is_typed_function(.fun)) {
    .fun <- untype_function(.fun)
  }

  # TODO: We'll need to handle `...` specially maybe?
  body <- rlang::fn_body(.fun)
  args <- map(rlang::fn_fmls(.fun), \(fml) rlang::call2(quote(untyped), fml))

  typed_args <- rlang::enexprs(...)
  typed_args_names <- rlang::names2(typed_args)

  if (!all(typed_args_names %in% names(args))) {
    bad_arg_name <- typed_args_names[[which.min(typed_args_names %in% names(args))]]
    typewriter_abort(
      message = c(
        "Can't convert `.fun` to a typed function.",
        x = "Argument `%s` supplied to `...` is not a formal argument of `.fun`."
      )
    )
  }

  args[names(typed_args)] <- typed_args

  new_typed_function(
    args = args,
    body = body,
    env = rlang::fn_env(.fun)
  )
}

new_typed_function <- function(args, body, env, error_call = rlang::caller_env()) {
  args_names <- names(args)
  type_calls <- list()
  new_args <- args

  typed_args_names <- character()
  typed_args_descs <- character()

  # TODO: I think we should pick up and record the description of each typed
  #       argument as we go (e.g. using the <alias> "desc" for aliases and the
  #       type_check call for non-aliases). We can also take arguments with
  #       optional, maybe, and required.
  #
  # `x`   "An integer vector" [optional, maybe]
  # `vvv` "An object checked by `check_integer(vvv, len = 1L)` [required]

  is_reserved_arg <- map_lgl(args, function(arg) ".__dot__." %in% all.vars(arg))
  if (any(is_reserved_arg)) {
    bad_arg_pos <- which.min(is_reserved_arg)
    typewriter_abort_invalid_input(
      message = c(
        "Arguments in `call` may not contain the symbol `.__dot__`.",
        x = sprintf("`call` argument at position `%i` contains the symbol `.__dot__.`", bad_arg_pos),
        i = "The symbol `.__dot__.` is reversed for internal use in <typed> functions."
      ),
      call = error_call
    )
  }

  is_defused_alias <- function(call) {
    is_named_symbol(call) && is_alias(try(eval(call, env), silent = TRUE))
  }

  stop_incompatible_dots_modifier <- function(modifier_name) {
    typewriter_abort_invalid_input(
      message = c(
        "Can't convert `call` into a typed function.",
        x = sprintf("Argument `...` of `call` contains a call to `%s()`.", modifier_name),
        x = sprintf("Typed function dots can't be %s.", modifier_name)
      ),
      call = error_call
    )
  }

  typed_arg_desc <- function(type_call, arg_modifiers) {
    type_call_fun <- type_call[[1]]
    suffix <- if (is_empty(arg_modifiers)) "" else paste0(" [", commas(sort(unique(arg_modifiers))), "]")
    maybe_alias <- eval(type_call_fun, env)
    if (is_alias(maybe_alias)) {
      paste(attr(maybe_alias, "desc"), suffix)
    } else {
      sprintf("An object checked by `%s`.%s", rlang::as_label(type_call), suffix)
    }
  }

  # TODO: Document what this mega loop does
  #
  # For each argument in `function(arg1 = <expr1>, arg2 = <expr2>, ...)` we:
  #
  for (i in seq_along(args)) {

    maybe_type_call <- args[[i]]
    if (rlang::is_missing(maybe_type_call)) {
      next
    }
    if (is_defused_alias(maybe_type_call)) {
      maybe_type_call <- rlang::call2(maybe_type_call)
    }
    # TODO: We may want to raise an error here and mandate that `untyped()` be
    #       used explicitly to stop complex calls. We may also want to skip
    #       calls to `base::c`.
    if (!rlang::is_call_simple(maybe_type_call)) {
      next
    }

    arg_name <- args_names[[i]]
    arg_is_dots <- arg_name == "..."

    # In the <typed> function, we substitute `.__dot__.` with `..{i}` (e.g. `..1`)
    # when type checking each of the `...` arguments, so that error messages can
    # refer to a given dot by name.
    arg_sym <- if (arg_is_dots) quote(.__dot__.) else rlang::sym(arg_name)
    arg_modifiers <- character()

    # Strips and records modifiers, e.g. `maybe(required(f(x)))` -> `f(x)`. We
    # enforce that modifiers contain 1 argument and that the argument is a call.
    while (is_modified_call(maybe_type_call)) {

      n_args <- length(rlang::call_args(maybe_type_call))
      if (n_args != 1) {
        malformed_fun <- rlang::as_label(maybe_type_call[[1]])
        typewriter_abort_invalid_input(
          message = c(
            "Can't convert `call` into a typed function.",
            x = sprintf("Argument `%s` of `call` contains a malformed call to `%s()`.", arg_name, malformed_fun),
            x = sprintf("`%s` requires 1 argument, but %i were supplied.", malformed_fun, n_args)
          ),
          call = error_call
        )
      } else if (!rlang::is_call(maybe_type_call[[2]])) {
        malformed_fun <- rlang::as_label(maybe_type_call[[1]])
        typewriter_abort_invalid_input(
          message = c(
            "Can't convert `call` into a typed function.",
            x = sprintf(
              "Argument `%s` of `call` contains a malformed call to `%s()`.",
              arg_name, malformed_fun
            ),
            x = sprintf(
              "`%s` requires a call as it's first argument, not %s.",
              malformed_fun, obj_type_friendly(maybe_type_call[[2]])
            )
          ),
          call = error_call
        )
      }

      modifier <- get_modifier_name(maybe_type_call)
      arg_modifiers <- c(arg_modifiers, modifier)
      maybe_type_call <- maybe_type_call[[2]]
    }

    if ("required" %in% arg_modifiers) {
      if ("optional" %in% arg_modifiers) {
        typewriter_abort_invalid_input(
          message = c(
            "Can't convert `call` into a typed function.",
            x = sprintf("Argument `%s` of `call` contains calls to `required()` and `optional()`.", arg_name),
            x = "Typed function arguments may be either optional or required, not both."
          ),
          call = error_call
        )
      }
      if (arg_is_dots) {
        stop_incompatible_dots_modifier(modifier_name = "required")
      }
      check <- rlang::call2("check_required_arg", arg_sym, .ns = "typewriter")
      type_calls <- append(type_calls, check)
    }

    # Converts `function(arg = untyped(<expr>))`, to `function(arg = <expr>)`.
    # We skip the `optional()` and `maybe()` modifiers for untyped arguments,
    # since they have no effect on the typed function in this case.
    if (is_untyped_call(maybe_type_call)) {
      if (arg_is_dots) {
        stop_incompatible_dots_modifier(modifier_name = "untyped")
      }
      # Prevent calls to `untyped()` with > 1 arguments, but allow 0 arguments
      # to be used, in which case we use `rlang::missing_arg()` as the default
      # argument value.
      n_args <- length(rlang::call_args(maybe_type_call))
      if (n_args > 1) {
        typewriter_abort_invalid_input(
          message = c(
            "Can't convert `call` into a typed function.",
            x = sprintf("Argument `%s` of `call` contains a malformed call to `untyped()`.", arg_name),
            x = sprintf("`untyped` requires 1 argument, but %i were supplied.", n_args)
          ),
          call = error_call
        )
      } else if (n_args == 0) {
        new_args[[i]] <- rlang::missing_arg()
      } else {
        new_args[[i]] <-  maybe_type_call[[2]]
      }
      next
    }

    # TODO: At this point we may want to check that the call function exists in `env`.
    #
    # maybe_function <- check_is_evaluable(maybe_type_call[[1]], env = env)
    # check_is_function(maybe_function)

    call_args <- rlang::call_args(maybe_type_call)
    call_args_names <- rlang::names2(call_args)

    # Take the first argument of `call` to be the default value of the
    # corresponding argument, if the first argument to call is unnamed.
    if (length(call_args) != 0 && call_args_names[[1]] == "") {
      # TODO: Test initialization value, note that you'll have to do this AFTER
      #       you've added the modifiers, so you skip `NULL` or missing.
      if (arg_is_dots) {
        typewriter_abort_invalid_input(
          message = c(
            "Can't convert `call` into a typed function.",
            x = "Argument `...` of `call` can't have an initialization value.",
            x = sprintf("Supplied an initialization value of `%s` to `...`.", rlang::as_label(call_args[[1]]))
          ),
          call = error_call
        )
      }
      # Converts `function(arg = call(<value>, ...))` to `function(arg = <value>)`
      # and we insert `call(arg, ...)` into the function body as a check.
      new_args[[i]] <- call_args[[1]]
      type_call <- call_prepend_args(maybe_type_call[-2], !!arg_sym)
    } else {
      # Converts `function(arg = call(...))` to `function(arg)` and we insert
      # `call(arg, ...)` into the function body as a check.
      new_args[[i]] <- rlang::missing_arg()
      type_call <- call_prepend_args(maybe_type_call, !!arg_sym)
    }

    # Transform the call into a description before it's modified with `maybe()`
    # or `optional()`, for a prettier description.
    typed_args_descs <- c(typed_args_descs, typed_arg_desc(type_call, arg_modifiers))
    typed_args_names <- c(typed_args_names, arg_name)

    # It's important that "maybe" comes before "optional", so we do
    # `if (!missing(arg)) if (!is.null(arg)) call(arg)`
    # in the correct order and prevent a missing argument error in `is.null`
    if ("maybe" %in% arg_modifiers) {
      type_call <- if_not_null_call(call = type_call, sym = arg_sym)
    }
    if ("optional" %in% arg_modifiers) {
      if (arg_is_dots) {
        stop_incompatible_dots_modifier(modifier_name = "optional")
      }
      type_call <- if_not_missing_call(call = type_call, sym = arg_sym)
    }

    if (arg_is_dots) {
      # `eval(substitute(...))` here inserts the correct `..1`, `..2`, etc.
      # symbol in place of `.__dot__.` in `type_call(.__dot__.)`. This allows
      # functions which capture their argument name (e.g. `rlang::caller_arg()`)
      # to reference the correct dot. We also prevent immediate evaluation of
      # all the dots, at the expense of {rlang} injection support.
      type_call <- rlang::expr({
        for (.__dot__. in seq_len(...length())) {
          eval(substitute(!!type_call, list(.__dot__. = as.symbol(paste0("..", .__dot__.)))))
        }
      })[[2]] # Removes the redundant `{}` in `{ for (...) {...} }`
    }

    type_calls <- append(type_calls, type_call)
  }

  typed_function <- rlang::new_function(
    args = new_args,
    body = rlang::call2("{", !!!type_calls, body),
    env = env
  )
  structure(
    typed_function,
    class = c("typewriter_typed_function", "function"),
    type_calls = type_calls,
    typed_args_names = typed_args_names,
    typed_args_descs = typed_args_descs
  )
}

#' @export
is_typed_function <- function(x) {
  inherits(x, "typewriter_typed_function")
}

#' @export
untype_function <- function(x) {
  check_is_function(x)
  if (!is_typed_function(x)) {
    return(x)
  }
  typed_body <- body(x)
  n_type_calls <- length(attr(x, "type_calls"))

  # Typed functions have a body:
  # {
  #   type_call_1(...)
  #   type_call_2(...)
  #   ...
  #   type_call_n(...)
  #   {
  #     <original function implementation>
  #   }
  # }
  # This sets the untyped function body back to:
  # {
  #   <original function implementation>
  # }
  body(x) <- typed_body[[n_type_calls + 2L]]
  attr(x, "type_calls") <- NULL
  x
}


# TODO: Use `cli::cat_bullet` if {cli} is available (check version if needed)
#       and otherwise use a homegrown method. Or check the {rlang} standalone-cli
#       for tips. I really just need to get the bullet symbol, don't overcomplicate.
#       If {cli} not available, use "*" instead.
#
# TODO: Print a bulleted list of the typed arguments after the function definition
#       using the `type_calls` attribute. Note if an argument is required, optional,
#       or a maybe. If the type call is to an alias, use the alias description.
#       Otherwise, say "type-checked with `call(arg, ...)`.
#' @export
print.typewriter_typed_function <- function(x, ...) {
  # TODO: Don't print arguments if there are no typed args
  arg_names <- paste0("`", attr(x, "typed_args_names"), "`")
  arg_names <- sprintf(paste0("%-", max(nchar(arg_names)), "s"), arg_names)
  bullets <- sprintf("* %s: %s\n", arg_names, attr(x, "typed_args_descs"))

  cat("<typed>\n")
  print(untype_function(x), ...)
  cat("Typed Arguments:\n")
  for (bullet in bullets) {
    cat(bullet)
  }
}

# TODO: Divert the `cat()`-ing of typed argument descriptions to here
cat_typed_args <- function(x) {

}

if_not_missing_call <- function(call, sym) {
  not_missing <- rlang::expr(!missing(!!sym) && !identical(!!sym, quote(expr = )))
  rlang::call2("if", not_missing, call)
}

if_not_null_call <- function(call, sym) {
  not_null <- rlang::call2("!", rlang::call2("is.null", sym))
  rlang::call2("if", not_null, call)
}

call_prepend_args <- function(call, ...) {
  if (!is.call(call)) {
    typewriter_abort(
      sprintf("Expected `call` to be a call, not %s", typeof(call)),
      internal = TRUE
    )
  }
  call_args <- as.list(call[-1])
  rlang::call_modify(call[1], !!!rlang::enexprs(...), !!!call_args)
}

# modifiers --------------------------------------------------------------------

#' @export
untyped <- function(x) {
  typewriter_stop_invalid_context()
}

#' @export
required <- function(x) {
  typewriter_stop_invalid_context()
}

#' @export
optional <- function(x) {
  typewriter_stop_invalid_context()
}

#' @export
maybe <- function(x) {
  typewriter_stop_invalid_context()
}

is_untyped_call <- function(x) {
  rlang::is_call(x, name = "untyped", ns = c("", "typewriter"))
}

is_modified_call <- function(x) {
  modifiers <- c("required", "optional", "maybe")
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

typewriter_stop_invalid_context <- function() {
  typewriter_abort(
    "Must only be used in a function typing context (e.g. when calling `%<~%` or `typewriter::typed()`).",
    class = "typewriter_error_invalid_context",
    call = rlang::caller_env()
  )
}

# dependencies -----------------------------------------------------------------

# These are not functions intended for external use. They are functions which
# are injected into `typed()` functions and thus need to be made available to
# the user.

#' @export
check_required_arg <- function(x) {
  if (!missing(x)) {
    return(invisible(TRUE))
  }
  typewriter_abort(
    message = sprintf("`%s` is absent but must be supplied.", rlang::caller_arg(x)),
    call = rlang::caller_env(),
    class = "typewriter_error_typed_arg_missing"
  )
}

#' @export
check_dot <- function(expr, i) {
  call <- rlang::caller_env()
  rlang::try_fetch(
    expr,
    error = function(cnd) {
      typewriter_abort(
        message = sprintf("Argument `..%i` failed a type check.", i),
        call = call,
        class = "typewriter_error_typed_dot_invalid",
        parent = cnd
      )
    }
  )
}
