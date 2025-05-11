# function typing --------------------------------------------------------------

#' Declare a function with typed arguments
#'
#' @description
#'
#' `typed()` modifies a function declaration (e.g. `function(x) { x + x }`) by
#' inserting additional calls into the function body to check the types of its
#' arguments.
#'
#' To declare the type of an argument in `function`, set it's default value to a
#' type-checking call. For instance, [chk::chk_integer()] raises an error if its
#' first argument is not an integer. Using this, we can declare that the argument
#' `x` must be an integer like so:
#'
#' ```r
#' integer_doubler <- typed(function(x = chk::chk_integer()) {
#'  x + x
#' })
#' ```
#'
#' This returns a function which looks like `function(x) { x + x }` and which
#' calls `chk::chk_integer(x)` prior to executing its body.
#'
#' Default argument values are taken from the first argument of the type-checking
#' call. For instance, the following:
#'
#' ```r
#' integer_doubler_2 <- typed(function(x = chk::chk_integer(0L)) {
#'  x + x
#' })
#' ```
#'
#' Returns a function like `function(x = 0L) { x + x }` and which still calls
#' `chk::chk_integer(x)` prior to executing its body.
#'
#' @details
#'
#' `typed()` works by inserting code into the body of a function. This is in
#' contrast to [type_alias()] and [assign_typed()], both of which create wrappers
#' around type-checking functions. Consider the following example:
#'
#' ```r
#' integer_doubler <- typed(function(x = chk_integer()) {
#'  x + x
#' })
#' ```
#'
#' This is roughly equivalent to the following:
#'
#' ```r
#' integer_doubler <- function(x) {
#'  chk_integer(x)
#'  x + x
#' }
#' ```
#'
#' In both cases, what `integer_doubler()` does will change if the function
#' `chk_integer()` is overwritten in the environment of `integer_doubler()`. In
#' the worst case scenario, this breaks our contract that the argument `x` is
#' going to be an integer. To prevent such changes, type-checking calls should
#' be namespaced:
#'
#' ```r
#' integer_doubler_stable <- typed(function(x = chk::chk_integer()) {
#'  x + x
#' })
#' ```
#'
#' @param call `[call]`
#'
#' A call to `function`.
#'
#' @param env `[environment]`
#'
#' The environment of the returned function, passed to [rlang::new_function()].
#' By default `env` is the caller's environment.
#'
#' @return
#'
#' A typed function.
#'
#' @seealso [as_typed_function()], [is_typed_function()], [untype_function()],
#' [has_typed_args()]
#'
#' @examplesIf requireNamespace("chk", quietly = TRUE)
#' # Declare the types of arguments
#' add <- typed(function(x = chk::chk_numeric(), y = chk::chk_numeric()) {
#'   x + y
#' })
#' add(5, 6)
#' try(add("A", 10))
#'
#' # Printing the function shows the types of it's arguments
#' print(add)
#'
#' # Typed arguments can be declared using type aliases
#' a_num <- type_alias(chk::chk_numeric(), desc = "A numeric vector.")
#' add2 <- typed(function(x = a_num, y = a_num) {
#'   x + y
#' })
#' add2(10.5, 0.1)
#' try(add2(5, TRUE))
#'
#' # Arguments use the alias's description if available
#' print(add2)
#'
#' # Dots can be typed as well
#' sum2 <- typed(function(... = a_num, na.rm = chk::chk_flag(FALSE)) {
#'   sum(..., na.rm = na.rm)
#' })
#' sum2(1, 2, 3, 4)
#' try(sum2(1, 2, "C"))
#' try(sum2(1, 2, 3, na.rm = "no"))
#' @export
typed <- function(call, env = rlang::caller_env()) {
  check_is_environment(env)
  call <- rlang::enexpr(call)
  if (!rlang::is_call(call, "function", ns = "")) {
    if (rlang::is_call(call)) {
      typewriter_abort_invalid_input(
        message = sprintf(
          "`call` must be a defused call to `function`, not a call to `%s`.",
          rlang::as_label(call[[1]])
        )
      )
    }
    typewriter_abort_invalid_input(
      sprintf("`call` must be a defused call to `function`, not %s.", obj_type_friendly(call))
    )
  }
  new_typed_function(
    args = call[[2]],
    body = call[[3]],
    env = env,
    error_arg = "call"
  )
}


#' Type the arguments of an existing function
#'
#' @param .fun `[function]`
#'
#' A function whose arguments will be typed. If `.fun` is already a typed
#' function, it's previous argument types will be overwritten.
#'
#' @param ... `[call / alias]`
#'
#' Type declarations of arguments in `.fun`. Each dot must be a named call or
#' type alias, the name of which corresponds to an argument in `.fun`.
#'
#' For example, we can create a type-strict version of [base::paste0()] like so:
#'
#' ```r
#' my_paste0 <- as_typed_function(
#'  .fun = base::paste0,
#'  ... = chk::chk_character(),
#'  collapse = maybe(chk::chk_string(NULL)),
#'  recycle0 = chk::chk_flag(FALSE)
#' )
#' ```
#'
#' `my_paste0(..., collapse, recycle0)` checks for the following types:
#' * Each dot `..i` must be a character vector.
#' * `collapse` is either `NULL` (via [maybe]) or a string (default is `NULL`)
#' * `recycle0` is either `TRUE` or `FALSE` (default is `FALSE`).
#'
#' If these checks pass, then the result of `base::paste0(..., collapse, recycle0)`
#' is returned.
#'
#' @returns
#'
#' A typed function.
#'
#' @seealso [typed()]
#'
#' @examplesIf requireNamespace("chk", quietly = TRUE)
#' my_paste0 <- as_typed_function(
#'  .fun = base::paste0,
#'  ... = chk::chk_character(),
#'  collapse = maybe(chk::chk_string(NULL)),
#'  recycle0 = chk::chk_flag(FALSE)
#' )
#' my_paste0("A", "B", "C")
#' try(my_paste0("A", "B", collapse = 10))
#'
#' # Typed functions `print()` their typed arguments
#' print(my_paste0)
#' @export
as_typed_function <- function(.fun, ...) {
  check_is_function(.fun)
  assert_dots_named(...)
  if (is_typed_function(.fun)) {
    .fun <- untype_function(.fun)
  }

  # TODO: `.fun` can't be a primitive or special function, must be a closure

  body <- rlang::fn_body(.fun)
  args <- map(rlang::fn_fmls(.fun), \(fml) rlang::call2(quote(typewriter::untyped), fml))

  # `...` can't be `untyped()`
  if ("..." %in% names(args)) {
    args$`...` <- rlang::missing_arg()
  }

  typed_args <- rlang::enexprs(...)
  typed_args_names <- rlang::names2(typed_args)

  if (!all(typed_args_names %in% names(args))) {
    bad_arg_name <- typed_args_names[[which.min(typed_args_names %in% names(args))]]
    typewriter_abort_invalid_input(
      message = c(
        "Can't convert `.fun` to a typed function.",
        x = sprintf("Argument `%s` in `...` is not a formal argument of `.fun`.", bad_arg_name)
      )
    )
  }

  args[names(typed_args)] <- typed_args

  new_typed_function(
    args = args,
    body = body,
    env = rlang::fn_env(.fun),
    error_arg = ".fun"
  )
}

# Warning to the reader, this function is 90% input validation, since we're
# tossing around unevaluated calls. Error messages reference the `error_arg`,
# which is either `call` in `typed(call, env)` or `.fun` in
# `as_typed_function(.fun, ...)`.
new_typed_function <- function(
    args,
    body,
    env,
    error_arg,
    error_call = rlang::caller_env()
  ) {

  args_names <- rlang::names2(args)
  type_calls <- list()
  new_args <- args

  typed_args_names <- character()
  typed_args_descs <- character()

  is_reserved_arg <- map_lgl(args, function(arg) "..i" %in% all.vars(arg))
  if (any(is_reserved_arg)) {
    bad_arg <- args_names[[which.max(is_reserved_arg)]]
    typewriter_abort_invalid_input(
      message = c(
        sprintf("Arguments in `%s` must not contain the symbol `..i`.", error_arg),
        x = sprintf("`%s` argument `%s` contains the symbol `..i`", error_arg, bad_arg),
        i = "The symbol `..i` is reserved for internal use in <typed> functions."
      ),
      call = error_call
    )
  }

  is_defused_alias <- function(call) {
    is_named_symbol(call) && is_type_alias(try(eval(call, env), silent = TRUE))
  }

  stop_incompatible_dots_modifier <- function(modifier_name) {
    typewriter_abort_invalid_input(
      message = c(
        sprintf("Can't convert `%s` into a <typed> function.", error_arg),
        x = sprintf(
          "Argument `...` of `%s` contains a call to `typewriter::%s()`.",
          error_arg, modifier_name
        ),
        x = sprintf("Typed function dots can't be %s.", modifier_name)
      ),
      call = error_call
    )
  }

  typed_arg_desc <- function(type_call, arg_modifiers) {
    type_call_fun <- type_call[[1]]
    maybe_alias <- try(eval(type_call_fun, env), silent = TRUE)
    if (is_type_alias(maybe_alias)) {
      desc <- attr(maybe_alias, "desc")
    } else {
      desc <- sprintf("An object checked by `%s`.", rlang::as_label(type_call))
    }
    if (is_empty(arg_modifiers)) {
      return(desc)
    }
    paste0(desc, " [", commas(sort(unique(arg_modifiers))), "]")
  }

  # Generate type checks for each of `args`. An argument is not typed if (1) its
  # wrapped in `untyped()`, (2) it's not a call (e.g. a literal), (3) it missing.
  for (i in seq_along(args)) {

    maybe_type_call <- args[[i]]
    arg_name <- args_names[[i]]
    arg_is_dots <- arg_name == "..."

    if (rlang::is_missing(maybe_type_call)) {
      next
    } else if (is_defused_alias(maybe_type_call)) {
      maybe_type_call <- rlang::call2(maybe_type_call)
    } else if (!rlang::is_call(maybe_type_call)) {
      next
    } else if (!rlang::is_call_simple(maybe_type_call)) {
      call_label <- rlang::as_label(maybe_type_call)
      typewriter_abort_invalid_input(
        message = c(
          sprintf("Can't type argument `%s` of `%s`.", arg_name, error_arg),
          i = sprintf("Argument `%s` contains a complex call `%s`.", arg_name, call_label),
          x = "Only simple calls (e.g. `ns::foo()` or `foo()`) may be used to type function arguments.",
          i = sprintf("Use `%s = typewriter::untyped(%s)` to opt out of typing.", arg_name, call_label)
        ),
        call = error_call
      )
    } # `maybe_type_call` is confirmed to be a simple call (e.g. `foo()`)

    # In the <typed> function, we substitute `..i` with `..{i}` (e.g. `..1`)
    # when type checking each of the `...` arguments, this allows error messages
    # to refer to a given dot by name.
    arg_sym <- if (arg_is_dots) quote(..i) else rlang::sym(arg_name)
    arg_modifiers <- character()

    # Strips and records modifiers, e.g. `maybe(required(f(x)))` -> `f(x)`. Checks
    # that modifiers contain 1 argument and that the argument is a call or alias.
    while (is_modified_call(maybe_type_call)) {

      n_args <- length(rlang::call_args(maybe_type_call))
      if (n_args != 1) {
        malformed_fun <- rlang::as_label(maybe_type_call[[1]])
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't convert `%s` into a <typed> function.", error_arg),
            x = sprintf(
              "Argument `%s` of `%s` contains a malformed call to `%s()`.",
              arg_name, error_arg,malformed_fun
            ),
            x = sprintf("`%s` requires 1 argument, but %i were supplied.", malformed_fun, n_args)
          ),
          call = error_call
        )
      }

      # Modifiers expect a call as their argument (e.g. `static(check_int())`),
      # but we also allow a named alias (e.g. `static(a_int)`). In this case, we
      # are done stripping modifiers and can make the alias a call (e.g. `a_int()`).
      if (is_defused_alias(maybe_type_call[[2]])) {
        modifier <- get_modifier_name(maybe_type_call)
        arg_modifiers <- c(arg_modifiers, modifier)
        maybe_type_call <- rlang::call2(maybe_type_call[[2]])
        break
      }

      if (!rlang::is_call(maybe_type_call[[2]])) {
        malformed_fun <- rlang::as_label(maybe_type_call[[1]])
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't convert `%s` into a <typed> function.", error_arg),
            x = sprintf(
              "Argument `%s` of `%s` contains a malformed call to `typewriter::%s()`.",
              arg_name, error_arg, malformed_fun
            ),
            x = sprintf(
              "`%s` requires a call or an <alias> as it's first argument, not %s.",
              malformed_fun, obj_type_friendly(maybe_type_call[[2]])
            )
          ),
          call = error_call
        )
      }

      modifier <- get_modifier_name(maybe_type_call)
      arg_modifiers <- c(arg_modifiers, modifier)
      maybe_type_call <- maybe_type_call[[2]]
    } # Done stripping modifiers

    if ("required" %in% arg_modifiers) {
      if ("optional" %in% arg_modifiers) {
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't convert `%s` into a <typed> function.", error_arg),
            x = sprintf(
              "Argument `%s` of `%s` contains calls to `typewriter::required()` and `typewriter::optional()`.",
              arg_name, error_arg
            ),
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
    } # Added a `required()` check, if necessary

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
            "Can't convert `call` into a <typed> function.",
            x = sprintf("Argument `%s` of `call` contains a malformed call to `typewriter::untyped()`.", arg_name),
            x = sprintf("`untyped` requires 1 argument, but %i were supplied.", n_args)
          ),
          call = error_call
        )
      } else if (n_args == 0) {
        new_args[[i]] <- rlang::missing_arg()
      } else {
        # RHS `value` might be `NULL`, so can't assign via `new_args[[i]] <- value`
        # as this would just remove the ith argument of `new_args`.
        new_args[i] <-  list(maybe_type_call[[2]])
      }
      # Argument is untyped, there's nothing left to do
      next
    } # Handled an `untyped()` argument, if necessary

    call_args <- rlang::call_args(maybe_type_call)
    call_args_names <- rlang::names2(call_args)

    # Take the first argument of `call` to be the default value of the
    # corresponding argument, if the first argument to call is unnamed.
    # Note, intentionally not forcing the initialization value to pass
    # the `maybe_type_call(<value>, ...)` check, since a function author
    # could plausibly want a default value to fail.
    if (length(call_args) != 0 && call_args_names[[1]] == "") {
      if (arg_is_dots) {
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't convert `%s` into a <typed> function.", error_arg),
            x = sprintf("Argument `...` of `%s` can't have an initialization value.", error_arg),
            x = sprintf(
              "Supplied an initialization value of `%s` to `...`.",
              rlang::as_label(call_args[[1]])
            )
          ),
          call = error_call
        )
      }
      # Converts `function(arg = call(<value>, ...))` to `function(arg = <value>)`
      # and we insert `call(arg, ...)` into the function body as a check. <value>
      # might be `NULL`, so we can't insert using `new_args[[i]] <- <value>`, as
      # this would just remove the ith element of `new_args`.
      new_args[i] <- list(call_args[[1]])
      type_call <- call_prepend_args(maybe_type_call[-2], !!arg_sym)
    } else {
      # Converts `function(arg = call(...))` to `function(arg)` and we insert
      # `call(arg, ...)` into the function body as a check.
      new_args[[i]] <- rlang::missing_arg()
      type_call <- call_prepend_args(maybe_type_call, !!arg_sym)
    } # Updated the call argument and created the (unmodified) type call

    # Transform the call into a description before it's modified with `maybe()`
    # or `optional()`, for a prettier description.
    typed_args_descs <- c(typed_args_descs, typed_arg_desc(type_call, arg_modifiers))
    typed_args_names <- c(typed_args_names, arg_name)

    # It's important that we order this "static" < "maybe" < "optional", so that
    # `if (!missing(arg)) if (!is.null(arg)) assign_typed(arg, call(arg))`
    # is in the correct order to prevent a missing argument error in `is.null`.
    if ("static" %in% arg_modifiers) {
      if (arg_is_dots) {
        stop_incompatible_dots_modifier(modifier_name = "static")
      }
      type_call <- typed_assign_call(call = type_call, sym = arg_sym)
    }
    if ("maybe" %in% arg_modifiers) {
      type_call <- if_not_null_call(call = type_call, sym = arg_sym)
    }
    if ("optional" %in% arg_modifiers) {
      if (arg_is_dots) {
        stop_incompatible_dots_modifier(modifier_name = "optional")
      }
      type_call <- if_not_missing_call(call = type_call, sym = arg_sym)
    } # Modified the type call, if necessary

    if (arg_is_dots) {
      # `eval(substitute(...))` here inserts the correct `..1`, `..2`, etc.
      # symbol in place of `..i` in `type_call(..i)`. This allows functions
      # which capture their argument name (e.g. `rlang::caller_arg()`) to
      # reference the correct dot. We also prevent immediate evaluation of all
      # dots, at the expense of {rlang} injection support.
      type_call <- rlang::expr({
        for (..i in seq_len(...length())) {
          eval(substitute(!!type_call, list(..i = as.symbol(paste0("..", ..i)))))
        }
      })[[2]] # Removes the redundant `{}` in `{ for (...) {...} }`
    }

    type_calls <- append(type_calls, type_call)
  } # Processed all of `args`

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

call_prepend_args <- function(call, ...) {
  if (!is.call(call)) {
    typewriter_abort(sprintf("Expected `call` to be a call, not %s", typeof(call)), internal = TRUE) # nocov
  }
  call_args <- as.list(call[-1])
  rlang::call_modify(call[1], !!!rlang::enexprs(...), !!!call_args)
}

if_not_missing_call <- function(call, sym) {
  not_missing <- rlang::expr(!missing(!!sym) && !identical(!!sym, quote(expr = )))
  rlang::call2("if", not_missing, call)
}

if_not_null_call <- function(call, sym) {
  not_null <- rlang::call2("!", rlang::call2("is.null", sym))
  rlang::call2("if", not_null, call)
}

# When we use this in a typed function we'll generate a body like:
# function(x) {
#   rethrow_parent_assignment_error(assign_typed(x, type_call(x, ...)))
#   {
#     # Implementation
#   }
# }
# This (1) actively binds `x` in the typed function environment and (2) performs
# the initial type check `type_call(x, ...)` on the input argument. We re-throw
# only the parent error if (2) fails, since the child error references an
# "invalid assignment" of `x <- x`, which doesn't make sense in this context.
typed_assign_call <- function(call, sym) {
  if (!identical(call[[2]], sym)) {
    typewriter_abort("Expected first argument of `call` to be `sym`.", internal = TRUE) # nocov
  }
  assign_call <- rlang::call2("assign_typed", sym, call, .ns = "typewriter")
  rlang::call2("rethrow_parent_assignment_error", assign_call, .ns = "typewriter")
}

# typed class ------------------------------------------------------------------

#' @export
is_typed_function <- function(x) {
  inherits(x, "typewriter_typed_function")
}

#' @export
has_typed_args <- function(x) {
  check_is_function(x)
  is_typed_function(x) && length(attr(x, "typed_args_names")) != 0L
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

#' @export
print.typewriter_typed_function <- function(x, ...) {
  cat("<typed>\n")
  print(untype_function(x), ...)
  cat_typed_args(x)
}

cat_typed_args <- function(x) { # nocov start
  if (!has_typed_args(x)) {
    return(invisible(NULL))
  }
  arg_names <- paste0("`", attr(x, "typed_args_names"), "`")

  cat("Typed Arguments:\n")
  if (rlang::is_installed("cli")) {
    prefix <- cli::format_bullets_raw(rlang::set_names(arg_names, "*"))
    # Pad bullets post-format, since {cli} collapses > 2 whitespace characters
    bullets <- paste0(
      prefix,
      strrep(" ", max(nchar(arg_names)) - nchar(arg_names)), # Aligns the descriptions
      ": ", attr(x, "typed_args_descs")
    )
    writeLines(bullets)
  } else {
    pad_names <- function(x) sprintf(paste0("%-", max(nchar(x)), "s"), x)
    bullets <- sprintf("%s: %s", pad_names(arg_names), attr(x, "typed_args_descs"))
    writeLines(paste("*", bullets))
  }
} # nocov end

# modifiers --------------------------------------------------------------------

#' @export
untyped <- function(x) {
  typewriter_stop_invalid_context() # nocov
}

#' @export
static <- function(x) {
  typewriter_stop_invalid_context() # nocov
}

#' @export
required <- function(x) {
  typewriter_stop_invalid_context() # nocov
}

#' @export
optional <- function(x) {
  typewriter_stop_invalid_context() # nocov
}

#' @export
maybe <- function(x) {
  typewriter_stop_invalid_context() # nocov
}

is_untyped_call <- function(x) {
  rlang::is_call(x, name = "untyped", ns = c("", "typewriter"))
}

is_modified_call <- function(x) {
  modifiers <- c("required", "optional", "maybe", "static")
  rlang::is_call(x, name = modifiers, ns = c("", "typewriter"))
}

get_modifier_name <- function(modifier_call) { # nocov start
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
} # nocov end

typewriter_stop_invalid_context <- function() {
  typewriter_abort(
    "Must only be used in a function typing context (e.g. when calling `%<~%` or `typewriter::typed()`).",
    class = "typewriter_error_invalid_context",
    call = rlang::caller_env()
  )
}

# dependencies -----------------------------------------------------------------

# These are not functions intended for external use, but are functions which are
# injected into `typed()` functions and thus need to be made available to the
# user.

#' @export
rethrow_parent_assignment_error <- function(expr) {
  rlang::try_fetch(
    expr = expr,
    typewriter_error_invalid_assignment = function(cnd) {
      rlang::cnd_signal(cnd$parent)
    }
  )
}

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
