# todos ------------------------------------------------------------------------

# - Maybe re-export `rlang::exprs()` and `rlang::pairlist2()` since they're
#   useful for `new_typed_function()`

# TODO: This script is so messy, clean it up

# function typing --------------------------------------------------------------

typed <- function(call) {
  call <- rlang::enexpr(call)
  if (!rlang::is_call(call, "function", ns = "")) {
    typewriter_abort_invalid_input(
      "`call` must be a call to `function`.",
    )
  }
  new_typed_function(
    args = call[[2]],
    body = call[[3]],
    env = rlang::caller_env()
  )
}

is_typed_function <- function(x) {
  inherits(x, "typewriter_typed_function")
}

as_typed_function <- function(.fun, ...) {
  check_is_function(.fun)
  if (is_typed_function(.fun)) {
    .fun <- untyped_function(.fun)
  }

  body <- rlang::fn_body(.fun)
  args <- map(rlang::fn_fmls(.fun), \(fml) rlang::call2(quote(untyped), fml))

  # TODO: Nicer error here
  typed_args <- rlang::enexprs(...)
  stopifnot(all(rlang::names2(typed_args) %in% names(args)))

  args[names(typed_args)] <- typed_args

  new_typed_function(
    args = args,
    body = body,
    env = rlang::fn_env(.fun)
  )
}

untype_function <- function(x) {
  stopifnot(is.function(x))
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

new_typed_function <- function(args, body, env, error_call = rlang::caller_env()) {
  args_names <- names(args)
  arg_type_calls <- list()
  new_args <- args

  for (i in seq_along(args)) {

    maybe_arg_type_call <- args[[i]]
    if (rlang::is_missing(maybe_arg_type_call)) {
      next
    }

    arg_name <- args_names[[i]]
    arg_sym <- rlang::sym(arg_name)
    arg_modifiers <- character()

    if (rlang::is_call(maybe_arg_type_call)) {
      # Converts `function(arg = untyped(<expr>))`, to `function(arg = <expr>)`
      if (is_untyped_call(maybe_arg_type_call)) {
        new_fml_default <- maybe_arg_type_call[[2]]
        new_fun_fmls[[i]] <- new_fml_default
        next
      }

      # Strips and records modifiers, e.g. `maybe(required(f(x)))` -> `f(x)`
      while (is_modified_call(maybe_arg_type_call)) {
        modifier <- get_modifier_name(maybe_arg_type_call)
        maybe_arg_type_call <- maybe_arg_type_call[[2]]
        arg_modifiers <- c(arg_modifiers, modifier)
      }

      call_args <- rlang::call_args(maybe_arg_type_call)
      type <- call_type(maybe_arg_type_call)

      if (type == "valued_call") {
        # Converts `function(arg = call(<value>, ...))` to `function(arg = <value>)`
        # and we insert `call(arg, ...)` into the function body as a check.
        new_args[[i]] <- call_args[[1]]
        arg_type_call <- call_push_args(maybe_arg_type_call[-2], !!arg_sym)
      } else if (type == "unvalued_call") {
        # Converts `function(arg = call(...))` to `function(arg)` and we insert
        # `call(arg, ...)` into the function body as a check.
        new_args[[i]] <- rlang::missing_arg()
        arg_type_call <- call_push_args(maybe_arg_type_call, !!arg_sym)
      } else {
        # TODO: You actually might not need this... Since we're just inserted the
        #       exact call into the function.
        unnamed_args_at <- rlang::have_name(call_args)
        n_unnamed_args <- sum(unamed_args_at)
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't insert a type check for argument `%s`.", arg_name),
            i = sprintf("Attempted to convert the call `%s` into a type check.", rlang::as_label(maybe_arg_type_call)),
            i = "Type check calls must have named arguments (except potentially the first).",
            x = sprintf(
              "Call has %i unamed argument%s %s.",
              n_unnamed_args, ngettext(n_unnamed_args, "", "s"),
              at_positions(unnamed_args_at)
            )
          ),
          call = error_call
        )
      }

      # TODO: I also think we can raise an error for `optional(required())` being
      #       used at the same time, might make more sense.
      #
      # TODO: I think it's probably safer to inject `typewriter::check_required()`
      # or other {typewriter} calls, in case {rlang} changes somehow (or use
      # {base} functions). This way I can ensure that the injected code doesn't
      # change.
      #
      # Allowing both `required()` and `optional()`, although the result is that
      # the argument will be required. It's important that "maybe" comes before
      # "optional", so we do `if (!is_missing(arg)) if (!is_null(arg)) call(arg)`
      # in the correct order and prevent a missing argument error.
      if ("required" %in% arg_modifiers) {
        check <- rlang::call2("check_required", arg_sym, .ns = "rlang")
        arg_type_calls <- append(arg_type_calls, check)
      }
      if ("maybe" %in% arg_modifiers) {
        arg_type_call <- if_not_null_call(call = arg_type_call, sym = arg_sym)
      }
      if ("optional" %in% arg_modifiers) {
        arg_type_call <- if_not_missing_call(call = arg_type_call, sym = arg_sym)
      }

      arg_type_calls <- append(arg_type_calls, arg_type_call)
    }
  }

  typed_function <- rlang::new_function(
    args = new_args,
    body = rlang::call2("{", !!!arg_type_calls, body),
    env = env
  )
  structure(
    typed_function,
    class = c("typewriter_typed_function", "function"),
    type_calls = arg_type_calls
  )
}

# TODO: Print a bulleted list of the typed arguments after the function definition
#       using the `type_calls` attribute. Note if an argument is required, optional,
#       or a maybe. If the type call is to an alias, use the alias description.
#       Otherwise, say "type-checked with `call(arg, ...)`.
#' @export
print.typewriter_typed_function <- function(x, ...) {
  cat("<typed>\n")
  print(untype_function(x), ...)
}

if_not_missing_call <- function(call, sym) {
  not_missing <- rlang::call2("!", rlang::call2("is_missing", sym, .ns = "rlang"))
  rlang::call2("if", not_missing, call)
}

if_not_null_call <- function(call, sym) {
  not_null <- rlang::call2("!", rlang::call2("is_null", sym, .ns = "rlang"))
  rlang::call2("if", not_null, call)
}

# modifiers --------------------------------------------------------------------

untyped <- function(x) {
  typewriter_stop_invalid_context()
}

required <- function(x) {
  typewriter_stop_invalid_context()
}

optional <- function(x) {
  typewriter_stop_invalid_context()
}

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

# testing ----------------------------------------------------------------------

# Implementation of `typed()`
if (FALSE) {
  # TODO: We'll need to deal with `...` specially
  call <- quote(function(a, x = int(10L), y = num(len = 1L), z = untyped(c(1, 2, 3))) { print("Hi") })
  env <- rlang::caller_env()

  fun_fmls <- call[[2]]
  fun_body <- call[[3]]
  fun_fmls_nms <- names(fun_fmls)

  arg_type_calls <- list()
  new_fun_fmls <- fun_fmls

  for (i in seq_along(fun_fmls)) {
    maybe_arg_type_call <- fun_fmls[[i]]
    if (rlang::is_missing(maybe_arg_type_call)) {
      next
    }

    fml_name <- fun_fmls_nms[[i]]
    fml_sym <- rlang::sym(fml_name)

    if (rlang::is_call(maybe_arg_type_call)) {
      # Converts `function(arg = untyped(<expr>))`, to `function(arg = <expr>)`
      if (rlang::is_call(maybe_arg_type_call, "untyped", ns = c("", "typewriter"))) {
        new_fml_default <- maybe_arg_type_call[[2]]
        new_fun_fmls[[i]] <- new_fml_default
        next
      }

      call_args <- rlang::call_args(maybe_arg_type_call)
      type <- call_type(maybe_arg_type_call)

      if (type == "valued_call") {
        # Converts `function(arg = call(<value>, ...))` to `function(arg = <value>)`
        # and we insert `call(arg, ...)` into the function body for to check `arg`.
        new_fun_fmls[[i]] <- call_args[[1]]
        arg_type_call <- call_push_args(maybe_arg_type_call[-2], !!fml_sym)
      } else if (type == "unvalued_call") {
        # Converts `function(arg = call(...))` to `function(arg)` and we insert
        # `call(arg, ...)` into the function body for to check `arg`.
        new_fun_fmls[[i]] <- rlang::missing_arg()
        arg_type_call <- call_push_args(maybe_arg_type_call, !!fml_sym)
      } else {
        # TODO: We need to make clear that the type call to the i-th formal is
        # invalid. Prompt the user with a reminded, use `untyped()` to indicate
        # that an argument should not be typed.
        print(i)
        stop("Call is malformed.")
      }

      arg_type_calls <- append(arg_type_calls, arg_type_call)
    }
  }

  new_fun_body <- rlang::call2("{", !!!arg_type_calls, fun_body)

  out <- rlang::new_function(
    args = new_fun_fmls,
    body = new_fun_body,
    env = env
  )
}

call_push_args <- function(call, ...) {
  if (!is.call(call)) {
    typewriter_abort(
      sprintf("Expected `call` to be a call, not %s", typeof(call)),
      internal = TRUE
    )
  }
  call_args <- as.list(call[-1])
  rlang::call_modify(call[1], !!!rlang::enexprs(...), !!!call_args)
}

# NOTE: What are the contexts that you'll be using these in?
# 1. For `assign_typed(sym, call)` I need to check that the call can be evaluated
# 2. For `typed()` I need to check that the arguments to `function(x = type, y = type)`
#    are valid type calls.

# NOTE: You can provide `...` as a named argument, of course it can't have a default value
# foo <- quote(function(x, ... = check_integer, y = check_numeric(10.5)) {
#   NULL
# })

# lobstr::ast(!!foo)
#
# # Index 2 is the pairlist of arguments
# foo[[1]]
# foo[[2]]
# foo[[2]] |> class()

# For each argument in the pairlist:
# 1. Extract it's `VALUE`, if it exists
#    - Doesn't need to be evaluable, since we don't inline it in `ARGS`
#    - So we just need it's expression
# 2. Compose it's `call`, I think we do want to make sure that the function exists.

# is_integer <- function(x) is.integer(x)
#
# call <- quote(is_integer)

## Actually Doing Stuff

call_type <- function(call) {
  if (!rlang::is_call(call)) {
    typewriter_abort(
      sprintf("Expected `call` to be a call, not type %s.", typeof(call)),
      internal = TRUE
    )
  }

  call_args <- rlang::call_args(call)
  named_at <- rlang::have_name(call_args)

  if (all(named_at)) {
    "unvalued_call"
  } else if (!named_at[[1]] && all(named_at[-1])) {
    "valued_call"
  } else {
    "malformed_call"
  }
}

# fun <- quote(function(y = check_integer(10), z = check_numeric, q = FALSE) {})
# env <- rlang::current_env()
#
# args <- fun[[2]]
#
# for (arg in args) {
#   arg_is_alias <- is_alias(try(eval(arg, env)))
#
# }


# switch(
#   call_type(call),
#   unvalued_call = {
#     VALUE <- new_uninitialized()
#   },
#   valued_call = {
#     VALUE <- call[[2]]
#     call <- call[-2]
#   },
#   malformed_call = stop_malformed_call(call)
# )

## Error Functions

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

check_is_callish <- function(
    x,
    x_name = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {

  if (rlang::is_quosure(x)) {
    x <- rlang::quo_get_expr(x)
  }

  x_type <- expr_type(x)

  if (x_type %in% c("symbol", "namespaced")) {
    return(rlang::call2(x))
  } else if (x_type == "simple_call") {
    return(x)
  } else if (x_type == "call") {
    typewriter_abort_invalid_input(
      c(
        sprintf("`%s` must be a simple call (e.g. `foo()` or `ns::foo()`, not `foo$bar()`).", x_name),
        x = sprintf("`%s = %s` is a complex call.", x_name, rlang::as_label(x))
      ),
      call = call
    )
  } else {
    typewriter_abort(
      sprintf("Expected `x` to be an expression, not type %s.", x_type),
      internal = TRUE
    )
  }
}

check_can_call <- function(
    x,
    env,
    x_name = rlang::caller_arg(x),
    message = NULL,
    call = rlang::caller_env()
) {

  if (rlang::is_quosure(x)) {
    env <- rlang::quo_get_env(x)
    x <- rlang::quo_get_expr(x)
  }

  if (!rlang::is_call(x)) {
    typewriter_abort(
      sprintf("Expected `x` to be a call, not type %s.", typeof(x)),
      internal = TRUE
    )
  }
  check_is_evaluable(
    x = x[[1]],
    env = env,
    x_name = x_name,
    message = message,
    call = call
  )
}

if (FALSE) {
  x_type <- expr_type(call)

  if (x_type %in% c("symbol", "namespaced")) {
    # 1. Is this evaluable in the `env`
    # 2. Is this a function?
  }

  if (x_type == "simple_call") {
    # 1. Is the function evaluable in the `env`?
  }

  if (x_type == "call") {
    # Raise an error, must be a simple call
  }

  typewriter_abort(
    sprintf("Expected `x` to be an expression, not type %s.", x_type),
    internal = TRUE
  )

  type <- expr_type(call)

  if (type %in% c("symbol", "namespaced")) {
    check_evaluable(call, env = env, error_call = error_call)
  }
}

# call collection --------------------------------------------------------------

prepare_type_fun_pieces <- function(sym, call, env, error_call) {
  call_args <- rlang::call_args(call)
  named_at <- rlang::have_name(call_args)

  # `call` may contain the value to used initialize `sym` (e.g. `chk_int(10L)`).
  # We only allow the initialization value to be in the first un-named argument
  # of the `call` and all other arguments must be named.
  if (all(named_at)) {
    VALUE <- new_uninitialized()
  } else if (named_at[[1]] || sum(!named_at) > 1) {
    unamed_at <- which(!named_at)
    n_unnamed <- length(unamed_at)
    typewriter_abort_invalid_input(
      message = c(
        "All arguments to `call` must be named (except potentially the first).",
        x = paste0(
          "`call = ", rlang::as_label(call), "` has ", n_unnamed,
          " unnamed argument", "s"[n_unnamed > 1], " ", at_positions(unamed_at), "."
        )
      ),
      call = error_call
    )
  } else {
    # If an initialization value is available, we attempt to (1) evaluate that
    # value and (2) to evaluate `call` (raising chained error otherwise). Doing
    # (1) and (2) separately provides better context for the error message.
    VALUE <- rlang::try_fetch(
      eval(call_args[[1]], envir = env),
      error = function(cnd) {
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't initialize object `%s` using the first argument of `call`.", rlang::as_name(sym)),
            i = sprintf("Attempted assignment `%s <- %s`.", rlang::as_name(sym), rlang::as_label(call_args[[1]]))
          ),
          call = error_call,
          parent = cnd
        )
      }
    )
    rlang::try_fetch(
      eval(call, envir = env),
      error = function(cnd) {
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't initialize typed object `%s` using `call`.", rlang::as_name(sym)),
            i = sprintf("Attempted to evaluate `call = %s`.", rlang::as_label(call))
          ),
          call = error_call,
          parent = cnd
        )
      }
    )
    call <- call[-2] # Remove the first argument after extracting the value
  }

  # Pre-evaluate every named argument to `call` and modify `call` to (1) reference
  # these evaluated arguments and (2) take `sym` as it's first argument.  E.g.
  # `foo(x = 1, y = 1 + 1)` becomes `foo(sym, x = ARGS$x, y = ARGS$y)` where
  # `ARGS = list(x = 1, y = 2)` is the list of pre-evaluated arguments.
  ARGS <- mapply(
    arg = call_args[named_at],
    arg_name = names(call_args[named_at]),
    FUN = \(arg, arg_name) {
      rlang::try_fetch(
        eval(arg, envir = env),
        error = function(cnd) {
          typewriter_abort_invalid_input(
            message = c(
              sprintf("Can't evaluate argument `%s = %s` of `call`.", arg_name, rlang::as_label(arg)),
              x = sprintf("Every argument to `call` must be evaluable in `env = <environment: %s>`.", rlang::env_label(env))
            ),
            call = error_call,
            parent = cnd
          )
        }
      )
    },
    SIMPLIFY = FALSE
  )

  call <- rlang::call_modify(
    .call = call[1], # Gets `call` with no arguments, we re-supply them all here
    sym,
    # Supplies `arg_name = ARGS$arg_name` arguments to the `call`
    !!!lapply(
      rlang::set_names(names(ARGS)),
      \(arg_name) rlang::call2("$", quote(ARGS), as.symbol(arg_name))
    )
  )

  list(
    CALL = call,
    VALUE = VALUE,
    ARGS = ARGS
  )
}
