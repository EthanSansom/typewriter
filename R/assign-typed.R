# todos ------------------------------------------------------------------------

# TODO: Check if you're using any newer {rlang} functions (maybe `rlang::try_fetch()`)
#       and up the version number if so (or use `withCallingHandlers`).

# functions --------------------------------------------------------------------

`%<~%` <- function(sym, call) {
  env <- rlang::caller_env()
  internal_assign_typed(
    sym = rlang::enexpr(sym),
    call = rlang::new_quosure(rlang::enexpr(call), env = env),
    env = env
  )
}

assign_typed <- function(sym, call, env = rlang::caller_env()) {
  internal_assign_typed(
    sym = rlang::enexpr(sym),
    call = rlang::new_quosure(rlang::enexpr(call), env = env),
    env = env
  )
}

internal_assign_typed <- function(sym, call, env) {

  error_call <- rlang::caller_env()
  sym <- check_is_symbol(sym, call = error_call)
  call <- check_quo_is_callish(call, call = error_call)

  prepared <- prepare_type_call(sym, call, env, error_call = error_call)
  sym_name <- rlang::as_name(sym)

  args <- rlang::pairlist2(x = )
  names(args) <- sym_name

  # `VALUE`, `ENV`, and the evaluated arguments of `TYPE_CALL` are passed in via
  # the parent environment of the constructed `check_type()` function.
  body <- rlang::expr({
    if (missing(!!sym)) {
      return(VALUE)
    }
    # TODO: Should be `typewriter::with_chained_assignment_error()`
    with_chained_assignment_error(
      expr = !!prepared$TYPE_CALL,
      sym_name = !!sym_name,
      # TODO: I don't know if there's a reliable way to get the correct value
      #       here. The active binding calls this function with it's argument
      #       quoted in `base::quote(arg)`, but the expression `arg` is going
      #       to be the `dput()` version (e.g. a tibble() is `structure(list(), class = "tbl", etc.)`)
      # NOTE: The {typed} package doesn't attempt this either, so I think I'll bail.
      # value_name = rlang::as_label(match.call()[[2]][2]),
      error_call = ENV
    )
    # NOTE: Recording the input argument and NOT the result of `TYPE_CALL()`,
    # this means that we can allow assertions which return NULL or checks which
    # return their input.
    VALUE <<- !!sym
    return(VALUE)
  })

  check_type <- rlang::new_function(
    args = args,
    body = body,
    env = rlang::new_environment(
      data = list(VALUE = prepared$VALUE, ARGS = prepared$ARGS, ENV = env),
      parent = env
    )
  )
  if (rlang::env_has(env, sym_name)) rlang::env_unbind(env, sym_name)
  makeActiveBinding(sym = sym, fun = check_type, env = env)
}

# TODO: This is only exported so that the correct function is used within a
#       generated function. We could also just inline this entire thing into
#       the `body <- ` part of `internal_assign_typed` which might make more
#       sense. Don't export if you don't have to...
#' @export
with_chained_assignment_error <- function(expr, sym_name, error_call) {
  rlang::try_fetch(
    expr = expr,
    error = function(cnd) {
      typewriter_abort(
        message = sprintf("Attempted to assign an invalid value to typed object `%s`.", sym_name),
        class = "typewriter_error_invalid_assignment",
        call = error_call,
        parent = cnd
      )
    }
  )
}

new_type_check_args <- function(sym) {
  args <- rlang::pairlist2(x = )
  names(args) <- rlang::as_name(sym)
  args
}

# TODO: I want `alias()` functions to NOT have a chained error, the idea being
#       that these are type specific.
#
# NOTE: I don't want to needlessly export any {typewriter} functions (e.g. `typewriter_abort()`)
#       to generate the type check function, so I'm just going to use {base} and {rlang}
#       functions only (since the caller should have them installed).
new_type_check_body <- function(sym, call) {
  sym_name <- rlang::as_name(sym)
  rlang::expr({
    if (missing(!!sym)) {
      return(VALUE)
    }
    VALUE <<- rlang::try_fetch(
      expr = !!call,
      error = function(cnd) {
        rlang::abort(
          message = sprintf("Attempted to assign an invalid value to typed object `%s`.", !!sym_name),
          class = c("typewriter_error", "typewriter_error_invalid_assignment"),
          call = ENV,
          parent = cnd
        )
      }
    )
    VALUE
  })
}

# TODO: Most of this function is error handling, might be cleaner to separate
#       these out so we see what's going on.
prepare_type_call <- function(sym, call, env, error_call = rlang::caller_env()) {
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
    typewriter_abort(
      message = c(
        "All arguments to `call` must be named (except potentially the first).",
        x = paste0(
          "`call = ", rlang::as_label(call), "` has ", n_unnamed,
          " unnamed argument", "s"[n_unnamed > 1], " ", at_positions(unamed_at), "."
        )
      ),
      call = error_call,
      class = "typewriter_error_input_type"
    )
  } else {
    # If an initialization value is available, we attempt both to evaluate that
    # value and to evaluate `call` (raising a chained error otherwise). Doing
    # these separately to provide better context on which step caused an error.
    VALUE <- rlang::try_fetch(
      eval(call_args[[1]], envir = env),
      error = function(cnd) {
        typewriter_abort(
          message = c(
            sprintf("Can't initialize typed object `%s` to the first argument of `call`.", rlang::as_name(sym)),
            i = sprintf("Attempted assignment `%s <- %s`.", rlang::as_name(sym), rlang::as_label(call_args[[1]]))
          ),
          call = error_call,
          class = "typewriter_error_input_type",
          parent = cnd
        )
      }
    )
    rlang::try_fetch(
      eval(call, envir = env),
      error = function(cnd) {
        typewriter_abort(
          message = c(
            sprintf("Can't initialize typed object `%s` using `call`.", rlang::as_name(sym)),
            i = sprintf("Attempted to evaluate `call = %s`.", rlang::as_label(call))
          ),
          call = error_call,
          class = "typewriter_error_input_type",
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
          typewriter_abort(
            message = c(
              sprintf("Can't evaluate argument `%s = %s` of `call`.", arg_name, rlang::as_label(arg)),
              x = sprintf("Every argument to `call` must be evaluable in `env = <env: %s>`.", rlang::env_label(env))
            ),
            call = error_call,
            class = "typewriter_error_input_type",
            parent = cnd
          )
        }
      )
    },
    SIMPLIFY = FALSE
  )
  call <- rlang::call_modify(
    .call = call[1], # Gets `call` with no arguments, we resupply them all here
    sym,
    !!!lapply(
      rlang::set_names(names(ARGS)),
      \(arg_name) rlang::call2("$", quote(ARGS), as.symbol(arg_name))
    )
  )

  list(
    TYPE_CALL = call,
    VALUE = VALUE,
    ARGS = ARGS
  )
}

# temp check functions for testing ---------------------------------------------

check_integer <- function(x, x_name = rlang::caller_arg(x)) {
  if (is.integer(x)) {
    return(x)
  }
  stop(sprintf("`%s` must be an integer.", x_name))
}

check_numeric <- function(x, len = NULL, x_name = rlang::caller_arg(x)) {
  if (is.numeric(x) && (is.null(len) || length(x) == len)) {
    return(x)
  }
  if (!is.numeric(x)) stop(sprintf("`%s` must be an numeric.", x_name))
  stop(sprintf("`%s` must be length %i.", x_name, len))
}
