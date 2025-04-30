# todos ------------------------------------------------------------------------

# TODO: Check if you're using any newer {rlang} functions (maybe `rlang::try_fetch()`)
#       and up the version number if so (or use `withCallingHandlers`).

# TODO: Move the implementation or `internal_assign_typed()` into `assign_typed()`,
#       no reason currently to sperate them

# assignment -------------------------------------------------------------------

# TODO: Once `typed_function()` exists, update this to dispatch differently when
#       the RHS is a call to `function`.
`%<~%` <- function(sym, call) {
  internal_assign_typed(
    sym = rlang::enexpr(sym),
    call = rlang::enexpr(call),
    env = rlang::caller_env()
  )
}

assign_typed <- function(sym, call, env = rlang::caller_env()) {
  internal_assign_typed(
    sym = rlang::enexpr(sym),
    call = rlang::enexpr(call),
    env = env
  )
}

internal_assign_typed <- function(sym, call, env) {

  error_call <- rlang::caller_env()
  sym <- check_is_symbol(sym, call = error_call)

  # An <alias> may be provided by name, e.g. `call = ns::a_int` or `call = a_int`
  # In this case we convert it into a call, e.g. `a_int()`. Otherwise, `call`
  # must be a simple call to a function which exists in `env`.
  if (is_named_symbol(call)) {
    maybe_alias <- check_is_evaluable(
      x = call,
      env = env,
      message = c(
        "`call` must be an <alias> provided by name or a simple call.",
        i = sprintf("`call` is the object `%s`.", rlang::as_label(call)),
        x = sprintf("Can't evaluate `call` in `env = %s`", env_desc(env))
      ),
      call = error_call
    )
    if (!is_alias(maybe_alias)) {
      typewriter_abort_invalid_input(
        sprintf("`call` must be an <alias> or a simple call, not %s.", obj_type_friendly(maybe_alias)),
        call = error_call
      )
    }
    call_fun <- maybe_alias
    call_fun_sym <- call
    call <- rlang::call2(call)
  } else {
    check_is_simple_call(
      x = call,
      message = c(
        "`call` must be an <alias> or a simple call.",
        x = sprintf("`call = %s` is a complex call.", rlang::as_label(call)),
        i = "A simple call has the form `foo()` or `ns::foo()`, not e.g. bar$foo()."
      ),
      call = error_call
    )
    maybe_function <- check_is_evaluable(
      x = call[[1]],
      env = env,
      message = c(
        "`call` must be an <alias> or a simple call.",
        i = sprintf("`call` is a simple call to `%s`.", rlang::as_label(call[[1]])),
        x = sprintf("Can't evaluate `%s` in `env = %s`.", rlang::as_label(call[[1]]), env_desc(env))
      ),
      call = error_call
    )
    fun <- check_is_function(
      maybe_function,
      message = c(
        "`call` must be an <alias> or a simple call.",
        x = sprintf("`call` is a malformed call to `%s`.", rlang::as_label(call[[1]])),
        x = sprintf("`%s` is %s in `env = %s`, not a function.", env_desc(env), obj_type_friendly(maybe_function))
      ),
      call = error_call
    )
    if (typeof(fun) %in% c("builtin", "special")) {
      typewriter_abort_invalid_input(
        message = c(
          '`call` must be an <alias> or a simple call to a function of type "closure".',
          i = sprintf("`call` is a simple call to function `%s`.", rlang::as_label(call[[1]])),
          x = sprintf('The function `%s` is of type "%s".', rlang::as_label(call[[1]]), typeof(fun))
        ),
        call = error_call
      )
    }
    call_fun_sym <- call[[1]]
    call_fun <- fun
  }

  call_args <- rlang::call_args(call)
  call_args_names <- rlang::names2(call_args)

  args <- mapply(
    arg = call_args,
    arg_name = call_args_names,
    arg_pos = seq_along(call_args),
    FUN = \(arg, arg_name, arg_pos) {
      check_is_evaluable(
        arg,
        env = env,
        call = error_call,
        message = if (arg_name == "") {
          sprintf("Can't evaluate argument `%s` at postion %i of `call`.", rlang::as_label(arg), arg_pos)
        } else {
          sprintf("Can't evaluate argument `%s = %s` of `call`.", arg_name, rlang::as_label(arg))
        }
      )
    },
    SIMPLIFY = FALSE
  )

  # Take the first argument of `call` to be the initialization value of the
  # assigned object if the argument is unnamed.
  if (length(call_args) != 0 && call_args_names[[1]] == "") {
    VALUE <- args[[1]]
    args <- args[-1]

    # Ensure that the initialization value is of the correct type by attempting
    # to evaluate the provided type-checking `call` with `VALUE` as it's first
    # argument.
    rlang::try_fetch(
      expr = eval(call, env),
      error = function(cnd) {
        typewriter_abort_invalid_input(
          message = c(
            sprintf("Can't declare typed object `%s`.", rlang::as_name(sym)),
            i = sprintf("Attempted assignment `%s <- %s`.", rlang::as_name(sym), rlang::as_label(call_args[[1]]))
          ),
          call = error_call,
          parent = cnd
        )
      }
    )
  } else {
    # TODO: We'll want to provide information about the call to the `new_uninitialized`
    #       so that it can reference the type using either alias info or quoting the call
    VALUE <- new_uninitialized()
  }

  # We in-line the definition of `call_fun` into the active binding function
  # and assign that definition to `call_fun_sym`. This allows error messages
  # to reference a call of the correct name. However, we don't want to overwrite
  # a namespaced function during assignment, so we take the `foo` from `ns::foo`
  # if `call_fun_sym` is a namespaced symbol `ns::foo`.
  if (rlang::is_call(call_fun_sym, c("::", ":::"))) {
    call_fun_sym <- as.symbol(call_fun_sym[[3]])
  }

  new_call <- rlang::call_modify(rlang::call2(call_fun_sym), sym, !!!args)
  sym_name <- rlang::as_name(sym)

  active_binding_args <- symbol_to_pairlist(sym)
  active_binding_body <- rlang::expr({
    if (missing(!!sym)) {
      return(VALUE)
    }

    # In-lined definition of the function called in `call`
    !!call_fun_sym <- !!call_fun

    # Attempt to call the in-lined version of `call`, with `sym` as it's first argument
    rlang::try_fetch(
      expr = !!new_call,
      error = function(cnd) {
        rlang::abort(
          message = sprintf("Attempted to assign an invalid value to typed object `%s`.", !!sym_name),
          class = c("typewriter_error", "typewriter_error_invalid_assignment"),
          call = !!env,
          parent = cnd
        )
      }
    )
    # Since we passed the check above, update the value of the typed object which
    # we store in the parent environment of this function.
    VALUE <<- !!sym
    VALUE
  })

  active_binding_function <- rlang::new_function(
    args = active_binding_args,
    body = active_binding_body,
    env = rlang::new_environment(data = list(VALUE = VALUE), parent = env)
  )

  rlang::env_unbind(env, sym_name)
  makeActiveBinding(sym = sym, fun = active_binding_function, env = env)
}

# helpers ----------------------------------------------------------------------

symbol_to_pairlist <- function(sym) {
  args <- rlang::pairlist2(x = )
  names(args) <- rlang::as_name(sym)
  args
}

env_desc <- function(env) {
  sprintf("<environment: %s>", rlang::env_label(env))
}

is_named_symbol <- function(x) {
  is.symbol(x) || rlang::is_call(x, c("::", ":::"))
}

type_fun_body <- function(sym, call) {
  sym_name <- rlang::as_name(sym)
  rlang::expr({
    if (missing(!!sym)) {
      return(VALUE)
    }
    rlang::try_fetch(
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
    VALUE <<- !!sym
    VALUE
  })
}

# TODO: We will want to split off the `VALUE` extraction part of this, since
#       `typed_function()` will need to do a similar process for each typed
#       argument in the function.
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
    VALUE <- check_is_evaluable(
      x = call_args[[1]],
      env = env,
      message = c(
        sprintf("Can't initialize object `%s` using the first argument of `call`.", rlang::as_name(sym)),
        i = sprintf("Attempted assignment `%s <- %s`.", rlang::as_name(sym), rlang::as_label(call_args[[1]]))
      ),
      call = error_call
    )
    check_is_evaluable(
      x = call,
      env = env,
      message = c(
        sprintf("Can't initialize typed object `%s` using `call`.", rlang::as_name(sym)),
        i = sprintf("Attempted to evaluate `call = %s`.", rlang::as_label(call))
      ),
      call = error_call
    )
    call <- call[-2] # Remove the first argument after extracting the value
  }

  # Pre-evaluate every named argument to `call` and modify `call` to (1) reference
  # these evaluated arguments and (2) take `sym` as it's first argument.  E.g.
  # `foo(x = 1, y = 1 + 1)` becomes `foo(sym, x = ARGS$x, y = ARGS$y)` where
  # `ARGS = list(x = 1, y = 2)` is the list of pre-evaluated arguments.
  ARGS <- map2(
    .x = call_args[named_at],
    .y = names(call_args[named_at]),
    .f = \(arg, arg_name) {
      check_is_evaluable(
        x = arg,
        env = env,
        message = c(
          sprintf("Can't evaluate argument `%s = %s` of `call`.", arg_name, rlang::as_label(arg)),
          x = sprintf("Every argument to `call` must be evaluable in `env = <environment: %s>`.", rlang::env_label(env))
        ),
        call = error_call
      )
    }
  )

  # Creates a named list like `arg_name = ARGS$arg_name`
  inlined_ARGS <- function(arg_names) {
    map(arg_names, \(arg_name) rlang::call2("$", quote(ARGS), rlang::sym(arg_name)))
  }

  call <- rlang::call_modify(
    .call = call[1], # Gets `call` with no arguments, we re-supply them all here
    sym,
    !!!inlined_ARGS(names(ARGS))
  )

  list(
    CALL = call,
    VALUE = VALUE,
    ARGS = ARGS
  )
}
