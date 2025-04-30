# todos ------------------------------------------------------------------------

# TODO: `as_alias()` takes a function, we wrap it and supply it's first argument

# TODO: What do we do with the alias when the first argument is unnamed?
# - This is normally the initialization value, but not in our context
# - I feel like we should throw an error, since we're expecting the same
#   kind of function as in `%<~%`, in which the first argument should be
#   an initialization value...

# TODO: Implement the `bullets` argument of the `alias`, which we'll need for implementing
#       list_of and other aliases.

# functions --------------------------------------------------------------------
new_alias <- function(call) {

}

is_alias <- function(x) {
  inherits(x, "typewriter_alias")
}

# Creating an alias

alias <- function(call, name = NULL, desc = NULL, bullets = NULL) {
  call <- check_is_simple_call(x = rlang::enexpr(call), x_name = "call")
  error_call <- rlang::current_env()
  env <- rlang::caller_env()

  call_fun_sym <- call[[1]]
  call_args <- as.list(call[-1])

  maybe_function <- check_is_evaluable(
    call_fun_sym,
    env = env,
    message = c(
      "`call` must be a simple call.",
      i = sprintf("`call` is a simple call to `%s`.", rlang::as_label(call_fun_sym)),
      x = sprintf("Can't evaluate `%s` in `env = %s`.", rlang::as_label(call_fun_sym), env_desc(env))
    ),
    call = error_call
  )
  fun <- check_is_function(
    maybe_function,
    message = c(
      "`call` must be a simple call.",
      x = sprintf("`call` is a malformed call to `%s`.", rlang::as_label(call_fun_sym)),
      x = sprintf("`%s` is %s in `env = %s`, not a function.", env_desc(env), obj_type_friendly(maybe_function))
    ),
    call = error_call
  )
  if (typeof(fun) %in% c("builtin", "special")) {
    typewriter_abort_invalid_input(
      message = c(
        '`call` must be a simple call to a function of type "closure".',
        i = sprintf("`call` is a simple call to function `%s`.", rlang::as_label(call_fun_sym)),
        x = sprintf('The function `%s` is of type "%s".', rlang::as_label(call_fun_sym), typeof(fun))
      ),
      call = error_call
    )
  }
  args <- mapply(
    arg = call_args,
    arg_name = rlang::names2(call_args),
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

  if (rlang::is_call(call_fun_sym, c("::", ":::"))) {
    call_fun_sym <- as.symbol(call_fun_sym[[3]])
  }
  fun_call <- rlang::call_modify(rlang::call2(call_fun_sym), ... = , !!!args)

  # TODO: These inserted `rlang::abort` calls should be some {typewriter} wrapper instead
  body <- rlang::expr({
    if (!is.null(...names())) {
      rlang::abort("Arguments to `...` must be unnamed.")
    }
    if (...length() != 1) {
      rlang::abort(c(
        "Must supply exactly one argument to `...`.",
        x = sprintf("Supplied %i arguments to `...`.", ...length())
      ))
    }
    !!call_fun_sym <- !!fun
    !!fun_call
    ...elt(1L)
  })

  out <- rlang::new_function(
    args = rlang::pairlist2(... = ),
    body = body,
    env = env
  )

  new_alias(
    fun = out,
    name = name %||% rlang::as_name(call_fun_sym),
    desc = desc %||% rlang::as_label(call)
  )
}

new_alias <- function(fun, name, desc) {
  structure(
    fun,
    class = c("typewriter_alias", "function"),
    name = name,
    desc = desc
  )
}

# NOTE: Allows `check_integer(error_call = rlang::caller_env())` to get the correct env
# when the check is wrapped in an alias.
#' @export
alias_caller <- function() {
  quote(parent.frame(1))
}

#' @export
print.typewriter_alias <- function(x, ...) {
  cat(sprintf("<alias<%s>>\n", attr(x, "name")))
  # cat(paste0(attr(x, "desc"), "\n"))

  # TODO: For testing, set back to old version later.
  print(unclass(x))
}

# Test `alias()`
if (FALSE) {
  # Typical `check` function from {checkwriter}
  check_is_integer <- function(x, x_name = rlang::caller_arg(x), len = NULL, call = rlang::caller_env()) {
    if (is.integer(x) && (is.null(len) || length(x) == len)) {
      return(x)
    }
    if (is.integer(x)) rlang::abort(sprintf("`%s` is the wrong length.", x_name), call = call)
    rlang::abort(sprintf("`%s` must be an integer.", x_name), call = call)
  }

  # Define the alias
  scalar_int <- alias(check_is_integer(len = 1L))
  scalar_int(10.5) # Raises the correct `check_is_integer()` error

  # These all throw an error (Good)
  scalar_int()
  scalar_int(100, 11)
  scalar_int(x = 10)

  scalar_int(10L)

  # Namespaced calls work as expected
  pkg <- alias(rlang::check_installed())
  pkg

  # Base calls work as expected
  mean2 <- alias(base::mean(na.rm = TRUE))
  mean2
  mean2(1:5) # We always return the input, so you won't see result of `base::mean`

  int <- alias(check_is_integer(), "integer", "An integer.")
  scalar_int <- alias(check_is_integer(len = 1L), "integer(1)", "A scalar integer.")

  int
  scalar_int

  int2 <- alias(check_is_integer(call = alias_caller()), "integer", "An integer vector.")

  foo1 <- function(x) {
    int(x)
  }
  foo2 <- function(x) {
    int2(x)
  }

  # Huh, I'm not really sure what's going on here
  foo1(10.5)
  foo2(10.5)
}


# `purrr::partial()` investigation
if (FALSE) {
  ?purrr::partial

  purrr::partial(foo, x = 10, y = 12)

  foo <- function(x, x_name = rlang::caller_arg(x), z = 10) {
    list(x_name = x_name, z = z)
  }

  foo_par <- purrr::partial(foo, z = 5)
  foo_par(y)

  foo_par <- purrr::partial(foo, z = !!as.Date("2020-01-01"))
  foo_par(y)

  foo_par |> rlang::fn_body()
  foo_par |> rlang::fn_env()

  bar <- function(...) {
    foo(...)
  }

  bar2 <- function(...) {
    foo(x)
  }

  bar(y)

  lst <- list(x = foo)

  purrr::partial(lst$x)

  purrr::partial(lst$x) |> rlang::fn_env() |> rlang::env_print()

  load_all()

  # NOTE: So the {typed} package uses my intended approach, where you simply
  #       in-line the exact call into the active binding function. This does
  #       mean HOWEVER that if the function being called in CHANGED in the
  #       global environment, then the behavior changes as well.

  # Original version of `check_is_integer`
  check_is_integer <- function(x, x_name = rlang::caller_arg(x)) {
    if (is.integer(x)) {
      return(x)
    }
    stop(sprintf("`%s` must be an integer, not %s.", x_name, typeof(x)))
  }

  # We declare `x` using `check_is_integer`
  typed::declare("x", check_is_integer)

  # Observe that `x` is actively bound to a function where the following is called:
  # `try(check_is_integer(assigned_value), silent = TRUE)`
  activeBindingFunction(quote(x), globalenv())

  # Expected behavior, raises the error
  x <- "11"
  x <- 10.5

  # We now change the function `check_is_integer`
  check_is_integer <- function(x) {
    print("I've changed")
    return(TRUE)
  }

  # Bad behavior
  x <- "11"
  x

  # TODO: Can we break `purrr::partial`? What cases could negatively effect the
  #       inlining of a function definition?
  global_value <- 10
  global_function <- function() { print("Called global_function") }

  # Let's see if partialing reflects changes to the global state for `foo()`
  foo <- function(x, y) {
    global_function()
    global_value
  }

  foo_par <- purrr::partial(foo, y = 10)

  foo_par()
  foo()

  global_function <- function() { print("Called global_function V2") }
  global_value <- 11

  foo_par()
  foo()

  # What if `foo()` was created as a wrapped function with it's own environment vars
  make_foo <- function() {
    local_value <- 1
    local_function <- function() { print("Called local_function") }
    function(x, y) {

      print(paste("Parent env:", format(rlang::current_env() |> rlang::env_parent())))
      print("Parent env details:")
      rlang::current_env() |> rlang::env_parent() |> rlang::env_print()
      print(paste("Caller env:", format(rlang::caller_env())))
      cat("\n")

      local_function()
      local_value
    }
  }

  # Interesting, so when you inline a function definition with `!!.fn` in `purrr::partial`,
  # even though it LOOKS like the defined function should inherit from a different
  # parent it does not.
  foo <- make_foo()
  foo_par <- purrr::partial(foo, y = 10)

  # For `foo()`, the caller of `foo()` is global. But for `foo_par()`, its the
  # function environment of the wrapped function.
  foo()
  foo_par()

  # See that `foo` is actually re-defined BEFORE it is called by `foo_par`. It
  # looks like this version of `foo` shouldn't know about the `local_function()`
  # and `local_value` defined in `make_foo()`.
  # HOWEVER, this print just isn't showing that the `foo <- ` definition in there
  # is STILL defined in the `make_foo()` environment. That's why the parent environment
  # printed for "Parent env" is the same between `foo` and `foo_par`
  foo_par |> rlang::fn_body()

  # Even if we change these here, the partialled function STILL knows which
  # `local_function` and `local_value` to use.
  local_function <- function() { print("Called local_function V2") }
  local_value <- 2

  foo()
  foo_par()
}
