test_that("`%<~%` works for typed object assignment.", {
  # Should be able to assign a call (with/without initialization value) or alias
  int1 %<~% check_integer()
  int2 %<~% check_integer_alias
  int3 %<~% check_integer(1:5)
  scalar_int1 %<~% check_integer(len = 1L)
  scalar_int2 %<~% check_integer(1L, len = 1L)

  expect_true(is_uninitialized(int1))
  expect_true(is_uninitialized(int2))
  expect_true(is_uninitialized(scalar_int1))

  expect_false(is_uninitialized(int3))
  expect_false(is_uninitialized(scalar_int2))
  expect_identical(int3, 1:5)
  expect_identical(scalar_int2, 1L)

  expect_error(int1 <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(int2 <- TRUE, class = "typewriter_error_invalid_assignment")
  expect_error(int3 <- Inf, class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int1 <- 1:5, class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int2 <- 1:2, class = "typewriter_error_invalid_assignment")

  # Invalid assignment doesn't overwrite the previous value of a typed object
  rlang::try_fetch(int1 <- "A", error = rlang::cnd_muffle)
  rlang::try_fetch(int3 <- "A", error = rlang::cnd_muffle)
  expect_true(is_uninitialized(int1))
  expect_identical(int3, 1:5)

  int1 <- 1:10
  int2 <- NA_integer_
  int3 <- int2
  scalar_int1 <- 0L
  expect_identical(int1, 1:10)
  expect_identical(int2, NA_integer_)
  expect_identical(int3, int2)
  expect_identical(scalar_int1, 0L)

  # Operator `<<-` works as expected
  (function() int1 <<- 10L)()
  expect_identical(int1, 10L)
  expect_error((function() int1 <<- "A")(), class = "typewriter_error_invalid_assignment")

  # `base::assign()` works as expected
  base::assign("int1", 2L)
  expect_identical(int1, 2L)
  expect_error(base::assign("int1", "A"), class = "typewriter_error_invalid_assignment")
})

test_that("`%<~%` works for typed function assignment.", {
  fun %<~% function(
    x = check_integer(),
    y = check_integer_alias,
    z = check_integer(len = 1L),
    q = check_integer(0L),
    r = check_integer(1:2, len = 2L)
  ) {
    TRUE
  }

  fun_dots %<~% function(... = check_integer(), x = check_integer(1L)) {
    TRUE
  }

  expect_true(is_typed_function(fun))
  expect_true(is_typed_function(fun_dots))

  expect_identical(
    attr(fun, "type_calls"),
    rlang::exprs(
      check_integer(x),
      check_integer_alias(y),
      check_integer(z, len = 1L),
      check_integer(q),
      check_integer(r, len = 2L),
      .named = NULL
    )
  )
  expect_identical(
    formals(fun),
    rlang::pairlist2(x = , y = , z = , q = 0L, r = quote(1:2))
  )
  expect_identical(
    formals(fun_dots),
    rlang::pairlist2(... = , x = 1L)
  )

  expect_true(fun(x = 1:5, y = 1:5, z = 1L))
  expect_true(fun_dots(1:5, 1:5, 1L, x = 0L))

  expect_error(
    fun(x = "A", y = 1L, z = 1L),
    class = "invalid_input",
    regexp = "Invalid object x",
    fixed = TRUE
  )
  expect_error(
    fun_dots(1L, "A"),
    class = "invalid_input",
    regexp = "Invalid object ..2",
    fixed = TRUE
  )
})

test_that("`%<~%` works with external package checks.", {
  int1 %<~% chk::chk_integer()
  int2 %<~% chk::check_values(values = integer())
  int3 %<~% chk::chk_integer(1:4)

  expect_true(is_uninitialized(int1))
  expect_true(is_uninitialized(int2))
  expect_false(is_uninitialized(int3))

  expect_identical(int3, 1:4)

  expect_error(int1 <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(int2 <- 0.5, class = "typewriter_error_invalid_assignment")
  expect_error(int3 <- TRUE, class = "typewriter_error_invalid_assignment")

  expect_no_error(int1 <- 10L)
  expect_no_error(int2 <- 1:5)
  expect_no_error(int3 <- NA_integer_)
})

test_that("`%<~%` errors on invalid inputs.", {
  # `sym` must be a symbol
  expect_error("A" %<~% check_integer(), class = "typewriter_error_invalid_input")
  expect_error("A" %<~% check_integer_alias, class = "typewriter_error_invalid_input")
  expect_error(c(x) %<~% check_integer(), class = "typewriter_error_invalid_input")

  # `call` must be to function of type "closure"
  checks <- list(check = check_integer)
  expect_error(x %<~% checks[[1]], class = "typewriter_error_invalid_input")
  expect_error(x %<~% checks$check, class = "typewriter_error_invalid_input")
  expect_error(x %<~% sum(), class = "typewriter_error_invalid_input")

  # `call` must be a simple call
  expect_error(x %<~% checks$check(), class = "typewriter_error_invalid_input")
  expect_error(x %<~% check_integer, class = "typewriter_error_invalid_input")
  expect_error(x %<~% 10, class = "typewriter_error_invalid_input")
  expect_error(x %<~% chk::chk_integer, class = "typewriter_error_invalid_input")

  # `call` must exist
  expect_error(x %<~% non_extant_symbol, class = "typewriter_error_invalid_input")
  expect_error(x %<~% non_extant_call(), class = "typewriter_error_invalid_input")

  # `call` is evaluated if an initialization value is provided
  expect_error(x %<~% check_integer("A"), class = "typewriter_error_invalid_assignment")
  expect_error(x %<~% check_integer(1L, len = 2L), class = "typewriter_error_invalid_assignment")

  # `call` arguments must all be evaluable
  expect_error(x %<~% check_integer(stop()), class = "typewriter_error_invalid_input")
  expect_error(x %<~% check_integer(stop(), len = 1L), class = "typewriter_error_invalid_input")
  expect_error(x %<~% check_integer(len = stop()), class = "typewriter_error_invalid_input")
  expect_error(x %<~% check_integer(1L, len = stop()), class = "typewriter_error_invalid_input")

  # Invalid function typing throws an error from `typed()`
  expect_error(
    foo %<~% function(x = typewriter::optional()) { TRUE },
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    foo %<~% function(x = optional(required(check_integer()))) { TRUE },
    class = "typewriter_error_invalid_input"
  )
})

test_that("`%<~%` errors when `call` doesn't exist in `env`", {
  not_a_function <- 10L
  expect_error(x %<~% not_a_function(), class = "typewriter_error_invalid_input")
  expect_error(x %<~% non_extant_alias, class = "typewriter_error_invalid_input")
  expect_error(x %<~% non_extant_fun(), class = "typewriter_error_invalid_input")
  expect_error(x %<~% non_extant_ns::non_extant_alias, class = "typewriter_error_invalid_input")
  expect_error(x %<~% non_extant_ns::non_extant_fun(), class = "typewriter_error_invalid_input")
})

test_that("`%<~%` overwrites existing symbols in the caller's enironment.", {
  x <- "A"
  y <- 10L

  x %<~% check_integer()
  y %<~% check_integer(1:5)
  expect_true(is_uninitialized(x))
  expect_identical(y, 1:5)

  x <- 1L
  expect_identical(x, 1L)
})

test_that("Typed objects within a function reference the correct function call in errors.", {
  foo <- function() {
    x %<~% check_integer()
    x <- "A"
  }
  expect_error(foo(), class = "typewriter_error_invalid_assignment")
  expect_identical(rlang::catch_cnd(foo())$call, quote(foo()))
})

test_that("`assign_typed()` works", {
  assign_typed(int1, check_integer())
  assign_typed(int2, check_integer_alias)
  assign_typed(int3, check_integer(1:5))
  assign_typed(scalar_int1, check_integer(len = 1L))
  assign_typed(scalar_int2, check_integer(1L, len = 1L))

  expect_true(is_uninitialized(int1))
  expect_true(is_uninitialized(int2))
  expect_true(is_uninitialized(scalar_int1))

  expect_false(is_uninitialized(int3))
  expect_false(is_uninitialized(scalar_int2))
  expect_identical(int3, 1:5)
  expect_identical(scalar_int2, 1L)

  expect_error(int1 <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(int2 <- TRUE, class = "typewriter_error_invalid_assignment")
  expect_error(int3 <- Inf, class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int1 <- 1:5, class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int2 <- 1:2, class = "typewriter_error_invalid_assignment")

  # Invalid assignment doesn't overwrite the previous value of a typed object
  rlang::try_fetch(int1 <- "A", error = rlang::cnd_muffle)
  rlang::try_fetch(int3 <- "A", error = rlang::cnd_muffle)
  expect_true(is_uninitialized(int1))
  expect_identical(int3, 1:5)

  int1 <- 1:10
  int2 <- NA_integer_
  int3 <- int2
  scalar_int1 <- 0L
  expect_identical(int1, 1:10)
  expect_identical(int2, NA_integer_)
  expect_identical(int3, int2)
  expect_identical(scalar_int1, 0L)
})

test_that("`assign_typed()` `env` argument works correctly.", {
  env <- new.env()
  int <- 1L
  assign_typed(int, check_integer(2L), env = env)

  # We don't alter the caller's environment
  expect_identical(int, 1L)

  # We do alter `env`
  expect_true(rlang::env_has(env, "int"))
  expect_identical(ls(envir = env), "int")
  expect_identical(env$int, 2L)

  # Active binding is attached correctly in `env`
  expect_error(env$int <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(assign("int", "A", envir = env), class = "typewriter_error_invalid_assignment")

  # New typed assignment overwrites existing objects in `env`
  env$x <- "A"
  assign_typed(int, check_integer(10L), env = env)
  assign_typed(x, check_integer(11L), env = env)
  expect_identical(env$int, 10L)
  expect_identical(env$x, 11L)
})

test_that("`assign_typed()` errors when `call` doesn't exist in `env`", {
  env <- new.env()
  env$not_a_function <- 10
  expect_error(assign_typed(z, not_a_function(), env = env), class = "typewriter_error_invalid_input")
  expect_error(assign_typed(z, non_extant_alias, env = env), class = "typewriter_error_invalid_input")
  expect_error(assign_typed(z, non_extant_fun(), env = env), class = "typewriter_error_invalid_input")
  expect_error(assign_typed(z, non_extant_ns::non_extant_alias, env = env), class = "typewriter_error_invalid_input")
  expect_error(assign_typed(z, non_extant_ns::non_extant_fun(), env = env), class = "typewriter_error_invalid_input")
})
