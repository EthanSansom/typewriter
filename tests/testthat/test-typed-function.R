# todos ------------------------------------------------------------------------

# - errors on invalid inputs
# - test `as_typed()` once finished

# TODO: Check the use of external package checks using:
# - rlang::rlang::arg_match
# - rlang::check_required
#
# We could also add {chk} to Suggests..., might be worth it for documentation
# sake. Look back at how {friendlynumber} used Suggested package {bignum} in
# tests, examples, and documentation.

# tests ------------------------------------------------------------------------

test_that("`typed()` works.", {
  fun <- typed(function(
      x = check_integer(),
      y = check_integer_alias,
      z = check_integer(len = 1L),
      q = check_integer(0L),
      r = check_integer(1:2, len = 2L)
    ) {
    TRUE
  })

  expect_true(is_typed_function(fun))
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

  # Arguments of valid types are passed through the function
  expect_true(fun(x = 1:5, y = 1:5, z = 1L))

  # Arguments of invalid types cause the expected error
  expect_error(
    fun(x = "A", y = 1L, z = 1L),
    class = "invalid_input",
    regexp = "Invalid object x",
    fixed = TRUE
  )
  expect_error(
    fun(x = 1L, y = "A", z = 1L),
    class = "invalid_input",
    regexp = "Invalid object y",
    fixed = TRUE
  )
  expect_error(
    fun(x = 1L, y = 1L, z = 1:2),
    class = "invalid_input",
    regexp = "Invalid object z",
    fixed = TRUE
  )
  expect_error(
    fun(x = 1L, y = 1L, z = 1L, q = "A"),
    class = "invalid_input",
    regexp = "Invalid object q",
    fixed = TRUE
  )
  expect_error(
    fun(x = 1L, y = 1L, z = 1L, r = 0L),
    class = "invalid_input",
    regexp = "Invalid object r",
    fixed = TRUE
  )
})

test_that("`typed()` errors on invalid inputs.", {
  expect_error(typed(10), class = "typewriter_error_invalid_input")
  expect_error(typed(quote(x)), class = "typewriter_error_invalid_input")

  # Calls must be to functions which exist in `env`
  not_a_function <- 10L
  expect_error(
    typed(function(x = not_a_function()) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = non_extant_function()) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = non_extant_ns::non_extant_function()) {}),
    class = "typewriter_error_invalid_input"
  )
  # Calls must be simple calls
  expect_error(
    typed(function(x = bar$foo()) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = bar()()) {}),
    class = "typewriter_error_invalid_input"
  )
  # Can't use primitive or special functions
  expect_error(
    typed(function(x = bar$foo) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = sum()) {}),
    class = "typewriter_error_invalid_input"
  )
})

test_that("`typed()` works with no typed arguments.", {
  foo1 <- typed(function() { TRUE })
  foo2 <- typed(function(x, y, ..., z = 10) { TRUE })

  expect_true(is_typed_function(foo1))
  expect_true(is_typed_function(foo2))
  expect_length(attr(foo1, "typed_args_names"), 0)
  expect_length(attr(foo2, "typed_args_names"), 0)
  expect_true(foo1())
  expect_true(foo2(x = 1, y = 2, 1))
})

test_that("`typed()` works with `...` argument.", {
  dots_only <- typed(function(... = check_integer()) { TRUE })
  dots_only_alias <- typed(function(... = check_integer_alias) { TRUE })
  dots_and_others <- typed(function(... = check_integer(), x = check_integer(10L)) { TRUE })

  expect_true(is_typed_function(dots_only))
  expect_true(is_typed_function(dots_only_alias))
  expect_true(is_typed_function(dots_and_others))

  expect_identical(formals(dots_only), rlang::pairlist2(... = ))
  expect_identical(formals(dots_only_alias), rlang::pairlist2(... = ))
  expect_identical(formals(dots_and_others), rlang::pairlist2(... = , x = 10L))

  # Arguments of valid types are passed through the function
  expect_true(dots_only(1:5, 1:5, 1L))
  expect_true(dots_only(x = 1:5, y = 1:5, z = 1L))
  expect_true(dots_only_alias(1:5, 1:5, 1L))
  expect_true(dots_and_others(1:5, 1:5, 1L, x = 12L))

  # Arguments of invalid types cause the expected error
  expect_error(
    dots_only(1:5, 1:5, "A"),
    class = "invalid_input",
    regexp = "Invalid object ..3",
    fixed = TRUE
  )
  expect_error(
    dots_only_alias(1:5, 1:5, "A"),
    class = "invalid_input",
    regexp = "Invalid object ..3",
    fixed = TRUE
  )
  expect_error(
    dots_and_others(x = "A"),
    class = "invalid_input",
    regexp = "Invalid object x",
    fixed = TRUE
  )
})

# modifiers --------------------------------------------------------------------

# TODO:
# - maybe(), optional(), required()
# - test whether combinations of modifiers work as expected

test_that("`untyped()` modifier works.", {
  foo1 <- typed(function(x = untyped()) { TRUE })
  foo2 <- typed(function(x = untyped(as.Date("2020-01-01"))) { TRUE })

  expect_true(is_typed_function(foo1))
  expect_true(is_typed_function(foo2))
  expect_identical(formals(foo1), rlang::pairlist2(x = ))
  expect_identical(formals(foo2), rlang::pairlist2(x = quote(as.Date("2020-01-01"))))
  expect_true(foo1())
  expect_true(foo2())

  expect_error(
    typed(function(... = untyped()) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = untyped(1, 1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
})

test_that("`optional()` modifier works.", {
  foo <- typed(function(x = optional(check_integer())) {
    if (rlang::is_missing(x)) rlang::missing_arg() else x
  })

  expect_error(foo("A"), class = "invalid_input")
  expect_identical(foo(1L), 1L)
  expect_identical(foo(), rlang::missing_arg())

  expect_error(
    typed(function(x = optional(1, 1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = optional()) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = optional(required(check_integer()))) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
})

test_that("`static()` modifier works.", {
  foo_bad <- typed(function(x = static(check_integer())) {
    x <- "A"
  })
  foo_good <- typed(function(x = static(check_integer())) {
    x <- 10L
    x
  })

  # If the input argument is bad, we should raise the `check_integer()` error
  expect_error(foo_bad("A"), class = "invalid_input")
  expect_error(foo_good("A"), class = "invalid_input")

  # If the "statically" typed object `x` is re-assigned in `foo_bad()`, we
  # should raise an assignment error.
  expect_error(foo_bad(10L), class = "typewriter_error_invalid_assignment")

  expect_no_error(foo_good(10L))
  expect_identical(foo_good(1L), 10L)
})
