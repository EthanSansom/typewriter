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

test_that("`typed()` works with package checks.", {
  foo <- typed(function(x = chk::chk_character()) { x })

  expect_identical(foo("A"), "A")
  expect_error(foo(10L))

  withr::local_package("chk")
  bar <- typed(function(... = chk_count()) { list(...) })

  expect_identical(bar(1L, 2.0), list(1L, 2.0))
  expect_error(bar(1L, 7.5))
})

test_that("`typed()` allows the use of undefined functions.", {
  not_a_function <- 10L
  foo_1 <- typed(function(x = not_a_function()) {})
  foo_2 <- typed(function(x = non_extant_function()) {})
  foo_3 <- typed(function(x = non_extant_ns::non_extant_function()) {})
  foo_4 <- typed(function(x = required(not_a_function())) {})

  expect_true(is_typed_function(foo_1))
  expect_true(is_typed_function(foo_2))
  expect_true(is_typed_function(foo_3))
  expect_true(is_typed_function(foo_4))

  expect_error(foo_4(), class = "typewriter_error_typed_arg_missing")
})

test_that("`typed()` errors on invalid inputs.", {
  expect_error(typed(10), class = "typewriter_error_invalid_input")
  expect_error(typed(quote(x)), class = "typewriter_error_invalid_input")

  # Calls must be simple calls
  expect_error(
    typed(function(x = bar$foo()) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = bar()()) {}),
    class = "typewriter_error_invalid_input"
  )
  # `..i` is a reserved symbol
  expect_error(
    typed(function(x = ..i) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = untyped(..i)) {}),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = check_integer(..i)) {}),
    class = "typewriter_error_invalid_input"
  )
  # `...` can't have an initialization value
  expect_error(
    typed(function(... = check_integer(10L)) {}),
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
  bar <- typed(function(x = optional(check_integer_alias)) {
    if (rlang::is_missing(x)) rlang::missing_arg() else x
  })

  expect_true(is_typed_function(foo))
  expect_error(foo("A"), class = "invalid_input")
  expect_identical(foo(1L), 1L)
  expect_identical(foo(), rlang::missing_arg())

  expect_true(is_typed_function(bar))
  expect_error(bar("A"), class = "invalid_input")
  expect_identical(bar(1L), 1L)
  expect_identical(bar(), rlang::missing_arg())

  expect_error(
    typed(function(x = optional(1, 1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = optional(1)) { TRUE }),
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
  expect_error(
    typed(function(... = optional(check_integer())) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
})

test_that("`required()` modifier works.", {
  foo <- typed(function(x = required(check_integer())) {
    x
  })
  bar <- typed(function(x = required(check_integer_alias)) {
    x
  })

  expect_true(is_typed_function(foo))
  expect_identical(foo(1L), 1L)
  expect_error(foo("A"), class = "invalid_input")
  expect_error(foo(), class = "typewriter_error_typed_arg_missing")

  expect_true(is_typed_function(bar))
  expect_identical(bar(1L), 1L)
  expect_error(bar("A"), class = "invalid_input")
  expect_error(bar(), class = "typewriter_error_typed_arg_missing")

  expect_error(
    typed(function(x = required(1, 1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = required(1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = required()) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = required(optional(check_integer()))) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(... = required(check_integer())) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
})

test_that("`maybe()` modifier works.", {
  foo <- typed(function(x = maybe(check_integer())) { x })
  bar <- typed(function(x = maybe(check_integer_alias)) { x })
  baz <- typed(function(... = maybe(check_integer())) { list(...) })

  expect_identical(foo(1L), 1L)
  expect_identical(foo(NULL), NULL)
  expect_error(foo("A"), class = "invalid_input")

  expect_identical(bar(1L), 1L)
  expect_identical(bar(NULL), NULL)
  expect_error(bar("A"), class = "invalid_input")

  expect_identical(baz(1L, NULL, 1:5), list(1L, NULL, 1:5))
  expect_error(baz(1L, NULL, "A"), class = "invalid_input")

  expect_error(
    typed(function(x = maybe(1, 1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = maybe(1)) { TRUE }),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    typed(function(x = maybe()) { TRUE }),
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

  expect_error(
    typed(function(... = static(check_integer())) {}),
    class = "typewriter_error_invalid_input"
  )
})

test_that("Modifier combinations work.", {
  maybe_optional <- typed(function(x = maybe(optional(check_integer()))) {
    if (rlang::is_missing(x)) rlang::missing_arg() else x
  })
  maybe_required <- typed(function(x = maybe(required(check_integer()))) {
    x
  })
  static_optional <- typed(function(x = static(optional(check_integer()))) {
    if (rlang::is_missing(x)) rlang::missing_arg() else x
  })
  static_required <- typed(function(x = static(required(check_integer()))) {
    x
  })
  maybe_static_optional <- typed(function(x = maybe(static(optional(check_integer())))) {
    if (rlang::is_missing(x)) rlang::missing_arg() else x
  })
  maybe_static_required <- typed(function(x = maybe(static(required(check_integer())))) {
    x
  })

  expect_identical(maybe_optional(1L), 1L)
  expect_identical(maybe_optional(NULL), NULL)
  expect_identical(maybe_optional(), rlang::missing_arg())
  expect_error(maybe_optional("A"), class = "invalid_input")

  expect_identical(maybe_required(1L), 1L)
  expect_identical(maybe_required(NULL), NULL)
  expect_error(maybe_required(), class = "typewriter_error_typed_arg_missing")
  expect_error(maybe_required("A"), class = "invalid_input")

  expect_identical(static_optional(1L), 1L)
  expect_identical(static_optional(), rlang::missing_arg())
  expect_error(static_optional("A"), class = "invalid_input")

  expect_identical(static_required(1L), 1L)
  expect_error(static_required(), class = "typewriter_error_typed_arg_missing")
  expect_error(static_required("A"), class = "invalid_input")

  expect_identical(maybe_static_optional(1L), 1L)
  expect_identical(maybe_static_optional(NULL), NULL)
  expect_identical(maybe_static_optional(), rlang::missing_arg())
  expect_error(maybe_static_optional("A"), class = "invalid_input")

  expect_identical(maybe_static_required(1L), 1L)
  expect_identical(maybe_static_required(NULL), NULL)
  expect_error(maybe_static_required(), class = "typewriter_error_typed_arg_missing")
  expect_error(maybe_static_required("A"), class = "invalid_input")

  # Only the modifier set is important (e.g. ordering isn't important)
  expect_identical(
    maybe_required,
    typed(function(x = required(maybe(check_integer()))) { x })
  )
  expect_identical(
    static_required,
    typed(function(x = required(static(check_integer()))) { x })
  )
  expect_identical(
    maybe_static_required,
    typed(function(x = required(maybe(static(check_integer())))) { x })
  )
  expect_identical(
    maybe_required,
    typed(function(x = maybe(required(required(maybe(check_integer()))))) { x })
  )

  # Static works as expected
  bad_static_optional <- typed(function(x = static(optional(check_integer()))) {
    x <- "A"
  })
  bad_maybe_static_optional <- typed(function(x = maybe(static(optional(check_integer())))) {
    x <- "A"
  })

  expect_error(bad_static_optional(10L), class = "typewriter_error_invalid_assignment")
  expect_error(bad_maybe_static_optional(10L), class = "typewriter_error_invalid_assignment")

  # We don't "statically" type an un-supplied optional or `NULL` maybe argument
  expect_no_error(bad_static_optional())
  expect_no_error(bad_maybe_static_optional())
  expect_no_error(bad_maybe_static_optional(NULL))
})

test_that("Namespaced modifiers work as expected.", {
  foo_maybe <- typed(function(x = typewriter::maybe(check_integer())) {
    x
  })
  foo_maybe_required <- typed(function(x = typewriter::maybe(typewriter::required(check_integer()))) {
    x
  })

  expect_identical(foo_maybe(1L), 1L)
  expect_identical(foo_maybe(NULL), NULL)
  expect_error(foo_maybe("A"), class = "invalid_input")

  expect_identical(foo_maybe_required(1L), 1L)
  expect_identical(foo_maybe_required(NULL), NULL)
  expect_error(foo_maybe_required(), class = "typewriter_error_typed_arg_missing")
  expect_error(foo_maybe_required("A"), class = "invalid_input")
})

test_that("Modifiers can't be used in incorrect context.", {
  expect_error(untyped(), class = "typewriter_error_invalid_context")
})

# class ------------------------------------------------------------------------

test_that("`untype_function()` works as expected.", {
  foo_untyped <- function(x) { x }
  foo_typed <- typed(function(x = check_integer()) { x })

  expect_identical(untype_function(foo_untyped), foo_untyped)
  expect_identical(untype_function(foo_typed), foo_untyped)

  expect_error(untype_function(10), class = "typewriter_error_invalid_input")
})

test_that("`print.typewriter_typed_function()` works as expected.", {
  # `env = baseenv()` stops the snaps from printing a new {testthat} environment
  # on each run (note that these functions can't actually run in `baseenv()`).
  foo <- typed(function(x = check_integer()) { x }, env = baseenv())
  bar <- typed(function(
    a,
    x = required(check_integer()),
    y = chk::chk_atomic(),
    ... = chk::chk_character()
  ) {
    TRUE
  }, env = baseenv())

  expect_snapshot(print(foo))
  expect_snapshot(print(bar))
})

test_that("`has_typed_args()` works as expected.", {
  foo <- function(x) { x }
  bar <- typed(function(x) { x })
  baz <- typed(function(x = check_integer()) { x })

  expect_false(has_typed_args(foo))
  expect_false(has_typed_args(bar))
  expect_true(has_typed_args(baz))

  expect_error(has_typed_args(10), class = "typewriter_error_invalid_input")
})
