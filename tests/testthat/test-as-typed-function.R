# todos ------------------------------------------------------------------------

# - modifiers (simple tests)
# - errors on invalid inputs (simple tests, check for name differences in formals + `...`)

# tests ------------------------------------------------------------------------

test_that("`as_typed_function()` works.", {
  foo <- function(x) {
    TRUE
  }
  foo_dots <- function(...) {
    TRUE
  }
  foo_both <- function(a, ..., x, y = 10L, z = 1L, v = as.Date("2020-01-01")) {
    TRUE
  }

  foo_notype <- as_typed_function(foo)
  foo_t <- as_typed_function(foo, x = check_integer())
  foo_dots_t <- as_typed_function(foo_dots, ... = check_integer())
  foo_both_t <- as_typed_function(
    foo_both,
    ... = check_integer(),
    x = check_integer_alias,
    y = check_integer_alias(1L),
    z = check_integer(len = 1L)
  )

  expect_true(is_typed_function(foo_notype))
  expect_true(is_typed_function(foo_t))
  expect_true(is_typed_function(foo_dots_t))
  expect_true(is_typed_function(foo_both_t))

  expect_true(foo_notype(1:5))
  expect_true(foo_t(1:5))
  expect_true(foo_dots_t(1:5, 1L, 10L))
  expect_true(foo_both_t(a = "A", 0L, 1:2, x = 1:2, y = 1:2, z = 0L))

  expect_identical(formals(foo_notype), rlang::pairlist2(x = ))
  expect_identical(formals(foo_t), rlang::pairlist2(x = ))
  expect_identical(formals(foo_dots_t), rlang::pairlist2(... = ))
  expect_identical(
    formals(foo_both_t),
    rlang::pairlist2(a = , ... = , x = , y = 1L, z = , v = quote(as.Date("2020-01-01")))
  )

  expect_no_error(foo_notype("A"))
  expect_error(foo_t("A"), class = "invalid_input")
  expect_error(foo_dots_t(1L, "A", 10L), class = "invalid_input")
  expect_error(foo_both_t(x = 0L, z = 1:2), class = "invalid_input")
})

test_that("`as_typed_function()` supports inline and generated functions.", {
  foo_gen <- function() { function(x) { TRUE }}
  inline <- as_typed_function(function(x) { TRUE }, x = check_integer(1L))
  generated <- as_typed_function(foo_gen(), x = check_integer())

  expect_true(is_typed_function(inline))
  expect_true(is_typed_function(generated))

  expect_identical(formals(inline), rlang::pairlist2(x = 1L))
  expect_identical(formals(generated), rlang::pairlist2(x = ))

  expect_true(inline())
  expect_true(inline(1:5))
  expect_true(generated(1:5))

  expect_error(inline("A"), class = "invalid_input")
  expect_error(generated(10.5), class = "invalid_input")
})

test_that("`as_typed_function()` overwriter previous argument types.", {
  foo_int <- as_typed_function(function(x) { TRUE }, x = check_integer())
  expect_error(foo_int("A"))
  expect_no_error(foo_int(1L))

  foo_chr <- as_typed_function(foo_int, x = check_character())
  expect_error(foo_chr(1L))
  expect_no_error(foo_chr("A"))
})

test_that("`as_typed_function()` errors on invalid inputs.", {
  foo <- function(x) { TRUE }

  # Input must be a function
  expect_error(
    as_typed_function(10),
    class = "typewriter_error_invalid_input"
  )
  # Arguments must be present
  expect_error(
    as_typed_function(foo, y = check_integer()),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    as_typed_function(foo, ... = check_integer()),
    class = "typewriter_error_invalid_input"
  )
  # Arguments must be named
  expect_error(
    as_typed_function(foo, check_integer()),
    class = "typewriter_error_invalid_input"
  )
  expect_error(
    as_typed_function(foo, x = check_integer(), 1L, 2L),
    class = "typewriter_error_invalid_input"
  )
})
