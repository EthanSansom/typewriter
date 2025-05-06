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

  foo_t <- as_typed_function(foo, x = check_integer())
  foo_dots_t <- as_typed_function(foo_dots, ... = check_integer())
  foo_both_t <- as_typed_function(
    foo_both,
    ... = check_integer(),
    x = check_integer_alias,
    y = check_integer_alias(1L),
    z = check_integer(len = 1L)
  )

  expect_true(is_typed_function(foo_t))
  expect_true(is_typed_function(foo_dots_t))
  expect_true(is_typed_function(foo_both_t))

  expect_true(foo_t(1:5))
  expect_true(foo_dots_t(1:5, 1L, 10L))
  expect_true(foo_both_t(a = "A", 0L, 1:2, x = 1:2, y = 1:2, z = 0L))

  expect_identical(formals(foo_t), rlang::pairlist2(x = ))
  expect_identical(formals(foo_dots_t), rlang::pairlist2(... = ))
  expect_identical(
    formals(foo_both_t),
    rlang::pairlist2(a = , ... = , x = , y = 1L, z = , v = quote(as.Date("2020-01-01")))
  )

  expect_error(foo_t("A"), class = "invalid_input")
  expect_error(foo_dots_t(1L, "A", 10L), class = "invalid_input")
  expect_error(foo_both_t(x = 0L, z = 1:2), class = "invalid_input")
})
