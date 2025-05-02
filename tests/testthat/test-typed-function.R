# todos ------------------------------------------------------------------------

# - errors on invalid inputs
# - test `as_typed()` once finished

# tests ------------------------------------------------------------------------

test_that("`typed()` works.", {
  check_integer <- function(x, len = NULL) {
    if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
      rlang::abort(
        message = sprintf("Invalid object %s", rlang::caller_arg(x)),
        class = "invalid_input"
      )
    }
  }
  check_integer_alias <- alias(check_integer())

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

test_that("`typed()` works with `...` argument.", {
  check_integer <- function(x, len = NULL) {
    if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
      rlang::abort(
        message = sprintf("Invalid object %s", rlang::caller_arg(x)),
        class = "invalid_input",
        call = rlang::caller_env()
      )
    }
  }
  check_integer_alias <- alias(check_integer())

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
