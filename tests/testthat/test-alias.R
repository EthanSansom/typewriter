# todos ------------------------------------------------------------------------

# TODO: Test `name` and `desc` once incorporated.

# TODO: Check the use of external package checks using:
# - rlang::rlang::arg_match
# - rlang::check_required
#
# We could also add {chk} to Suggests..., might be worth it for documentation
# sake. Look back at how {friendlynumber} used Suggested package {bignum} in
# tests, examples, and documentation.

# tests ------------------------------------------------------------------------
test_that("`alias()` works", {
  a_int <- alias(check_integer())
  a_scalar_int <- alias(check_integer(len = 1L))

  expect_error(a_int("A"), class = "invalid_input")
  expect_error(a_scalar_int(1:2), class = "invalid_input")
  expect_identical(a_int(1:5), 1:5)
  expect_identical(a_scalar_int(0L), 0L)
})

test_that("`alias()` works with `%<-%` and `assign_typed()`", {
  a_int <- alias(check_integer())
  a_scalar_int <- alias(check_integer(len = 1L))

  int %<~% a_int
  scalar_int %<~% a_scalar_int

  expect_error(int <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(int <- 0.5, class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(scalar_int <- 1:2, class = "typewriter_error_invalid_assignment")

  expect_identical(int <- 1:5, 1:5)
  expect_identical(scalar_int <- 10L, 10L)

  env <- new.env()
  assign_typed(int2, a_int, env)
  assign_typed(scalar_int2, a_scalar_int, env)

  expect_error(env$int2 <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(env$int2 <- 0.5, class = "typewriter_error_invalid_assignment")
  expect_error(env$scalar_int2 <- "A", class = "typewriter_error_invalid_assignment")
  expect_error(env$scalar_int2 <- 1:2, class = "typewriter_error_invalid_assignment")

  expect_identical(env$int2 <- 1:5, 1:5)
  expect_identical(env$scalar_int2 <- 10L, 10L)
})

test_that("`alias()` errors on invalid inputs.", {
  expect_error(alias(10), class = "typewriter_error_invalid_input")
  expect_error(alias(check_integer), class = "typewriter_error_invalid_input")
  expect_error(alias(check_integer(len = stop())), class = "typewriter_error_invalid_input")
  expect_error(alias(non_extant_function()), class = "typewriter_error_invalid_input")
})

test_that("`alias()` prints nicely.", {
  skip_on_cran()

  a_int <- alias(
    call = check_integer(),
    name = "integer",
    desc = "An integer vector.",
    bullets = c(
      "unnamed",
      "named" = "named",
      " " = "space",
      "i" = "i",
      "x" = "x",
      "v" = "v",
      "!" = "!",
      "*" = "*",
      ">" = ">"
    )
  )
  expect_snapshot(print(a_int))
})
