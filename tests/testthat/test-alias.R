# todos ------------------------------------------------------------------------

# TODO: Test `name` and `desc` once incorporated.

# tests ------------------------------------------------------------------------
test_that("`alias()` works", {
  check_integer <- function(x, len = NULL) {
    if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
      rlang::abort("Invalid integer", class = "invalid_input")
    }
  }

  a_int <- alias(check_integer())
  a_scalar_int <- alias(check_integer(len = 1L))

  expect_error(a_int("A"), class = "invalid_input")
  expect_error(a_scalar_int(1:2), class = "invalid_input")
  expect_identical(a_int(1:5), 1:5)
  expect_identical(a_scalar_int(0L), 0L)
})

test_that("`alias()` works with `%<-%` and `assign_typed()`", {
  check_integer <- function(x, len = NULL) {
    if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
      rlang::abort("Invalid integer", class = "invalid_input")
    }
  }

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
  check_integer <- function(x, len = NULL) {
    if (!(is.integer(x) && (is.null(len) || length(x) == len))) {
      rlang::abort("Invalid integer", class = "invalid_input")
    }
  }

  expect_error(alias(10), class = "typewriter_error_invalid_input")
  expect_error(alias(check_integer), class = "typewriter_error_invalid_input")
  expect_error(alias(check_integer(len = stop())), class = "typewriter_error_invalid_input")
  expect_error(alias(non_extant_function()), class = "typewriter_error_invalid_input")
})
