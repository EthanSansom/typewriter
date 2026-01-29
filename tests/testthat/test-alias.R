# tests ------------------------------------------------------------------------

test_that("`type_alias()` works.", {
  a_int <- type_alias(check_integer())
  a_scalar_int <- type_alias(check_integer(len = 1L))

  expect_error(a_int("A"), class = "invalid_input")
  expect_error(a_scalar_int(1:2), class = "invalid_input")

  expect_identical(a_int(1:5), 1:5)
  expect_identical(a_scalar_int(0L), 0L)
})

test_that("`return_call` argument works.", {
  a_fun <- type_alias(check_funish(), return_call = TRUE)
  a_fun_no_coerce <- type_alias(check_funish(), return_call = FALSE)

  expect_error(a_fun("A"), class = "invalid_input")
  expect_error(a_fun_no_coerce("A"), class = "invalid_input")

  expect_identical(a_fun(mean), mean)
  expect_identical(a_fun(~ .x + 10), rlang::as_function(~ .x + 10))
  expect_identical(a_fun_no_coerce(mean), mean)
  expect_identical(a_fun_no_coerce(~ .x + 10), ~ .x + 10)
})

test_that("`type_alias()` works with external package functions.", {
  skip_if_not_installed("chk")

  a_num <- type_alias(chk::chk_numeric())
  a_lgl <- type_alias(chk::check_values(values = logical()))

  expect_error(a_num(data.frame()))
  expect_error(a_lgl(mean))

  expect_identical(a_num(0.5), 0.5)
  expect_identical(a_lgl(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA))
})

test_that("A generated alias expects exactly one unnamed argument.", {
  a_int <- type_alias(check_integer())

  expect_error(a_int(x = 10L), class = "typewriter_error_type_alias_invalid_input")
  expect_error(a_int(10L, x = 10L, y = 10L), class = "typewriter_error_type_alias_invalid_input")
  expect_error(a_int(10L, 10L), class = "typewriter_error_type_alias_invalid_input")
  expect_error(a_int(), class = "typewriter_error_type_alias_invalid_input")
})

test_that("`type_alias()` works with `%<-%` and `assign_typed()`", {
  a_int <- type_alias(check_integer())
  a_scalar_int <- type_alias(check_integer(len = 1L))

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

test_that("`type_alias()` errors on invalid inputs.", {
  expect_error(type_alias(10), class = "typewriter_error_invalid_input")
  expect_error(type_alias(check_integer), class = "typewriter_error_invalid_input")
  expect_error(type_alias(check_integer(len = stop("No"))), class = "typewriter_error_invalid_input")
  expect_error(type_alias(check_integer(stop("No"))), class = "typewriter_error_invalid_input")
  expect_error(type_alias(non_extant_function()), class = "typewriter_error_invalid_input")

  expect_error(type_alias(check_integer(), name = 10L), class = "typewriter_error_invalid_input")
  expect_error(type_alias(check_integer(), desc = c("A", "B")), class = "typewriter_error_invalid_input")
  expect_error(type_alias(check_integer(), bullets = 10L), class = "typewriter_error_invalid_input")

  # Functions of type "builtin" and "special" aren't allowed
  expect_error(type_alias(`$`()), class = "typewriter_error_invalid_input")
  expect_error(type_alias(sum()), class = "typewriter_error_invalid_input")
})

test_that("`type_alias()` prints nicely.", {
  skip_on_cran()
  skip_if_not_installed("chk")

  a_num <- type_alias(chk::chk_numeric())
  a_int <- type_alias(
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
  expect_snapshot(print(a_num))
  expect_snapshot(print(a_int))
})

test_that("`alias_caller()` makes `rlang::abort()` reference the correct call.", {
  check_is_logical <- function(x, x_name = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (is.logical(x)) {
      return(x)
    }
    rlang::abort(
      sprintf("`%s` must be logical.", x_name),
      class = "invalid_input",
      call = call
    )
  }

  a_lgl_1 <- type_alias(check_is_logical())
  a_lgl_2 <- type_alias(check_is_logical(call = alias_caller()))

  foo_1 <- typed(function(arg = a_lgl_1) { TRUE })
  foo_2 <- typed(function(arg = a_lgl_2) { TRUE })

  # `a_lgl_2()` should correctly reference it's caller (`foo_2()`), while
  # `a_lgl_1()` will reference itself.
  expect_identical(rlang::catch_cnd(foo_1("A"))$call, quote(a_lgl_1(arg)))
  expect_identical(rlang::catch_cnd(foo_2("A"))$call, quote(foo_2("A")))

  # Error messages should be fine either way
  expect_error(foo_1("A"), class = "invalid_input", regexp = "`arg` must be logical.", fixed = TRUE)
  expect_error(foo_2("A"), class = "invalid_input", regexp = "`arg` must be logical.", fixed = TRUE)
})
