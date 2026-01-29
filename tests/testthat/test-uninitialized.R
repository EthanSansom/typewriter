test_that("<uninitialized> objects work as expected.", {
  skip_if_not_installed("chk")

  v %<~% chk::chk_logical()
  w %<~% chk::chk_numeric(x_name = "w")
  x %<~% check_integer()
  y %<~% check_integer_alias
  z %<~% check_integer(len = 1L)

  expect_true(is_uninitialized(v))
  expect_true(is_uninitialized(w))
  expect_true(is_uninitialized(x))
  expect_true(is_uninitialized(y))
  expect_true(is_uninitialized(z))
})

test_that("<uninitialized> objects print as expected.", {
  skip_on_cran()
  skip_if_not_installed("chk")

  v %<~% chk::chk_logical()
  w %<~% chk::chk_numeric(x_name = "w")
  x %<~% check_integer()
  y %<~% check_integer_alias
  z %<~% check_integer(len = 1L)

  expect_snapshot(print(v))
  expect_snapshot(print(w))
  expect_snapshot(print(x))
  expect_snapshot(print(y))
  expect_snapshot(print(z))

  # <uninitialized> uses the description attributes of an <alias>
  a_scalar_int <- type_alias(
    call = check_integer(len = 1L),
    name = "integer(1)",
    desc = "A scalar integer."
  )
  a_int_with_bullets <- type_alias(
    call = check_integer(),
    name = "integer",
    desc = "An integer vector.",
    bullets = c("i" = "Info", "Unnamed bullet")
  )

  a %<~% a_scalar_int
  b %<~% a_int_with_bullets

  expect_snapshot(print(a))
  expect_snapshot(print(b))
})
