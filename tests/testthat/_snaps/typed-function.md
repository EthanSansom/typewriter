# `print.typewriter_typed_function()` works as expected.

    Code
      print(foo)
    Output
      <typed>
      function (x) 
      {
          x
      }
      <environment: base>
      Typed Arguments:
      * `x`: An object checked by `check_integer(x)`.

---

    Code
      print(bar)
    Output
      <typed>
      function (a, x, y, ...) 
      {
          TRUE
      }
      <environment: base>
      Typed Arguments:
      * `x`  : An object checked by `check_integer(x)`. [required]
      * `y`  : An object checked by `chk::chk_atomic(y)`.
      * `...`: An object checked by `chk::chk_character(..i)`.

