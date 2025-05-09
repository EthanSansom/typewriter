# <uninitialized> objects print as expected.

    Code
      print(v)
    Output
      <uninitialized<chk::chk_logical>> 
      An object checked using `chk::chk_logical()`. 

---

    Code
      print(w)
    Output
      <uninitialized<chk::chk_numeric>> 
      An object checked using `chk::chk_numeric(x_name = "w")`. 

---

    Code
      print(x)
    Output
      <uninitialized<check_integer>> 
      An object checked using `check_integer()`. 

---

    Code
      print(y)
    Output
      <uninitialized<check_integer>> 
      An object checked using `check_integer()`. 

---

    Code
      print(z)
    Output
      <uninitialized<check_integer>> 
      An object checked using `check_integer(len = 1L)`. 

---

    Code
      print(a)
    Output
      <uninitialized<integer(1)>> 
      A scalar integer. 

---

    Code
      print(b)
    Output
      <uninitialized<integer>> 
      An integer vector. 
      i Info
      Unnamed bullet

