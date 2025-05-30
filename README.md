typewriter
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/EthanSansom/typewriter/graph/badge.svg)](https://app.codecov.io/gh/EthanSansom/typewriter)
<!-- badges: end -->

{typewriter} implements a minimal system for adding type safety to
objects and functions in R. The following functions form the core
functionality of the {typewriter} package.

- `%<~%`, a typed assignment operator for functions and other objects
- `const()`, for creating constants
- `dots()`, `optional()`, `required()`, for setting the type of function
  arguments
- `returns()`, for setting the return type of functions

## Installation

⚠️ This package is still under construction. ⚠️

You can install the development version of {typewriter} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/typewriter")
```

## Features

This package, for the time being, lives primarily in a long Notion
document. Below are some (un-run) examples which showcase the planned
{typewriter} interface.

The typed assignment operator `%<~%` take a symbol as it’s
left-hand-side argument and an arbitrary check function as it’s
right-hand-side value. Check functions must satisfy the following
criteria:

- The first argument must be an object to check
- On a successful check, the first argument is returned
- On a failed check, an error is emitted

The [{ckh}](https://poissonconsulting.github.io/chk/) package’s family
of `chk_*()` functions and the
[{checkmate}](https://mllg.github.io/checkmate/index.html) package’s
family of `check_*()` functions both use this pattern.

``` r
# Create function to check that it's input `x` is an integer
check_integer <- function(x) {
  if (is.integer(x)) {
    return(x)
  }
  stop("Object must be an integer.")
}

# Type `my_int` as an integer
my_int %<~% check_integer(10L)
print(my_int)
#> [1] 10
```

The `%<~%` operator retrieves the value to assign from the first
argument supplied to the right-hand-side check. Anytime a value is
re-assigned to the symbol `my_int` in the global environment, the check
`check_integer()` will be re-run.

``` r
try(my_int <- "A")
#> Error: Object must be an integer.
```

You can supply additional arguments to a right-hand-side check function,
which are evaluated once during assignment and are then supplied as
arguments in subsequent checks.

``` r
library(chk)
probability %<~% chk::chk_range(c(0, 0.5, 0.75), range = c(0, 1))

try(probability <- 12)
#> Error:
#> ! `probability` must be between 0 and 1, not 12.
```

The helper function `const()` checks that it’s input is unchanged and
can be used to declare constants.

``` r
dimensions %<~% const(c(10, 8))

try(dimensions <- c(2, 4))
#> Error:
#> ! Can't assign a new value to the constant `dimensions`.
```

The `%<~%` operator can also be used to type the arguments and return
value of a function. The syntax is the same as before, but now we assign
a check to a function argument instead of a symbol.

``` r
sum_int %<~% function(
    dots = dots(check_integer),  # `dots()` is helper to assign a type to `...`
    na.rm = chk::chk_flag(FALSE) # `na.rm` must be `TRUE` or `FALSE` (default)
  ) {
  sum(..., na.rm = na.rm)
}

# The result is a typed function
print(sum_int)
#> <typed>
#> function(..., na.rm = FALSE) {
#>   lapply(list(...), check_integer)
#>   chk::chk_flag(na.rm)
#>   
#>   sum(..., na.rm = na.rm)
#> }
```

You can specify the return type of a function using `returns()` as well
as required or optional arguments using `required()` and `optional()`.

``` r
# Make an alias for a <numeric> vector
num <- chk::chk_numeric

# Create a typed sum function which requires numeric arguments, of 
# which only `x` and `y` are required, and has a numeric return type.
my_sum %<~% function(
    x = required(num), 
    y = required(num), 
    z = optional(num), 
    returns = returns(num)
) {
  if (rlang::is_missing(z)) { z <- 0 }
  x + y + z
}

# The resulting typed function
print(my_sum)
#> <typed>
#> function(x, y, z) {
#>   rlang::check_required(x)
#>   num(x)
#>   rlang::check_required(y)
#>   num(y)
#>   if (!rlang::is_missing(z)) num(z)
#>   
#>   if (rlang::is_missing(z)) { z <- 0 }
#>   num(x + y + z)
#> }
```

## Inspiration

This package was primarily inspired by the
[{typed}](https://github.com/moodymudskipper/typed) package, which also
implements a type system which uses an overloaded `?` operator for
assigning types.
