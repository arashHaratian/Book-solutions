## sloop ------

library(sloop)

otype(1:10)

otype(mtcars)

mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj)


# 12 Base types --------

## 12.2 Base versus OO objects ======

# A base object:
is.object(1:10)
sloop::otype(1:10)

# An OO object
is.object(mtcars)
sloop::otype(mtcars)


attr(1:10, "class")

attr(mtcars, "class")


x <- matrix(1:4, nrow = 2)
class(x)

sloop::s3_class(x)


## 12.3 Base types =========

typeof(1:10)

typeof(mtcars)


# Vectors
typeof(NULL)
typeof(1L)
typeof(1i)


# Functions
typeof(mean)
typeof(`[`)
typeof(sum) 


# Environments
typeof(globalenv())


# S4
mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
typeof(mle_obj)


# Language Components
typeof(quote(a))
typeof(quote(a + 1))
typeof(formals(mean))
typeof(expression())


### 12.3.1 Numeric type #######


# Numeric is alias for double
      # as.numeric() is identical to as.double(), and numeric() is identical to double().

# in s3 and s4 int and dbl are numeric
sloop::s3_class(1)
sloop::s3_class(1L)


# check for objects that behave like numbers. 
typeof(factor("x"))
is.numeric(factor("x"))
