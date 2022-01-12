# 19 Quasiquotation -----------
library(rlang)
library(purrr)
## 19.2 Motivation =========

paste("Good", "morning", "Hadley")
paste("Good", "afternoon", "Alice")


cement <- function(...) {
  args <- ensyms(...)
  paste(purrr::map(args, as_string), collapse = " ")
}

cement(Good, morning, Hadley)
cement(Good, afternoon, Alice)




name <- "Hadley"
time <- "morning"

paste("Good", time, name)

cement(Good, time, name)



cement(Good, !!time, !!name)


paste("Good", time, name)
cement(Good, !!time, !!name)


### 19.2.1 Vocabulary ######
# works
library(MASS)

# fails
MASS

###  19.2.2 Exercises #######

# 1
library(MASS) # quoted

mtcars2 <- subset(mtcars, cyl == 4) # evaluated / quoted

with(mtcars2, sum(vs)) # evaluated / qouted
sum(mtcars2$am)        # evaluated

rm(mtcars2)   # quoted


# 2
library(dplyr)
library(ggplot2)

by_cyl <- mtcars %>%  # evaluated
  group_by(cyl) %>%   # quoted
  summarise(mean = mean(mpg))   ## quoted

ggplot(by_cyl, aes(cyl, mean)) + geom_point()  ## evaluated / qouted



## 19.3 Quoting

### 19.3.1 Capturing expressions #######
expr(x + y)
expr(1 / 2 / 3)

f1 <- function(x) expr(x)
f1(a + b + c)


f2 <- function(x) enexpr(x)
f2(a + b + c)


f <- function(...) enexprs(...)
f(x = 1, y = 10 * z)


exprs(x = x ^ 2, y = y ^ 3, z = z ^ 4)
# shorthand for
# list(x = expr(x ^ 2), y = expr(y ^ 3), z = expr(z ^ 4))


### 19.3.2 Capturing symbols ######

f <- function(...) ensyms(...)
f(x)
f("x")

### 19.3.3 With base R ######

# The base equivalent of expr() is quote():
quote(x + y)


# The base function closest to enexpr() is substitute():
f3 <- function(x) substitute(x)
f3(x + y)


# The base equivalent to exprs() is alist():
alist(x = 1, y = x + 2)


#The equivalent to enexprs() is an undocumented feature of substitute():
f <- function(...) as.list(substitute(...()))
f(x = 1, y = 10 * z)


### 19.3.4 Substitution #########
f4 <- function(x) substitute(x * 2)
f4(a + b + c)

substitute(x * y * z, list(x = 10, y = quote(a + b)))

### 19.3.6 Exercises #####

# 1
expr
enexpr

# 2
f1 <- function(x, y) {
  exprs(x = x, y = y)
}
f2 <- function(x, y) {
  enexprs(x = x, y = y)
}
f1(a + b, c + d)
f2(a + b, c + d)


# 3
enexpr(x + y)
enexpr(missing_arg())

f3 <- function(x) {enexpr(x)}
f3()

# 4
exprs(a)
out <- exprs(a = )
is_missing(out$a)

f4 <- function(x, y) {
  enexprs(a = x, b = y)
}
f4(1+3, 3+4)
f4(c, d)

f4 <- function(x, y) {
  enexprs(x, y)
}
f4(1+3, 3+4)
f4(c, d)


# 5
?exprs


# 6

my_env <- env()
substitute(x, my_env)


f6 <- function(x) substitute(x)
f6(x + y * sin(0))


my_env$x <- 7
substitute(x, my_env)


x <- 7
substitute(x, .GlobalEnv)

## 19.4 Unquoting =======
### 19.4.1 Unquoting one argument #######

x <- expr(-1)
expr(f(!!x, y))


lobstr::ast(f(!!x, y))


a <- sym("y")
b <- 1
expr(f(!!a, !!b))



mean_rm <- function(var) {
  var <- ensym(var)
  expr(mean(!!var, na.rm = TRUE))
}

expr(mean_rm(x) + !!mean_rm(y))
expr(!!mean_rm(x) + !!mean_rm(y))



x1 <- expr(x + 1)
x2 <- expr(x + 2)


expr(!!x1 / !!x2)
ast(!!x1 / !!x2)
ast(x + 1 / x + 2)


### 19.4.2 Unquoting a function #########

f <- expr(foo)
expr((!!f)(x, y))


f <- expr(pkg::foo)
expr((!!f)(x, y))


f <- expr(pkg::foo)
call2(f, expr(x), expr(y))


### 19.4.3 Unquoting a missing argument #######
arg <- missing_arg()
expr(foo(!!arg, !!arg))

expr(foo(!!maybe_missing(arg), !!maybe_missing(arg)))


### 19.4.4 Unquoting in special forms ########

expr(df$!!x)
x <- expr(x)
expr(`$`(df, !!x))


### 19.4.5 Unquoting many arguments ####

xs <- exprs(1, a, -b)
expr(f(!!!xs, y))

# Or with names
ys <- set_names(xs, c("a", "b", "c"))
expr(f(!!!ys, d = 4))

call2("f", !!!xs, expr(y))

### 19.4.6 The polite fiction of !! #########
!!TRUE
!!!TRUE


?Syntax


x <- quote(variable)
!!x


df <- data.frame(x = 1:5)
y <- 100
with(df, x + !!y)


### 19.4.7 Non-standard ASTs #######
x1 <- expr(class(!!data.frame(x = 10)))
x1
eval(x1)


expr_print(x1)
lobstr::ast(!!x1)



x2 <- expr(f(!!c(1L, 2L, 3L, 4L, 5L)))
x2
expr_print(x2)
lobstr::ast(!!x2)



x3 <- expr(1 + !!expr(2 + 3))
x3

lobstr::ast(!!x3)



### 19.4.8 Exercises #######

# 1
xy <- expr(x + y)
xz <- expr(x + z)
yz <- expr(y + z)
abc <- exprs(a, b, c)
# --
(x + y) / (y + z)
-(x + z) ^ (y + z)
(x + y) + (y + z) - (x + y)
atan2(x + y, y + z)
sum(x + y, x + y, y + z)
sum(a, b, c)
mean(c(a, b, c), na.rm = TRUE)
foo(a = x + y, b = y + z)


expr(!!xy / !!yz)
expr(-(!!xz) ^ !!yz)
expr(!!xy + !!yz - !!xy)
expr(atan2(!!xy, !!yz))
expr(sum(!!xy , !!yz, !!xy))
expr(sum(!!!abc))
expr(mean(!!!abc, na.rm = TRUE))
expr(foo(a = !!xy, b = !!yz))


# 2
(a <- expr(mean(1:10)))
(b <- expr(mean(!!(1:10))))
identical(a, b)



lobstr::ast(mean(1:10))
lobstr::ast(mean(!!(1:10)))

## 19.5 Non-quoting =========

xyz <- bquote((x + y + z))
bquote(-.(xyz) / 2)


x <- list(var = 1, y = 2)
var <- "y"

x$var
x[[var]]





x <- 1
rm(x)

y <- 2
vars <- c("y", "vars")
rm(list = vars)



library(MASS)

pkg <- "MASS"
library(pkg, character.only = TRUE)





# Shows help for var
help(var)

var <- "mean"
# Shows help for mean
help(var)

var <- 10
# Shows help for var
help(var)


palette(RColorBrewer::brewer.pal(3, "Set1"))
plot(
  Sepal.Length ~ Petal.Length, 
  data = iris, 
  col = Species, 
  pch = 20, 
  cex = 2
)


## 19.6 ... (dot-dot-dot) ========

dfs <- list(
  a = data.frame(x = 1, y = 2),
  b = data.frame(x = 3, y = 4)
)
rbind(dfs$a, dfs$b)




var <- "x"
val <- c(4, 3, 9)
setNames(data.frame(val), var)

# ---

dplyr::bind_rows(!!!dfs)





tibble::tibble(!!var := val)
tibble::tibble(!!var = value)


### 19.6.1 Examples #######


set_attr <- function(.x, ...) {
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}

attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% 
  str()

### 19.6.2 exec() ########

# Directly
exec("mean", x = 1:10, na.rm = TRUE, trim = 0.1)

# Indirectly
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec("mean", !!!args)

# Mixed
params <- list(na.rm = TRUE, trim = 0.1)
exec("mean", x = 1:10, !!!params)


arg_name <- "na.rm"
arg_val <- TRUE
exec("mean", 1:10, !!arg_name := arg_val)


x <- c(runif(10), NA)
funs <- c("mean", "median", "sd")

purrr::map_dbl(funs, exec, x, na.rm = TRUE)


### 19.6.3 dots_list() ########

# Can easily move x to first entry:
tibble::tibble(
  y = 1:5,
  z = 3:-1,
  x = 5:1,
)

# Need to remove comma from z and add comma to x
data.frame(
  y = 1:5,
  z = 3:-1,
  x = 5:1
)



str(dots_list(x = 1, x = 2))
str(dots_list(x = 1, x = 2, .homonyms = "first"))
str(dots_list(x = 1, x = 2, .homonyms = "last"))
str(dots_list(x = 1, x = 2, .homonyms = "error"))



### 19.6.4 With base R ######
do.call("rbind", dfs)



args <- list(val)
names(args) <- var

do.call("data.frame", args)




f <- function(...) {
  dots <- list(...)
  if (length(dots) == 1 && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }
  
  # Do something
  ...
}



f <- function(..., .dots) {
  dots <- c(list(...), .dots)
  # Do something
}


### 19.6.5 Exercises ##########

# 1
exec <- function(f, ..., .env = caller_env()) {
  args <- list2(...)
  do.call(f, args, envir = .env)
}


# 2
interaction
expand.grid
par


# 3
set_attr <- function(x, ...) {
  attr <- rlang::list2(...)
  attributes(x) <- attr
  x
}
set_attr(1:10, x = 10)


set_attr(a = 1:10, x = 10)




set_attr <- function(.x, ...) {
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}
set_attr(1:10, x = 10)


## 19.7 Case studies ============
### 19.7.1 lobstr::ast() #########
z <- expr(foo(x, y))
lobstr::ast(z)


lobstr::ast(!!z)


### 19.7.2 Map-reduce to generate code #######
intercept <- 10
coefs <- c(x1 = 5, x2 = -4)

coef_sym <- syms(names(coefs))
coef_sym


summands <- map2(coef_sym, coefs, ~ expr((!!.x * !!.y)))
summands

summands <- c(intercept, summands)
summands


eq <- reduce(summands, ~ expr(!!.x + !!.y))
eq



var <- expr(y)
coef_sym <- map(seq_along(coefs), ~ expr((!!var)[[!!.x]]))
coef_sym



linear <- function(var, val) {
  var <- ensym(var)
  coef_name <- map(seq_along(val[-1]), ~ expr((!!var)[[!!.x]]))
  
  summands <- map2(val[-1], coef_name, ~ expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)
  
  reduce(summands, ~ expr(!!.x + !!.y))
}

linear(x, c(10, 5, -4))


### 19.7.3 Slicing an array ######

indices <- rep(list(missing_arg()), 3)
expr(x[!!!indices])

indices[[2]] <- 1
expr(x[!!!indices])


slice <- function(x, along, index) {
  stopifnot(length(along) == 1)
  stopifnot(length(index) == 1)
  
  nd <- length(dim(x))
  indices <- rep(list(missing_arg()), nd)
  indices[[along]] <- index
  
  expr(x[!!!indices])
}

x <- array(sample(30), c(5, 2, 3))
slice(x, 1, 3)
slice(x, 2, 2)
slice(x, 3, 1)



### 19.7.4 Creating functions #######
new_function(
  exprs(x = , y =), 
  expr({x + y})
)


power <- function(exponent) {
  new_function(
    exprs(x = ), 
    expr({
      x ^ !!exponent
    }), 
    caller_env()
  )
}


power(0.5)



curve(sin(exp(4 * x)), n = 1000)




curve2 <- function(expr, xlim = c(0, 1), n = 100) {
  expr <- enexpr(expr)
  f <- new_function(exprs(x = ), expr)
  
  x <- seq(xlim[1], xlim[2], length = n)
  y <- f(x)
  
  plot(x, y, type = "l", ylab = expr_text(expr))
}
curve2(sin(exp(4 * x)), n = 1000)



### 19.7.5 Exercises ######

# 1
linear <- function(var, val) {
  var <- ensym(var)
  coef_name <- map(seq_along(val[-1]), ~ expr((!!var)[[!!.x]]))
  
  summands <- map2(val[-1], coef_name, ~ expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)
  
  reduce(summands, ~ expr(!!.x + !!.y))
}
linear(x, c(10, 5, -4))

# --
reduce(summands, ~ expr(!!.x + !!.y))
reduce(summands, call2, "+")

# --
linear <- function(var, val) {
  var <- ensym(var)
  coef_name <- map(seq_along(val[-1]), ~ expr((!!var)[[!!.x]]))
  
  summands <- map2(val[-1], coef_name, ~ expr((!!.x * !!.y)))
  summands <- c(val[[1]], summands)
  
  reduce(summands, call2, .fn = "+")
}
linear(x, c(10, 5, -4))


# 2
bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}
bc(0)(1)
bc(2)(2)



?Syntax
bc <- function(lambda){
  lambda <- enexpr(lambda)
  
  if(!! lambda == 0){
    new_function(exprs(x = ), expr(log(x)))
  } else {
    new_function(exprs(x = ), expr((x ^ (!!lambda) - 1) / !!lambda))
  }
}
bc(0)(1)
bc(2)(2)


# 3
compose <- function(f, g) {
  function(...) f(g(...))
}
compose(sin, cos)
compose(sin, cos)(pi)
# --
compose <- function(f, g){
  args <- enexprs(f, g)
  new_function(exprs(... =),
    expr({
      # dots <- list2(...)
      # exec(args[[1]], do.call(!!args[[2]], dots) )
      exec(!!args[[1]], exec(args[[2]], ...))
    }))
}
compose(sin, cos)
compose(sin, cos)(pi)


