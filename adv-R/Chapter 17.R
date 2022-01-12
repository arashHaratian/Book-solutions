# 17 Big picture -----------
library(rlang)
library(lobstr)


## 17.2 Code is data ==============


expr(mean(x, na.rm = TRUE))
expr(10 + 100 + 1000)


capture_it <- function(x) {
  expr(x)
}
capture_it(a + b + c)



capture_it <- function(x) {
  enexpr(x)
}
capture_it(a + b + c)





f <- expr(f(x = 1, y = 2))

# Add a new argument
f$z <- 3
f

# Or remove an argument:
f[[2]] <- NULL
f


## 17.3 Code is a tree =======

lobstr::ast(f(a, "b"))



lobstr::ast(f1(f2(a, b), f3(1, f4(2))))

lobstr::ast(1 + 2 * 3)


## 17.4 Code can generate code =======
call2("f", 1, 2, 3)
call2("+", 1, call2("*", 2, 3))






xx <- expr(x + x)
yy <- expr(y + y)

expr(!!xx / !!yy)





cv <- function(var) {
  var <- enexpr(var)
  expr(sd(!!var) / mean(!!var))
}

cv(x)
cv(x + y)

cv(`)`)


## 17.5 Evaluation runs code =======
eval(expr(x + y), env(x = 1, y = 10))
eval(expr(x + y), env(x = 2, y = 100))


x <- 10
y <- 100
eval(expr(x + y))



## 17.6 Customising evaluation with functions ========
string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )
  
  eval(enexpr(x), e)
}

name <- "Hadley"
string_math("Hello " + name)
string_math(("x" * 2 + "-y") * 3)


library(dplyr)


con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- copy_to(con, mtcars)

mtcars_db %>%
  filter(cyl > 2) %>%
  select(mpg:hp) %>%
  head(10) %>%
  show_query()

DBI::dbDisconnect(con)


## 17.7 Customising evaluation with data ======

df <- data.frame(x = 1:5, y = sample(5))
eval_tidy(expr(x + y), df)


with2 <- function(df, expr) {
  eval_tidy(enexpr(expr), df)
}

with2(df, x + y)


## 17.8 Quosures ======
with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enexpr(expr), df)
}

df <- data.frame(x = 1:3)
a <- 10
with2(df, x + a)


with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enquo(expr), df)
}

with2(df, x + a)
