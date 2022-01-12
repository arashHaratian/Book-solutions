# 20 Evaluation --------
library(rlang)
library(purrr)
## 20.2 Evaluation basics =======
x <- 10
eval(expr(x))

y <- 2
eval(expr(x + y))



eval(expr(x + y), env(x = 1000))



eval(print(x + 1), env(x = 1000))

eval(expr(print(x + 1)), env(x = 1000))


### 20.2.1 Application: local() ######

# Clean up variables created earlier
rm(x, y)


foo <- local({
  x <- 10
  y <- 200
  x + y
})

foo

x

y


local2 <- function(expr) {
  env <- env(caller_env())
  eval(enexpr(expr), env)
}

foo <- local2({
  x <- 10
  y <- 200
  x + y
})

foo

x

y



### 20.2.2 Application: source() ######

source2 <- function(path, env = caller_env()) {
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
  exprs <- parse_exprs(file)
  
  res <- NULL
  for (i in seq_along(exprs)) {
    res <- eval(exprs[[i]], env)
  }
  
  invisible(res)
}

#### Expression vectors

source3 <- function(file, env = parent.frame()) {
  lines <- parse(file)
  res <- eval(lines, envir = env)
  invisible(res)
}


### 20.2.3 Gotcha: function() ########

x <- 10
y <- 20
f <- eval(expr(function(x, y) !!x + !!y))
f

f()


attr(f, "srcref") <- NULL
f


### 20.2.4 Exercises #######

# 1
?source


# 2
eval(expr(eval(expr(eval(expr(2 + 2))))))
eval(eval(expr(eval(expr(eval(expr(2 + 2)))))))
expr(eval(expr(eval(expr(eval(expr(2 + 2)))))))




# 3
# name is a string
get2 <- function(name, env) {
  eval(name, env)
}
get2(x, global_env())


assign2 <- function(name, value, env) {
  # name <- sym(name)
  eval(expr(`<-`(!!name, !!value)), env)
}
assign2("x", 20, global_env())


# 4
source2 <- function(path, env = caller_env()) {
  file <- paste(readLines(path, warn = FALSE), collapse = "\n")
  exprs <- parse_exprs(file)
  
  invisible(map(exprs, eval, env))
}



# 5
local3 <- function(expr, envir = new.env()) {
  call <- substitute(eval(quote(expr), envir))
  print(call)
  print(parent.env(envir))
  print(current_env())
  eval(call, envir = parent.frame())
}


local3({
  x <- 10
  x * 2
})


## 20.3 Quosures =========
### 20.3.1 Creating #########
?enquo
?enquos
foo <- function(x) enquo(x)
foo(a + b)


?quo
?quos
quo(x + y + z)



?new_quosure
new_quosure(expr(x + y), env(x = 1, y = 10))


### 20.3.2 Evaluating #########

q1 <- new_quosure(expr(x + y), env(x = 1, y = 10))
eval_tidy(q1)
eval(quo_get_expr(q1), quo_get_env(q1))
### 20.3.3 Dots ######
f <- function(...) {
  x <- 1
  g(..., f = x)
}

g <- function(...) {
  enquos(...)
}

x <- 0
qs <- f(global = x)
qs

map_dbl(qs, eval_tidy)


### 20.3.4 Under the hood #######
f <- ~runif(3)
str(f)


q4 <- new_quosure(expr(x + y + z))
class(q4)


is_call(q4)

q4[[1]]
q4[[2]]


attr(q4, ".Environment")


get_expr(q4)
get_env(q4)


### 20.3.5 Nested quosures #####
q2 <- new_quosure(expr(x), env(x = 1))
q3 <- new_quosure(expr(x), env(x = 10))

x <- expr(!!q2 + !!q3)

eval_tidy(x)


x


expr_print(x)

### 20.3.6 Exercises #########

# 1
q1 <- new_quosure(expr(x), env(x = 1))
q1
expr_print(q1)
eval_tidy(q1)

q2 <- new_quosure(expr(x + !!q1), env(x = 10))
q2
expr_print(q2)
eval_tidy(q2)

q3 <- new_quosure(expr(x + !!q2), env(x = 100))
q3
expr_print(q3)
eval_tidy(q3)


# 2
enenv <- function(x){
  enquo(x) %>% 
  quo_get_env()
}
enenv()
enenv(a)


capture_env <- function(x){
  enenv(x)
}
capture_env(x)


## 20.4 Data masks ============

### 20.4.1 20.4.1 Basics ########
q1 <- new_quosure(expr(x * y), env(x = 100))
df <- data.frame(y = 1:10)

eval_tidy(q1, df)


with2 <- function(data, expr) {
  expr <- enquo(expr)
  eval_tidy(expr, data)
}
x <- 100
with2(df, x * y)



with3 <- function(data, expr) {
  expr <- substitute(expr)
  eval(expr, data, caller_env())
}



### 20.4.2 Pronouns ##########
with2(df, x)

x <- 1
df <- data.frame(x = 2)

with2(df, .data$x)
with2(df, .env$x)


with2(df, .data$y)


### 20.4.3 Application: subset() ########

sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))

# Shorthand for sample_df[sample_df$a >= 4, ]
subset(sample_df, a >= 4)

# Shorthand for sample_df[sample_df$b == sample_df$c, ]
subset(sample_df, b == c)



subset2 <- function(data, rows) {
  rows <- enquo(rows)
  rows_val <- eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
}

subset2(sample_df, b == c)


### 20.4.4 Application: transform #######

df <- data.frame(x = c(2, 3, 1), y = runif(3))
transform(df, x = -x, y2 = 2 * y)


transform2 <- function(.data, ...) {
  dots <- enquos(...)
  
  for (i in seq_along(dots)) {
    name <- names(dots)[[i]]
    dot <- dots[[i]]
    
    .data[[name]] <- eval_tidy(dot, .data)
  }
  
  .data
}

transform2(df, x2 = x * 2, y = -y)

### 20.4.5 Application: select() ###########

df <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5)
subset(df, select = b:d)


vars <- as.list(set_names(seq_along(df), names(df)))
str(vars)




select2 <- function(data, ...) {
  dots <- enquos(...)
  
  vars <- as.list(set_names(seq_along(data), names(data)))
  cols <- unlist(map(dots, eval_tidy, vars))
  
  data[, cols, drop = FALSE]
}
select2(df, b:d)

### 20.4.6 Exercises #######

# 1

df <- data.frame(x = c(2, 3, 1), y = runif(3))


transform2 <- function(.data, ...) {
  dots <- enquos(...)
  
  for (i in seq_along(dots)) {
    name <- names(dots)[[i]]
    dot <- dots[[i]]
    
    .data[[name]] <- eval_tidy(dot, .data)
  }
  
  .data
}


transform2(df, x = x * 2, x = x * 2)


# 2
subset2 <- function(data, rows) {
  rows <- enquo(rows)
  rows_val <- eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
}




subset3 <- function(data, rows) {
  rows <- enquo(rows)
  eval_tidy(expr(data[!!rows, , drop = FALSE]), data = data)
}

df <- data.frame(x = 1:3)
subset3(df, x == 1)
subset2(df, x == 1)



# 3
arrange2 <- function(.df, ..., .na.last = TRUE) {
  args <- enquos(...)
  
  order_call <- expr(order(!!!args, na.last = !!.na.last))
  
  ord <- eval_tidy(order_call, .df)
  stopifnot(length(ord) == nrow(.df))
  
  .df[ord, , drop = FALSE]
}



# 4
arrange2 <- function(.df, ..., .na.last = TRUE) {
  args <- enquos(...)
  
  order_call <- expr(order(!!!args, na.last = !!.na.last))
  
  ord <- eval_tidy(order_call, .df)
  stopifnot(length(ord) == nrow(.df))
  
  .df[ord, , drop = FALSE]
}


# --
df <- data.frame(x = 3:1)


.na.last <- FALSE
order_call <- expr(order(df, na.last = !!.na.last))
eval_tidy(order_call, df)

order_call <-expr(order(df, na.last = .na.last))
eval_tidy(order_call, df)

## 20.5 Using tidy evaluation ==========

### 20.5.1 Quoting and unquoting ########
resample <- function(df, n) {
  idx <- sample(nrow(df), n, replace = TRUE)
  df[idx, , drop = FALSE]
}

subsample <- function(df, cond, n = nrow(df)) {
  df <- subset2(df, cond)
  resample(df, n)
}

df <- data.frame(x = c(1, 1, 1, 2, 2), y = 1:5)
subsample(df, x == 1)



subsample <- function(df, cond, n = nrow(df)) {
  cond <- enquo(cond)
  
  df <- subset2(df, !!cond)
  resample(df, n)
}

subsample(df, x == 1)


### 20.5.2 Handling ambiguity ########
threshold_x <- function(df, val) {
  subset2(df, x >= val)
}


x <- 10
no_x <- data.frame(y = 1:3)
threshold_x(no_x, 2)



has_val <- data.frame(x = 1:3, val = 9:11)
threshold_x(has_val, 2)




threshold_x <- function(df, val) {
  subset2(df, .data$x >= .env$val)
}

x <- 10
threshold_x(no_x, 2)
threshold_x(has_val, 2)




threshold_x <- function(df, val) {
  subset2(df, .data$x >= !!val)
}



### 20.5.3 Quoting and ambiguity ########
threshold_var <- function(df, var, val) {
  var <- as_string(ensym(var))
  subset2(df, .data[[var]] >= !!val)
}

df <- data.frame(x = 1:10)
threshold_var(df, x, 8)


threshold_expr <- function(df, expr, val) {
  expr <- enquo(expr)
  subset2(df, !!expr >= !!val)
}


#### 20.5.4 Exercises #########

# 1
df <- data.frame(x = 1:10)

threshold_var <- function(df, var, val) {
  var <- ensym(var)
  subset2(df, `$`(.data, !!var) >= !!val)
}
arg <- "x"
threshold_var(df, arg, 8)


## 20.6 Base evaluation ==========
### 20.6.1 substitute() ###########
subset_base <- function(data, rows) {
  rows <- substitute(rows)
  rows_val <- eval(rows, data, caller_env())
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
}

subset_tidy <- function(data, rows) {
  rows <- enquo(rows)
  rows_val <- eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop = FALSE]
}




#### 20.6.1.1 Programming with subset() #########
f1 <- function(df, ...) {
  xval <- 3
  subset_base(df, ...)
}

my_df <- data.frame(x = 1:3, y = 3:1)
xval <- 1
f1(my_df, x == xval)

local({
  zzz <- 2
  dfs <- list(data.frame(x = 1:3), data.frame(x = 4:6))
  lapply(dfs, subset_base, x == zzz)
})






f2 <- function(df1, expr) {
  call <- substitute(subset_base(df1, expr))
  expr_print(call)
  eval(call, caller_env())
}

my_df <- data.frame(x = 1:3, y = 3:1)
f2(my_df, x == 1)



f3 <- function(df) {
  call <- substitute(subset_base(df, z > 0))
  expr_print(call)
  eval(call, caller_env())
}

my_df <- data.frame(x = 1:3, y = 3:1)
z <- -1
f3(my_df)


#### 20.6.1.2 What about [? #########
subset(df, x == y)
df[x == y & !is.na(x == y), , drop = FALSE]

### 20.6.2 match.call() ########
g <- function(x, y, z) {
  match.call()
}
g(1, 2, z = 3)


write.csv <- function(...) {
  call <- match.call(write.table, expand.dots = TRUE)
  
  call[[1]] <- quote(write.table)
  call$sep <- ","
  call$dec <- "."
  
  eval(call, parent.frame())
}



write.csv <- function(...) {
  write.table(..., sep = ",", dec = ".")
}


#### 20.6.2.1 Wrapping modelling functions ######

lm2 <- function(formula, data) {
  lm(formula, data)
}

lm2(mpg ~ disp, mtcars)



lm3 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  data <- enexpr(data)
  
  lm_call <- expr(lm(!!formula, data = !!data))
  expr_print(lm_call)
  eval(lm_call, env)
}

lm3(mpg ~ disp, mtcars)



resp <- expr(mpg)
disp1 <- expr(vs)
disp2 <- expr(wt)
lm3(!!resp ~ !!disp1 + !!disp2, mtcars)



#### 20.6.2.2 Evaluation environment #######
resample_lm0 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  resample_data <- resample(data, n = nrow(data))
  
  lm_call <- expr(lm(!!formula, data = resample_data))
  expr_print(lm_call)
  eval(lm_call, env)
}

df <- data.frame(x = 1:10, y = 5 + 3 * (1:10) + round(rnorm(10), 2))
resample_lm0(y ~ x, data = df)


resample_lm1 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  resample_data <- resample(data, n = nrow(data))
  
  lm_call <- expr(lm(!!formula, data = !!resample_data))
  expr_print(lm_call)
  eval(lm_call, env)
}
resample_lm1(y ~ x, data = df)$call




resample_lm2 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  resample_data <- resample(data, n = nrow(data))
  
  lm_env <- env(env, resample_data = resample_data)
  lm_call <- expr(lm(!!formula, data = resample_data))
  expr_print(lm_call)
  eval(lm_call, lm_env)
}
resample_lm2(y ~ x, data = df)


### 20.6.3 Exercises #########

# 1
lm3a <- function(formula, data) {
  formula <- enexpr(formula)
  
  lm_call <- expr(lm(!!formula, data = data))
  eval(lm_call, caller_env())
}
lm3a(mpg ~ disp, mtcars)$call

## --
lm3a <- function(formula, data) {
  formula <- enexpr(formula)
  data <- enexpr(data)
  lm_call <- expr(lm(!!formula, data = !!data))
  eval(lm_call, caller_env())
}
lm3a(mpg ~ disp, mtcars)$call




lm3a <- function(formula, data) {
  formula <- enexpr(formula)
  
  lm_call <- expr(lm(!!formula, data = data))
  eval(lm_call, current_env())
}
lm3a(mpg ~ disp, mtcars)$call



lm3a <- function(formula, data) {
  # formula <- enexpr(formula)
  
  # lm_call <- expr(lm(!!formula, data = data))
  # print(current_env())
  eval(print, caller_env())
}
lm3a()
lm3a

lm3a <- function(formula, data, env = caller_env()) {
  # formula <- enexpr(formula)
  
  # lm_call <- expr(lm(!!formula, data = data))
  print(env)
}
lm3a()




# 2
lm(mpg ~ disp, data = mtcars)
lm(mpg ~ I(1 / disp), data = mtcars)
lm(mpg ~ disp * cyl, data = mtcars)


lm2 <- function(data, response, predictors, env = caller_env()){
  response <- enexpr(response)
  data <- enexpr(data)
  predictors <- enexprs(predictors)
  
  call <- expr(lm(formula = !!response ~ !!! predictors, data = !! data))
  eval(call, env)
}

lm2(data = mtcars, response = mpg, predictors = disp)
lm2(data = mtcars, response = mpg, predictors = I(1 / disp))
lm2(data = mtcars, response = mpg, predictors = disp * cyl)


# 3


resample_lm4 <- function(
  formula, data,
  resample_data = data[sample(nrow(data), replace = TRUE), ,
                       drop = FALSE],
  env = current_env()
) {
  formula <- enexpr(formula)
  
  lm_call <- expr(lm(!!formula, data = resample_data))
  expr_print(lm_call)
  eval(lm_call, env)
}
df <- data.frame(x = 1:10, y = 5 + 3 * (1:10) + round(rnorm(10), 2))
(resamp_lm1 <- resample_lm4(y ~ x, data = df))






resample_lm4 <- function(formula, data, env = caller_env()) {
  formula <- enexpr(formula)
  
  lm_call <- expr(lm(!!formula, data = (!!data[sample(nrow(data), replace = TRUE), , drop = FALSE])))
  expr_print(lm_call)
  eval(lm_call, env)
}

df <- data.frame(x = 1:10, y = 5 + 3 * (1:10) + round(rnorm(10), 2))
resample_lm4(y ~ x, data = df)
