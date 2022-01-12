# 9 Functionals -------------

randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(mean)
randomise(sum)



library(purrr)


## 9.2 My first functional: map() ==========

triple <- function(x) x * 3
map(1:3, triple)


simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}



### 9.2.1 Producing atomic vectors ############

# map_chr() always returns a character vector
map_chr(mtcars, typeof)


# map_lgl() always returns a logical vector
map_lgl(mtcars, is.double)


# map_int() always returns a integer vector
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)


# map_dbl() always returns a double vector
map_dbl(mtcars, mean)



pair <- function(x) c(x, x)
map_dbl(1:2, pair)


map(1:2, pair)
map(1:2, as.character)


### 9.2.2 Anonymous functions and shortcuts ########
map_dbl(mtcars, function(x) length(unique(x)))
map_dbl(mtcars, ~ length(unique(.x)))


as_mapper(~ length(unique(.x)))

x <- map(1:3, ~ runif(2))
str(x)



x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
# Or by position
map_dbl(x, 1)
# Or by both
map_dbl(x, list("y", 1))

# You'll get an error if a component doesn't exist:
map_chr(x, "z")

# Unless you supply a .default value
map_chr(x, "z", .default = NA)


### 9.2.3 Passing arguments with ... ##########
x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))


map_dbl(x, mean, na.rm = TRUE)



plus <- function(x, y) x + y

x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))

map_dbl(x, ~ plus(.x, runif(1)))



### 9.2.4 Argument names ######

boostrap_summary <- function(x, f) {
  f(sample(x, replace = TRUE))
}

simple_map(mtcars, boostrap_summary, f = mean)
# =
simple_map(x = mtcars, f = mean, bootstrap_summary)



### 9.2.5 Varying another argument #######
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)


map_dbl(trims, ~ mean(x, trim = .x))

map_dbl(trims, function(trim) mean(x, trim = trim))


map_dbl(trims, mean, x = x)



### 9.2.6 Exercises #######
# 1
?as_mapper



as_mapper(c(1, 2))
as_mapper(c("a", "b"))
as_mapper(c(1, "a"))


as_mapper(~as.double)

as_mapper(~as.character)

as_mapper(~as.list())



# 2
map(1:3, ~ runif(2))
map(1:3, runif(2))


as_mapper(~runif(2))
as_mapper(runif(2))



# 3
map_dbl(mtcars, sd)

mtcars$char <- "a"
cols <- map_lgl(mtcars, is.numeric)
map_dbl(mtcars[cols], sd)



map_int(mtcars, ~ length(levels(as.factor(.x))))


# 4
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10))) %>% 
  map_dbl("p.value")

data.frame(trials) %>% 
  ggplot() +
  geom_freqpoly(aes(trials))

# 5
x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)

map(x, map, triple)


# 6
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

lms <- map(formulas, lm, data = mtcars)


# 7
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

R_squ <- map(bootstraps, lm, formula = mpg~disp) %>% 
  map(summary) %>%
  map_dbl("r.squared")

## 9.3 Purrr style =============

by_cyl <- split(mtcars, mtcars$cyl)


by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef) %>% 
  map_dbl(2)



by_cyl %>% 
  lapply(function(data) lm(mpg ~ wt, data = data)) %>% 
  lapply(coef) %>% 
  vapply(function(x) x[[2]], double(1))
# ==
models <- lapply(by_cyl, function(data) lm(mpg ~ wt, data = data))
vapply(models, function(x) coef(x)[[2]], double(1))


intercepts <- double(length(by_cyl))
for (i in seq_along(by_cyl)) {
  model <- lm(mpg ~ wt, data = by_cyl[[i]])
  intercepts[[i]] <- coef(model)[[2]]
}
intercepts

## 9.4 Map variants ==========

### 9.4.1 Same type of output as input: modify() ########

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)

modify(df, ~ .x * 2)
df <- modify(df, ~ .x * 2)


simple_modify <- function(x, f, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- f(x[[i]], ...)
  }
  x
}



### 9.4.2 Two inputs: map2() and friends #########
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)
map_dbl(xs, mean)

map_dbl(xs, weighted.mean, w = ws)


map2_dbl(xs, ws, weighted.mean)

map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)

simple_map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

### 9.4.3 No outputs: walk() and friends ##########

welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

map(names, welcome)


walk(names, welcome)
(walk(names, welcome))


temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)


### 9.4.4 Iterating over values and indices #######
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))


x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The highest value of ", .y, " is ", max(.x)))
seq_along(x)

### 9.4.5 Any number of inputs: pmap() and friends ########
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

map2_dbl(xs, ws, weighted.mean)
pmap_dbl(list(xs, ws), weighted.mean)


pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)



trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)



params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif)



### 9.4.6 Exercises ########
# 1
modify(mtcars, 1)

# 2
cyls <- split(mtcars, mtcars$cyl)

temp <- tempfile()
dir.create(temp)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

iwalk(paths, ~ write.csv(cyls[[.y]], .x))


# 3
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))


vars <- names(trans)
mtcars[vars] <- map(vars, ~ trans[[.x]](mtcars[[.x]]))


# 4

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(tempdir(), paste0("cyl-", names(cyls), ".csv"))

map2(cyls, paths, write.csv)


## 9.5 Reduce family ========

### 9.5.1 Basics #######
l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)

out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
out


reduce(l, intersect)

reduce(l, union)


simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

### 9.5.2 Accumulate #######

accumulate(l, intersect)

x <- c(4, 3, 10)
reduce(x, `+`)

accumulate(x, `+`)


### 9.5.3 Output types########
reduce(1, `+`)

reduce("a", `+`)

reduce(integer(), `+`)


reduce(integer(), `+`, .init = 0)


reduce("a", `+`, .init = 0)



sum(integer())  # x + 0 = x
prod(integer()) # x * 1 = x
min(integer())  # min(x, Inf) = x
max(integer())  # max(x, -Inf) = x



    ###If you’re using reduce() in a function, you should always supply ".init" arg.

### 9.5.4 Multiple inputs ########
?reduce2

x <- list(c(0, 1), c(2, 3), c(4, 5))
y <- list(c(6, 7), c(8, 9))
reduce2(x, y, paste)
accumulate2(x, y, paste)


## 9.6 Predicate functionals ===========
### 9.6.1 Basics ###########
?some(.x, .p)
?every(.x, .p)


?detect
?detect_index


?keep
?discard


df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor)
detect_index(df, is.factor)

str(keep(df, is.factor))
str(discard(df, is.factor))


### 9.6.2 Map variants ######

df <- data.frame(
  num1 = c(0, 10, 20),
  num2 = c(5, 6, 7),
  chr1 = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

str(map_if(df, is.numeric, mean))
str(modify_if(df, is.numeric, mean))
str(map(keep(df, is.numeric), mean))

### 9.6.3 Exercises ########
# 1
is.na(c(NA, 2, NA))
is.numeric(c(NA, 2, NA))

anyNA(c(NA, 2, NA))
some(c(NA, 2, NA), is.na)
any(is.na(c(NA, 2, NA)))


# 2

simple_reduce <- function(x, f) {
  out <- x[[1]]
  for (i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}
simple_reduce(1, mean)

#--

simple_reduce2 <- function(x, f) {
  out <- x[[1]]
  for (i in seq_along(length(x))) {
    out <- f(out, x[[i]])
  }
  out
}
simple_reduce2(1, mean)
#--
simple_reduce3 <- function(x, f) {
  if(length(x) < 2)
    return(x)
  out <- x[[1]]
  for (i in seq_along(length(x))) {
    out <- f(out, x[[i]])
  }
  out
}
simple_reduce3(1, mean)

# 3

?rle
span <- function(.x, .p) {
  result <- map_lgl(.x, .p) %>% 
    unname() %>% 
    rle()
  if (!any(result$values)) {
    return(integer(0))
  }
  longest <- max(result$lengths[result$values])
  longest_idx <- which(result$values & result$lengths == longest)[1]
  ind_before_longest <- sum(result$lengths[seq_len(longest_idx - 1)])
  out_start <- ind_before_longest + 1L
  out_end <- ind_before_longest + longest
  out_start:out_end
}
span(mtcars, is.numeric)
span(mtcars, is.factor)
span(mtcars, is.character)


  
# 4
arg_max <- function(x, f){
  values <- map_dbl(x, f)
  x[values == max(values)]
}
arg_max(-10:5, function(x) x ^ 2)
arg_max(-5:5, function(x) x ^ 2)


# 5
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

modify_if(mtcars, is.numeric, scale01)

## 9.7 Base functionals ========
### 9.7.1 Matrices and arrays ##########

a2d <- matrix(1:20, nrow = 5)
apply(a2d, 1, mean)
apply(a2d, 2, mean)


a3d <- array(1:24, c(2, 3, 4))
apply(a3d, 1, mean)
apply(a3d, c(1, 2), mean)


a1 <- apply(a2d, 1, identity)
identical(a2d, a1)

a2 <- apply(a2d, 2, identity)
identical(a2d, a2)


df <- data.frame(x = 1:3, y = c("a", "b", "c"))
apply(df, 2, mean)


### 9.7.2 Mathematical concerns ########



    # integrate() finds the area under the curve defined by f()
    # uniroot() finds where f() hits zero
    # optimise() finds the location of the lowest (or highest) value of f()


integrate(sin, 0, pi)

str(uniroot(sin, pi * c(1 / 2, 3 / 2)))

str(optimise(sin, c(0, 2 * pi)))

str(optimise(sin, c(0, pi), maximum = TRUE))

### 9.7.3 Exercises ########
# 1
?apply


# 2
?eapply
?rapply











