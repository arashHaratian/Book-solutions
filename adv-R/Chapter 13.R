# 13 S3 --------
library(sloop)

## 13.2 Basics =====

f <- factor(c("a", "b", "c"))

typeof(f)
attributes(f)

unclass(f)



ftype(print)
ftype(str)
ftype(unclass)


print(f)

# stripping class reverts to integer behaviour
print(unclass(f))


time <- strptime(c("2017-01-01", "2020-05-04 03:21"), "%Y-%m-%d")
str(time)

str(unclass(time))



s3_dispatch(print(f))



ftype(t.test)
ftype(t.data.frame)
ftype(t)


weighted.mean.Date

s3_get_method(weighted.mean.Date)


### 13.2.1 Exercises ########

# 4
set.seed(1014)
some_days <- as.Date("2017-01-31") + sample(10, 5)

mean(some_days)
mean(unclass(some_days))

as.Date(mean(unclass(some_days)), origin = "1970-01-01")


# 5
x <- ecdf(rpois(100, 10))
x


otype(x)
s3_class(x)

class(x)
typeof(unclass(x))
attributes(x)


# 6

x <- table(rpois(100, 5))
x

otype(x)
s3_class(x)

class(x)
typeof(unclass(x))
attributes(x)

## 13.3 Classes ========

# Create and assign class in one step
x <- structure(list(), class = "my_class")

# Create, then set class
x <- list()
class(x) <- "my_class"



class(x)
inherits(x, "my_class")
inherits(x, "your_class")


# Create a linear model
mod <- lm(log(mpg) ~ log(disp), data = mtcars)
class(mod)

print(mod)

# Turn it into a date (?!)
class(mod) <- "Date"

# Unsurprisingly this doesn't work very well
print(mod)

### 13.3.1 Constructors ######
new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}


new_Date(c(-1, 0, 1))


new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  structure(x,
            class = "difftime",
            units = units
  )
}

new_difftime(c(1, 10, 3600), "secs")

new_difftime(52, "weeks")

### 13.3.2 Validators #######

new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a")
new_factor(0:1, "a")




validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

validate_factor(new_factor(1:5, "a"))
validate_factor(new_factor(0:1, "a"))


### 13.3.3 Helpers ##########

new_difftime(1:10)

difftime <- function(x = double(), units = "secs") {
  x <- as.double(x)
  new_difftime(x, units = units)
}

difftime(1:10)



factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

factor(c("a", "a", "b"))



POSIXct <- function(year = integer(), 
                    month = integer(), 
                    day = integer(), 
                    hour = 0L, 
                    minute = 0L, 
                    sec = 0, 
                    tzone = "") {
  ISOdatetime(year, month, day, hour, minute, sec, tz = tzone)
}

POSIXct(2020, 1, 1, tzone = "America/New_York")


### 13.3.4 Exercises ######

# 1
new_data.frame <- function(x, row.names = NULL){
  stopifnot(is.list(x))
  stopifnot(purrr::map_int(x, length) %>% purrr::reduce(`==`))
  if(!is.null(row.names)){
    stopifnot(length(row.names) == length(x[[1]]) )
    row.names <- as.character(row.names)
  }
  else{
  row.names <- as.character( 1:length(x[[1]]) )
  }
  
  structure(x,
            class = "data.frame",
            row.names = row.names
  )
}

x <- list(a = 1, b = 2)
new_data.frame(x, row.names = 1)

new_data.frame(x)



# 2
new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}



factor(letters[1:4], letters[1:3])
new_factor(letters[1:4], letters[1:3])


# 3
factor
View(factor)

# 4
?C
x <- factor(letters[1:3])
y <- C(x)
str(attributes(y))
attr(y, "contrasts")


y <- C(x, "a")
y <- C(x)
str(attributes(y))
attr(y, "contrasts")


# 5
?utils::as.roman()
attributes(as.roman(13))


new_roman <- function(num = integer()){
  stopifnot(is.numeric(num))
  structure(num,
            class = "roman")
}

new_roman(1.2)


roman <- function(x){
  if(as.character(x)) stop("x must be numreic type")
  
  if(length(x) > 1)
    return(purrr::map(x, new_roman))
  new_roman(x)
}


## 13.4 Generics and methods ======
?UseMethod

mean

my_new_generic <- function(x) {
  UseMethod("my_new_generic")
}


### 13.4.1 Method dispatch ##########
x <- Sys.Date()
s3_dispatch(print(x))

x <- matrix(1:10, nrow = 2)
s3_dispatch(mean(x))

s3_dispatch(sum(Sys.time()))


### 13.4.2 Finding methods #########

s3_methods_generic("mean")

s3_methods_class("ordered")


### 13.4.4 Exercises ########

# 1
t
t.test

sloop::ftype(t.test)


x <- structure(1:10, class = "test")
t(x)
# --
t.test(1:10)
s3_dispatch(t(x))


# 2
s3_methods_class("table")


# 3
s3_methods_class("ecdf")


# 4
s3_methods_generic("print")
s3_methods_generic("summary")
s3_methods_generic("plot")



# 5
?UseMethod
g <- function(x) {
  x <- 10
  y <- 10
  UseMethod("g")
}
g.default <- function(x) c(x = x, y = y)

x <- 1
y <- 1
g(x)

g.default(x)


# 6
?`[`

ftype(`[`)

s3_methods_generic("[")

args <- purrr::map_if(s3_methods_generic("[")[[2]], .p = s3_methods_generic("[")[[3]],.f = ~names(formals(paste0("[.", .x, ""))))

t <- paste0("`[.", "AsIs", "`")

## 13.5 Object styles =======
# vector style (all objects in previous sections)

# record style
x <- as.POSIXlt(ISOdatetime(2020, 1, 1, 0, 0, 1:3))
x

length(x)
length(unclass(x))

x[[1]] # the first date time

unclass(x)[[1]] # the first component, the number of seconds
unclass(x)




#Data frames (are similar to record style objects)
x <- data.frame(x = 1:100, y = 1:100)
length(x)
nrow(x)


# Scalar objects typically use a list to represent a single thing
mod <- lm(mpg ~ wt, data = mtcars)
length(mod)


### 13.5.1 Exercises ###########

# 1
lm()          # scalar
factor()      # vector
table()       # vector
as.Date()     # vector
as.POSIXct()  # vector
ecdf()        # scalar
ordered()     # vector
I()           # scalar


# 2
?lm

## 13.6 Inheritance ============


class(ordered("x"))
class(Sys.time())


s3_dispatch(print(ordered("x")))
s3_dispatch(print(Sys.time()))



s3_dispatch(ordered("x")[1])
s3_dispatch(Sys.time()[1])

### 13.6.1 NextMethod() ############

new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "secret")
}

print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

x <- new_secret(c(15, 1, 456))
x


s3_dispatch(x[1])
x[1]


`[.secret` <- function(x, i) {
  new_secret(x[i])
}
x[1]


`[.secret` <- function(x, i) {
  x <- unclass(x)
  new_secret(x[i])
}
x[1]


`[.secret` <- function(x, i) {
  new_secret(NextMethod())
}
x[1]

s3_dispatch(x[1])


### 13.6.2 Allowing subclassing ##########


new_secret <- function(x, ..., class = character()) {
  stopifnot(is.double(x))
  
  structure(
    x,
    ...,
    class = c(class, "secret")
  )
}


new_supersecret <- function(x) {
  new_secret(x, class = "supersecret")
}

print.supersecret <- function(x, ...) {
  print(rep("xxxxx", length(x)))
  invisible(x)
}

x2 <- new_supersecret(c(15, 1, 456))
x2


`[.secret` <- function(x, ...) {
  new_secret(NextMethod())
}

x2[1:3]


vec_restore.secret <- function(x, to, ...) new_secret(x)
vec_restore.supersecret <- function(x, to, ...) new_supersecret(x)


`[.secret` <- function(x, ...) {
  vctrs::vec_restore(NextMethod(), x)
}
x2[1:3]


### 13.6.3 Exercises ##########


# 3
generic2 <- function(x) UseMethod("generic2")
generic2.a1 <- function(x) "a1"
generic2.a2 <- function(x) "a2"
generic2.b <- function(x) {
  cat(class(x), '\n\n')
  class(x) <- "a1"
  cat(class(x), '\n\n')
  NextMethod()
}

generic2(structure(list(), class = c("b", "a2")))


## 13.7 Dispatch details ========
### 13.7.1 S3 and base types ########
class(matrix(1:5))

s3_class(matrix(1:5))

s3_dispatch(print(matrix(1:5)))


x1 <- 1:5
class(x1)
s3_dispatch(mean(x1))

x2 <- structure(x1, class = "integer")
class(x2)

s3_dispatch(mean(x2))


### 13.7.2 Internal generics #######

s3_dispatch(Sys.time()[1])


### 13.7.3 Group generics ######

s3_dispatch(sum(Sys.time()))

y <- as.difftime(10, units = "mins")
s3_dispatch(abs(y))


date <- as.Date("2017-01-01")
integer <- 1L

date + integer
integer + date

### 13.7.5 Exercises ########

# 1
length.integer <- function(x) 10
length

x1 <- 1:5
class(x1)
length(x2)
s3_class(x1)
s3_dispatch(length(x1))


x2 <- structure(x1, class = "integer")
class(x2)
length(x2)
s3_dispatch(length(x2))


# 2
s3_methods_generic("Math")

Math.data.frame

# 3
Math.difftime




