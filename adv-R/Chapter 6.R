# 6 Functions ----------

## 6.2 Function fundamentals ========

### 6.2.1 Function components ########
formals()
body()
environment()



f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02)


attr(f02, "srcref")


### 6.2.2 Primitive functions #######

sum
`[`


typeof(sum)
typeof(`[`)


formals(sum)
body(sum)
environment(sum)


### 6.2.3 First-class functions #######

f01 <- function(x) {
  sin(1 / x ^ 2)
}


lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)


funs <- list(
  half = function(x) x / 2,
  double = function(x) x * 2
)

funs$double(10)



### 6.2.4 Invoking a function #######
mean(1:10, na.rm = TRUE)
#==
args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

### 6.2.5 Exercises ###########

# 1
match.fun("mean") 

match.fun(body(mean))


# 2
function(x) 3()

f <- function(x) 3()
f
f()

(function(x) 3)()


# 4
is.function(sum)
is.primitive(f1)
is.primitive(sum)


# 5
objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)

args <- sapply(funs, formals)
which.max(sapply(args, length))


Filter(function(x) x == 0, sapply(args, length)) # assigning, primitive, operators



primitives <- Filter(is.primitive, objs)


# 7
f1 # global
Filter



## 6.3 Function composition ============

square <- function(x) x^2
deviation <- function(x) x - mean(x)

x <- runif(100)

sqrt(mean(square(deviation(x))))

out <- deviation(x)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out

library(magrittr)

x %>%
  deviation() %>%
  square() %>%
  mean() %>%
  sqrt()


## 6.4 Lexical scoping =========
x <- 10
g01 <- function() {
  x <- 20
  x
}

g01()


### 6.4.1 Name masking ##########

x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()


x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()


y



x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()


### 6.4.2 Functions versus variables #######
g07 <- function(x) x + 1
g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}
g08()


g09 <- function(x) x + 100
g10 <- function() {
  g09 <- 10
  g09(g09)
}
g10()


### 6.4.3 A fresh start #######
g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()
g11()



### 6.4.4 Dynamic lookup ######
g12 <- function() x + 1
x <- 15
g12()

x <- 20
g12()



codetools::findGlobals(g12)


environment(g12) <- emptyenv()
g12()


### 6.4.5 Exercises ############
# 1
c <- 10
c(c = c)

# 2
# lexical scoping
# fresh start
# name masking
# dynamic lookup


#3
f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)


## 6.5 Lazy evaluation ============

h01 <- function(x) {
  10
}
h01(stop("This is an error!"))

### 6.5.1 Promises ##########

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}

h02(y)

h02(y <- 1000)
y



double <- function(x) { 
  message("Calculating...")
  x * 2
}

h03 <- function(x) {
  c(x, x)
}

h03(double(20))


### 6.5.2 Default arguments #######
h04 <- function(x = 1, y = x * 2, z = a + b) {
  a <- 10
  b <- 100
  
  c(x, y, z)
}

h04()



h05 <- function(x = ls()) {
  a <- 1
  x
}
h05()


h05(ls())



### 6.5.3 Missing arguments #######


h06 <- function(x = 10) {
  list(missing(x), x)
}
str(h06())


str(h06(10))


?missing



args(sample)


sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <- length(x)
  }
  
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

?`%||%`


sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  size <- size %||% length(x)
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}







### 6.5.4 Exercises #######
# 1
x_ok <- function(x) {
  !is.null(x) && length(x) == 1 && x > 0
}
x_ok(NULL)
x_ok(1)
x_ok(1:3)

##--

x_ok <- function(x) {
  !is.null(x) & length(x) == 1 & x > 0
}
x_ok(NULL)
x_ok(1)
x_ok(1:3)


# 2
f2 <- function(x = z) {
  z <- 100
  x
}
f2()


# 3
y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  print(missing(x))
  print(missing(y))
  c(x, y)
  print(missing(x))
  missing(y)
}
f1()
y


# 4
range("Sturges")
?hist


# 5
show_time <- function(x = stop("Error!")) {
  stop <- function(...) Sys.time()
  print(x)
}
show_time()


show_time(x = stop("Error!"))


# 6
args(library)


## 6.6 ... (dot-dot-dot) =============

i01 <- function(y, z) {
  list(y = y, z = z)
}

i02 <- function(x, ...) {
  i01(...)
}

str(i02(x = 1, y = 2, z = 3))


i03 <- function(...) {
  list(first = ..1, third = ..3)
}
str(i03(1, 2, 3))


i04 <- function(...) {
  list(...)
}
str(i04(a = 1, b = 2))




x <- list(c(1, 3, NA), c(4, NA, 6))
str(lapply(x, mean, na.rm = TRUE))



print(factor(letters), max.levels = 4)

print(y ~ x, showEnv = TRUE)



sum(1, 2, NA, na_rm = TRUE)


### 6.6.1 Exercises #######
# 1
sum(1, 2, 3)
mean(1, 2, 3)


sum(1, 2, 3, na.omit = TRUE)
mean(1, 2, 3, na.omit = TRUE)


# 2
plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")
?par

# 3
plot(1:10, col = "red")
?plot.default


## 6.7 Exiting a function ======

### 6.7.1 Implicit versus explicit returns ######

j01 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}

j02 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}
j01(5)
j01(15)

### 6.7.2 Invisible values ######

j03 <- function() 1
j03()


j04 <- function() invisible(1)
j04()


print(j04())
(j04())



str(withVisible(j04()))
str(withVisible(j03()))



a <- 2
(a <- 2)


a <- b <- c <- d <- 2


### 6.7.3 Errors #######
j05 <- function() {
  stop("I'm an error")
  return(10)
}
j05()

### 6.7.4 Exit handlers #######


j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}

j06(TRUE)
j06(FALSE)


cleanup <- function(dir, code) {
  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE)
  
  old_opt <- options(stringsAsFactors = FALSE)
  on.exit(options(old_opt), add = TRUE)
}



with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  
  force(code)
}

getwd()
with_dir("~", getwd())



j08 <- function() {
  on.exit(message("a"), add = TRUE)
  on.exit(message("b"), add = TRUE)
}
j08()


j09 <- function() {
  on.exit(message("a"), add = TRUE, after = FALSE)
  on.exit(message("b"), add = TRUE, after = FALSE)
}
j09()



### 6.7.5 Exercises ########

# 1
load()


# 2
write.table()


# 3
source(file = "")
with_dir()

# 4
plot_pdf <- function(code) {
  pdf("test.pdf")
  on.exit(dev.off(), add = TRUE)
  code
}

# 5

capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE, after = TRUE)
  
  sink(temp)
  on.exit(sink(), add = TRUE, after = TRUE)
  
  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))


## 6.8 Function forms =========

### 6.8.1 Rewriting to prefix form ########
x + y
`+`(x, y)

names(df) <- c("x", "y", "z")
`names<-`(df, c("x", "y", "z"))

for(i in 1:10) print(i)
`for`(i, 1:10, print(i))


`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}

add <- function(x, y) x + y
lapply(list(1:3, 4:5), add, 3)



lapply(list(1:3, 4:5), `+`, 3)



### 6.8.2 Prefix form #########

help(mean)
help(top = mean)
help(topic = mean)



k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
str(k01(1, 2, 3))

str(k01(2, 3, abcdef = 1))

# Can abbreviate long argument names:
str(k01(2, 3, a = 1))

# But this doesn't work because abbreviation is ambiguous
str(k01(1, 3, b = 1))



### 6.8.3 Infix functions #########


`%+%` <- function(a, b) paste0(a, b)
"new " %+% "string"



`% %` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)

"a" % % "b"
"a" %/\% "b"


`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"

-1
+10


### 6.8.4 Replacement functions #########


`second<-` <- function(x, value) {
  x[2] <- value
  x
}


x <- 1:10
second(x) <- 5L
x


x <- 1:10
tracemem(x)
second(x) <- 6L



`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x

x <- `modify<-`(x, 1, 10)



x <- c(a = 1, b = 2, c = 3)
names(x)

names(x)[2] <- "two"
names(x)



`*tmp*` <- x
x <- `names<-`(`*tmp*`, `[<-`(names(`*tmp*`), 2, "two"))
rm(`*tmp*`)


### 6.8.5 Special forms ###########

# (x) (`(`(x))
# {x} (`{`(x)).
# The subsetting operators:
#   
#   x[i] (`[`(x, i))
# x[[i]] (`[[`(x, i))
# And the tools of control flow:
#   
#   if (cond) true (`if`(cond, true))
# if (cond) true else false (`if`(cond, true, false))
# for(var in seq) action (`for`(var, seq, action))
# while(cond) action (`while`(cond, action))
# repeat expr (`repeat`(expr))
# next (`next`())
# break (`break`())
# Finally, the most complex is the function function:
#   
# function(arg1, arg2) {body} (`function`(alist(arg1, arg2), body, env))



### 6.8.6 Exercises ########

# 1
1 + 2 + 3
#
`+`(`+`(1, 2) , 3)



1 + (2 + 3)
#
`+`(`+`(2, 3), 1)


if (length(x) <= 5) x[[5]] else x[[n]]
#
`if`(`<=`(length(x), 5), `[[`(x, 5), `[[`(x, n))


# 2
x <- sample(replace = TRUE, 20, x = c(1:10, NA))
#
x <- sample(c(1:10, NA), size = 20, replace = TRUE)


y <- runif(min = 0, max = 1, 20)
#
y <- runif(20, min = 0, max = 1)


cor(m = "k", y = y, u = "p", x = x)
#
cor(x,y,  use = "pairwise.complete.obs", method = "kendall")



# 3
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(get("x"), 1) <- 10



get("x")[1]
get("x") <- 1
?get


# 4
`modify_random<-` <- function(x, value){
  index <- sample(length(x), 1)
  x[index] <- value
  x
}

x <- vector()
modify_random(x) <- (10)
x


# 5

`+` <- function(x, y){
  if(is.character(c(x,y))) paste0(x,y) else  base::`+` (x, y)
}

1 + 2
"a" + "b"


# 6
replacement_funs <- apropos('<-$') %>% 
  sapply(get) %>% 
  sapply(is.primitive)

replacement_funs[replacement_funs]


# 8
`%xor%` <- function(a, b){
  xor(a, b)
}

1 %xor% 0



# 9
`%n%` <- function(a, b){
  intersect(a, b)
}
`%u%` <- function(a, b){
  union(a, b)
}
  
`%/%` <- function(a, b){
  setdiff(a, b)
}


a <- 1:10
b <- 5:15

a %n% b
a %u% b
a %/% b
