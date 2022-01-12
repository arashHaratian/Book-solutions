#======================= chapter 18 ======================
library(magrittr)
foo_foo <- little_bunny()
foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)



diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

?object.size
?pryr::object_size

bop(
  scoop(
    hop(foo_foo, through = forest),
    up = field_mice
  ), 
  on = head
)


assign("x", 10)
x
"x" %>% assign(100)
x
"x" %>% assign(100) %>% print()

env <- environment()
"x" %>% assign(100, envir = env)
x
?get
?load


tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>% 
  tryCatch(error = function(e) "An error")
?try
?suppressMessages
?suppressWarnings()


rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>% 
  plot() %>%
  str()

mtcars %$%
  cor(disp, mpg)

mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)

mtcars %<>% transform(cyl = cyl * 2)


#=============================== chapter 19 ==============================
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))


df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

##exercises 19.2.1

rescale01_exer1 <- function(x) {
  rng <- range(x, na.rm = F)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01_exer1(c(-1,0,1,2,3,NA))

rescale01_exer2 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  x <- (x - rng[1]) / (rng[2] - rng[1])
  x[x == Inf] <- 1
  x[x == -Inf] <- -1
  x
}
rescale01_exer2(c(-1,0,1,2,3, Inf))

#--
x <- c(1,2,NA,NA,3,NA)
mean(is.na(x))

na_prop <- function(data){
  mean(is.na(x))
}
na_prop(x)

x <- c(-1:3,3,NA)
x / sum(x, na.rm = T)
w <- function(data){
  x / sum(x, na.rm = T)
}
w(x)

#--
x <- c(1:3,3,NA)
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
coef_var <- function(data){
  sd <- sd(x, na.rm = TRUE)
  ave <- mean(x, na.rm = TRUE)
  sd/ave
}
coef_var(x)


x <- c(1:10)
var_exer <- function(data, rm.na = T){
  mean <- mean(x, rm.na = rm.na)
  n <- length(x)
  sum_val <- (x - mean)^2 %>% 
    sum()
  sum_val / (n - 1)
}
var_exer(x)


skewness <- function(data) {
  var <- var_exer(data)
  mean <- mean(data)
  n <- length(data)
  sum <- (x - mean)^3 %>% 
    sum()
  (sum/(n - 2)) / var^(3/2)
  
}
skewness(x)

x <- c(1:3,NA)
y <- c(1,2,3,NA)
both_na <- function(x, y){
  x_na <- is.na(x)
  y_na <- is.na(y)
  sum(x_na & y_na)
}
both_na(x, y)


is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0


verse1 <- function() {
  foo_foo <- little_bunny()
  foo_foo_1 <- hop(foo_foo, through = forest)
  foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
  foo_foo_3 <- bop(foo_foo_2, on = head)
  
}

verse2 <- function(){
  foo_foo <- little_bunny()
  foo_foo_1 <- !see(foo_foo, through = you)
  foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
  foo_foo_3 <- bop(foo_foo_2, on = head)
}
compelete_little_bunny <- function(variables){ #
  verse1()
  down(state = "good")
  verse2
}

#----------------

##exercises 19.3.1
is_samePrefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

is_samePrefix("arash_h", "arash")


lag <- function(x) { #droplist
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}


recycling <- function(x, y) { #expand
  rep(y, length.out = length(x))
}


?rnorm()
?MASS::mvrnorm()
rnorm()
#-------------------

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
has_name()


if (c(TRUE, FALSE)) {}
if (NA) {}

c(T,F,T,F) | c(T,T,F,F)
c(T,F,T,F) || c(T,T,F,F)

c(T,F,T,F) & c(T,T,F,F)
c(T,F,T,F) && c(T,T,F,F)

all(c(T,F,T,F))
any(c(T,F,T,F))

c(T,T,F,F) == c(T,F,T,F)
any(c(T,T,F,F) == c(T,F,T,F))
all(c(T,T,F,F) == c(T,F,T,F))
identical(c(T,T,F,F) , c(T,F,T,F))

identical(0L, 0)

x <- sqrt(2) ^ 2
x
x == 2
x - 2
dplyr::near(x ,2)

x == NA

function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
  )
}

?cut

y <- 10
x <- if (y < 20) "Too low" else "Too high"
if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}

##exercises 19.4.4
?ifelse
x <- c(1:10)
ifelse(x>5, 1, 0)
if(x>5) print(1) else print(0)


greeting <- function(date = lubridate::now()) {
  hour <- hour(date)
  if ( 5 <= hour && 11 > hour){
    print("good morning! :]")
  } else if(11 <= hour && 17 > hour){
    print("good afternoon")
  } else if (17 <= hour && 19 > hour){
    print("good evening")
  } else {
    print("good night")
  }
}
greeting(now())


x <- 3
if(x%%15 == 0){ 
  print("fizzbuzz")
}else if(x%%3 == 0){
  print("fizz")
}else if(x%%5== 0){
  print("buzz")
}else {
  print(x)
}

temp <- 20

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}
cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
    right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot")
)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
    right = F,
    labels = c("freezing", "cold", "cool", "warm", "hot")
)

x <- 2
switch(x, "apple", "banana", "cantaloupe")
switch(2.3, "apple", "banana", "cantaloupe")

x <- c("a")
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)


wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}
wt_mean(1:6, 1:3)

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 1:3)

wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}# it is not good.  (a lot of work for little gain)


wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
} ##it's better
wt_mean(1:6, 6:1, na.rm = "foo")


sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stringr::str_c("a", "b", "c", "d", "e", "f")

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...) ## str_c(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")


x <- c(1, 2)
sum(x, na.mr = TRUE)

#list(...) makes a list of unmatched arguments

##exercises 19.5.5
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters, collapse = "-") #str_c(letters, collapse = "-", collapse = ", ")

commas <- function(...) stringr::str_c(...)
commas(letters, collapse = " - ")



rule <- function(..., pad = "-") {
  title <- paste0(...) ## str_c(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Title", pad = "-+")
rule <- function(..., pad = "-") {
  title <- paste0(...) ## str_c(...)
  width <- (getOption("width") - nchar(title) - 5) / nchar(pad)
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Title", pad = "-+")

?mean
x <- c(12:34)
mean(x)
mean(x, trim = 0.5)
x <- c(0:10, 50)
xm <- mean(x)
c(xm, mean(x, trim = 0.10))
#https://garstats.wordpress.com/2017/11/28/trimmed-means/

cor
?cor()
##------------------------

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}



show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}
show_missings(mtcars)   #call interactively 
x <- show_missings(mtcars) 
class(x)
dim(x)

mtcars %>% 
  show_missings() %>% 
  dplyr::mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 

?invisible


f <- function(x) {
  x + y
} 
y <- 100
f(10)
y <- 1000
f(10)



`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
rm(`+`)


##==================== chapter 20 =========================
library(tidyverse)

typeof(letters)
typeof(1:10)
x <- list("a", "b", 1:10)
length(x)

1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)


x <- sqrt(2) ^ 2
x - 2

c(-1, 0, 1) / 0


x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

NA            # logical
NA_integer_   # integer
NA_real_      # double
NA_character_ # character
typeof(NA_character_)



#
##exercises 20.3.5
x <- c(0, NA, NaN, Inf, -Inf)
is.finite(x)
!is.infinite(x)

dplyr::near

?integer
.Machine$integer.max + 1L
as.numeric(.Machine$integer.max) + 1
.Machine$double.max.exp
.Machine$double.xmax


x <- 2
typeof(x)
typeof(as.integer(x))
#-------------------------
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)  # how many are greater than 10?
mean(y) # what proportion are greater than 10?


typeof(c(TRUE, 1L))
#> [1] "integer"
typeof(c(1L, 1.5))
#> [1] "double"
typeof(c(1.5, "a"))
#> [1] "character"

?is_scalar_atomic()


sample(10) + 100
runif(10) > 0.5
1:10 + 1:2
1:10 + 1:3

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)
purrr::set_names(1:3, c("a", "b", "c"))


x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]

x <- c(10, 3, NA, 5, 8, 1, NA)
# All non-missing values of x
x[!is.na(x)]
# All even (or missing!) values of x
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
x[c("xyz", "def", "def")]
x[["def"]]
x[[c("def", "xyz")]]

##exercises 20.4.6
x <- c(1,3,NA, NA, NA)
mean(is.na(x))
x <- c(1, 3, Inf, Inf)
sum(!is.finite(x))


?is.vector()
?is.atomic()
x <- c(a = 1, b = 2)
is.vector(x)
is.vector(x, mode = "double")
is.vector(x, mode = "character")
as.vector(x)
x <- 1:10
attr(x, "something") <- TRUE
is.vector(x)
is.atomic(x)
is.atomic(1:10)
is.atomic(list(a = 1))


?setNames
setNames(1:10, c("A"))
setNames(nm = c("First", "2nd"))
?set_names
set_names(1:10, c("A"))
set_names(nm = c("First", "2nd"))
purrr::set_names(1:4, "a", "b", "c", "d")
set_names(1:3)
purrr::set_names(c(a = 1, b = 2, c = 3), toupper)


last_value <- function(data) {
  data[[length(data)]]  
}
last_value(1)

even_position <- function(data){
  index <- seq_along(data) %% 2 == 0
  data[index]
}
even_position(x)

drop_last <- function(data){
  data[-length(data)]
}
drop_last(x)

even_values <- function(data){
  data[data%%2 == 0 & !is.na(data)]
}
even_values(c(1:10, NA, NA))

x <- c(-1:1, Inf, -Inf, NaN, NA)
x[-which(x > 0)]
x[x <= 0]


x <- 1:3
x[5]
x[-5]
x[[5]]
x <- setNames(x, c("a", "b", "c"))
x["ab"]
x[["d"]]

x <- list(1, 2, 3)
x
str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))


a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

str(a[1:2])
str(a[4])

str(a[[1]])
str(a[[4]])

a$a
a[["a"]]

a[[4]]
a[[4]][1]
a[[4]][[1]]

##exercises 20.5.4
list(a, b, list(c, d), list(e, f))
list(list(list(list(list(list(a))))))

diamonds[1]
diamonds[[1]]
diamonds$carat
diamonds[1,]
diamonds[, 1]
#----------------------------

x <- 1:10
attr(x, "greeting")
#> NULL
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
str(x)
class(x)
typeof(x)

as.Date
methods("as.Date")
?methods

getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

methods("[[")



x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)


x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x


y <- as.POSIXlt(x)
typeof(y)
attributes(y)



tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)


##exercises 20.7.4
x <- hms::hms(3600)
typeof(x)
attributes(x)


x <- list("a" = 1:5, "b" = 1:2)
attr(x, "class") <- c("tbl_df", "tbl", "data.frame")
attr(x, "row.names") <- 1:5
typeof(x)
attributes(x)

tibble::tibble(x = 1:5, y = 1:2)


x <- tibble("a" = 1:10)
list("x" = x, 1)
y <- tibble("b" = 7:1)
list("x" = x, "y" = y, "end" = 1)

tibble(x = 1:3, y = list("a", 1, list(1:3)))
tibble(x = 1:3, y = list(1,2,3))
tibble(x = 1:3, y = list(1:3))
tibble(x = 1:3, y = list(1:10))
tibble(x = 1:3, y = list(1:10, 2))
#----------------------------------




#============================== chapter 21 ========================
library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
median(df$a)
median(df$b)
median(df$c)
median(df$d)


output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output


y <- vector("double", 0)
seq_along(y)
1:length(y)

##exercises 21.2.1
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in seq_along(mtcars)){
  output[[i]] <- mean(mtcars[[i]])
}

output <- vector("character", ncol(nycflights13::flights))
for(i in seq_along(nycflights13::flights)){
  output[[i]] <- typeof(nycflights13::flights[[i]])
}
output <- list()
for(i in seq_along(nycflights13::flights)){
  output[[i]] <- class(nycflights13::flights[[i]])
}

output <- vector("integer")
for(i in seq_along(iris)){
  output[[i]] <- n_distinct(iris[[i]])
}

means <- c(-10, 0, 10, 100)
output <- vector("list", length(means))
for(i in seq_along(means)){
  output[[i]] <- rnorm(10, means[[i]])
}
matrix(rnorm(10 * length(means), mean = means), ncol = 10)


out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
#--
out <- ""
out <- str_c(letters, collapse = "")
out <- paste0(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
#--
x <- sample(100)
sd(x)


x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
#--
cumsum(x)
all.equal(cumsum(x), out)



not_prealloc <- function(x){
  t <- Sys.time()
  output <- vector("integer", 0)
  for (i in seq_along(x)) {
    output <- c(output, lengths(x[[i]]))
  }
  print(Sys.time() - t)
}
prealloc <- function(x){
  t <- Sys.time()
  output <- vector("integer", length(x))
  for (i in seq_along(x)) {
    output[[i]] <- lengths(x[[i]])
  }
  print(Sys.time() - t)
}
x <- 1:100000
not_prealloc(x)
prealloc(x)

not_prealloc <- function(x){
  output <- vector("integer", 0)
  for (i in seq_along(x)) {
    output <- c(output, lengths(x[[i]]))
  }
}
prealloc <- function(x){
  output <- vector("integer", length(x))
  for (i in seq_along(x)) {
    output[[i]] <- lengths(x[[i]])
  }
}
timings <- microbenchmark::microbenchmark(not_prealloc(1:10000), prealloc(1:10000), times = 10)
timings
#---------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
#--
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

for (i in df) {print(i)}

results <- vector("list", length(mtcars))
names(results) <- names(mtcars)
for(i in names(mtcars)){
  # print(i)
  print(mtcars[[i]])
}
x <- mtcars
for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}


means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

?purrr::flatten_dbl()
?dplyr::bind_rows()


flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0
while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips


##exercises 21.3.5
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
output <- vector("list", length(files))
for (i in seq_along(files)){
  output[[i]] <- read_csv(files[[i]])
}
bind_rows(output)

x <- 1:5
out <- double()
for (nm in names(x)){
  print(x[[nm]])
  out <- x[[nm]]
}
x <- c("a" = 1, "b" = 2, "c" = 3, 4, 5)
for (nm in names(x)){
  print(nm)
  print(x[[nm]])
  out <- x[[nm]]
}
x <- c("a" = 1, "b" = 2, "c" = 3,"a" = 4, "b" = 5)
for (nm in names(x)){
  print(x[[nm]])
  out <- x[[nm]]
}


show_mean <- function(data){
  means <- vector("double", length = length(data))
  for (i in seq_along(data)){
    if (is.numeric(iris[[i]]))
      means[[i]] <- mean(data[[i]])
  }
  means
}
show_mean(iris)
#--
show_mean <- function(data){
  means <- vector("list", length = length(data))
  for (i in seq_along(data)){
    if (is.numeric(data[[i]])){
      names(means)[[i]] <- names(data)[[i]]
      means[[i]] <- mean(data[[i]])
    }
  }
  bind_cols(means)
}
t <- show_mean(iris)
#--
show_mean2 <- function(data){
  means <- show_mean(data)
  means <- unlist(means)
  nm <- names(means)
  max_len <- max(str_length(nm)) + 2
  for (i in seq_along(means)){
    print(
      str_c(nm[[i]],":") %>% 
        str_pad(width = max_len, side = "right") %>% 
        str_c(means[[i]]) #          format(mean(df[[nm]]), digits = digits, nsmall = digits),
    )
  }
}
show_mean2(iris)



trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
trans[["disp"]]
trans[["disp"]](1:10)
#----------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}

f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3
#--
f <- function(x, i) abs(x - mean(x)) ^ i


col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)
col_summary(df, mean)



##exercises 21.4.1
?apply
match.fun("*")
match.fun("mean")
match.fun(mean)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
apply(x, 2, mean, trim = .2)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5, NA))
apply(x, 2, mean, trim = .2, T)



col_summary <- function(df, fun) {
  index <- vector("logical", length(df))
  for(i in seq_along(df)){
    index[[i]] <- is.numeric(df[[i]])
  }
  new_df <- df[index]
  out <- vector("double", length = length(new_df))
  for (i in seq_along(new_df)) {
    out[i] <- fun(new_df[[i]])
  }
  names(out) <- names(new_df)
  out
}

col_summary(iris, mean)

#----------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)


df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

map_dbl(df, mean, trim = 0.5)

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))


models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))
# mtcars %>%
#   split(.$cyl) %>%
#   map(~lm(mpg ~ wt, data = ..1)) #or ".x"

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)
?map_dbl

mtcars %>% map_dbl(1)


x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

vapply(df, is.numeric, logical(1))
map_lgl(df, is.numeric)


##exercises 21.5.3
mtcars %>% 
  map_dbl(mean)

nycflights13::flights %>% 
  map(class)

iris %>% 
  map_int(n_distinct)

c(10, 100, 0, -10) %>% 
  map(rnorm, n = 10)
lapply(c(10, 100, 0, -10), FUN = rnorm, n = 10)
# vapply(c(10, 100, 0, -10), rnorm, double(), n = 10)
mapply(c(10, 100, 0, -10), FUN = rnorm, n = 10)
sapply(c(10, 100, 0, -10), FUN = rnorm, n = 10)



map_lgl(diamonds, is.factor)



c(10, 100, 0, -10) %>% 
  map(rnorm, n = 10)
map(1:5, runif)


map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)

map(-2:2, rnorm, n = 5) %>%
  flatten_dbl()



map(x, function(df) lm(mpg ~ wt, data = df))
x <- split(mtcars, mtcars$cyl)
map(x, ~lm(mpg ~ wt, data = .))
#--------------------

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)


is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()


x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()


mu <- list(5, 10, -3) #or vector
mu %>% 
  map(rnorm, n = 5) %>% 
  str()


sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

map2(mu, sigma, rnorm, n = 5) %>% str()

# map2 <- function(x, y, f, ...) {
#   out <- vector("list", length(x))
#   for (i in seq_along(x)) {
#     out[[i]] <- f(x[[i]], y[[i]], ...)
#   }
#   out
# }


n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()


params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)
params %>% # good for the times that code gets complicated
  pmap(rnorm)

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>% str()


sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))


x <- list(1, "a", 3)
x %>% 
  walk(print)


library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")
pwalk(list(paths, plots), ggsave, path = tempdir())

pwalk(list(paths, plots), ggsave, path = tempdir()) %>% str()


iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()


x <- list(1:5, letters, list(10))
?some
x %>% 
  some(is_character)
?every
x %>% 
  every(is_vector)

x <- sample(10)
x
?detect
x %>% 
  detect(~ . > 5)
?detect_index
x %>% 
  detect_index(~ . > 5)


?head_while
x %>% 
  head_while(~ . > 5)
?tail_while
x %>% 
  tail_while(~ . > 5)


dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs %>% reduce(full_join)


vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)


x <- sample(10)
x
?accumulate
x %>% accumulate(`+`)

vs %>% accumulate(intersect)


##exercises 21.9.3
x <- list(1:5, letters, list(10))
x
x %>% 
  every(is_character)
x %>% 
  every(is_vector)
#--
my_every <- function(data, fun){
  for(i in seq_along(data)){
    if(!fun(data[[i]]))
      return(FALSE)
  }
  TRUE
}
x %>% 
  my_every(is_character)
x %>% 
  my_every(is_vector)
vs %>% 
  my_every(function(df){df > 1})
vs %>% 
  every(function(df){df > 1})
every

my_every <- function(data, fun){ 
  for(i in seq_along(data)){
    val <- !fun(data[[i]])
    if(reduce(val, `&`))
      return(FALSE)
  }
  TRUE
}
vs %>% 
  my_every(function(df){df >5})



col_summary <- function(df, fun) {
  df %>% 
    keep(is.numeric) %>% 
    map(fun)
}
col_summary(iris, mean)


col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)#in"df_num <- df[, is_num]"line it wants to split 0 list(and also it can not use list)
#--
col_sum3 <- function(df, f) {
  is_num <- map_lgl(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean) 
#--------------------------

