# 3 Vectors --------------


## 3.2 Atomic vectors ===============

### 3.2.2 Making longer vectors with c() ########


c(c(1, 2), c(3, 4))

?typeof
?length

### 3.2.3 Missing values ############
NA > 5
10 * NA
!NA

NA ^ 0
NA | TRUE
NA & FALSE

x <- c(NA, 5, NA, 10)
x == NA
is.na(x)

### 3.2.4 Testing and coercion #######

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)
# Total number of TRUEs
sum(x)
# Proportion that are TRUE
mean(x)


as.integer(c("1", "1.5", "a"))


### 3.2.5 Exercises #######
# 1
?complex
?raw
raw(2)
1+2i


# 2

c(1, FALSE)
c("a", 1)
c(TRUE, 1L)


# 3

1 == "1"
-1 < FALSE
"one" < 2


# 4

c(FALSE, NA_character_)

# 5

?is.vector()
?is.atomic
?is.numeric


## 3.3 Attributes =============
### 3.3.1 Getting and setting ###########
a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")

attr(a, "y") <- 4:6
str(attributes(a))


# Or equivalently
a <- structure(
  1:3, 
  x = "abcdef",
  y = 4:6
)
str(attributes(a))


attributes(a[1])
attributes(sum(a))


### 3.3.2 Names  #############

# When creating it: 
x <- c(a = 1, b = 2, c = 3)

# By assigning a character vector to names()
x <- 1:3
names(x) <- c("a", "b", "c")

# Inline, with setNames():
x <- setNames(1:3, c("a", "b", "c"))

# remove the names by either
unname(x)
names(x) <- NULL

### 3.3.3 Dimensions ############


# Two scalar arguments specify row and column sizes
a <- matrix(1:6, nrow = 2, ncol = 3)
a

# One vector argument to describe all dimensions
b <- array(1:12, c(2, 3, 2))
b

# You can also modify an object in place by setting dim()
c <- 1:6
dim(c) <- c(3, 2)
c



str(1:3)                   # 1d vector
str(matrix(1:3, ncol = 1)) # column vector
str(matrix(1:3, nrow = 1)) # row vector
str(array(1:3, 3))         # "array" vector


### 3.3.4 Exercises ##############

# 1
setNames
unname

# 2
x <- vector(length = 10)
dim(x)

?NROW

NROW(x)
NCOL(x)

x <- vector()
length(x)
NROW(x)


# 3
x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))

# 4
structure(1:5, comment = "my attribute")
attributes(structure(1:5, comment = "my attribute"))
?comment
print(structure(1:5, comment = "my attribute"))



## 3.4 S3 atomic vectors ==============

### 3.4.1 Factors ###########

x <- factor(c("a", "b", "b", "a"))
x

typeof(x)

attributes(x)



sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

table(sex_char)

table(sex_factor)


grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade


### 3.4.2 Dates ##############

today <- Sys.Date()

typeof(today)

attributes(today)


date <- as.Date("1970-02-01")
unclass(date)


### 3.4.3 Date-times #########

now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct

typeof(now_ct)
attributes(now_ct)



structure(now_ct, tzone = "Asia/Tokyo")
structure(now_ct, tzone = "America/New_York")
structure(now_ct, tzone = "Australia/Lord_Howe")
structure(now_ct, tzone = "Europe/Paris")


### 3.4.4 Durations ##########

one_week_1 <- as.difftime(1, units = "weeks")
one_week_1

typeof(one_week_1)
attributes(one_week_1)

one_week_2 <- as.difftime(7, units = "days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)

### 3.4.5 Exercises #################

# 1
sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f", "z"))

t <- table(sex_factor)
attributes(t)
typeof(t)
str(t)

t <- table(mtcars[c("vs", "cyl", "am")])
t <- table(sex_factor)
attributes(t)
typeof(t)
str(t)


# 2
f1 <- factor(letters)
labels(f1)
levels(f1)
levels(f1) <- rev(levels(f1))
f1
labels(f1)
levels(f1)


unclass(f1)


# 3
f2 <- rev(factor(letters))
f2

f3 <- factor(letters, levels = rev(letters))
as.numeric(f3)
labels(f3)
levels(f3)
f3


## 3.5 Lists ===============
### 3.5.1 Creating #########

l1 <- list(
  1:3, 
  "a", 
  c(TRUE, FALSE, TRUE), 
  c(2.3, 5.9)
)

typeof(l1)
str(l1)



lobstr::obj_size(mtcars)

l2 <- list(mtcars, mtcars, mtcars, mtcars)
lobstr::obj_size(l2)


l3 <- list(list(list(1)))
str(l3)


l4 <- list(list(1, 2), c(3, 4))
l5 <- c(list(1, 2), c(3, 4))

str(l4)
str(l5)


### 3.5.2 Testing and coercion ##########

list(1:3)
as.list(1:3)


unlist()

### 3.5.3 Matrices and arrays ##########

l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)
l

l[[1, 1]]

### 3.5.4 Exercises ##########

# 1
#> list can store other list and any multiple type of vectors
#> lists can save vectors as they are but vectors can not save the lists specially when there is another vector

lobstr::ref(1:2)
lobstr::ref(list(1:2, 2))

# 2
?unlist

l <- list(1:5)
l
as.vector(l)

l <- list(list(1:3), 5)
l

as.vector(l)
unlist(l)


# 3

date <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
attributes(date)
str(date)

str(c(date))


## 3.6 Data frames and tibbles ==============

df1 <- data.frame(x = 1:3, y = letters[1:3])
typeof(df1)
attributes(df1)


library(tibble)

df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)

attributes(df2)


### 3.6.1 Creating #########
df <- data.frame(
  x = 1:3, 
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
str(df)


df2 <- tibble(
  x = 1:3, 
  y = c("a", "b", "c")
)
str(df2)




names(data.frame(`1` = 1))
names(tibble(`1` = 1))



data.frame(x = 1:4, y = 1:2)
data.frame(x = 1:4, y = 1:3)

tibble(x = 1:4, y = 1)
tibble(x = 1:4, y = 1:2)



tibble(
  x = 1:3,
  y = x * 2
)


### 3.6.2 Row names ###############

df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blond", "brown", "black"),
  row.names = c("Bob", "Susan", "Sam")
)
df3

rownames(df3)
df3["Bob", ]

df3[c(1, 1, 1), ]
as_tibble(df3, rownames = "name")



### 3.6.3 Printing #########
dplyr::starwars

### 3.6.4 Subsetting ################

df1 <- data.frame(xyz = "a")
df2 <- tibble(xyz = "a")

str(df1$x)
str(df2$x)


### 3.6.5 Testing and coercing ############

is.data.frame(df1)
is.data.frame(df2)
is_tibble(df1)
is_tibble(df2)


### 3.6.6 List columns #########

df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)

data.frame(
  x = 1:3, 
  y = I(list(1:2, 1:3, 1:4))
)


tibble(
  x = 1:3, 
  y = list(1:2, 1:3, 1:4)
)


### 3.6.7 Matrix and data frame columns #########
dfm <- data.frame(
  x = 1:3 * 10
)
dfm$y <- matrix(1:9, nrow = 3)
dfm$z <- data.frame(a = 3:1, b = letters[1:3], stringsAsFactors = FALSE)

str(dfm)

dfm[1, ]


### 3.6.8 Exercises ########

# 1

df <- data.frame("a" = NULL)
df <- data.frame()


# 2
df <- data.frame(1:3)
rownames(df) <- c("a", "a", "a")

df[c(1, 1, 1), , drop = FALSE]


# 3
df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = F)
df
t(df)
t(t(df))

is.matrix(df)
is.matrix(t(df))

# 4
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = F)

?as.matrix
?data.matrix
as.matrix(df)
data.matrix(df)


## 3.7 NULL ===========
typeof(NULL)
length(NULL)
x <- NULL
attr(x, "y") <- 1
is.null(NULL)


c()
