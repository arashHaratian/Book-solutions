# 4 Subsetting ---------------

## 4.2 Selecting multiple elements =============

### 4.2.1 Atomic vectors ########
x <- c(2.1, 4.2, 3.3, 5.4)

x[c(3, 1)]
x[order(x)]

# Duplicate indices will duplicate values
x[c(1, 1)]

# Real numbers are silently truncated to integers
x[c(2.1, 2.9)]
x[-c(3, 1)]

x[c(-1, 2)]


x[c(TRUE, TRUE, FALSE, FALSE)]

x[x > 3]


x[c(TRUE, FALSE)]


x[c(TRUE, TRUE, NA, FALSE)]


x[]

x[0]



(y <- setNames(x, letters[1:4]))
y[c("d", "c", "a")]
y[c("a", "a", "a")]
z <- c(abc = 1, def = 2)
z[c("a", "d")]



y[factor("b")]


### 4.2.3 Matrices and arrays #######

a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ]


a[c(TRUE, FALSE, TRUE), c("B", "A")]

a[0, -2]



a[1, ]
a[1, 1]


vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals

vals[c(4, 15)]



select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))
vals[select]


### 4.2.4 Data frames and tibbles ######
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

df[df$x == 2, ]
df[c(1, 3), ]
df[c("x", "z")]
df[, c("x", "z")]


df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])

str(df["x"])
str(df[, "x"])


### 4.2.5 Preserving dimensionality #########

a <- matrix(1:4, nrow = 2)
str(a[1, ])

str(a[1, , drop = FALSE])


df <- data.frame(a = 1:2, b = 1:2)
str(df[, "a"])

str(df[, "a", drop = FALSE])


z <- factor(c("a", "b"))
z[1]

z[1, drop = TRUE]

### 4.2.6 Exercises ###########

# 1
mtcars[mtcars$cyl = 4, ]
#
mtcars[mtcars$cyl == 4, ]


mtcars[-1:4, ]
#
mtcars[-(1:4), ]


mtcars[mtcars$cyl <= 5]
#
mtcars[mtcars$cyl <= 5,]


mtcars[mtcars$cyl == 4 | 6, ]
#
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]
mtcars[mtcars$cyl %in% c(4, 6), ]


# 2
x <- 1:5
x[NA] ## because this NA is logical NA and it will follow the recycling rule
x[NA_real_]


# 3
? upper.tri()

x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)]



# 4
mtcars[1:20]
mtcars[1:20, ]

str(mtcars)


# 5
x <- outer(1:5, 1:5, FUN = "*")
diag(x)

my_diag <- function(object){
خ  n <- min(nrow(x), ncol(x))
  output <- vector(mode = typeof(object), length = n)
  for(i in 1:length(output))
    output[[i]] <- object[i, i]
  
  output
}

my_diag(x)



# 6

df[is.na(df)] <- 0







## 4.3 Selecting a single element ===============

### 4.3.1 [[ ###########

x <- list(1:3, "a", 4:6)


### 4.3.2 $ ##########
var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var
# Instead use [[
mtcars[[var]]

x <- list(abc = 1)
x$a
x[["a"]]


options(warnPartialMatchDollar = TRUE)
x$a


### 4.3.3 Missing and out-of-bounds indices ##########


x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)

purrr::pluck(x, "a", 1)

purrr::pluck(x, "c", 1)

purrr::pluck(x, "c", 1, .default = NA)



### 4.3.5 Exercises #########

# 1
mtcars$cyl[[3]]
mtcars$cyl[3]
mtcars[[c(2,3)]]
mtcars[['cyl']][[3]]


# 2

mod <- lm(mpg ~ wt, data = mtcars)

mod$df.residual
mod[["df.residual"]]
mod$df
mod["df.residual"]



names(summary(mod))
summary(mod)$r.squared
summary(mod)[[8]]





## 4.4 Subsetting and assignment ========


x <- 1:5
x[c(1, 2)] <- c(101, 102)




x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

y <- list(a = 1, b = 2)
y["b"] <- list(NULL)
str(y)




mtcars[] <- lapply(mtcars, as.integer)
is.data.frame(mtcars)


mtcars <- lapply(mtcars, as.integer)
is.data.frame(mtcars)




## 4.5 Applications ==============

### 4.5.1 Lookup tables (character subsetting) ##########

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]

unname(lookup[x])

### 4.5.2 Matching and merging by hand (integer subsetting) #######

grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)


id <- match(grades, info$grade)
id
info[id, ]


### 4.5.3 Random samples and bootstraps (integer subsetting) ######

df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])

# Randomly reorder
df[sample(nrow(df)), ]

# Select 3 random rows
df[sample(nrow(df), 3), ]


# Select 6 bootstrap replicates
df[sample(nrow(df), 6, replace = TRUE), ]



### 4.5.4 Ordering (integer subsetting) ######

?order

x <- c("b", "c", "a")
order(x)
x[order(x)]
?order


# Randomly reorder 'df
df2 <- df[sample(nrow(df)), 3:1]
df2


df2[order(df2$x), ]

df2[, order(names(df2))]



### 4.5.5 Expanding aggregated counts (integer subsetting) ######


df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)

df[rep(1:nrow(df), df$n), ]


### 4.5.6 Removing columns from data frames (character) ########


df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df$z <- NULL


df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]

df[setdiff(names(df), "z")]
?setdiff

### 4.5.7 Selecting rows based on a condition (logical subsetting) ############

mtcars[mtcars$gear == 5, ]

mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]


### 4.5.8 Boolean algebra versus sets (logical and integer) ##########

x <- sample(10) < 4
which(x)


unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
unwhich(which(x), 10)



(x1 <- 1:10 %% 2 == 0)

(x2 <- which(x1))

(y1 <- 1:10 %% 5 == 0)

(y2 <- which(y1))


# X & Y <-> intersect(x, y)
x1 & y1

intersect(x2, y2)


# X | Y <-> union(x, y)
x1 | y1

union(x2, y2)


# X & !Y <-> setdiff(x, y)
x1 & !y1

setdiff(x2, y2)


# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
xor(x1, y1)

setdiff(union(x2, y2), intersect(x2, y2))


### 4.5.9 Exercises #######

# 1
names(mtcars)
names(mtcars[, sample(ncol(mtcars))])

x <- data.frame(
  x = 1:3,
  y = 13:11,
  z = letters[1:3]
)
x
x[sample(3), sample(3)]


# 2
sample(nrow(df), m)

start <- sample(nrow(mtcars) - m + 1, 1)
end <- start + m - 1
mtcars[start:end, , drop = FALSE]


# 3
names(mtcars)
names(mtcars[order(names(mtcars))])
