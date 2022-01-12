# 2 Names and values ----------------

                                        ##### run the codes in terminal


## 2.1 Introduction ======================

### Quiz #######
# 1
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)

df$`3` <- df$`1` + df$`2`


# 2
x <- runif(1e6)
y <- list(x, x, x)

object.size(x)
object.size(y)


# 3
a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10




##2.2 Binding basics ===================

library(lobstr)


x <- c(1, 2, 3) # binding left hand side (name) to the ride hand side object
y <- x

obj_addr(x)
obj_addr(y)

### 2.2.1 Non-syntactic names ######

?Reserved

_abc <- 1
if <- 10


`_abc` <- 1
`if` <- 10

"_abc" <- 1


### 2.2.2 Exercises #########

# 1
a <- 1:10
b <- a
c <- b
d <- 1:10
obj_addrs(list(a,b,c,d))

# 2

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

obj_addrs(list(mean,
               base::mean,
               get("mean"),
               evalq(mean),
               match.fun("mean")
))


# 3 , 4 , 5


?read.csv()
?make.names
make.names(1)
make.names("_abc")

make.names(.123e1)


## 2.3 Copy-on-modify ===========
x <- c(1, 2, 3)
y <- x

y[[3]] <- 4
x

### 2.3.1 tracemem() ###########

## must use R markdown or terminal
x <- c(1, 2, 3)
cat(tracemem(x), "\n")

y <- x
y[[3]] <- 4L


y[[3]] <- 5L

untracemem(x)


### 2.3.2 Function calls ########


f <- function(a) {
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")
z <- f(x)


untracemem(x)




### 2.3.3 Lists ##########

l1 <- list(1, 2, 3)
l2 <- l1
l2[[3]] <- 4

ref(l1, l2)

# obj_addrs(l1)
# obj_addrs(l2)

### 2.3.4 Data frames ########


d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))

d2 <- d1
d2[, 2] <- d2[, 2] * 2


d3 <- d1
d3[1, ] <- d3[1, ] * 3

ref(d1, d2)
ref(d1, d3)
### 2.3.5 Character vectors #########

x <- c("a", "a", "abc", "d")

ref(x, character = TRUE)


### 2.3.6 Exercises ##########

# 1

tracemem(1:10)


# 2 
x <- c(1L, 2L, 3L)
tracemem(x)
str(x)

x[[3]] <- 4
str(x)


# 3
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)
ref(a, b, c)



# 4
x <- list(1:10)
x
obj_addr(x)
ref(x)
tracemem(x)

x[[2]] <- x
obj_addr(x)
ref(x)



## 2.4 Object size ==============

obj_size(letters)

obj_size(ggplot2::diamonds)

x <- runif(1e6)
obj_size(x)

y <- list(x, x, x)
obj_size(y)


obj_size(list(NULL, NULL, NULL))


banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana, 100))


obj_size(x, y)

# ALTREP(alternative representation) feature 
obj_size(1:3)
obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)



### 2.4.1 Exercises ############

# 1
y <- rep(list(runif(1e4)), 100)
object.size(y)
obj_size(y)

?object.size


# 2
funs <- list(mean, sd, var)
obj_size(funs)
? obj_size


# 3
a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)


## 2.5 Modify-in-place ===========
### 2.5.1 Objects with a single binding ################


 # run in terminal
v <- c(1, 2, 3)
obj_addr(v)
v[[3]] <- 4
obj_addr(v)


x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}



cat(tracemem(x), "\n")

for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}
untracemem(x)


y <- as.list(x)
cat(tracemem(y), "\n")

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}


### 2.5.2 Environments #########

e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1


e1$c <- 4
e2$c



e <- rlang::env()
e$self <- e

ref(e)


### 2.5.3 Exercises ##############

# 1
x <- list()
obj_addr(x)
tracemem(x)

x[[1]] <- x
obj_addr(x)
obj_addr(x[[1]])


# 2
create_random_df <- function(nrow, ncol) {
  random_matrix <- matrix(runif(nrow * ncol), nrow = nrow)
  as.data.frame(random_matrix)
}

create_random_df(2, 2)




subtract_df <- function(x, medians){
  for (i in seq_along(medians)) {
    x[[i]] <- x[[i]] - medians[[i]]
  }
  x
}

subtract_list <- function(x, medians){
  x <- as.list(x)
  x <- subtract_df(x, medians)
  as.data.frame(x)
}


benchmark_medians <- function(ncol){
  df <- create_random_df(nrow = 1e4, ncol = ncol)
  medians <- vapply(df, median, numeric(1), USE.NAMES = FALSE)
  
  bench::mark(df   = subtract_df(df, medians),
              list = subtract_list(df, medians))
}

benchmark_medians(1)


results <- bench::press(
  ncol = c(1, 10, 50, 100, 250, 300, 400, 500, 750, 1000),
  benchmark_medians(ncol)
)


library(ggplot2)
ggplot(results, aes(ncol, median,
                    col = as.character(expression))) +
  geom_point(size = 2) + 
  geom_smooth() +
  labs(x = "Number of Data Frame Columns",
       y = "Computation Time",
       color = "Operating Data Structure",
       title = "Benchmark: Median Subtraction",
       subtitle = "For a data frame with 10,000 rows")


# 3

e <- rlang::env()
tracemem(e)
?tracemem



## 2.6 Unbinding and the garbage collector ===============

x <- 1:3
x <- 2:4
rm(x)


gcinfo(TRUE)
gc()
mem_used()
