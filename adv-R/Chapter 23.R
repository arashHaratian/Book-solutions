# 23 Measuring performance -----
library(profvis)
library(bench)

## 23.2 Profiling ===========


f <- function() {
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}
h <- function() {
  pause(0.1)
}

f()


tmp <- tempfile()
Rprof(tmp, interval = 0.1)
f()
Rprof(NULL)
writeLines(readLines(tmp))

### 23.2.1 Visualising profiles #######

?utils::summaryRprof()


source("profiling-example.R")
profvis(f())

### 23.2.2 Memory profiling ######


x <- integer()
for (i in 1:1e4) {
  x <- c(x, i)
}


profvis({
  x <- integer()
  for (i in 1:1e4) {
    x <- c(x, i)
  }
})

### 23.2.3 Limitations #######
profvis({
  i <- function() {
    pause(0.1)
    10
  }
  j <- function(x) {
    x + 10
  }
  j(i())
})


### 23.2.4 Exercises #####

# 1
f <- function(n = 1e5) {
  x <- rep(1, n)
  rm(x)
}
profvis(f(n = 2), torture = T)
rm
?profvis
## 23.3 Microbenchmarking =====

x <- runif(100)
(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5
, check = T))


### 23.3.1 bench::mark() results #####
plot(lb)


lb[c("expression", "min", "median", "itr/sec", "n_gc")]


### 23.3.3 Exercises ######

# 1
n <- 1e6
system.time(for (i in 1:n) sqrt(x)) / n
system.time(for (i in 1:n) x ^ 0.5) / n

lb <- bench::mark(
  sqrt(x), 
  x ^ 0.5,
  iterations = n
)

?proc.time


# 2
mark(sqrt(x), 
     x ^ 0.5,
     x ^ (1 / 2),
     exp(log(x) / 2)
)
