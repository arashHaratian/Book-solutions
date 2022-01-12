# 24 Improving performance ------
library(bench)

## 24.2 Code organisation ========
mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)


x <- runif(1e5)


bench::mark(
  mean1(x),
  mean2(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]


## 24.4 Doing as little as possible ========

### 24.4.1 mean() ##########

x <- runif(1e2)

bench::mark(
  mean(x),
  mean.default(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]



x <- runif(1e2)
bench::mark(
  mean(x),
  mean.default(x),
  .Internal(mean(x))
)[c("expression", "min", "median", "itr/sec", "n_gc")]


x <- runif(1e4)
bench::mark(
  mean(x),
  mean.default(x),
  .Internal(mean(x))
)[c("expression", "min", "median", "itr/sec", "n_gc")]


### 24.4.2 as.data.frame() ########
quickdf <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

l <- lapply(1:26, function(i) runif(1e3))
names(l) <- letters

bench::mark(
  as.data.frame = as.data.frame(l),
  quick_df      = quickdf(l)
)[c("expression", "min", "median", "itr/sec", "n_gc")]



quickdf(list(x = 1, y = 1:2))


### 24.4.3 Exercises #######

# 1
rowSums
.rowSums

?rowSums
?.rowSums


m <- matrix(rnorm(1e6), nrow = 1000)

bench::mark(
  rowSums(m),
  .rowSums(m, 1000, 1000)
)


## 24.5 Vectorise ======
rowAny <- function(x) rowSums(x) > 0
rowAll <- function(x) rowSums(x) == ncol(x)


lookup <- setNames(as.list(sample(100, 26)), letters)

x1 <- "j"
x10 <- sample(letters, 10)
x100 <- sample(letters, 100, replace = TRUE)

bench::mark(
  lookup[x1],
  lookup[x10],
  lookup[x100],
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc")]


### 24.5.1 Exercises ########

# 1
?dnorm
?rnorm

rnorm(10, mean = 10:1)


# 2
rowsums <- bench::press(
  p = seq(500, 5000, length.out = 10),
  {
    mat <- tcrossprod(rnorm(p), rnorm(p))
    bench::mark(
      rowSums = rowSums(mat),
      apply = apply(mat, 1, sum)
    )
  }
)




# 3
x <- matrix(rnorm(1e6))
w <- matrix(rnorm(1e6))
mark(
  sum(x * w),
  crossprod(x, w)[[1]]
)

weightedsum <- bench::press(
  n = 1:10,
  {
    x <- matrix(rnorm(n * 1e6))
    w <- matrix(rnorm(n * 1e6))
    mark(
      sum(x * w),
      crossprod(x, w)[[1]]
    )
  }
)


weightedsum %>% 
  summary() %>% 
  dplyr::mutate(expression = as.character(expression)) %>% 
  ggplot(aes(n, median, color = expression, group = expression)) +
  geom_point() +
  geom_line() + 
  labs(x = "n (millions)") + 
  theme(legend.position = "bottom")



## 24.6 Avoiding copies ==========


random_string <- function() {
  paste(sample(letters, 50, replace = TRUE), collapse = "")
}
strings10 <- replicate(10, random_string())
strings100 <- replicate(100, random_string())

collapse <- function(xs) {
  out <- ""
  for (x in xs) {
    out <- paste0(out, x)
  }
  out
}

bench::mark(
  loop10  = collapse(strings10),
  loop100 = collapse(strings100),
  vec10   = paste(strings10, collapse = ""),
  vec100  = paste(strings100, collapse = ""),
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc")]



## 24.7 Case study: t-test ========
m <- 1000
n <- 50
X <- matrix(rnorm(m * n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n / 2)


system.time(
  for (i in 1:m) {
    t.test(X[i, ] ~ grp)$statistic
  }
)

system.time(
  for (i in 1:m) {
    t.test(X[i, grp == 1], X[i, grp == 2])$statistic
  }
)



compT <- function(i){
  t.test(X[i, grp == 1], X[i, grp == 2])$statistic
}
system.time(t1 <- purrr::map_dbl(1:m, compT))






my_t <- function(x, grp) {
  t_stat <- function(x) {
    m <- mean(x)
    n <- length(x)
    var <- sum((x - m) ^ 2) / (n - 1)
    
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(x[grp == 1])
  g2 <- t_stat(x[grp == 2])
  
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}

system.time(t2 <- purrr::map_dbl(1:m, ~ my_t(X[.,], grp)))
stopifnot(all.equal(t1, t2))






rowtstat <- function(X, grp){
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}
system.time(t3 <- rowtstat(X, grp))
stopifnot(all.equal(t1, t3))
