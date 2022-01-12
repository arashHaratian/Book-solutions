# 11 Function operators ----------
library(purrr)
library(memoise)


chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}
f <- function(x) x ^ 2
s <- c(3, 2, 1)

purrr::map_dbl(s, chatty(f))


## 11.2 Existing function operators ==========
### 11.2.1 Capturing errors with purrr::safely() ############

x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops",
  1:3
)

out <- rep(NA_real_, length(x))
for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}

out

map_dbl(x, sum)


safe_sum <- safely(sum)
safe_sum



str(safe_sum(x[[1]]))
str(safe_sum(x[[4]]))


out <- map(x, safely(sum))
str(out)


out <- transpose(map(x, safely(sum)))
str(out)


ok <- map_lgl(out$error, is.null)
ok

x[!ok]

out$result[ok]





fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

# which data failed to converge?
datasets[!ok]

# which models were successful?
models[ok]


?possibly
?quietly
?auto_browse

### 11.2.2 Caching computations with memoise::memoise() ######

slow_function <- function(x) {
  Sys.sleep(1)
  x * 10 * runif(1)
}
system.time(print(slow_function(1)))

system.time(print(slow_function(1)))




fast_function <- memoise::memoise(slow_function)
system.time(print(fast_function(1)))

system.time(print(fast_function(1)))



fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}
system.time(fib(23))
system.time(fib(24))


fib2 <- memoise::memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
system.time(fib2(24))


### 11.2.3 Exercises ###########

# 1
?Vectorize()
vrep <- Vectorize(rep.int)
vrep(1:4, 4:1)
vrep(times = 1:4, x = 4:1)



# 2
possibly

# 3
safely
purrr:::capture_error

## 11.3 Case study: Creating your own function operators =======

urls <- c(
  "adv-r" = "https://adv-r.hadley.nz", 
  "r4ds" = "http://r4ds.had.co.nz/"
  # and many many more
)
path <- paste(tempdir(), names(urls), ".html")

walk2(urls, path, download.file, quiet = TRUE)




for(i in seq_along(urls)) {
  Sys.sleep(0.1)
  if (i %% 10 == 0) cat(".")
  download.file(urls[[i]], path[[i]], quiet = T)
}



delay_by <- function(f, amount) {
  force(f)
  force(amount)
  
  function(...) {
    Sys.sleep(amount)
    f(...)
  }
}


system.time(runif(100))
system.time(delay_by(runif, 0.1)(100))


walk2(urls, path, delay_by(download.file, 0.1), quiet = TRUE)

dot_every <- function(f, n) {
  force(f)
  force(n)
  
  i <- 0
  function(...) {
    i <<- i + 1
    if (i %% n == 0) cat(".")
    f(...)
  }
}


walk(1:100, runif)
walk(1:100, dot_every(runif, 10))


walk2(
  urls, path, 
  dot_every(delay_by(download.file, 0.1), 10), 
  quiet = TRUE
)


walk2(
  urls, path, 
  download.file %>% dot_every(10) %>% delay_by(0.1), 
  quiet = TRUE
)


