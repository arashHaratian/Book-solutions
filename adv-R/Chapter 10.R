# 10 Function factories ------------
library(rlang)
library(ggplot2)
library(scales)


power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

square(3)
cube(3)




## 10.2 Factory fundamentals ===========
### 10.2.1 Environments #######
square
cube



env_print(square)
env_print(cube)


fn_env(square)$exp
fn_env(cube)$exp

### 10.2.3 Forcing evaluation #####
x <- 2
square <- power1(x)
x <- 3


square(2)


power2 <- function(exp) {
  force(exp)
  function(x) {
    x ^ exp
  }
}

x <- 2
square <- power2(x)
x <- 3
square(2)


### 10.2.4 Stateful functions ########
new_counter <- function() {
  i <- 0
  
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_one()
counter_two()

### 10.2.5 Garbage collection ######
f1 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  function() m
}

g1 <- f1(1e6)
lobstr::obj_size(g1)

f2 <- function(n) {
  x <- runif(n)
  m <- mean(x)
  rm(x)
  function() m
}

g2 <- f2(1e6)
lobstr::obj_size(g2)

### 10.2.6 Exercises #####
# 1
force
?force


# 2
?approxfun()
?ecdf()

ap <- approxfun(c(1,2))
ec <- ecdf(2)

# 3
pick <- function(i){
  function(x) x[i]
}

pick(1)(x)
# should be equivalent to
x[[1]]

lapply(mtcars, pick(5))
# should be equivalent to
lapply(mtcars, function(x) x[[5]])

# 4
moment <- function(n){
  force(n)
  function(X){
    mean((X-mean(X, na.rm = T))^n, na.rm = T)
  }
}

m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

# 5

i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}
new_counter2()
new_counter2()
new_counter2()

# 6
new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}

counter3 <- new_counter3()
counter3()
counter3()
counter3()


## 10.3 Graphical factories ==========

### 10.3.1 Labelling #######
y <- c(12345, 123456, 1234567)
comma_format()(y)

number_format(scale = 1e-3, suffix = " K")(y)

df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)


### 10.3.2 Histogram bins #######

# construct some sample data with very different numbers in each cell
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = 2) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)



base_bins <- function(type) {
  fun <- switch(type,
                Sturges = nclass.Sturges,
                scott = nclass.scott,
                FD = nclass.FD,
                stop("Unknown type", call. = FALSE)
  )
  
  function(x) {
    (max(x) - min(x)) / fun(x)
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = base_bins("FD")) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)


### 10.3.3 ggsave() #######
plot_dev <- function(ext, dpi = 96) {
  force(dpi)
  
  switch(ext,
         eps =  ,
         ps  =  function(path, ...) {
           grDevices::postscript(
             file = filename, ..., onefile = FALSE, 
             horizontal = FALSE, paper = "special"
           )
         },
         pdf = function(filename, ...) grDevices::pdf(file = filename, ...),
         svg = function(filename, ...) svglite::svglite(file = filename, ...),
         emf = ,
         wmf = function(...) grDevices::win.metafile(...),
         png = function(...) grDevices::png(..., res = dpi, units = "in"),
         jpg = ,
         jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
         bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
         tiff = function(...) grDevices::tiff(..., res = dpi, units = "in"),
         stop("Unknown graphics extension: ", ext, call. = FALSE)
  )
}

plot_dev("pdf")

plot_dev("png")


## 10.4 Statistical factories ==========

### 10.4.1 Box-Cox transformation #####
boxcox1 <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1) / lambda
  }
}




boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <- function(lambda) {
  stat_function(aes(colour = lambda), fun = boxcox2(lambda), size = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# visually, log() does seem to make sense as the transformation
# for lambda = 0; as values get smaller and smaller, the function
# gets close and closer to a log transformation
ggplot(data.frame(x = c(0.01, 1)), aes(x)) + 
  lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))


### 10.4.2 Bootstrap generators ####
boot_permute <- function(df, var) {
  n <- nrow(df)
  force(var)
  
  function() {
    col <- df[[var]]
    col[sample(n, replace = TRUE)]
  }
}

boot_mtcars1 <- boot_permute(mtcars, "mpg")
head(boot_mtcars1())
head(boot_mtcars1())




boot_model <- function(df, formula) {
  mod <- lm(formula, data = df)
  fitted <- unname(fitted(mod))
  resid <- unname(resid(mod))
  rm(mod)
  
  function() {
    fitted + sample(resid)
  }
} 

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())
head(boot_mtcars2())


### 10.4.3 Maximum likelihood estimation ######

lprob_poisson <- function(lambda, x) {
  n <- length(x)
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}

x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

lprob_poisson(10, x1)
lprob_poisson(20, x1)
lprob_poisson(30, x1)


ll_poisson1 <- function(x) {
  n <- length(x)
  
  function(lambda) {
    log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
  }
}


ll_poisson2 <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  c <- sum(lfactorial(x))
  
  function(lambda) {
    log(lambda) * sum_x - n * lambda - c
  }
}


ll1 <- ll_poisson2(x1)

ll1(10)
ll1(20)
ll1(30)

optimise(ll1, c(0, 100), maximum = TRUE)

optimise(lprob_poisson, c(0, 100), x = x1, maximum = TRUE)


### 10.4.4 Exercises #####

# 2

boxcox3 <- function(x) {
  function(lambda) {
    if (lambda == 0) {
      log(x)
    } else {
      (x ^ lambda - 1) / lambda
    }
  }  
}

### 10.5 Function factories + functionals #####

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

funs <- purrr::map(names, power1)

funs$root(64)
funs$root





with(funs, root(100))


attach(funs)
root(100)
detach(funs)


rlang::env_bind(globalenv(), !!!funs)
root(100)

rlang::env_unbind(globalenv(), names(funs))

### 10.5.1 Exercises #####

# 2

funs <- list(
  mean = function(x) mean(x, na.rm = TRUE),
  sum = function(x) sum(x, na.rm = TRUE)
)

attach(funs)

mean <- function(x) stop("Hi!")
detach(funs)

identical(funs$mean, mean)


env_bind(globalenv(), !!!funs)
mean <- function(x) stop("Hi!") 

identical(funs$mean, mean)

env_unbind(globalenv(), names(funs))

