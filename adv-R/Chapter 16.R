# 16 Trade-offs -------------
## 16.3 R6 versus S3 ========
### 16.3.1 Namespacing #######

plot(data)       # plot some data
plot(bank_heist) # plot a crime
plot(land)       # create a new plot of land
plot(movie)      # extract plot of a movie



data$plot()
bank_heist$plot()
land$plot()
movie$plot()


###16.3.2 Threading state #######

new_stack <- function(items = list()) {
  structure(list(items = items), class = "stack")
}

push <- function(x, y) {
  x$items <- c(x$items, list(y))
  x
}

pop <- function(x) {
  n <- length(x$items)
  
  item <- x$items[[n]]
  x$items <- x$items[-n]
  
  list(item = item, x = x)
}


    ## threading state or accumulator programming
s <- new_stack()
s <- push(s, 10)
s <- push(s, 20)

out <- pop(s)
out$item
s <- out$x
s



    ## multiple assign
library(zeallot)

c(value, s) %<-% pop(s)
value


## vs.  R6
Stack <- R6::R6Class("Stack", list(
  items = list(),
  push = function(x) {
    self$items <- c(self$items, x)
    invisible(self)
  },
  pop = function() {
    item <- self$items[[self$length()]]
    self$items <- self$items[-self$length()]
    item
  },
  length = function() {
    length(self$items)
  }
))


s <- Stack$new()
s$push(10)
s$push(20)
s$pop()


### 16.3.3 Method chaining ########

s <- Stack$new()
s$
  push(10)$
  push(20)$
  pop()

