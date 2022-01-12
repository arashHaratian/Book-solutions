# 14 R6 --------
library(R6)

## 14.2 Classes and methods ======


Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x 
    invisible(self)
  })
)

Accumulator
sloop::otype(Accumulator)

x <- Accumulator$new()

x$add(4) 
x$sum


### 14.2.1 Method chaining ######

x$add(10)$add(10)$sum

x$
  add(10)$
  add(10)$
  sum


### 14.2.2 Important methods #####

Person <- R6Class("Person", list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(age), length(age) == 1)
    
    self$name <- name
    self$age <- age
  }
))

hadley <- Person$new("Hadley", age = "thirty-eight")

hadley <- Person$new("Hadley", age = 38)


        # all methods must return self invisible ( because they are used for their side effect)


Person <- R6Class("Person", list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    self$name <- name
    self$age <- age
  },
  print = function(...) {
    cat("Person: \n")
    cat("  Name: ", self$name, "\n", sep = "")
    cat("  Age:  ", self$age, "\n", sep = "")
    invisible(self)
  }
))

hadley2 <- Person$new("Hadley")
hadley2


hadley

hadley$print


### 14.2.3 Adding methods after creation #######

Accumulator <- R6Class("Accumulator")
Accumulator$set("public", "sum", 0)
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x 
  invisible(self)
})


### 14.2.4 Inheritance #########
AccumulatorChatty <- R6Class("AccumulatorChatty", 
  inherit = Accumulator,
  public = list(
    add = function(x = 1) {
      cat("Adding ", x, "\n", sep = "")
      super$add(x = x)
    }
  )
)

x2 <- AccumulatorChatty$new()
x2$add(10)$add(1)$sum


### 14.2.5 Introspection #########

class(hadley2)
names(hadley2)
sloop::s3_dispatch(print(hadley2))

### 14.2.6 Exercises #######

# 1
bankAccount <- R6Class("bankAccount",
  public = list(
    balance = 0,
    initialize = function(balance){
      stopifnot(!is.null(balance), is.numeric(balance))
      self$balance = balance
    },
    deposit = function(amount = 0){
      self$balance = self$balance + amount
      invisible(self)
    },
    withdraw = function(amount){
      if(self$balance - amount < 0)
        stop("you can not do this")
      
      self$balance = self$balance - amount
      invisible(self)
    },
    print = function(){
      cat(self$balance)
    }
  )
)

a <- bankAccount$new(balance = 200)
a
a$deposit(200)
a
a$withdraw(200)
a
a$withdraw(1000)


bankAccount2 <- R6Class("bankAccount2",
  inherit = bankAccount,
  public = list(
    withdraw = function(amount){
      if(self$balance - amount < 0)
        message("blah blah blah")
      
      self$balance = self$balance - amount
      invisible(self)
    }
  )
)


a2 <- bankAccount2$new(balance = 200)
a2
a2$deposit(200)
a2
a2$withdraw(200)
a2
a2$withdraw(1000)
a2


# 2
suit <- c("♠", "♥", "♦", "♣")
value <- c("A", 2:10, "J", "Q", "K")
cards <- paste0(rep(value, 4), suit)

ShuffledDeck <- R6Class(
  classname = "ShuffledDeck", 
  public = list(
    deck = NULL,
    initialize = function(deck = cards) {
      self$deck <- sample(deck)
    },
    reshuffle = function() {
      self$deck <- sample(cards)
      invisible(self)
    },
    draw = function(n = 1) {
      if (n > self$n()) {
        stop("Only ", self$n(), " cards remaining", call. = FALSE)
      }
      
      output <- self$deck[seq_len(n)]
      self$deck <- self$deck[-seq_len(n)]
      output
    }
  )
)

my_deck <- ShuffledDeck$new()
my_deck$draw(52)

my_deck$draw(10)

my_deck$reshuffle()$draw(5)
my_deck$reshuffle()$draw(5)



# 4
Timezone <- R6Class(
  classname = "Timezone", 
  public = list(
    get = function() {
      Sys.timezone()
    },
    set = function(value) {
      stopifnot(value %in% OlsonNames())
      
      old <- self$get()
      Sys.setenv(TZ = value)
      invisible(old)
    })
)


# 5
WorkingDirectory <- R6Class(
  classname = "WorkingDirectory", 
  public = list(
    get = function() {
      getwd()
    },
    set = function(value) {
      setwd(value)
    }
  )
)




# 6

# attributes(bankAccount)
# attributes(bankAccount2)
# class(bankAccount)
# typeof(attributes(bankAccount))
typeof(a)
typeof(a2)
attributes(a)
attributes(a2)


  
## 14.3 Controlling access ==========
### 14.3.1 Privacy ######
Person <- R6Class("Person", 
  public = list(
    initialize = function(name, age = NA) {
      private$name <- name
      private$age <- age
    },
    print = function(...) {
      cat("Person: \n")
      cat("  Name: ", private$name, "\n", sep = "")
      cat("  Age:  ", private$age, "\n", sep = "")
    }
  ),
  private = list(
    age = NA,
    name = NULL
  )
)

hadley3 <- Person$new("Hadley")
hadley3
hadley3$name

### 14.3.2 Active fields ######

Rando <- R6::R6Class("Rando", active = list(
  random = function(value) {
    if (missing(value)) {
      runif(1)  
    } else {
      stop("Can't set `$random`", call. = FALSE)
    }
  }
))
x <- Rando$new()
x$random
x$random
x$random



Person <- R6Class("Person", 
  private = list(
    .age = NA,
    .name = NULL
  ),
  active = list(
    age = function(value) {
      if (missing(value)) {
        private$.age
      } else {
        stop("`$age` is read only", call. = FALSE)
      }
    },
    name = function(value) {
      if (missing(value)) {
        private$.name
      } else {
        stopifnot(is.character(value), length(value) == 1)
        private$.name <- value
        self
      }
    }
  ),
  public = list(
    initialize = function(name, age = NA) {
      private$.name <- name
      private$.age <- age
    }
  )
)

hadley4 <- Person$new("Hadley", age = 38)
hadley4$name
hadley4$name <- 10
hadley4$age <- 20


### 14.3.3 Exercises ######
# 1
bankAccount3 <- R6Class("bankAccount3",
  private = list(balance = 0),
  public = list(
    initialize = function(balance){
      stopifnot(!is.null(balance), is.numeric(balance))
      private$balance = balance
    },
    deposit = function(amount = 0){
      private$balance = private$balance + amount
      invisible(self)
    },
    withdraw = function(amount){
      if(private$balance - amount < 0)
        stop("you can not do this")
      
      private$balance = private$balance - amount
      invisible(self)
    },
    print = function(){
      cat(private$balance)
    }
  )
)


a3 <- bankAccount3$new(200)
a3
a3$balance

a3$deposit(200)
a3
a3$withdraw(200)
a3
a3$withdraw(1000)


# 2
password <- R6Class("password",
  private  = list(
    .password = NULL
  ),
  public = list(
    initialize = function(x = character()) {
      private$.password <- as.character(x)
    },
    check_password = function(x) {
      private$.password == as.character(x)
    }
  ),
  active = list(
    password = function(x){
      if(missing(x))
        stop("write-only field")
      
      private$.password <- as.character(x)
      self
    }
  )
)

pass <- password$new("aaa")
pass$check_password("aaa")
pass$check_password("aaaa")
pass$password <- "aaaa"
pass$check_password("aaa")
pass$check_password("aaaa")


# 3


Rando <- R6::R6Class("Rando",
  active = list(
    random = function(value) {
      if (missing(value)) {
        private$previous_rand <- runif(1)
      } else {
        stop("Can't set `$random`", call. = FALSE)
      }
    },
    previous = function(value) {
      if (missing(value)) {
        private$previous_rand
      } else{
        warning("read-only field")
      }
    }
  ),
  private = list (
    previous_rand = NULL
  )
)
x <- Rando$new()
x$random
x$random
x$previous


# 4
A <- R6Class(
  classname = "A",
  private = list(
    field = "foo",
    method = function() {
      "bar"
    }
  )
)

B <- R6Class(
  classname = "B",
  inherit = A,
  public = list(
    test = function() {
      cat("Field:  ", super$field, "\n", sep = "")
      cat("Method: ", super$method(), "\n", sep = "")
    }
  )
)

B$new()$test()


## 14.4 Reference semantics ########

y1 <- Accumulator$new() 
y2 <- y1
c(y1 = y1$sum, y2 = y2$sum)

y1$add(10)
c(y1 = y1$sum, y2 = y2$sum)




y1 <- Accumulator$new() 
y2 <- y1$clone()
c(y1 = y1$sum, y2 = y2$sum)

y1$add(10)
c(y1 = y1$sum, y2 = y2$sum)

### 14.4.1 Reasoning ######

x <- list(a = 1)
y <- list(b = 2)

z <- f(x, y)


x <- List$new(a = 1)
y <- List$new(b = 2)

z <- f(x, y)

        # The final line is much harder to reason about: if f()
        # calls methods of x or y, it might modify them as well
        # as z. This is the biggest potential downside of R6 and
        # you should take care to avoid it by writing functions
        # that either return a value, or modify their R6 inputs,
        # but not both. That said, doing both can lead to substantially
        # simpler code in some cases,



### 14.4.2 Finalizer ########
TemporaryFile <- R6Class("TemporaryFile",
  list(
    path = NULL,
    initialize = function() {
      self$path <- tempfile()
    },
    finalize = function() {
      message("Cleaning up ", self$path)
      unlink(self$path)
    }
  )
)

tf <- TemporaryFile$new()
rm(tf)


### 14.4.3 R6 fields #######

TemporaryDatabase <- R6Class("TemporaryDatabase", list(
  con = NULL,
  file = TemporaryFile$new(),
  initialize = function() {
    self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
  },
  finalize = function() {
    DBI::dbDisconnect(self$con)
  }
))

db_a <- TemporaryDatabase$new()
db_b <- TemporaryDatabase$new()

db_a$file$path == db_b$file$path




TemporaryDatabase <- R6Class("TemporaryDatabase", list(
  con = NULL,
  file = NULL,
  initialize = function() {
    self$file <- TemporaryFile$new()
    self$con <- DBI::dbConnect(RSQLite::SQLite(), path = file$path)
  },
  finalize = function() {
    DBI::dbDisconnect(self$con)
  }
))

db_a <- TemporaryDatabase$new()
db_b <- TemporaryDatabase$new()

db_a$file$path == db_b$file$path



### 14.4.4 Exercises #######
FileWriter <- R6::R6Class(
  classname = "FileWriter",
  public = list(
    con = NULL,
    initialize = function(filename) {
      self$con <- file(filename, open = "a")
    },
    finalize = function() {
      close(self$con)
    },
    append_line = function(x) {
      cat(x, "\n", sep = "", file = self$con)
    }
  )
)

tmp_file <- tempfile()
my_fw <- FileWriter$new(tmp_file)

readLines(tmp_file)
my_fw$append_line("First")
my_fw$append_line("Second")
readLines(tmp_file)
rm(my_fw)


### 14.5 Why R6? #######
?setRefClass
# vignette("Performance","R6")
