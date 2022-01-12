# 15 S4 ---------
library(methods)

## 15.2 Basics ==============
setClass("Person", 
 slots = c(
   name = "character", 
   age = "numeric"
 )
)


john <- new("Person", name = "John Smith", age = NA_real_)

is(john)
john@name
slot(john, "age")



setGeneric("age", function(x) standardGeneric("age"))
setGeneric("age<-", function(x, value) standardGeneric("age<-"))


setMethod("age", "Person", function(x) x@age)
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  x
})

age(john) <- 50
age(john)


?age(john)
class$Person



sloop::otype(john)

sloop::ftype(age)

### 15.2.1 Exercises ###########

# 1
per <- lubridate::period(1:5, c("second", "minute", "hour", "day", "week"))
per@day
per@year

is(per)

str(per)


# 2

?"?"
?method()
?genericName
methods?genericName
ClassName?methodName


## 15.3 Classes ==========
setClass("Person", 
  slots = c(
    name = "character", 
    age = "numeric"
  ), 
  prototype = list(
    name = NA_character_,
    age = NA_real_
  )
)

me <- new("Person", name = "Hadley")
str(me)


### 15.3.1 Inheritance #########
setClass("Employee", 
  contains = "Person", 
  slots = c(
    boss = "Person"
  ),
  prototype = list(
    boss = new("Person")
  )
)

str(new("Employee"))


### 15.3.2 Introspection #####
is(new("Person"))
is(new("Employee"))

is(john, "Person")
is(new("Employee"), "Person")


### 15.3.3 Redefinition ########
setClass("A", slots = c(x = "numeric"))
a <- new("A", x = 10)

setClass("A", slots = c(a_different_slot = "numeric"))
a

### 15.3.4 Helper ##########

Person <- function(name, age = NA) {
  age <- as.double(age)
  
  new("Person", name = name, age = age)
}

Person("Hadley")

### 15.3.5 Validator ######
Person(mtcars)

Person("Hadley", age = c(30, 37))

setValidity("Person", function(object) {
  if (length(object@name) != length(object@age)) {
    "@name and @age must be same length"
  } else {
    TRUE
  }
})


Person("Hadley", age = c(30, 37))

alex <- Person("Alex", age = 30)
alex@age <- 1:10
validObject(alex)


### 15.3.6 Exercises ######

# 1
?utils::person()

setClass("NewPerson",
  contains = "Person",
  slots = c(
    email = "character",
    family = "character",
    role = "character"
  ),
  prototype = c(
    email = NA_character_,
    family = NA_character_,
    role = NA_character_
  )
)


# 2
?setClass

## 15.4 Generics and methods =========

setGeneric("myGeneric", function(x) standardGeneric("myGeneric"))

# Don't do this!
setGeneric("myGeneric", function(x) {
  standardGeneric("myGeneric")
})


### 15.4.1 Signature ########

setGeneric("myGeneric", 
  function(x, ..., verbose = TRUE) standardGeneric("myGeneric"),
  signature = "x"
)

### 15.4.2 Methods #########
setMethod("myGeneric", "Person", function(x) {
  # method implementation
})


methods("generic")
methods(class = "class")
selectMethod("generic", "class")


### 15.4.3 Show method #######

args(getGeneric("show"))
?show


setMethod("show", "Person", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})
john

### 15.4.4 Accessors ######
person_name <- function(x) x@name



setGeneric("name", function(x) standardGeneric("name"))
setMethod("name", "Person", function(x) x@name)
name(john)



setGeneric("name<-", function(x, value) standardGeneric("name<-"))
setMethod("name<-", "Person", function(x, value) {
  x@name <- value
  validObject(x)
  x
})


name(john) <- "Jon Smythe"
name(john)


name(john) <- letters

### 15.4.5 Exercises ############

# 1

age <- function(x) x@age
age(john)

rm(age)

setGeneric("age", function(x) standardGeneric("age"))
setMethod("age", "Person", definition = function(x) x@age)

age(john)


# 2
setMethod("show", "Person", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})

new("Employee", name = "arash")



# 4
formals(show)
setGeneric("age", function(x) standardGeneric("age"))
setMethod("age", "Person", definition = function(y) y@age)

age(john)

setGeneric("age", function(x, y) standardGeneric("age"))
setMethod("age", "Person", definition = function(y) y@age)

setGeneric("age", function(y, x) standardGeneric("age"))
setMethod("age", "Person", definition = function(y) y@age)


## 15.5 Method dispatch =======
### 15.5.1 Single dispatch #########
### 15.5.2 Multiple inheritance #######
?Methods_Details

### 15.5.3 Multiple dispatch ######
### 15.5.4 Multiple dispatch and multiple inheritance #######


            ## If you have to draw diagrams to figure out what method is actually going to be called,
            ## it’s a strong indication that you should go back and simplify your design.


### 15.5.5 Exercises #######
## 15.6 S4 and S3 =========
### 15.6.1 Classes #######
setOldClass("data.frame")
setOldClass(c("ordered", "factor"))
setOldClass(c("glm", "lm"))

sloop::otype(new("data.frame"))



setClass("factor",
  contains = "integer",
  slots = c(
    levels = "character"
  ),
  prototype = structure(
    integer(),
    levels = character()
  )
)
setOldClass("factor", S4Class = "factor")


RangedNumeric <- setClass(
  "RangedNumeric",
  contains = "numeric",
  slots = c(min = "numeric", max = "numeric"),
  prototype = structure(numeric(), min = NA_real_, max = NA_real_)
)

rn <- RangedNumeric(1:10, min = 1, max = 10)
rn@min

rn@.Data


?Methods_for_S3

### 15.6.2 Generics #######

setGeneric("mean")
selectMethod("mean", "ANY")

### 15.6.3 Exercises #######

# 1
setOldClass("ordered")

x <- new("ordered", ordered(1:3))
str(x)

##  ++

OrderedFactor <- setClass(
  "OrderedFactor",
  contains = "factor",  # inherit from registered S3 class
  slots = c(
    levels = "character",
    ordered = "logical"  # add logical order slot
  ),
  prototype = structure(
    integer(),           # .Data = is OK
    levels = character(),
    ordered = logical()  # add default value
  )
)
setOldClass("ordered", S4Class = "OrderedFactor")

x <- new("OrderedFactor", .Data = c(1L, 2L, 2L), levels = c("a", "b", "c"), ordered = TRUE)
x
str(x)

# 2
setMethod("length", "Person", function(x){
  length(x@name)
})

Arash <- new("Person", name = c("Arash", "no body", "anyone"))
length(Arash)






