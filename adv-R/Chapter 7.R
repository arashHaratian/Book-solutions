# 7 Environments -------
library(rlang)

## 7.2 Environment basics =======

### 7.2 Environment basics #######

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)

new.env() # in base package

e1$d <- e1

e1


env_print(e1)

env_names(e1)

names(e1)
ls(e1, all.names = T)


### 7.2.2 Important environments ########

current_env()


global_env() 

identical(global_env(), current_env())

global_env() == current_env()


environment() ## current env




### 7.2.3 Parents ########

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)



env_parent(e2b)
env_parent(e2a)


e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)


env_parents(e2b)
env_parents(e2d)


env_parents(e2b, last = empty_env())


parent.env(e2d) ### base package


### 7.2.4 Super assignment, <<- ##########

x <- 0
f <- function() {
  x <<- 1
}
f()
x


### 7.2.5 Getting and setting ###########

e3 <- env(x = 1, y = 2) ## set and get like lists
e3$x
e3$z <- 3
e3[["z"]]


e3[[1]]
e3[c("x", "y")]


e3$xyz

env_get(e3, "xyz")
env_get(e3, "xyz", default = NA)


env_poke(e3, "a", 100)
e3$a




env_bind(e3, a = 10, b = 20)
env_names(e3)


env_has(e3, "a")


e3$a <- NULL
env_has(e3, "a")


env_unbind(e3, "a")
env_has(e3, "a")


get()
assign()
exists()
rm()

### 7.2.6 Advanced bindings #######

?env_bind_lazy()

env_bind_lazy(current_env(), b = {Sys.sleep(1); 1})
system.time(print(b))


system.time(print(b))


env_bind_active(current_env(), z1 = function(val) runif(1))
z1
z1

?delayedAssign()
?makeActiveBinding()


### 7.2.7 Exercises ######
# 1
  
  # there is no ordering
  # if we assign something to null, the object did not remove
  # have an ansestor
  # did not use copy-on-modify


# 2

e2 <- env()
env_bind(e2, loop = e2)
env_names(e2)
env_print(e2)
env_get(e2, "loop")


# 3
e3a <- env()
e3b <- env()

env_bind(e3a, loop = e3b)
env_bind(e3b, deloop = e3a)

env_print(e3a)
env_print(e3b)


# 5
my_env_poke <- function(env, name, value){
  name <- as.character(name)
  if(!env_has(env, name))
    env[[name]] <- value
    # env_bind(env, nam = value)
  # env_poke(env, name, value)
}


my_env_poke(e3a, "a", 10)
env_print(e3a)


# 6
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}
rebind("a", 10)
a <- 5
rebind("a", 10)
a


## 7.3 Recursing over environments =========

where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}


where("yyy")
x <- 5
where("x")
where("mean")



f2 <- function(..., env = caller_env()) {
  while (!identical(env, empty_env())) {
    if (success) {
      # success case
      return()
    }
    # inspect parent
    env <- env_parent(env)
  }
  
  # base case
}



### 7.3.1 Exercises ######


# 1
where1 <- function(name, env = caller_env(), results = list()) {
  if (identical(env, empty_env())) {
    # Base case
    results
  } else {
    # Recursive case
    if (env_has(env, name)) {
      results <- c(results, env)
    }
    where1(name, env_parent(env), results)
  }
}


e1a <- env(empty_env(), a = 1, b = 2)
e1b <- env(e1a, b = 10, c = 11)
e1c <- env(e1b, a = 12, d = 13)
where1("a", e1c)



# 2
fget <- function(name, env = caller_env(), inherits = T) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    obj <- env_get(env, name)
    if(is.function(obj)){
      # Success case
      return(obj)
    }
  }
  if(inherits){
    # Recursive case
    fget(name, env_parent(env))
  }
  else {
    stop("Can't find ", name, call. = FALSE)
  }
}


mean <- 10
fget("mean", inherits = TRUE)

## 7.4 Special environments ====================
### 7.4.1 Package environments and the search path #######

search()
search_envs()



base_env()

### 7.4.2 The function environment ########
y <- 1
f <- function(x) x + y
fn_env(f)
# ==
environment(f)



e <- env()
e$g <- function() 1


### 7.4.3 Namespaces ######
sd

### 7.4.4 Execution environments ##########
g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g(10)
g(10)




g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}



h2 <- function(x) {
  a <- x * 2
  current_env()
}

e <- h2(x = 10)
env_print(e)
fn_env(h2)



plus <- function(x) {
  function(y) x + y
}

plus_one <- plus(1)
plus_one

plus_one(2)


### 7.4.5 Exercises #########

# 1
search_envs()
env_parents(global_env())


# 2
f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)


# 3
fget2 <- function(name, env = caller_env()) {
  # Base case
  if (env_has(env, name)) {
    obj <- env_get(env, name)
    
    if (is.function(obj)) {
      return(list(fun = obj, env = env))
    }
  }
  
  if (identical(env, emptyenv())) {
    stop("Could not find function called \"", name, "\"",
         call. = FALSE)
  }
  
  # Recursive Case
  fget2(name, env_parent(env))
}



fstr <- function(fun_name, env = caller_env()) {
  result <- fget2(fun_name, env)
  list(
  result$env,
  fn_env(result$fun)
  )
}

fstr("mean")


## 7.5 Call stacks =========
parent.frame()
caller_env()


### 7.5.1 Simple call stacks ##########
f <- function(x) {
  g(x = 2)
}
g <- function(x) {
  h(x = 3)
}
h <- function(x) {
  stop()
}


f(x = 1)
traceback()
# ==
h <- function(x) {
  lobstr::cst()
}
f(x = 1)



### 7.5.2 Lazy evaluation #########
a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x

a(f())


### 7.5.3 Frames ###########

?eval()
eval(a <- 1)

o

### 7.5.5 Exercises ############
# 1
ls2 <- function(env = caller_env()){
  env_names(env, )
}


ls()
ls2()

e1 <- env(a = 1, b = 2)
invoke(ls, .env = e1)
invoke(ls2, .env = e1)


## 7.6 As data structures ==========


my_env <- new.env(parent = emptyenv())
my_env$a <- 1

get_a <- function() {
  my_env$a
}
set_a <- function(value) {
  old <- my_env$a
  my_env$a <- value
  invisible(old)
}
  #USEFUL IN CONJUNCTION WITH "on.exit() 















