# 18 Expressions -------
library(rlang)
library(lobstr)


y <- x * 10

z <- rlang::expr(y <- x * 10)
z

x <- 4
eval(z)
y


## 18.2 Abstract syntax trees ======
### 18.2.1 Drawing #######

lobstr::ast(f(x, "y", 1))


lobstr::ast(f(g(1, 2), h(3, 4, i())))


### 18.2.2 Non-code components #######
ast(
  f(x,  y)  # important!
)

lobstr::ast(y <- x)

lobstr::ast(y < -x)

### 18.2.3 Infix calls #######

y <- x * 10
`<-`(y, `*`(x, 10))


lobstr::ast(y <- x * 10)


expr(`<-`(y, `*`(x, 10)))

### 8.2.4 Exercises ########

# 1
ast(f(g(h())))

ast(1+2+3)


ast((x+y)*z)


# 2
ast(f(g(h(i(1, 2, 3)))))
ast(f(1, g(2, h(3, i()))))
ast(f(g(1, 2), h(3, i(4, 5))))


# 3
lobstr::ast(`x` + `y`)
lobstr::ast(x ** y)
lobstr::ast(1 -> x)

?"^"


# 4
lobstr::ast(function(x = 1, y = 2) {})

f02 <- function(x, y) {
  # A comment
  x + y
}
attr(f02, "srcref")


# 5
ast(if(T){1})
ast(if(T){1} else {2})
ast(if(x<11) 1 else if (x>22) 2)
ast(if(x<11){1} else if (x>22){2})

## 18.3 Expressions =========
### 18.3.1 Constants #######

identical(expr(TRUE), TRUE)
identical(expr(1), 1)
identical(expr(2L), 2L)
identical(expr("x"), "x")


### 18.3.2 Symbols #########
expr(x)
sym("x")


as_string(expr(x))
as_string(expr(x <- y))
as.character(expr(x <- y))



str(expr(x))
is.symbol(expr(x))


syms(x = list("x", "Y"))
list(sym("x"), sym("y"))


### 18.3.3 Calls ########


lobstr::ast(read.table("important.csv", row.names = FALSE))
x <- expr(read.table("important.csv", row.names = FALSE))

typeof(x)
is.call(x)


#### 18.3.3.1 Subsetting ########
x[[1]]
is.symbol(x[[1]])

as.list(x[-1])

x[[2]]
x$row.names
length(x) - 1


rlang::call_standardise(x)


x$header <- TRUE
x


#### 18.3.3.2 Function position #########
lobstr::ast(foo())
lobstr::ast("foo"())


lobstr::ast(pkg::foo(1))
lobstr::ast(obj$foo(1))
lobstr::ast(foo(1)(2))


#### 18.3.3.3 Constructing ######
call2("mean", x = expr(x), na.rm = TRUE)
call2(expr(base::mean), x = expr(x), na.rm = TRUE)


call2("<-", expr(x), 10)

### 18.3.5 Exercises ########
# 1
ast(c(1,2,3))

# 2
expr(read.csv("foo.csv", header = TRUE))[-1]

t <- expr(read.csv("foo.csv", header = TRUE))
t[-1]


# 3
x <- 1:10

call2(median, x, na.rm = TRUE)
call2(expr(median), x, na.rm = TRUE)
call2(median, expr(x), na.rm = TRUE)
call2(expr(median), expr(x), na.rm = TRUE)




# 4
call_standardise(quote(mean(1:10, na.rm = TRUE)))
call_standardise(quote(mean(n = T, 1:10)))
call_standardise(quote(mean(x = 1:10, , TRUE)))

?mean


call_standardise(quote(mean.default(1:10, na.rm = TRUE)))
call_standardise(quote(mean.default(n = T, 1:10)))
call_standardise(quote(mean.default(x = 1:10, , TRUE)))



# 5
x <- expr(foo(x = 1))
x
names(x) <- c("x", "y")
x


# 6
if(x > 1) "a" else "b"

call2("if", call2(">", sym("x"), 1), "a", "b")

## 18.4 Parsing and grammar =========
### 18.4.1 Operator precedence ########
lobstr::ast(1 + 2 * 3)

lobstr::ast(!x %in% y)

?Syntax


lobstr::ast((1 + 2) * 3)


### 18.4.2 Associativity #########

# all left assuciative 
lobstr::ast(1 + 2 + 3)

# exception
lobstr::ast(2^2^3)
lobstr::ast(x <- y <- z)


### 18.4.3 Parsing and deparsing #######
x1 <- "y <- x + 10"
x1
is.call(x1)

x2 <- rlang::parse_expr(x1)
x2
is.call(x2)


x3 <- "a <- 1; a + 1"
rlang::parse_exprs(x3)


as.list(parse(text = x1))


z <- expr(y <- x + 10)
expr_text(z)


cat(expr_text(expr({
  # This is a comment
  x <-             `x` + 1
})))


### 18.4.4 Exercises #########

# 1
ast(f((1)))
ast(`(`(1 + 1))


# 2
ast(expr(b = c(c = 1)))



# 3
ast(-2 ^ 2)
?Syntax


# 4
!1 + !1
ast(!1 + !1)



# 5
ast(x1 <- x2 <- x3 <- 0)
(x3 <- 0)


# 6
?Syntax


ast(x + y + z)
ast(x + y %+% z)
ast(x %+% y + z)
ast(x ^ y %+% z)
ast(x %+% y ^ z)




# 7
parse_expr("x + 1; y + 1")


# 8
parse_expr("a +")
parse_expr("f())")

parse("a +")
parse("f())")


# 9
expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + 
                 m + n + o + p + q + r + s + t + u + v + w + x + y + z))

deparse(expr)

expr_text(expr)


# 10
?pairwise.t.test()

## 18.5 Walking AST with recursive functions =======

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}


expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))



switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}



recurse_call <- function(x) {
  switch_expr(x,
    # Base cases
    symbol = ,
    constant = ,
    
    # Recursive cases
    call = ,
    pairlist =
  )
}


### 18.5.1 Finding F and T #########
expr_type(expr(TRUE))
expr_type(expr(T))



logical_abbr_rec <- function(x) {
  switch_expr(
    x,
    constant = FALSE,
    symbol = as_string(x) %in% c("F", "T")
  )
}

logical_abbr_rec(expr(TRUE))
logical_abbr_rec(expr(T))




logical_abbr <- function(x) {
  logical_abbr_rec(enexpr(x))
}

logical_abbr(T)
logical_abbr(FALSE)




logical_abbr_rec <- function(x) {
  switch_expr(
    x,
    # Base cases
    constant = FALSE,
    symbol = as_string(x) %in% c("F", "T"),
    
    # Recursive cases
    call = ,
    pairlist = purrr::some(x, logical_abbr_rec)
  )
}

logical_abbr(mean(x, na.rm = T))
logical_abbr(function(x, na.rm = T) FALSE)


### 18.5.2 Finding all variables created by assignment #########

ast(x <- 10)


find_assign_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol = character()
  )
}
find_assign <- function(x) find_assign_rec(enexpr(x))

find_assign("x")
find_assign(x)

flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

flat_map_chr(letters[1:3], ~ rep(., sample(3, 1)))





find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(as.list(x), find_assign_rec),
              call = {
                if (is_call(x, "<-")) {
                  as_string(x[[2]])
                } else {
                  flat_map_chr(as.list(x), find_assign_rec)
                }
              }
  )
}

find_assign(a <- 1)
find_assign({
  a <- 1
  {
    b <- 2
  }
})




find_assign({
  a <- 1
  a <- 2
})

find_assign <- function(x) unique(find_assign_rec(enexpr(x)))

find_assign({
  a <- 1
  a <- 2
})




find_assign({
  a <- b <- c <- 1
})




find_assign_call <- function(x) {
  if (is_call(x, "<-") && is_symbol(x[[2]])) {
    lhs <- as_string(x[[2]])
    children <- as.list(x)[-1]
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}

find_assign_rec <- function(x) {
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(x, find_assign_rec),
              call = find_assign_call(x)
  )
}

find_assign(a <- b <- c <- 1)
find_assign(system.time(x <- print(y <- 5)))


## 18.6 Specialised data structures =========


### 18.6.1 Pairlists #########
f <- expr(function(x, y = 10) x + y)

args <- f[[2]]
args
typeof(args)



pl <- pairlist(x = 1, y = 2)
length(pl)
pl$x


### 18.6.2 Missing arguments #######
expr()
missing_arg()
typeof(missing_arg())


is_missing(missing_arg())


f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
is_missing(args[[1]])

f <- expr(function(...) list(...))
args <- f[[2]]
is_missing(args[[1]])


m <- missing_arg()
m


ms <- list(missing_arg(), missing_arg())
ms[[1]]


?rlang::maybe_missing()




### 18.6.3 Expression vectors #######

exp1 <- parse(text = c("
x <- 4
x
"))
exp2 <- expression(x <- 4, x)

typeof(exp1)
typeof(exp2)

exp1
exp2


length(exp1)
exp1[[1]]

eval(exp1)
