# 5 Control flow ----------

## 5.2 Choices ==========
x1 <- if (TRUE) 1 else 2
x2 <- if (FALSE) 1 else 2

c(x1, x2)

greet <- function(name, birthday = FALSE) {
  paste0(
    "Hi ", name,
    if (birthday) " and HAPPY BIRTHDAY"
  )
}
greet("Maria", FALSE)
greet("Jaime", TRUE)

### 5.2.1 Invalid inputs ########

if ("x") 1
if (logical()) 1
if (NA) 1
if (c(TRUE, FALSE)) 1

### 5.2.2 Vectorised if #########

x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))

ifelse(x %% 2 == 0, "even", "odd")


x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))

ifelse(x %% 2 == 0, "even", "odd")


dplyr::case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "???",
  TRUE ~ as.character(x)
)


### 5.2.3 switch() statement #######

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value")
  )
}



(switch("c", a = 1, b = 2))

legs <- function(x) {
  switch(x,
         cow = ,
         horse = ,
         dog = 4,
         human = ,
         chicken = 2,
         plant = 0,
         stop("Unknown input")
  )
}
legs("cow")
legs("dog")


### 5.2.4 Exercises ########

# 1
ifelse(TRUE, 1, "no")
ifelse(FALSE, 1, "no")
ifelse(NA, 1, "no")


?ifelse


# 2
x <- 1:10
if (length(x)) "not empty" else "empty"

x <- numeric()
if (length(x)) "not empty" else "empty"



## 5.3 Loops ===============


for (i in 1:10) {
  if (i < 3) 
    next
  
  print(i)
  
  if (i >= 5)
    break
}


### 5.3.1 Common pitfalls ######

means <- c(1, 50, 20)
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}


xs <- as.Date(c("2020-01-01", "2010-01-01"))
for (x in xs) {
  print(x)
}
# --
for (i in seq_along(xs)) {
  print(xs[[i]])
}


# for >> while >> repeat





### 5.3.3 Exercises ######

# 1
x <- numeric()
out <- vector("list", length(x))
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2
}
out

#--
x <- numeric()
out <- vector("list", length(x))
for (i in seq_along(x)) {
  out[i] <- x[i] ^ 2
}
out


# 2
xs <- c(1, 2, 3)
for (x in xs) {
  xs <- c(xs, x * 2)
}
xs


# 3
for (i in 1:3) {
  i <- i * 2
  print(i) 
}
