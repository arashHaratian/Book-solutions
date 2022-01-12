# 25 Rewriting R code in C++ --------
library(Rcpp)

## 25.2 Getting started with C++ =========

cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
# add works like a regular R function
add
add(1, 2, 3)


### 25.2.1 No inputs, scalar output #########
one <- function() 1L

cppFunction('int one() {
  return 1;
}')


### 25.2.2 Scalar input, scalar output ########

signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')




### 25.2.3 Vector input, scalar output ########

sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}


cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')



x <- runif(1e3)
bench::mark(
  sum(x),
  sumC(x),
  sumR(x)
)[1:6]




### 25.2.4 Vector input, vector output ########

pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

## NumericVector zs = clone(ys)

cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);   

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')



y <- runif(1e6)
bench::mark(
  pdistR(0.5, y),
  pdistC(0.5, y)
)[1:6]


### 25.2.5 Using sourceCpp ########

#        #include <Rcpp.h>
#        using namespace Rcpp;

#        // [[Rcpp::export]]




#          /*** R
#         # This is R code
#          */


sourceCpp("cpp-example.cpp")


# RMarkdown: specify 'engine = "Rcpp"'

### 25.2.6 Exercises #######

# 2

cppFunction('bool allC(logicalVector x){
  for(int i; i < x.size(); ++i){
    if(x[i] == false) return false ;
  }
  return true;
}')


cppFunction('numericVector cumprodC(numericVector x){
  n = x.size();
  numericVector out(n);
  out [0] = x [0];
  for(int i = 1; i < n; ++i){
    out[i] = out[i-1] * x[i];
  }
  return out;
}')

cppFunction('numericVector cumminC(numericVector x){
  n = x.size();
  numericVector out(n);
  out [0] = x [0];
  for(int i = 1; i < n; ++i){
    out[i] = min(out[i-1],  x[i]);
  }
  return out;
}')



cppFunction('numericVector cummaxC(numericVector x){
  n = x.size();
  numericVector out(n);
  out [0] = x [0];
  for(int i = 1; i < n; ++i){
    out[i] = max(out[i-1], x[i]);
  }
  return out;
}')


cppFunction('numericVector diffC(numericVector x){
  n = x.size();
  numericVector out(n-1);
  
  for(int i = 1; i < n; ++i){
    out[i - 1] = x[i] - x[i-1];
  }
  return out;
}')



cppFunction('numericVector rangeC(numericVector x){
  n = x.size();
  numericVector out(2);
  out[0] = out[1] = 0;
  for(int i = ; i < n; ++i){
    out[0] = min(out[0], x[i]);
    out[1] = max(out[1], x[i]);
  }
  return out;
}')



cppFunction('double varC(numericVector x){
  n = x.size();
  double out = 0;
  double sum = sumSq = 0;
  
  for(int i = 0; i < n; ++i){
    sum += x[i];
    sumSq += (x[i] * x[i]);
  }
  out = (sumSq - (sum * sum)/n)/(n-1);
  return out;
}')

## 25.3 Other classes =========

### 25.3.1 Lists and data frames #########

sourceCpp("cpp/Lists_and_data_frames.cpp")

mod <- lm(mpg ~ wt, data = mtcars)
mpe(mod)

### 25.3.2 Functions #######

sourceCpp("cpp/functions.cpp")


callWithOne(function(x) x + 1, 100)
callWithOne(paste)

### 25.3.3 Attributes ##########
sourceCpp("cpp/Attributes.cpp")

attribs()


# For S4 objects, .slot() plays a similar role to .attr(). 


## 25.4 Missing values ========

### 25.4.1 Scalars ##########
sourceCpp('cpp/NA_Scalars.cpp')
str(scalar_missings())


#### 25.4.1.1 Integers ######
evalCpp('NA_INTEGER + 1')



#### 25.4.1.2 Doubles #########
evalCpp("NAN == 1")
evalCpp("NAN < 1")
evalCpp("NAN > 1")
evalCpp("NAN == NAN")


evalCpp("NAN && TRUE")
evalCpp("NAN || FALSE")


evalCpp("NAN + 1")
evalCpp("NAN - 1")
evalCpp("NAN / 1")
evalCpp("NAN * 1")


### 25.4.4 Vectors ##########

sourceCpp('cpp/NA_Vectors.cpp')
str(missing_sampler())

## 25.5 Standard Template Library ===========

### 25.5.1 Using iterators ########
sourceCpp('cpp/Using_iterators.cpp')
sum3(1:10)


sourceCpp('cpp/Using_iterators.cpp')
sum4(1:10)


sourceCpp('cpp/Using_iterators.cpp')
sum5(1:10)


### 25.5.2 Algorithms ##########
sourceCpp('cpp/Algorithms.cpp')


### 25.5.4 Vectors ########
sourceCpp('cpp/Vectors.cpp')


### 25.5.5 Sets #######
sourceCpp('cpp/unordered_set.cpp')

### 25.5.6 Map #####
sourceCpp('cpp/Map.cpp')


### 25.5.7 Exercises #####

# 1
sourceCpp("cpp/exercise1.cpp")
# 2
sourceCpp("cpp/exercise2.cpp")

find(c("x", "y"), letters)

# 3
sourceCpp("cpp/exercise3.cpp")
uniqueC(c(1:3, 3:4))

# 5
sourceCpp("cpp/exercise5.cpp")

# 6
sourceCpp("cpp/exercise6.cpp")


x <- c(1, 2, 3, 3)
y <- c(3, 2, 5)


setdiffC(x, y)
intersectC(x, y)
unionC(x, y)


## 25.6 Case studies ==========
### 25.6.1 Gibbs sampler #########

gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}


sourceCpp('cpp/CaseStudy1.cpp')


bench::mark(
  gibbs_r(100, 10),
  gibbs_cpp(100, 10),
  check = FALSE
)


### 25.6.2 R vectorisation versus C++ vectorisation ########

vacc1a <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * if (female) 1.25 else 0.75
  p <- max(0, p)
  p <- min(1, p)
  p
}



vacc1 <- function(age, female, ily) {
  n <- length(age)
  out <- numeric(n)
  for (i in seq_len(n)) {
    out[i] <- vacc1a(age[i], female[i], ily[i])
  }
  out
}


vacc2 <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * ifelse(female, 1.25, 0.75)
  p <- pmax(0, p)
  p <- pmin(1, p)
  p
}


sourceCpp("cpp/CaseStudy2.cpp")



n <- 1000
age <- rnorm(n, mean = 50, sd = 10)
female <- sample(c(T, F), n, rep = TRUE)
ily <- sample(c(T, F), n, prob = c(0.8, 0.2), rep = TRUE)

stopifnot(
  all.equal(vacc1(age, female, ily), vacc2(age, female, ily)),
  all.equal(vacc1(age, female, ily), vacc3(age, female, ily))
)




bench::mark(
  vacc1 = vacc1(age, female, ily),
  vacc2 = vacc2(age, female, ily),
  vacc3 = vacc3(age, female, ily)
)

## 25.8 Learning more ==========

vignette("Rcpp-attributes")
vignette("Rcpp-modules")
vignette("Rcpp-quickref")

