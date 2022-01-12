# f <- function() {
#   pause(0.1)
#   g()
#   h()
# }
# g <- function() {
#   pause(0.1)
#   h()
# }
# h <- function() {
#   pause(0.1)
# }
# 
# f()


f <- function(n = 1e5) {
  x <- rep(1, n)
  rm(x)
}