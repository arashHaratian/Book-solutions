# 22 Debugging -------------
## 22.3 Locating errors =======
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}


f("a")
traceback()



### 22.3.1 Lazy evaluation #########

j <- function() k()
k <- function() stop("Oops!", call. = FALSE)
f(j())


rlang::with_abort(f(j()))
rlang::last_trace()

## 22.4 Interactive debugger ========
g <- function(b) {
  browser()
  h(b)
}
f(10)

g <- function(b) {
  if (b < 0) {
    browser()
  }
  h(b)
}


### 22.4.1 browser() commands ########

      # Next, n
      
      # Step into, s
      
      # Finish, f
      
      # Continue, c
      
      # Stop, Q
      
      # Enter: repeats the previous command , turn it off using options(browserNLdisabled = TRUE)
      
      # where: prints stack trace of active calls


### 22.4.2 Alternatives ###########
#### 22.4.2.2 recover() #########
options(error = recover)
f("x")
options(error = NULL)

#### 22.4.2.3 debug() ###########
  #debug() inserts a browser statement in the first line of the specified function.
  #undebug() removes it. Alternatively, you can use debugonce() to browse only on the next run.

  # utils::setBreakpoint() works similarly, but instead of taking a function name,
  # it takes a file name and line number and finds the appropriate function for you.


?trace
?untrace


## 22.5 Non-interactive debugging ==========
callr::r(f, list(1, 2))
# ==
f(1, 2)

### 22.5.1 dump.frames() #########


# In batch R process ----
dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit)

# In a later interactive session ----
load("last.dump.rda")
debugger()

### 22.5.2 Print debugging ########
f <- function(a) {
  cat("f()\n")
  g(a)
}
g <- function(b) {
  cat("g()\n")
  cat("b =", b, "\n")
  h(b)
}
h <- function(c) {
  cat("i()\n")
  i(c)
}

f(10)

### 22.5.3 RMarkdown ##########

rmarkdown::render("path/to/file.Rmd")


options(error = function() {
  sink()
  recover()
})


options(rlang_trace_top_env = rlang::current_env())
options(error = function() {
  sink()
  print(rlang::trace_back(bottom = sys.frame(-1)), simplify = "none")
})

## 22.6 Non-error failures =======

doWithOneRestart()
withOneRestart()



withRestarts()
.signalSimpleWarning()




f <- function() g()
g <- function() message("Hi!")
f()

rlang::with_abort(f(), "message")
rlang::last_trace()
