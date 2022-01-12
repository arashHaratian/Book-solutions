# 21 Translating R code ---------
library(rlang)
library(purrr)


library(dbplyr)

translate_sql(x ^ 2)
translate_sql(x < 5 & !is.na(x))
translate_sql(!first %in% c("John", "Roger", "Robert"))
translate_sql(select == 7)


## 21.2 HTML ===========

### 21.2.1 Goal #########
with_html(
  body(
    h1("A heading", id = "first"),
    p("Some text &", b("some bold text.")),
    img(src = "myimg.png", width = 100, height = 100)
  )
)


### 21.2.2 Escaping ########
html <- function(x) structure(x, class = "advr_html")

print.advr_html <- function(x, ...) {
  out <- paste0("<HTML> ", x)
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}





escape <- function(x) UseMethod("escape")

escape.character <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  
  html(x)
}

escape.advr_html <- function(x) x



escape("This is some text.")
escape("x > 1 & y < 2")

# Double escaping is not a problem
escape(escape("This is some text. 1 > 2"))

# And text we know is HTML doesn't get escaped.
escape(html("<hr />"))



### 21.2.3 Basic tag functions #######
p("Some text. ", b(i("some bold italic text")), class = "mypara")


dots_partition <- function(...) {
  dots <- list2(...)
  
  if (is.null(names(dots))) {
    is_named <- rep(FALSE, length(dots))
  } else {
    is_named <- names(dots) != ""
  }
  
  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}

str(dots_partition(a = 1, 2, b = 3, 4))





source("dsl-html-attributes.r")
p <- function(...) {
  dots <- dots_partition(...)
  attribs <- html_attributes(dots$named)
  children <- map_chr(dots$unnamed, escape)
  
  html(paste0(
    "<p", attribs, ">",
    paste(children, collapse = ""),
    "</p>"
  ))
}

p("Some text")
p("Some text", id = "myid")
p("Some text", class = "important", `data-value` = 10)



### 21.2.4 Tag functions ########
tag <- function(tag) {
  new_function(
    exprs(... = ),
    expr({
      dots <- dots_partition(...)
      attribs <- html_attributes(dots$named)
      children <- map_chr(dots$unnamed, escape)
      
      html(paste0(
        !!paste0("<", tag), attribs, ">",
        paste(children, collapse = ""),
        !!paste0("</", tag, ">")
      ))
    }),
    caller_env()
  )
}
tag("b")



p <- tag("p")
b <- tag("b")
i <- tag("i")
p("Some text. ", b(i("some bold italic text")), class = "mypara")




void_tag <- function(tag) {
  new_function(
    exprs(... = ),
    expr({
      dots <- dots_partition(...)
      if (length(dots$unnamed) > 0) {
        abort(!!paste0("<", tag, "> must not have unnamed arguments"))
      }
      attribs <- html_attributes(dots$named)
      
      html(paste0(!!paste0("<", tag), attribs, " />"))
    }),
    caller_env()
  )
}

img <- void_tag("img")
img
img(src = "myimage.png", width = 100, height = 100)


### 21.2.5 Processing all tags ########
tags <- c("a", "abbr", "address", "article", "aside", "audio",
          "b","bdi", "bdo", "blockquote", "body", "button", "canvas",
          "caption","cite", "code", "colgroup", "data", "datalist",
          "dd", "del","details", "dfn", "div", "dl", "dt", "em",
          "eventsource","fieldset", "figcaption", "figure", "footer",
          "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
          "hgroup", "html", "i","iframe", "ins", "kbd", "label",
          "legend", "li", "mark", "map","menu", "meter", "nav",
          "noscript", "object", "ol", "optgroup", "option", "output",
          "p", "pre", "progress", "q", "ruby", "rp","rt", "s", "samp",
          "script", "section", "select", "small", "span", "strong",
          "style", "sub", "summary", "sup", "table", "tbody", "td",
          "textarea", "tfoot", "th", "thead", "time", "title", "tr",
          "u", "ul", "var", "video"
)

void_tags <- c("area", "base", "br", "col", "command", "embed",
               "hr", "img", "input", "keygen", "link", "meta", "param",
               "source", "track", "wbr"
)




html_tags <- c(
  tags %>% set_names() %>% map(tag),
  void_tags %>% set_names() %>% map(void_tag)
)


html_tags$p(
  "Some text. ",
  html_tags$b(html_tags$i("some bold italic text")),
  class = "mypara"
)



with_html <- function(code) {
  code <- enquo(code)
  eval_tidy(code, html_tags)
}



with_html(
  body(
    h1("A heading", id = "first"),
    p("Some text &", b("some bold text.")),
    img(src = "myimg.png", width = 100, height = 100)
  )
)


### 21.2.6 Exercises ########

# 1
p <- tag("p")
b <- tag("b")
p("&","and <", b("& > will be escaped"))


script <- tag("script")
script("Don't escape &, <, > - escape </script> and </style>")




escape <- function(x, script = FALSE) UseMethod("escape")

escape.character <- function(x, script = FALSE) {
  
  if (script) {
    x <- gsub("</script>", "<\\/script>", x, fixed = TRUE)
    x <- gsub("</style>",  "<\\/style>",  x, fixed = TRUE)
  } else {
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
  }
  
  html(x)
}

escape.advr_html <- function(x, script = FALSE) x


tag <- function(tag, script = FALSE) {
  
  new_function(
    exprs(... = ),
    expr({
      dots <- dots_partition(...)
      attribs <- html_attributes(dots$named)
      children <- map_chr(dots$unnamed, escape, script = !!script)
      html(paste0(
        !!paste0("<", tag), attribs, ">",
        paste(children, collapse = ""),
        !!paste0("</", tag, ">")
      ))
    }),
    caller_env()
  )
}




# 2
list(
  a = c("href"),
  img = c("src", "width", "height")
)



tag_factory <- function(tag, tag_attrs) {
  attrs <- c("class", "id", tag_attrs)
  
  attr_args <- set_names(rep(list(NULL), length(attrs)), attrs)
  attr_list <- call2("list", !!!syms(set_names(attrs)))
  
  new_function(
    exprs(... = , !!!attr_args),
    expr({
      ellipsis::check_dots_unnamed()
      
      attribs <- html_attributes(compact(!!attr_list))
      dots <- compact(list(...))
      children <- map_chr(dots, escape)
      
      html(paste0(
        !!paste0("<", tag), attribs, ">",
        paste(children, collapse = ""),
        !!paste0("</", tag, ">")
      ))
    })
  )
}





# 3
greeting <- "Hello!"
with_html(p(greeting))

p <- function() "p"
address <- "123 anywhere street"
with_html(p(address))

with_html(p(!!address))


## 21.3 LaTeX ======
?plotmath
### 21.3.3 to_math() ######
to_math <- function(x) {
  expr <- enexpr(x)
  out <- eval_bare(expr, latex_env(expr))
  
  latex(out)
}

latex <- function(x) structure(x, class = "advr_latex")
print.advr_latex <- function(x) {
  cat("<LATEX> ", x, "\n", sep = "")
}


greek <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi"
)
greek_list <- set_names(paste0("\\", greek), greek)
greek_env <- as_environment(greek_list)



latex_env <- function(expr) {
  greek_env
}

to_math(pi)
to_math(beta)



### 21.3.5 Unknown symbols #######
switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

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

all_names_rec <- function(x) {
  switch_expr(x,
              constant = character(),
              symbol =   as.character(x),
              call =     flat_map_chr(as.list(x[-1]), all_names)
  )
}

all_names <- function(x) {
  unique(all_names_rec(x))
}

all_names(expr(x + y + f(a, b, c, 10)))



latex_env <- function(expr) {
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names))
  
  symbol_env
}

to_math(x)
to_math(longvariablename)
to_math(pi)



latex_env <- function(expr) {
  # Unknown symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names))
  
  # Known symbols
  env_clone(greek_env, parent = symbol_env)
}

to_math(x)
to_math(longvariablename)
to_math(pi)






unary_op <- function(left, right) {
  new_function(
    exprs(e1 = ),
    expr(
      paste0(!!left, e1, !!right)
    ),
    caller_env()
  )
}

binary_op <- function(sep) {
  new_function(
    exprs(e1 = , e2 = ),
    expr(
      paste0(e1, !!sep, e2)
    ),
    caller_env()
  )
}

unary_op("\\sqrt{", "}")
binary_op("+")


# Binary operators
f_env <- child_env(
  .parent = empty_env(),
  `+` = binary_op(" + "),
  `-` = binary_op(" - "),
  `*` = binary_op(" * "),
  `/` = binary_op(" / "),
  `^` = binary_op("^"),
  `[` = binary_op("_"),
  
  # Grouping
  `{` = unary_op("\\left{ ", " \\right}"),
  `(` = unary_op("\\left( ", " \\right)"),
  paste = paste,
  
  # Other math functions
  sqrt = unary_op("\\sqrt{", "}"),
  sin =  unary_op("\\sin(", ")"),
  log =  unary_op("\\log(", ")"),
  abs =  unary_op("\\left| ", "\\right| "),
  frac = function(a, b) {
    paste0("\\frac{", a, "}{", b, "}")
  },
  
  # Labelling
  hat =   unary_op("\\hat{", "}"),
  tilde = unary_op("\\tilde{", "}")
)




latex_env <- function(expr) {
  # Known functions
  f_env
  
  # Default symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names), parent = f_env)
  
  # Known symbols
  greek_env <- env_clone(greek_env, parent = symbol_env)
  
  greek_env
}

to_math(sin(x + pi))
to_math(log(x[i]^2))
to_math(sin(sin))


#### 21.3.7 Unknown functions #######

all_calls_rec <- function(x) {
  switch_expr(x,
              constant = ,
              symbol =   character(),
              call = {
                fname <- as.character(x[[1]])
                children <- flat_map_chr(as.list(x[-1]), all_calls)
                c(fname, children)
              }
  )
}
all_calls <- function(x) {
  unique(all_calls_rec(x))
}

all_calls(expr(f(g + b, c, d(a))))



unknown_op <- function(op) {
  new_function(
    exprs(... = ),
    expr({
      contents <- paste(..., collapse = ", ")
      paste0(!!paste0("\\mathrm{", op, "}("), contents, ")")
    })
  )
}
unknown_op("foo")

latex_env <- function(expr) {
  calls <- all_calls(expr)
  call_list <- map(set_names(calls), unknown_op)
  call_env <- as_environment(call_list)
  
  # Known functions
  f_env <- env_clone(f_env, call_env)
  
  # Default symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names), parent = f_env)
  
  # Known symbols
  greek_env <- env_clone(greek_env, parent = symbol_env)
  greek_env
}


to_math(sin(pi) + f(a))


### 21.3.8 Exercises ######
# 1