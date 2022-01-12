library(tidyverse)

##========================= chapter 10 ========================
vignette("tibble")

as_tibble(iris)

tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number")
tb


tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE))

nycflights13::flights %>% 
  print(n = 10, width = Inf)

package?tibble

df <- tibble(
  x = runif(5),
  y = rnorm(5))
df$x
df[["x"]]
df[[1]]

df %>% .$x
df %>% .[["x"]]

class(as.data.frame(tb))
class(tb)


## Exercises 10.5
class(mtcars)
str(mtcars)
class(df)
str(df)
is_tibble(df)

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]
df <- tibble(
  abc = 1,
  xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]


var <- "mpg"
mtcars[[var]]
var <- "xyz"
df[[var]]
df[var]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying
annoying$`1`
annoying[["1"]]
annoying["1"]
annoying %>% 
  ggplot() +
  geom_point(aes(`1`,`2`))
annoying <- annoying %>% 
  mutate(`3` = `2`/`1`)
annoying %>% 
  rename("one" = `1`, "two" = `2`, "three" = `3`)

? tibble::enframe()
enframe(c(a = 1, b = 2, c = 3))

print(flights)
print(flights, n_extra = 2)
#--------------------

?read_fwf()

diamondz <- read_csv("data/diamonds.csv")

read_csv("a,b,c
  1,2,3
  4,5,6")

read_csv("# A comment I want to skip
    x,y,z
    1,2,3", comment = "#")
read_csv("The first line of metadata
    The second line of metadata
    x,y,z
    1,2,3", skip = 2)

read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("a,b,c\n1,2,.", na = ".")

?data.table::fread()

#exercises 11.2.2
read_delim(
  "x|y\n1|2", delim = "|")

?read_csv()

read_csv("x,y\n1,'a,b'", quote = "'")


read_csv("a,b,\n1,2,3\n4,5,6")
read_csv("a,b,c,\n1,2,,\n1,2,3,4")
read_csv("a,b\n\'1','2")
read_delim("a,b\n1,2\na,b", delim = "\n")
read_csv2("a;b\n1;3")
##------------------------


str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems(x)

parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))
vignette("locales")

parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")


# Used in America
parse_number("$123,456,789")
# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))
# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))


charToRaw("Arash")


x1 <- "El Ni\xf1o was particularly bad this year"
x1
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x2

parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

parse_date("2010-10-01")
parse_date("2010/10/01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
?date_names()


#exercises 11.3.5
?locale
locale(grouping_mark = ".")
locale(decimal_mark = ",")
locale(grouping_mark = ",", decimal_mark = ",")


parse_date("January 1, 2010", "%B %d, %Y")
parse_date("2015-Mar-07", "%Y-%b-%d")
parse_date("06-Jun-2017", "%d-%b-%Y")
c("August 19 (2015)", "July 1 (2015)") %>% 
  parse_date("%B %d (%Y)")

"12/30/14"  %>% 
  # Dec 30, 2014
  parse_date("%m/%d/%y")

"1705" %>% 
  parse_time("%H%M")

"11:15:10.12 PM" %>% 
  parse_time("%I:%M:%OS %p")
##-----------------------------------------

guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))


challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)

?parse_number
?col_number
#I highly recommendalways supplying col_types,
#building up from the print-out provided by readr.
#This ensures that you have a consistent and reproducible data import script.

challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)
type_convert(challenge2)
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character(),
                                        x = col_double())
)

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df
type_convert(df)

?read_lines()
as_tibble(read_lines(readr_example("challenge.csv")))
as_tibble(read_file(readr_example("challenge.csv")))
t <-read_file(readr_example("challenge.csv"))

?write_csv()
?write_rds()
?read_rds()
write_rds(challenge, "challenge.rds")
challenge <- read_rds("challenge.rds")

library(feather)
write_feather(challenge2, "challenge.feather")
read_feather("challenge.feather")


##========================= chapter 12 ===================================

table1
table2
table3
table4a
table4b
table5


table1 %>% 
  mutate(rate = cases / population * 10000)
table1 %>% 
  count(year, wt = cases)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


##exercises 12.2.1
tibble(
  table2 %>% 
    filter(type == "cases") %>% 
    select(country, year, cases = count),
  table2 %>%
    filter(type == "population") %>% 
    select(pop = count) %>% 
    mutate(rate = cases / pop * 10000))

tibble(rate1 = table4a$`1999`/table4b$`1999` * 10000,
       rate2 = table4a$`2000`/table4b$`2000` * 10000)


table2 %>%
  filter(type == "cases") %>% 
  ggplot(aes(year, count, color = country)) +
  geom_line() +
  geom_point()
##-----------------
tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

left_join(tidy4a, tidy4b)

table2 %>%
  pivot_wider(names_from = type, values_from = count)

##exercises 12.3.3
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
               names_transform = list(year = as.double))
names_ptype = list(year = double()) #used for changing names to number and use them with `` 

table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
table4a %>% 
  pivot_longer(c(2, 3), names_to = "year", values_to = "cases")


people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people %>% 
  pivot_wider(names_from = "names", values_from = "values")
people %>% 
  pivot_wider(names_from = "names", values_from = "values", values_fn = mean) #"values_fn = mean" works like group_by() and summarise()
people <- tribble(
  ~name,             ~names,  ~values, ~id,
  #-----------------|--------|-------|-----
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 1,# or id can be 3 and it works properly
  "Jessica Cordero", "height",   156, 1
)
people %>% 
  pivot_wider(names_from = "names", values_from = "values")


preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg %>% 
  pivot_longer(c("male", "female"), names_to ="gender", values_to = "count", values_drop_na = T)
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg %>% 
  pivot_longer(c("male", "female"), names_to ="gender", values_to = "count") %>% 
  pivot_wider(names_from = "pregnant", values_from = "test" )
preg %>%
  pivot_longer(c("male", "female"), names_to ="gender", values_to = "count", values_drop_na = T) %>% 
  mutate(
    female = gender == "female",
    pregnant = pregnant == "yes"
  ) %>%
  select(female, pregnant, count) #this presentation make it easy to use it like: filter( female, !pregnant)
#------------------

table3
table3 %>% 
  separate(rate, into = c("cases", "population"))
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)


table5 %>% 
  unite(new, century, year)
table5 %>% 
  unite(new, century, year, sep = "")

##exercises 12.4.3
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge") #merge / drop
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "left") #right /left 
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three", "four"), fill = "right")
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three", "four"), fill = "left")


table3 %>% 
  separate(rate, into = c("cases", "population"), remove = F)
table5 %>% 
  unite(new, century, year, sep = "", remove = F)

?extract
#-----------------------


stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)  

stocks %>% 
  pivot_wider(names_from = year, values_from = return)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>% 
  fill(person)

##exercises 12.5.1

#in values_fill you can use it with col names

?fill
##----------------------

who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "count",
    values_drop_na = T
  )
who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>% 
  separate(names_from, c("new", "type", "sexage"), sep = "_")
who3
who3 %>% 
  count(new)
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5


who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

## exercises 12.6.1
who
complete(who, country, year)
who1 %>% filter(count == 0)
who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  count(new)


who1 %>% 
  select(country, iso2, iso3) %>% 
  distinct() %>% 
  group_by(country) %>% 
  filter(n() != 1)
#--------------------------

#=============================== chapter 13 ===============================
library(tidyverse)
library(nycflights13)

flights
airlines
airports
planes
weather

##exercises 13.2.1


# ithink it needs flights, airports
#the variable that needed for this task is :tailnum, faa, dest, origin, lat, lon


#connecting faa to origin

#then flights did not have a foregin key to weather

#year, month, day, and the  primery key will be year, month, day

#-------------------
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

## exercises 13.3.1
flights %>% 
  mutate(surrogate_key = row_number()) %>% 
  count(surrogate_key) %>% 
  filter(n > 1)


Lahman::Batting %>% as_tibble() %>% 
  count(playerID, yearID, teamID, stint) %>% 
  filter(n > 1)
babynames::babynames %>% as_tibble() %>% 
  count(year, sex , name) %>% 
  filter(n > 1)

nasaweather::atmos %>% 
  count(lat, long, year, month) %>% 
  filter(n > 1)
fueleconomy::vehicles %>% 
  count(id) %>% 
  filter(n > 1)
ggplot2::diamonds



library(Lahman)
Batting %>% as_tibble()
Master %>% as_tibble()
Salaries %>% as_tibble()
#---
Master
Managers
AwardsManagers

##---------------------

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>% 
  inner_join(y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")


x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")


flights2 %>% 
  left_join(airports, c("dest" = "faa"))
flights2 %>% 
  left_join(airports, c("origin" = "faa"))

## exercises 13.4.6
flights %>% 
  group_by(dest) %>% 
  summarise(ave_delay = mean(dep_delay, na.rm = T)) %>% 
  inner_join(airports, c("dest" = "faa")) %>% 
  ggplot(aes(lon, lat, color = ave_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap(xlim = c(-130, -65))

flights2 %>% 
  left_join(airports, c("dest" = "faa")) %>% 
  left_join(airports, c("origin" = "faa"), suffix = c(".origin", ".dest"))

flights %>% 
  group_by(tailnum) %>%  #### you can group it on age and then calculate the mean
  summarise(dep_delay = mean(dep_delay, na.rm = T),
            arr_delay = mean(arr_delay, na.rm = T)) %>% 
  left_join(planes, "tailnum") %>% 
  filter(!is.na(year)) %>% 
  mutate(plane_age = 2013 - year) %>%
  ggplot() +
  geom_point(aes(plane_age, dep_delay))
flights %>% 
  group_by(tailnum) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = T),
            arr_delay = mean(arr_delay, na.rm = T)) %>% 
  left_join(planes, "tailnum") %>% 
  filter(!is.na(year)) %>% 
  mutate(plane_age = 2013 - year) %>%
  ggplot() +
  geom_point(aes(plane_age, arr_delay),position =  "jitter") +
  coord_cartesian(ylim = c(0, 100))

flights %>% 
  left_join(weather) %>% 
  group_by(pressure) %>%         # change "pressure" here
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = pressure, y = delay)) + # change "pressure" here
  geom_point()


flights %>% 
  filter(month == 6, day == 13) %>%
  group_by(dest) %>% 
  summarise(ave_delay = mean(dep_delay, na.rm = T)) %>% 
  inner_join(airports, c("dest" = "faa")) %>% 
  ggplot(aes(lon, lat, size = ave_delay, colour = ave_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap(xlim = c(-130, -65))
#---------------

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

flights %>% 
  filter(dest %in% top_dest$dest)
flights %>% 
  semi_join(top_dest)


flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

##exercises 13.5.1
flights %>% 
  anti_join(planes, by = "tailnum") %>%  #  filter(!is.na(tailnum)) %>% 
  group_by(carrier) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(desc(prop))


over100 <- flights %>% 
  filter(!is.na(tailnum)) %>% 
  count(tailnum) %>% 
  filter(n >= 100)
flights %>% semi_join(over100, by = "tailnum")
#--
flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  mutate(n = n()) %>%
  filter(n >= 100)


fueleconomy::vehicles
fueleconomy::common
vehicles %>%
  semi_join(common)

t <- flights %>% 
  group_by(origin, year, month, day, time_hour) %>% 
  summarise(ave_arr = mean(arr_delay, na.rm = T),
            ave_dep = mean(dep_delay, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(ave_dep, ave_arr)) %>% 
  slice(1:48)
semi_join(weather, t) %>% 
  ggplot() +
  geom_point(aes(humid,  hour))


anti_join(flights, airports, by = c("dest" = "faa")) %>% 
  select(dest)
anti_join(airports, flights, by = c("faa" = "dest"))



flights %>% 
  filter(!is.na(tailnum)) %>% 
  distinct(tailnum, carrier) %>%
  group_by(tailnum) %>% 
  mutate(n = n()) %>% 
  filter(n != 1) %>% 
  left_join(airlines)
#---------------------


df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)


#======================== chapter 14 ========================
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
x <- c("\"", "\\")
x
writeLines(x)


?"'"

x <- "\u00b5"

c("one", "two", "three")

str_length(c("a", "R for data science", NA))

str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")


name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
str_c(c("x", "y", "z"), collapse = ", ")


x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")


x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English
str_sort(x, locale = "haw") # Hawaiian


#exercises 14.2.5
?paste #paste0 is efficent way of doing paste(sep = "")
str_c
x <- c("abc", NA)
str_c("|-", x, "-|")
paste("|-", x, "-|")

?str_wrap()

?str_trim()
str_trim("  String with trailing and leading white space\t")
str_trim("\n\nString with trailing and leading white space\n\n")
#-----------------



x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")


dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")


##exercises 14.3.1.1
x <- "\"'\\"
writeLines(x)
str_view(x, "\"'\\\\")


x <- ".a.b.c"
writeLines(x)
str_view(x, "\\..\\..\\..")
#-----------

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")
str_view(x, "a")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")


##exercises 14.3.2.1
x <- "$^$"
writeLines(x)
str_view(x, "\\$\\^\\$")

stringr::words %>% 
  str_view("^y", match = T)
words %>% 
  str_view("x$", match = TRUE)
words %>% 
  str_view("^...$", match = T)
words %>% 
  str_view("^.......", match = T)
#---------------------
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")


##exercises 14.3.3.1
str_subset(words, "^[aeoui]")
str_subset(words, "^[^aeoui]")
str_subset(words, "[^e]ed$")
str_subset(words, "(ing|ise)$")

str_subset(words, "([^c]ei|cie)")


str_view(words, "q[^u]")
str_view(words, "ou|ise$|ae|oe|yse$", match = T)
str_view("+98 930 807 4133", "[+]\\d\\d[ ]\\d\\d\\d[ ]\\d\\d\\d[ ]\\d\\d\\d\\d", match = T)
#------------

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

##exercises 14.3.4.1
str_subset(words, "^[^aeiou]{3}")
str_subset(words, "[aeiou]{3,}")
str_subset(words, "([aeiou][^aeiou]){2,}")
#--------------

str_view(fruit, "(..)\\1", match = TRUE)
str_match(fruit, "(..)\\1")

#exercises 14.3.5.1
str_view(words, "^(.).*\\1$", match = TRUE)
str_view(words, "(..).*\\1", match = TRUE)
str_view(words, "(.)(.*\\1){2,}", match = TRUE)
#----------------

x <- c("apple", "banana", "pear")
str_detect(x, "e")

sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))


# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)


words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% 
  filter(str_detect(word, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

mean(str_count(words, "[aeiou]"))

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

##exercises 14.4.1.1
a <- str_detect(words, "^x|x$")
start_x <- str_detect(words, "^x")
end_x <- str_detect(words, "x$")         
result <- start_x | end_x
identical(words[a], words[result])


b <- str_detect(words, "^[aieou].*[^aieou]$")
start_vowel <- str_detect(words, "^[aieou]")
end_cons <- str_detect(words, "[^aieou]$")
result <- start_vowel & end_cons
identical(words[b], words[result])

words[str_detect(words, "a") &
        str_detect(words, "e") &
        str_detect(words, "i") &
        str_detect(words, "o") &
        str_detect(words, "u")]


tibble(word = words,
       n_v = str_count(words, "[aioue]"),
       n_c = str_count(word, "[^aioue]"),
       n = str_count(words),
       prop = n_v/n) %>% 
  arrange(desc(n_v))
#---------------


head(sentences)
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)
str_extract(more, colour_match)
str_extract_all(more, colour_match)
str_extract_all(more, colour_match, simplify = TRUE)
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

##exercises 14.4.2.1
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c("\\b(", str_c(colours, collapse = "|"), ")\\b")
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract(sentences, "^.+?[ ]")
str_view(sentences, "\\b[A-Za-z]+ing\\b",match = T)
str_view(sentences, "\\b[^ ]+ing\\b",match = T)
str_view(sentences, "\\b[A-Za-z]{3,}s\\b", match = T)


noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)

has_noun %>% 
  str_match(noun)

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )

##exercises 14.4.3.1
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
str_c(numbers, collapse = "|")
str_view_all(match = T, sentences, str_c("\\b(", str_c(numbers, collapse = "|"), ")\\b", "[ ][^ ]+"))

str_view(sentences, match = T, "([^ ]+)'([^ ]+)")
tibble(sentence = str_subset(sentences, "([^ ]+)'([^ ]+)")) %>% 
  extract(sentence, c("before", "after"),  "([^ ]+)'([^ ]+)", remove = F)
#-----------------

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple"  "p-ar"   "b-nana"
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

##exercises 14.4.4.1
str_replace(string ="/", pattern = "/", replacement =  "\\\\")

str_replace_all(c("Ab", "De", "AR"), pattern = c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
                                                 "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
                                                 "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
                                                 "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
                                                 "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
                                                 "Z" = "z"))

switched_words <- str_replace(words, "(^.)(.*)(.$)", "\\3\\2\\1")
str_view(words, str_c("^(",str_c(switched_words, collapse = "|"), ")$"), match = T)
intersect(switched_words, words)
#-------------------

sentences %>%
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))
str_view_all(x, boundary("character"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

##exercises 14.4.5.1
str_split(string = "apples, pears, and bananas", "(, )+(and )+")

str_split(string = "apples, pears, and bananas", "")
str_split(string = "apples, pears, and bananas", boundary("character"))
#---------------------
?str_locate()
?str_sub()
fruit <- c("apple", "banana", "pear", "pineapple")
str_locate(fruit, "$")
str_locate(fruit, "a")
str_sub(fruit, str_locate(fruit, "[aeiou]"))


str_view(fruit, "nana")
str_view(fruit, regex("nana"))
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)
str_match("514-791-8141", phone)

microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2

?'"'

i <- c("I", "İ", "i", "ı")
i
str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

stringi::stri_locale_info()
x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

##exercises 14.5.1
str_view("\\", fixed("\\"))



as_tibble(str_extract_all(sentences, boundary("word"), simplify = T))%>%
  pivot_longer(c(1:12), values_to = "words", names_to = "test") %>% 
  select(words) %>%    # tibble(word = unlist(str_extract_all(sentences, boundary("word"))))
  filter(words != "") %>%
  mutate(words = str_to_lower(words)) %>% 
  count(words) %>% 
  arrange(desc(n))
#------------------

?apropos
apropos("replace")
apropos("\\bsum\\b")
apropos(fixed("sum"))

head(dir(pattern = "\\.Rmd$"))
head(dir(pattern = "\\.R$"))

?glob2rx()
glob2rx("*.R")

head(dir(pattern =glob2rx("*.R")))


??stringi
##exercises 14.7.1
?stri_count_words
stringi::stri_count_words(sentences)

?stri_duplicated
stringi::stri_duplicated(c("the", "brown", "cow", "jumped", "over",
                           "the", "lazy", "fox"))

?stri_rand_strings
stringi::stri_rand_strings(5, 10)

?stri_sort
stri_sort(c("hladny", "chladny"), locale="pl_PL")
stri_sort(c("hladny", "chladny"), locale="sk_SK")

stri_sort(c("number100", "number2"))
stri_sort(c("number100", "number2"), opts_collator = stri_opts_collator(numeric = TRUE))
#----------------------


#========================== Chapter 15 ======================================

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

y2 <- factor(x2, levels = month_levels)
y2

y2 <- parse_factor(x2, levels = month_levels)

factor(x1)

f1 <- factor(x1, levels = unique(x1))
f1
f2 <- x1 %>% factor() %>% fct_inorder()
f2
levels(f2)

gss_cat
?gss_cat

gss_cat %>%
  count(race,.drop = F) # . drop added
ggplot(gss_cat, aes(race)) +
  geom_bar()
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

##exercises 15.3.1
gss_cat %>% 
  count(rincome, .drop = F)
ggplot(gss_cat) +
  geom_bar(aes(y =rincome))

gss_cat %>% 
  count(relig, .drop = F)
ggplot(gss_cat) +
  geom_bar(aes(y =relig)) +
  scale_y_discrete(drop = FALSE)

gss_cat %>% 
  count(partyid, .drop = F)
ggplot(gss_cat) +
  geom_bar(aes(y = partyid)) +
  scale_y_discrete(drop = FALSE)


gss_cat %>% 
  group_by(relig) %>% 
  count(denom) %>% 
  arrange(desc(n))
gss_cat %>% 
  ggplot() +
  geom_tile(aes(relig, denom))
#--------------------

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()


ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()


by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))
ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")


gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

##exercises 15.4.1
gss_cat %>%
  group_by(tvhours) %>% 
  summarise(n(), 
            mean(age, na.rm = T)) %>% 
  arrange(desc(tvhours)) %>% 
  print(n = 25)

levels(gss_cat$marital)
levels(gss_cat$race)
levels(gss_cat$rincome)
levels(gss_cat$partyid)
levels(gss_cat$relig)
levels(gss_cat$denom)

levels(fct_relevel(rincome_summary$rincome, "Not applicable"))
#--------------------

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)  

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

##exercises 15.5.1
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid, democ = c("Strong democrat", "Not str democrat"),
                                ind = c("Ind,near dem", "Independent", "Ind,near rep"),
                                repub = c("Strong republican", "Not str republican"),
                                other = c("No answer", "Don't know", "Other party" ))) %>% 
  group_by(year, partyid) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(year, prop)
#--or
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid, democ = c("Strong democrat", "Not str democrat"),
                                ind = c("Ind,near dem", "Independent", "Ind,near rep"),
                                repub = c("Strong republican", "Not str republican"),
                                other = c("No answer", "Don't know", "Other party" ))) %>% 
  count(year, partyid) %>% 
  group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(year, prop) %>% 
  mutate(partyid = fct_reorder2(partyid, year, prop)) %>% 
  ggplot(aes(x = year, y = prop, color = partyid)) +
  geom_line() + geom_point()



levels(fct_lump(gss_cat$rincome, n = 10))


gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()
#------------------------


##========================= chapter 16 ========================


library(tidyverse)
library(lubridate)
library(nycflights13)


today()
now()

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")


ymd(20170131, tz = "UTC")


flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))


make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes


as_datetime(today())
as_date(now())

as_datetime(60 * 60 * 10)
as_date(365 * 10 +2)

##exercises 16.2.4
ymd(c("2010-10-10", "bananas"))

?today()
today("GMT")
?date
?now
?Sys.time


mdy("January 1, 2010")
ymd("2015-Mar-07")
dmy("06-Jun-2017")
mdy(c("August 19 (2015)", "July 1 (2015)"))
mdy("12/30/14") # Dec 30, 2014
#------------------------


datetime <- ymd_hms("2016-07-08 12:34:56")


year(datetime)
month(datetime, label = TRUE)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
wday(datetime, label = TRUE, abbr = FALSE)


flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())
ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()
?floor_date
?ceiling_date
?round_date

(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>% 
  update(mday = 30)
#> [1] "2015-03-02"
ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

##exercises 16.3.4
flights_dt %>% 
  mutate(day = floor_date(dep_time, "day")) %>% 
  ggplot() +
  geom_freqpoly(aes(day))

flights_dt %>% 
  mutate(day = update(dep_time, yday = 1),
         month = month(dep_time)) %>% 
  ggplot() +
  geom_freqpoly(aes(day, color = factor(month)))


flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  mutate(dep_time = update(dep_time, mday = mday(dep_time) + 1)) %>% 
  filter(dep_time_ != dep_time) %>%
  select(dep_time_, dep_time, sched_dep_time, dep_delay)


flights_dt %>%
  left_join(airports, by = c('origin' = 'faa')) %>%
  left_join(airports, by = c('dest' = 'faa'), suffix = c('.origin','.dest')) %>%
  select(dep_time, arr_time, air_time, contains('tzone')) %>%
  mutate(dep_time = force_tz(dep_time, tzone = tzone.origin),
         arr_time = force_tz(arr_time, tzone = tzone.dest),
         cal_air_time = as.numeric(arr_time - dep_time) * 60)

flights_dt %>% 
  mutate(hour = hour(sched_dep_time)) %>% 
  group_by(hour) %>%
  summarise(ave = mean(dep_delay, na.rm = T)) %>% 
  ggplot(aes(hour, ave)) +
  geom_point()


flights_dt %>% 
  mutate(day = wday(dep_time, label = T)) %>% 
  group_by(day) %>% 
  summarise(ave = mean(dep_delay, na.rm = T)) %>% 
  mutate(day = factor(day)) %>%
  mutate(day =fct_reorder(day, ave)) %>% 
  ggplot(aes(day, ave)) +
  geom_col()
flights_dt %>% 
  mutate(day = wday(dep_time, label = T)) %>% 
  group_by(day) %>% 
  summarise(ave = mean(arr_delay, na.rm = T)) %>% 
  mutate(day = factor(day)) %>%
  mutate(day =fct_reorder(day, ave)) %>% 
  ggplot(aes(day, ave)) +
  geom_col()


flights_dt %>% 
  ggplot(aes(minute(sched_dep_time))) +
  geom_freqpoly(binwidth = 1)
diamonds %>% 
  ggplot(aes(carat)) +
  geom_freqpoly(binwidth = 0.05)


flights_dt %>% 
  mutate(minute = minute(dep_time),
         have_delay = dep_delay < 0) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
            n = n(),
            have_delay = mean(have_delay)) %>% 
  ggplot()+
  geom_line(aes(minute, have_delay))

#------------------------------
h_age <- today() - ymd(19990528)
h_age
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)


tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm + ddays(1)

one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)



# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)
one_pm + days(1)

flights_dt %>% 
  filter(arr_time < dep_time) 
  
flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

years(1) / days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)


(today() %--% next_year) %/% days(1)

##exercises 16.4.5
ymd("20150101") + months(0:11)
floor_date(today(), "year") + months(0:11)

how_old <- function(date = today()){
  return (interval(date, today())%/%years(1))
}
how_old("19990528")


(today() %--% (today() + years(1))) / months(1)
##----------------------------
Sys.timezone()
length(OlsonNames())
head(OlsonNames())

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2
x1 - x3

x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4