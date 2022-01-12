###########################
#
# all codes until
# chapter 5.3 , fucked up!
# 
# 
###########################


library(tidyverse)
library(nycflights13)


arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

df <- tibble(x = c(5, 10, 2, NA))
arrange(df, x)
arrange(df, desc(x))

##exercises 5.3.1

arrange(df,!is.na(x), x)

arrange(flights, desc(dep_delay, arr_delay))
arrange(flights, dep_delay)

arrange(flights, distance/air_time)

arrange(flights, desc(distance))
##------------------

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

#exercises 5.4.1

select(flights, day)

vars <- c("year", "month", "day", "dep_delay", "arr_delay", "test")
select(flights, one_of(vars)) #its use to select cols that you do not know about the names  (see one_of/all_of/any_of)

select(flights, contains("TIME"), any_of(vars))
#-----------------
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

transmute(flights, 
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

(x <- seq(1, 10 , by = 2))
lag(x)
lead(x)
x != lag(x)

x <- 1:10
cumsum(x)
cummean(x)


y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
dense_rank(y)
percent_rank(y)
percent_rank(y)
row_number(y)

##exercises 5.5.2
transmute(flights, dep_time, dep_hour = dep_time %/% 100, dep_minute = dep_time %% 100, sched_dep_time, sched_dep_hour = sched_dep_time %/% 100, 
          sched_dep_minute = sched_dep_time %% 100) ### you must turn the 2400 time to 0 ,, so need to use %% 1440 at the end


filter(flights, air_time == arr_time - dep_time)
flights_sml <- transmute(flights, dep_time, 
                         dep_time_new = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                         arr_time,
                         arr_time_new = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
                         air_time,
                         air_time_new = arr_time_new-dep_time_new)
filter(flights_sml, near(air_time_new , air_time , 60))

flights_sml <- transmute(flights, dep_time, 
                         dep_time_new = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                         sched_dep_time,
                         sched_dep_time_new = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440,
                         dep_delay_new = dep_time_new - sched_dep_time_new,
                         dep_delay)
filter(flights_sml, dep_delay != dep_delay_new) ## problem with flights that delayed to next day

arrange(transmute(flights, x = min_rank(desc(dep_delay)),year, month, day, flight, dep_delay), x)
arrange(flights, desc(dep_delay))

?trig
##------------------

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarize(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))



delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay))
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)



delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n())
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10) + geom_vline(xintercept = 25)


delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE))
batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)


batters %>% 
  arrange(desc(ba))


not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )


not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r)) %>% select(r, dep_time)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)
not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))


daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

##exercises 5.6.7
not_cancelled %>% 
  group_by(flight) %>% 
  summarise(mean(dep_delay), mean(arr_delay))


not_cancelled %>% count(dest)
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(num = n())
not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(distance = sum(distance))

filter(flights, is.na(dep_delay) | is.na(arr_delay))
filter(flights, is.na(arr_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(num_can = sum(is.na(arr_delay)), num = n()) %>%
  # filter(num_can < 50) %>% 
  ggplot() + geom_point(aes(x = num , y = num_can))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(num_can = sum(is.na(arr_delay)),
            num = n(),
            ave_ad = mean(arr_delay, na.rm = T),
            ave_ap = mean(dep_delay, na.rm = T),
            prop = mean(is.na(arr_delay))) %>% 
  ggplot() + geom_point(aes(x = prop, y = num_can))




flights %>% 
  group_by(carrier) %>%
  summarise(ave_dep = mean(dep_delay, na.rm = T), 
            ave_arr = mean(arr_delay, na.rm = T)) %>% 
  arrange(ave_arr, ave_dep)
# flights %>% 
#   group_by(carrier, dest) %>%
#   summarise(ave_dep = mean(dep_delay, na.rm = T), 
#             ave_arr = mean(arr_delay, na.rm = T)) %>% 
#   summarise(ave_dep = mean(ave_dep, na.rm = T), 
#             ave_arr = mean(ave_arr, na.rm = T)) %>% 
#   arrange(ave_arr, ave_dep)
flights %>% 
  group_by(dest, carrier) %>% 
  summarise(ave_arr_delay = mean(arr_delay, na.rm = T)) %>% 
  mutate(dest_arr_delay = mean(ave_arr_delay, na.rm = T),
         removed_ave = ave_arr_delay - dest_arr_delay) %>% 
  #you can use scale() function for removing ave
  ungroup() %>% 
  group_by(carrier) %>% 
  summarise(real_delay = mean(removed_ave, na.rm = T)) %>% 
  arrange(real_delay)
flights %>% 
  group_by(origin, dest, carrier) %>% 
  summarise(ave_arr_delay = mean(arr_delay, na.rm = T)) %>% 
  mutate(dest_arr_delay = mean(ave_arr_delay, na.rm = T),
         removed_ave = ave_arr_delay - dest_arr_delay) %>% 
  #you can use scale() function for removing ave
  ungroup() %>% 
  group_by(carrier) %>% 
  summarise(real_delay = mean(removed_ave, na.rm = T)) %>% 
  arrange(real_delay)

?count
##-----------------


popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

vignette("window-functions")

##exercises 5.7.1
flights %>%
  filter(!is.na(tailnum)) %>% 
  group_by(tailnum) %>% 
  summarise(mean = mean(dep_delay, na.rm = T))


##-----------------


#===================================  chapter 7 ======================================
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
diamonds %>% 
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# smaller <- diamonds %>% 
#   filter(carat < 3)
# ggplot(data = smaller, mapping = aes(x = carat, fill = cut)) +
#   geom_histogram(binwidth = 0.1, position = "dodge2")

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual


##exercises 7.3.4
ggplot(diamonds) +
  geom_histogram(aes(x = x), binwidth = 0.1) +
  coord_cartesian(ylim = c(0, 50))
ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.1) +
  coord_cartesian(ylim = c(0, 50))
ggplot(diamonds) +
  geom_histogram(aes(x = z), binwidth = 0.1) +
  coord_cartesian(ylim = c(0, 50))

ggplot(diamonds) +
  geom_histogram(aes(x = price), binwidth = 10)

ggplot(diamonds) +
  geom_histogram(aes(carat), binwidth = 0.01) + 
  coord_cartesian(xlim = c(0.99, 1))

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = price)) +
  coord_cartesian(ylim = c(0, 50))
##----------------

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

##exercises 7.4.1
diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds2, mapping = aes(x = y)) + 
  geom_bar() # use geom_histogram() 
diamonds %>%
  mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>%
  ggplot() +
  geom_bar(mapping = aes(x = cut))
##----------------

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))
ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
  geom_boxplot()

##exercises 7.5.1.1
flights %>% 
  mutate(is_cancelled = is.na(dep_time),
         sched_dep_time = sched_dep_time %/% 100 + sched_dep_time %% 100 /60) %>% 
  ggplot(aes(x = sched_dep_time, y = stat(density), colour = is_cancelled)) +
  geom_freqpoly(binwidth = 0.2)
flights %>% 
  mutate(is_cancelled = is.na(dep_time),
         sched_dep_time = sched_dep_time %/% 100 + sched_dep_time %% 100 /60) %>% 
  ggplot(aes(x = sched_dep_time ,y = is_cancelled)) +
  geom_boxplot()


?diamonds
# ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
#   geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")
diamonds %>% 
  filter(cut %in% c("Ideal", "Fair")) %>% 
  ggplot(mapping = aes(x = price, y = z, color = cut)) +
  geom_point() 
# + coord_cartesian(ylim = c(50, 70))
diamonds %>% 
  filter(cut %in% c("Ideal", "Fair")) %>% 
  ggplot(mapping = aes(x = depth, color = cut)) +
  geom_density() #so the reason for  bigger mean of fair cut in price rather than ideal is that  
# fair cut have more sample in the variables that effect the price to get higher

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly( binwidth = 500)+
  facet_wrap(~cut, ncol = 1)
ggplot(data = diamonds, mapping = aes(price)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~cut, ncol = 1)
ggplot(data = diamonds, mapping = aes(price, cut)) +
  geom_violin()

ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='quasirandom')
ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='beeswarm')
##--------------------

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n)) +coord_flip()


##exercises 7.5.2.1
diamonds %>% 
  count(color, cut) %>% 
  group_by(color) %>% 
  mutate(prop = n/ sum(n)) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

not_cancelled %>% 
  group_by(month, dest) %>% 
  summarise(ave_delay = mean(dep_delay, na.rm = T)) %>% 
  ggplot() +
  geom_tile(aes(y= dest, factor(month), fill = ave_delay))
flights %>%
  group_by(month, dest) %>%                                 
  summarise(ave_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%                                      
  filter(n() == 12) %>% 
  ggplot() +
  geom_tile(aes(y= dest, factor(month), fill = ave_delay))
##---------------------------------

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/40)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
#same
smaller %>% as.tibble() %>% 
  group_by(cut_width(carat, 0.1)) %>% 
  count(price) %>% 
  ggplot() +
  geom_boxplot(aes(y= price, `cut_width(carat, 0.1)`))
#book plot edited
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxploth(mapping = aes(group = cut_width(carat, 0.1)))

smaller %>% as.tibble() %>% 
  group_by(cut_width(carat, 0.1)) %>% 
  count(price) %>% 
  ggplot() +
  geom_boxplot(aes(y= price, `cut_width(carat, 0.1)`))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxploth(mapping = aes(group = cut_width(carat, 0.1)),varwidth = TRUE)

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxploth(mapping = aes(group = cut_number(carat, 20)))


##exercises 7.5.3.1
ggplot(data = smaller, mapping = aes(price)) + 
  geom_freqpoly(mapping = aes(color = cut_width(carat, 0.1))) +
  facet_wrap(~cut_width(carat, 0.1))
ggplot(data = smaller, mapping = aes(price)) + 
  geom_freqpoly(mapping = aes(color = cut_number(carat, 20))) +
  facet_wrap(~cut_number(carat, 20))

ggplot(data = smaller, mapping = aes(carat)) + 
  geom_freqpoly(mapping = aes(color = cut_number(price, 20))) +
  facet_wrap(~cut_number(price, 20))
ggplot(data = smaller, mapping = aes(carat)) + 
  geom_freqpoly(mapping = aes(color = cut_width(price, 1000))) +
  facet_wrap(~cut_width(price, 1000))
ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip()

diamonds %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = cut_number(carat, 10),y = price)) +
  coord_flip()


diamonds %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = cut_number(carat, 10),y = price, color = cut))
diamonds %>% 
  ggplot() +
  geom_boxplot(mapping = aes(color = cut_number(carat, 10),y = price, x = cut))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = x, y = y), bins = 100) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
##-----------------------------


ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))


library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid)) # The residuals give us a view of the price of the diamond, once the effect of carat has been removed.
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid)) # shows us that lower carat have more difference that real price
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = price, y = resid, color = carat))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid)) # the cut / resid shows us how ideal and premium and ... close to their real price

##=============================== chapter 8 ===============================
library(tidyverse)


ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("diamonds.pdf")


write_csv(diamonds, "diamonds.csv")