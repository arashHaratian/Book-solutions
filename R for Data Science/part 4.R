#=============== chapter 23 =======================
library(tidyverse)


library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)
ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 


model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)
# ggplot(sim1, aes(x, y)) + 
#   geom_abline(aes(intercept = 7, slope = 1.5), alpha = 1/4) +
#   geom_point() 

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)


sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}
models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
    )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))


grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))


grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )


best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])


sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

##exercises 23.2.1
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
params <- sim1a %>% 
  lm(formula = y ~ x) %>% 
  coef() 
sim1a %>% 
  ggplot(aes(x, y)) + geom_point() +
  geom_abline(intercept = params[1], slope = params[2])



model2 <- function(a, data){
  a[1] + data$x * a[2]
}
measure_distance2 <- function(mod, data) {
  diff <- data$y - model2(mod, data)
  mean(abs(diff))
}
best2 <- optim(c(0, 0), measure_distance2, data = sim1)
ggplot() +
  geom_point(data = sim1, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]], slope = best2$par[[2]]))
best1 <- optim(c(0, 0), measure_distance, data = sim1)
ggplot() +
  geom_point(data = sim1, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]], slope = best2$par[[2]]), color = "blue") +
  geom_abline(aes(intercept = best1$par[[1]], slope = best1$par[[2]]), color = "red")


best2 <- optim(c(0, 0), measure_distance2, data = sim1a)
ggplot() +
  geom_point(data = sim1a, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]], slope = best2$par[[2]]))
best1 <- optim(c(0, 0), measure_distance, data = sim1a)
ggplot() +
  geom_point(data = sim1a, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]], slope = best2$par[[2]]), color = "blue") +
  geom_abline(aes(intercept = best1$par[[1]], slope = best1$par[[2]]), color = "red")
#--
sim1_dist2 <- function(a1, a2){
  measure_distance2(c(a1,a2), sim1)
}
grid2 <- expand.grid(
  a1 = seq(-10, 20, length.out = 40),
  a2 = seq(-5, 5, length.out = 25)
)
grid2 <- grid2 %>% 
  mutate(dist1 = map2_dbl(a1, a2, sim1_dist),
         dist2 = map2_dbl(a1, a2, sim1_dist2))
ggplot() +
  geom_point(data = sim1, aes(x, y), size = 2) +
  geom_abline(data = filter(grid2, rank(dist2)<=1), aes(intercept = a1, slope = a2, color = -dist2)) +
  geom_abline(data = filter(grid2, rank(dist1)<=1), aes(intercept = a1, slope = a2, color = -dist2))




sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
df <- sim1a
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
measure_distance1 <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
best1 <- optim(c(0, 0), measure_distance1, data = df)
best1$par
model2 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}
measure_distance2 <- function(mod, data) {
  diff <- data$y - model2(mod, data)
  sqrt(mean(diff ^ 2))
}
best2 <- optim(c(0, 0, 0), measure_distance2, data = df)
best2$par
ggplot() +
  geom_point(data = df, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]] + best2$par[[3]], slope = best2$par[[2]]), color = "blue") +
  geom_abline(aes(intercept = best1$par[[1]], slope = best1$par[[2]]), color = "red")

near(best1$par[[2]], best2$par[[2]], 0.01)
near(best1$par[[1]], best2$par[[1]] + best2$par[[3]], 0.01)

best2 <- optim(c(2, 0, -5), measure_distance2, data = df)
best2$par
ggplot() +
  geom_point(data = df, aes(x, y), size = 2) +
  geom_abline(aes(intercept = best2$par[[1]] + best2$par[[3]], slope = best2$par[[2]]), color = "blue") +
  geom_abline(aes(intercept = best1$par[[1]], slope = best1$par[[2]]), color = "red")
near(best1$par[[2]], best2$par[[2]], 0.01)
near(best1$par[[1]], best2$par[[1]] + best2$par[[3]], 0.01)
#------------------------

grid <- sim1 %>% 
  data_grid(x) 
grid

grid <- grid %>% 
  add_predictions(sim1_mod) 
# sim1 %>% 
#   add_predictions(sim1_mod) 
grid
## this plot tell us what pattern the model has captured
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1) 


sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

near(mean(sim1$resid), 0)


ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

##exercises 23.3.3
rm(sim1)
?loess
loess_mod <- loess(y ~ x, sim1, degree = 3)

grid <- sim1 %>% 
  data_grid(x)

grid <- grid %>% 
  add_predictions(loess_mod)

ggplot() +
  geom_point(aes(x, y), data = sim1) + 
  geom_line(aes(x, pred), data = grid, color = "blue", size = 1)
#--
# ggplot(sim1, aes(x)) +
#   geom_point(aes(y = y)) +
#   geom_line(aes(y = pred), data = grid, color = "blue", size = 1) 

ggplot(sim1 , aes(x, y)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  geom_line(aes(x, pred), data = grid, color = "red", size = 1)


sim1 %>%
  add_residuals(sim1_mod, var = "res_lm") %>% 
  add_residuals(loess_mod, var = "res_loess") %>% 
  ggplot(aes(x = x)) +
  geom_ref_line(h = 0) +
  geom_point(aes(y = res_lm), color = "blue") +
  geom_point(aes(y = res_loess), color = "red")



?add_predictions
df <- tibble::tibble(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)
m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
# grid <- data_grid(df, x)
grid %>% add_predictions(m1)
m2 <- lm(y ~ poly(x, 2), data = df)
grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)
#---
grid <- sim1 %>% 
  data_grid(x)
grid %>%
  gather_predictions(sim1_mod, loess_mod)
grid %>%
  spread_predictions(sim1_mod, loess_mod)
grid %>%
  gather_predictions(sim1_mod, loess_mod) %>% 
  pivot_wider(names_from = "model", values_from = "pred")


?geom_ref_line()


sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1
ggplot(sim1, aes(abs(resid))) +
  geom_freqpoly(binwidth = 0.5)
mean(abs(sim1$resid))
#---------------------

?model_matrix()
model_matrix(sim1, y~x) %>% print(n= 30)
model_matrix(mtcars, mpg ~ cyl)%>% print(n= 32)


df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)
lm(data = sim1, y ~ x -1)
lm(data = sim1, y ~ x)


df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
  # "unknown", 3
)
model_matrix(df, response ~ sex)


ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

tibble(x = "e") %>% 
  add_predictions(mod2)

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)



mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid


?seq_range
seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)


x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)


ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)





df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)


model_matrix(df, y ~ poly(x, 2)) #raw = T

library(splines)
model_matrix(df, y ~ ns(x, 2))



sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim5, aes(x, y)) +
  geom_point()


mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)


grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")


ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)


##exercises 23.4.5

ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x -1 , data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)
#++
grid <- sim2 %>%
  data_grid(x) %>%
  spread_predictions(mod2, mod2a)



model_matrix(y ~ x1 * x2, data = sim3)
model_matrix(y ~ x1 * x2, data = sim4)


x3 <- model_matrix(y ~ x1 * x2, data = sim3)
all(x3[["x1:x2b"]] == (x3[["x1"]] * x3[["x2b"]]))
all(x3[["x1:x2c"]] == (x3[["x1"]] * x3[["x2c"]]))
all(x3[["x1:x2d"]] == (x3[["x1"]] * x3[["x2d"]]))
x4 <- model_matrix(y ~ x1 * x2, data = sim4)
all(x4[["x1"]] * x4[["x2"]] == x4[["x1:x2"]])



mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
sim4 %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_tile(aes(fill = resid)) +
  facet_wrap(~model)

sim4 %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(x = x2, y = resid)) +
  geom_point(aes(fill = x1)) +
  facet_wrap(~model, ncol = 1)

sim4 %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(x = resid, colour = model)) +
  geom_freqpoly(binwidth = 0.5)

sim4 %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(x = abs(resid), colour = model)) +
  geom_freqpoly(binwidth = 0.5)
#-+-
sim4 %>% 
  gather_residuals(mod1, mod2) %>% 
  group_by(model) %>%
  summarise(resid = sd(resid))
#------------------------------


df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)
mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)


?stats::glm
?mgcv::gam()
?glmnet::glmnet()
?MASS::rlm()
?rpart::rpart()
?randomForest::randomForest()
?xgboost::xgboost





#====================== chapter 24
library(tidyverse)
library(modelr)
options(na.action = na.warn)


library(nycflights13)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()


ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)


diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)


grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)


diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()



mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)



##exercises 24.2.3
diamonds2 %>% 
  ggplot(aes(lcarat, lprice))+
  geom_hex(bins = 30) # presents the number of right price and also human-friendly numbers


mod_ex2 <- lm(log2(price)~log2(carat), diamonds)

grid1 <- diamonds %>% 
  data_grid(carat = seq_range(carat, n = 20)) %>% 
  add_predictions(mod_ex2, "price")
grid1
#--
grid2 <- diamonds %>% 
  data_grid(carat = seq_range(carat, n = 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "price")
grid2
ggplot()+
  geom_line(aes(x = carat, y = 2^price), data = grid1, color ="red", size = 2) +
  geom_line(aes(x = carat, y = 2^price), data = grid2, color ="blue", size = 2)



diamonds2 %>%
  ggplot(aes(2^lresid2)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)+
  geom_ref_line(h = 0)

summary(mod_diamond2)

diamonds2 %>% 
  summarise(
    rmse = sqrt(mean(lresid2 ^ 2)),
    mae = mean(abs(lresid2))
    # p025 = quantile(lresid2, 0.025),
    # p975 = quantile(lresid2, 0.975)
  )
#--------------------

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>%  #as_date(time_hour)
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) +
  geom_line()


daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()


mod <- lm(n ~ wday, data = daily)
grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")
ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod) #daily %>% left_join(grid, "wday") %>% mutate(res = n.x-n.y)
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

# daily %>% 
#   ggplot(aes(wday, resid)) + 
#   geom_ref_line(h = 0) + 
#   geom_boxplot()

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()

daily %>% 
  filter(resid < -100)


daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)



daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")



term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()


mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily) #use star to see the wday in term (not independently)


daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)


grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)


mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod3, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)




compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}
compute_vars(daily)

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)



library(splines)
mod <- MASS::rlm(n ~ wday * splines::ns(date, 5), data = daily)


daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()


##exercises 24.3.5
special_dates <- daily%>%
  filter(date %in% ymd("13Jan 20", "13May 26", "13sep 1"))
daily %>% 
  ggplot(aes(date, resid)) +
  geom_line() +
  geom_point(data = special_dates, color = "blue")


special_dates <- daily %>% 
  top_n(3, resid)
special_dates
daily %>% 
  ggplot(aes(date, resid)) +
  geom_line() +
  geom_point(data = special_dates, color = "blue")


ex2433 <- daily %>% 
  mutate(wday2 = if_else(wday %in% "Sat", str_c(as.character(wday), term, sep = '-'), as.character(wday)))

mod2433_1 <- lm(n ~ wday2, data = ex2433)
mod2433_2 <- lm(n ~ wday * term, data = ex2433)  
ex2433 %>% 
  gather_residuals(mod2433_1, mod2433_2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line()

ex2433 %>%
  spread_residuals(sat_term = mod2433_1, all_interact = mod2433_2) %>%
  mutate(resid_diff = sat_term - all_interact) %>%
  ggplot(aes(date, resid_diff)) +
  geom_line(alpha = 0.75)



months <- mutate(daily, month = factor(lubridate::month(date)))
mod6 <- lm(n ~ wday * month, data = months)
print(summary(mod6))
months %>% 
  gather_residuals(mod6, mod2433_2) %>% 
  ggplot(aes(date, resid, color = model)) +
  geom_line()


mod7 <- lm(n ~ wday + ns(date, 5), data = daily)
mod8 <- lm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  gather_residuals(mod7, mod8) %>%
  ggplot(aes(x = date, y = resid, color = model)) +
  geom_line(alpha = 0.75)



flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  geom_boxplot()


flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  stat_summary()


flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  ggplot(aes(x = hour, color = wday, y = ..density..)) +
  geom_freqpoly(binwidth = 1)

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  group_by(wday, hour) %>%
  summarise(distance = mean(distance)) %>%
  ggplot(aes(x = hour, color = wday, y = distance)) +
  geom_line()

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  group_by(wday, hour) %>%
  summarise(distance = sum(distance)) %>%
  group_by(wday) %>%
  mutate(prop_distance = distance / sum(distance)) %>%
  ungroup() %>%
  ggplot(aes(x = hour, color = wday, y = prop_distance)) +
  geom_line()


start_with <- function(data, start){
  fct_relevel(data, start)
}
start_with(daily$wday, "Mon")


#---------------------------


#========================= chapter 25 ============================
library(modelr)
library(tidyverse)


library(gapminder)
gapminder


gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)+
  geom_point(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")
#> gapminder %>% select(-continent, -pop, -gdpPercap) %>% pivot_wider(names_from = country, values_from = lifeExp) 


by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()


by_country$data[[1]]

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

by_country %>% 
  filter(continent == "Europe")
by_country %>% 
  arrange(continent, country)

by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country


resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)


broom::glance(nz_mod)


by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)


glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

glance %>% 
  arrange(r.squared)


glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)


bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()



##exercises 25.2.5
country_model_quad <- function(data){
  lm(formula = lifeExp ~ splines::ns((year - median(year)), 4), data = data)
}
models_quad <- gapminder %>% 
  group_by(continent, country) %>% 
  nest() %>% 
  mutate(models= map(data, country_model_quad))

models_quad_unnest <- models_quad %>%
  mutate(pred = map2(data, models, add_predictions)) %>%
  unnest(pred) %>% 
  select(-pred)
models_quad_unnest %>% 
  ggplot(aes(year, lifeExp, group = country)) + geom_line(alpha = 1/4)

models_quad_unnest <- models_quad %>%
  mutate(resid = map2(data, models, add_residuals)) %>%
  unnest(resid)
models_quad_unnest %>% 
  ggplot(aes(year, resid, group = country)) + geom_line(alpha = 1/4)

models_quad_unnest <- models_quad %>%
  mutate(glance = map(models, broom::glance)) %>%
  unnest(glance)


ggplot(models_quad_unnest, aes(continent, r.squared)) + 
  geom_jitter(width = 0.5) +
  geom_jitter(data = glance, width = 0.5, color = "red")
  

bad_fit <- filter(models_quad_unnest, r.squared < 0.8)
gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()




library(ggbeeswarm)
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_beeswarm()
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_quasirandom()

ggplot(models_quad_unnest, aes(continent, r.squared)) + 
  geom_quasirandom()
ggplot(models_quad_unnest, aes(continent, r.squared)) + 
  geom_beeswarm()


ggplot(models_quad_unnest, aes(continent, r.squared)) + 
  geom_violin()
ggplot(models_quad_unnest, aes(continent, r.squared)) + 
  lvplot::geom_lv()


bad_fit <- filter(glance, r.squared < 0.25)

bad_fit %>% unnest(data) %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()
#--------------------------

data.frame(x = list(1:3, 3:5))
data.frame(x = I(list(1:3, 3:5)))

data.frame(
  x = I(list(1:3, 3:5)), 
  y = c("1, 2", "3, 4, 5")
)

tibble(
  x = list(1:3, 3:5), 
  y = c("1, 2", "3, 4, 5")
)

tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)



?tibble::enframe()


gapminder %>% 
  group_by(country, continent) %>% 
  nest()
gapminder %>% 
  nest(year:gdpPercap)


df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 
df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

?tidyr::separate_rows()

tidyr::separate_rows(df, x1)


sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sims = invoke_map(f, params, n = 10))


mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))


probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()


x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 
x
df <- enframe(x)
df


df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )
df %>% 
  mutate(
    smry = map2(name, value, ~ stringr::str_c(.x, ": ", .y))
  )



##exercises 25.4.5
as.list
list
map
str_split
str_match_all


glance
coef
summary
range
cut
data_grid
boxplot.stats



mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>% 
  unnest() #prob
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = enframe(quantile(mpg)))
#--
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(enframe(quantile(mpg)))) %>% 
  unnest(q)



t <- mtcars %>% 
  group_by(cyl) %>% 
  summarise_each(funs(list)) 

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(.fns = list))
#---------------------------------------


df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)


df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)
df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)


tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)
df1

df1 %>% unnest(y, z)



df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2
df2 %>% unnest(c(y, z))


##exercises 25.5.3

#because it will help us to prevent unnesting two list with different number of elements simultaneously


# logical
# numeric
# integer
# character
# factor
# lists aren't atomic so and it they are uncommon because we can wrap up atomic vectors in list 
#(and as long as data frames are list so it's list of list) and it's get complicated compare to 
#dataframes with atomic vectors


#--------------------------------



nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")
nz_mod <- lm(lifeExp ~ year, data = nz)



?broom::glance
broom::glance(nz_mod)


?broom::tidy
coef(nz_mod)
broom::tidy(nz_mod)



?broom::augment
broom::augment(nz_mod, nz)
