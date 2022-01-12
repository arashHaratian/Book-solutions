#======================== chapter 27 ========================
file.edit('Diamond-sizes.rmd')
rmarkdown::render('Diamond-sizes.rmd')

##exercises 27.2.1
file.edit("exercise27.2.1-1.Rmd")


file.edit('Diamond-sizes.rmd')
#----------------------



##exercises 27.3.1
file.edit('27.3 Text Formating.Rmd')


file.edit("dimond sizes exercise.Rmd")
#---------------------------

file.edit('Testing options.Rmd')


?knitr::opts_chunk


comma <- function(x) format(x, digits = 2, big.mark = ",")
comma(3452345)
#> [1] "3,452,345"
comma(.12358124331)


##exercises 27.4.7
file.edit("exercise 27.4.7.4.Rmd")
#-----------------------------


file.edit("params testing.Rmd")
rmarkdown::render("fuel-economy.Rmd", params = list(my_class = "suv"))


reports %>% 
  select(output_file = filename, params) %>% 
  purrr::pwalk(rmarkdown::render, input = "params testing.Rmd")





file.edit("testing bibliography.rmd")



#=========================== chapter 28 ========================
library(tidyverse)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")




ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )



?quote
?plotmath


df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

##exercises 28.2.1

ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point()+
  labs(
    title = "big engine means more fuel (money)",
    subtitle = "buy appropriate car",
    caption = "nehhh",
    x = "fuel",
    y = "mile per galon",
    color = "car model"
  )



mpg %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2))



model_exer2.1 <- MASS::rlm(hwy ~ splines::ns(displ, 2), mpg)
mpg %>% 
  modelr::add_predictions(model_exer2.1) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_line(aes(displ, pred), size =2, color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 2))



mpg %>% 
  ggplot(aes(displ, hwy,  color = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 1))


model_exer2.2 <- lm (hwy ~ class, mpg)
mpg %>% modelr::add_residuals(model_exer2.2) %>% 
  ggplot(aes(displ, resid)) +
  # geom_point(aes(color = class)) +
  geom_point(aes(color = class)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ splines::ns(x, 3))
#--------------------------------------------


best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)



ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)



class_avg <- mpg %>%
  group_by(class) %>%
  summarise(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, colour = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")


label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")



"Increasing engine size is related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()



geom_hline() 
geom_vline()


?geom_rect()
df <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)
ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
  geom_rect(aes(fill = z), colour = "grey50")


?geom_segment()


ggplot(seals, aes(long, lat)) +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
               arrow = arrow(length = unit(0.1,"cm"))) +
  borders("state")



##exercises 28.3.1
tribble(
  ~displ, ~hwy, ~label, ~vjust, ~hjust,
  Inf, Inf, "Top right", "top", "right",
  Inf, -Inf, "Bottom right", "bottom", "right",
  -Inf, Inf, "Top left", "top", "left",
  -Inf, -Inf, "Bottom left", "bottom", "left"
) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(data = mpg) +
  geom_text(aes(label = label, vjust = vjust, hjust = hjust))



vjust <- c(bottom = 0, top = 1)
hjust <- c(left = 0, right = 1)
df <- tidyr::crossing(hj = names(hjust), vj = names(vjust)) %>%
  mutate(
    y = vjust[vj],
    x = hjust[hj],
    label = paste0("hjust = '", hj, "'\n", "vjust = '", vj, "'")
  )
ggplot(df, aes(x, y)) +
  geom_point(colour = "grey70", size = 5) +
  geom_point(size = 0.5, colour = "red") +
  geom_text(aes(label = label, hjust = hj, vjust = vj), size = 4) +
  labs(x = NULL, y = NULL) 


?annotate

ggplot(tibble(),aes(x = 1,y =1)) + geom_point() +
  annotate("text", x = Inf, y = Inf, label = "Some text", hjust = "top", vjust = "right")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  annotate("text",
           x = Inf, y = Inf,
           label = "Increasing engine size is \nrelated to decreasing fuel economy.", vjust = "top", hjust = "right")



mpg %>% 
  mutate(label = str_c("it is", class)) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label)) +
  facet_wrap(~label)


label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label),
            data = label, vjust = "top", hjust = "right",
            size = 2
  ) +
  facet_wrap(~class)


label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label),
            data = label, vjust = "top", hjust = "right",
            size = 2
  ) +
  facet_wrap(~class)

label <- tibble(
  displ = Inf,
  hwy = Inf,
  class = unique(mpg$class),
  label = str_c("Label for ", class)
)


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label),
            data = label, vjust = "top", hjust = "right",
            size = 3
  ) +
  facet_wrap(~class) #~label


?geom_label


?arrow
#--------------------

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()



ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)



presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y")
?waiver



base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default
base + theme(legend.position = "none")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

ggplot(mtcars, aes(vs, am, colour = factor(cyl))) +
  geom_jitter(alpha = 1/5, width = 0.01, height = 0.01) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()


ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()


ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")



presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))



?scale_colour_gradient()
?scale_fill_gradient()
?scale_colour_gradient2()



df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()


ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()


??scale_color_x

##exercises 28.4.4

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()


?scale_x_continuous

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous("Engine displacement (L)") +
  scale_y_continuous("Highway fuel economy (mpg)")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  ) +
  scale_x_continuous("scale x")



presidential %>% 
  mutate(id = row_number() + 33) %>% 
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date("term", breaks = presidential$start, date_labels = "'%y") + #, minor_breaks = NULL
  scale_color_manual(values = c(Democratic = "blue", Republican = "red")) +
  theme(legend.position = "bottom")

presidential %>% 
  ggplot(aes(x = start, y = name, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name)) +
  scale_x_date("term", breaks = presidential$start, date_labels = "'%y") + #, minor_breaks = NULL
  scale_color_manual(values = c(Democratic = "blue", Republican = "red")) +
  theme(legend.position = "bottom") 
                     o
year_range <- range(presidential$start, presidential$end)
four_years <- make_date(seq(year(year_range[1]), year(year_range[2]), by = 4),1, 20)
four_years
presidential%>% 
  mutate(id = row_number() + 33) %>% 
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date("term", breaks = four_years, date_labels = '%y') + #, minor_breaks = NULL
  scale_color_manual(values = c(Democratic = "blue", Republican = "red")) +
  theme(legend.position = "bottom") +
  labs(title = "Presidents of USA:",
       subtitle = "terms and parties")


ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
#-----------------------------------


ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()



suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")
ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point()
ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point()



x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))
ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() +
  facet_wrap(~class)





ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()






ggplot(mpg, aes(displ, hwy)) + geom_point()

ggsave("my-plot.pdf")
?ggsave






#=============================== chapter 29 =============================


rmarkdown::render("diamond-sizes.Rmd", output_format = "word_document")

?rmarkdown::html_document



file.edit("testing flexdashboard.Rmd")


library(leaflet)
leaflet() %>%
  setView(174.764, -36.877, zoom = 16) %>% 
  addTiles() %>%
  addMarkers(174.764, -36.877, popup = "Maungawhau") 


file.edit("testing shiny.rmd")
