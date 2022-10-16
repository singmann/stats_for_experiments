## ---- message=FALSE-----------------------------------------------------
library("tidyverse")
ws1a <- read_csv("data/ws2015_exp1a.csv")
ws1b <- read_csv("data/ws2015_exp1b.csv")
ws1a <- ws1a %>%
  mutate(subno = factor(paste0("e1a_", subno)))
ws1b <- ws1b %>%
  mutate(subno = factor(paste0("e1b_", subno)))
ws1 <- bind_rows(ws1a, ws1b)
ws1 <- ws1 %>% 
  mutate(
    response = factor(response, levels = c("reject", "accept")),
    condition = factor(
      condition, 
      levels = c(40.2, 20.2, 40.4, 20.4), 
      labels = c("-$20/+$40", "-$20/+$20", "-$40/+$40", "-$40/+$20")
    )
  )
glimpse(ws1)


## -----------------------------------------------------------------------
lot_sum <- ws1 %>% 
  group_by(gain) %>% 
  summarise(mean_acceptance = mean(resp))
lot_sum


## ----first-fig----------------------------------------------------------
ggplot(lot_sum, aes(x = gain, y = mean_acceptance)) +
  geom_point()


## -----------------------------------------------------------------------
ggplot(lot_sum, aes(x = gain, y = mean_acceptance)) +
  geom_point() +
  geom_line()


## -----------------------------------------------------------------------
lot_sum2 <- ws1 %>% 
  filter(condition %in% c("-$20/+$20", "-$40/+$40")) %>% 
  group_by(loss, gain) %>% 
  summarise(mean_acceptance = mean(resp))
lot_sum2


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance)) +
  geom_point()


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance)) +
  geom_point(alpha = 0.25)


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance)) +
  geom_jitter(width = 0.4, alpha = 0.25)


## -----------------------------------------------------------------------
lot_sum2 <- lot_sum2 %>% 
  mutate(ev = case_when(
    gain == loss ~ "EV = 0",
    gain < loss ~ "EV negative",
    gain > loss ~ "EV positive"
  )) %>% 
  mutate(
    ev = factor(ev, levels = c("EV negative", "EV = 0", "EV positive"))
  )
lot_sum2


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance, colour = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5)


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance, colour = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw()


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance, shape = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw()


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance, 
                     shape = ev, colour = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw()


## -----------------------------------------------------------------------
ggplot(lot_sum2, aes(x = gain, y = mean_acceptance, 
                     colour = ev, size = loss)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw()


## -----------------------------------------------------------------------
lot_sum_all <- ws1 %>% 
  group_by(condition, loss, gain) %>% 
  summarise(mean_acceptance = mean(resp)) %>% 
  mutate(ev = case_when(
    gain == loss ~ "EV = 0",
    gain < loss ~ "EV negative",
    gain > loss ~ "EV positive"
  )) %>% 
  mutate(
    ev = factor(ev, levels = c("EV negative", "EV = 0", "EV positive"))
  )
lot_sum_all


## ---- fig.asp = 0.8-----------------------------------------------------
ggplot(lot_sum_all, aes(x = gain, y = mean_acceptance, 
                        colour = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw() +
  facet_wrap(vars(condition)) 


## ---- fig.asp = 0.9-----------------------------------------------------
ggplot(lot_sum_all, aes(x = gain, y = mean_acceptance, 
                        colour = ev)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  ggthemes::scale_colour_colorblind() +
  theme_bw() +
  facet_wrap(vars(condition)) + 
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))


## -----------------------------------------------------------------------
part_sum <- ws1 %>%
  group_by(condition, subno) %>%   # aggregate for both, condition and subno
  summarise(mean_acc = mean(resp)) 
part_sum


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_point()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_jitter(alpha = 0.25)


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_jitter(width = 0.2, alpha = 0.25)


## -----------------------------------------------------------------------
library("ggbeeswarm")


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_beeswarm()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_quasirandom()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_boxplot()


## ----annotated-boxplot, fig.cap='Annotated variant of box plot.', echo=FALSE----
knitr::include_graphics("figures/annotated_boxplot.jpeg")


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_violin()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


## ----dynamite-plot, echo=FALSE, fig.cap="Example of a dynamite plot. This plot type is never really recommended."----
ggplot(part_sum) +
  aes(x = condition, y = mean_acc) +
  stat_summary(geom = "bar", fun = "mean", width = 0.8) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2)


## ----annotated-errorbars, echo=FALSE------------------------------------
knitr::include_graphics("figures/annotated-error-bars.jpeg")


## ----datasaurus, fig.cap='The "datasaurus dozen". Two variables that maintain their summary statistics while dramatically changing the shape of the data points. From Justin Matejka and George Fitzmaurice: https://www.autodesk.com/research/publications/same-stats-different-graphs', echo=FALSE----
knitr::include_graphics("figures/DinoSequentialSmaller.gif")


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_quasirandom() +
  stat_summary()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = condition, y = mean_acc)) +
  geom_quasirandom(alpha = 0.2) +
  stat_summary()


## ---- include=FALSE, eval=FALSE-----------------------------------------
## ## not included because probably too complicated here
## ggplot(part_sum, aes(x = condition, y = mean_acc)) +
##   geom_quasirandom(alpha = 0.2) +
##   stat_summary() +
##   stat_summary(aes(group = 1), geom = "line", fun = "mean")


## -----------------------------------------------------------------------
ggplot(part_sum, aes(mean_acc)) +
  geom_histogram()


## -----------------------------------------------------------------------
ggplot(part_sum, aes(mean_acc)) +
  geom_histogram(bins = 50)


## -----------------------------------------------------------------------
ggplot(part_sum, aes(mean_acc)) +
  geom_histogram(bins = 20)


## -----------------------------------------------------------------------
ggplot(part_sum, aes(mean_acc)) +
  geom_histogram(binwidth = 0.1 / 3, boundary = 0)


## -----------------------------------------------------------------------
part_sum <- part_sum %>% 
  mutate(
    condition2 = if_else(condition %in% c("-$20/+$20", "-$40/+$40"), 
                         "symmetric", as.character(condition))
  ) %>% 
  mutate(condition2 = factor(
    condition2, 
    levels = c("-$20/+$40", "symmetric", "-$40/+$20"))
  )


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = mean_acc)) +
  geom_histogram(binwidth = 0.1 / 3, boundary = 0) +
  facet_wrap(vars(condition2))


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = mean_acc, y = after_stat(density))) +
  geom_histogram(binwidth = 0.1 / 3, boundary = 0) +
  facet_wrap(vars(condition2))


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = mean_acc, y = after_stat(density * width))) +
  geom_histogram(binwidth = 0.1 / 3, boundary = 0) +
  facet_wrap(vars(condition2))


## -----------------------------------------------------------------------
ggplot(part_sum, aes(x = mean_acc, y = after_stat(density * width))) +
  geom_histogram(binwidth = 0.1 / 3, boundary = 0) +
  facet_wrap(vars(condition2)) +
  labs(y = "proportion")


## ---- include=FALSE, eval=FALSE-----------------------------------------
## ## just to check
## part_sum %>%
##   filter(condition == "-$20/+$40") %>%
##   ggplot(aes(x = mean_acc, y = after_stat(count/sum(count)))) +
##   geom_histogram(binwidth = 0.1 / 3, boundary = 0)  +
##   coord_cartesian(ylim = c(0, 0.15))

