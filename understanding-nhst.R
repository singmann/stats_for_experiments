## ---- message=FALSE------------------------------------------------------------------
library("tidyverse")
library("afex")
library("ggbeeswarm")  ## only needed later, but already loaded here
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))


## ----sel-dist, message=FALSE, fig.asp=1.2, echo=FALSE, warning=FALSE-----------------
data("laptop_urry")
p1 <- laptop_urry %>% 
  ggplot(aes(x = overall, y = after_stat(density))) +
  geom_histogram(binwidth = 10/3) + 
  labs(x = "Memory scores")

earworm <- read_csv("data/earworm_study.csv")
earworm <- earworm %>% 
  mutate(
    id = factor(id),
    group = factor(group, levels = c("Lyrics", "Instrumental"))
  )
p2 <- earworm %>% 
  ggplot(aes(sleep_time, y = after_stat(density))) +
  geom_histogram(bins = 15) + 
  labs(x = "Sleep time (minutes)")

load("data/pedroni-frey.rda")
p3 <- pedroni_frey %>% 
  ggplot(aes(LOT, y = after_stat(density))) +
  geom_histogram(binwidth = 0.02, boundary = 0) + 
  labs(x = "Lotteries (risk task)")

p4 <- pedroni_frey %>% 
  ggplot(aes(PRI, y = after_stat(density))) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  labs(x = "Risk questionnaire")

#psych::describe(pedroni_frey)

data("births14", package = "openintro")
births14 <- births14 %>% 
  filter(weight > 3.306937) %>%
  mutate(weight_kg = weight * 0.453592)
p5 <-  births14 %>% 
  ggplot(aes(x = weight_kg, y = after_stat(density))) +
  geom_histogram() +
  labs(x = "Birth weight (kg, > 1.5)")
data("speed_gender_height", package = "openintro")
height_f <- speed_gender_height %>% 
  filter(gender == "female") %>% 
  mutate(height_cm = height * 2.54) %>% 
  filter(!is.na(height))

p6 <- height_f %>% 
  ggplot(aes(x = height_cm, y = after_stat(density))) +
  geom_histogram(binwidth = 2.5) +
  labs(x = "Student height (f, cm)")

cowplot::plot_grid(p1, p2, p3, p4, p5, p6, 
                   nrow = 3, labels = "auto", align = "hv")


## ----norm-dist, echo=FALSE-----------------------------------------------------------
df <- data.frame(x = c(-4, 4))
ggplot(df, aes(x = x)) +
  geom_function(fun = dnorm, 
                args = list(mean = 0, sd = 1)) +
  labs(x = "Value of variable", y = "Density")


## ----sel-dist-pdf, message=FALSE, fig.asp=1.2, echo=FALSE, warning=FALSE-------------
vjust1 <- 1.05
vjust2 <- 2.2

p1_dist <- laptop_urry %>% 
  summarise(m = mean(overall), sd = sd(overall))

p1d <- laptop_urry %>% 
  ggplot() +
  geom_histogram(aes(x = overall, y = after_stat(density)), 
                 binwidth = 10/3) + 
  geom_function(fun = dnorm, 
                args = list(mean = p1_dist$m, sd = p1_dist$sd)) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p1_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p1_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Memory scores")

p2_dist <- earworm %>% 
  summarise(m = mean(sleep_time), sd = sd(sleep_time))
p2d <- earworm %>% 
  ggplot(aes(sleep_time, y = after_stat(density))) +
  geom_histogram(bins = 15) + 
  geom_function(fun = dnorm, 
                args = list(mean = p2_dist$m, sd = p2_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p2_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p2_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Sleep time (minutes)")

p3_dist <- pedroni_frey %>% 
  summarise(m = mean(LOT), sd = sd(LOT))
p3d <- pedroni_frey %>% 
  ggplot(aes(LOT, y = after_stat(density))) +
  geom_histogram(binwidth = 0.02, boundary = 0) + 
    geom_function(fun = dnorm, 
                args = list(mean = p3_dist$m, sd = p3_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p3_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p3_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Lotteries (risk task)") +
  coord_cartesian(xlim = c(0.15, 0.8))

p4_dist <- pedroni_frey %>% 
  summarise(m = mean(PRI), sd = sd(PRI))
p4d <- pedroni_frey %>% 
  ggplot(aes(PRI, y = after_stat(density))) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  geom_function(fun = dnorm, 
                args = list(mean = p4_dist$m, sd = p4_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p4_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p4_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Risk questionnaire")

#psych::describe(pedroni_frey)

p5_dist <- births14 %>% 
  summarise(m = mean(weight_kg), sd = sd(weight_kg))
p5d <-  births14 %>% 
  ggplot(aes(x = weight_kg, y = after_stat(density))) +
  geom_histogram() +
  geom_function(fun = dnorm, 
                args = list(mean = p5_dist$m, sd = p5_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p5_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p5_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Birth weight (kg, > 1.5)")

p6_dist <- height_f %>% 
  summarise(m = mean(height_cm), sd = sd(height_cm))
p6d <- height_f %>% 
  ggplot(aes(x = height_cm, y = after_stat(density))) +
  geom_histogram(binwidth = 2.5) +
  geom_function(fun = dnorm, 
                args = list(mean = p6_dist$m, sd = p6_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p6_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p6_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Student height (f, cm)") +
  coord_cartesian(xlim = c(120, 200))

cowplot::plot_grid(p1d, p2d, p3d, p4d, p5d, p6d, 
                   nrow = 3, labels = "auto", align = "hv")


## ----sim_multi, echo = FALSE, fig.asp=1.2--------------------------------------------
set.seed(345678901)
ns <- c(25, 50, 100, 500, 1000, 10000)
sim_dat <- map_dfr(ns, ~tibble(
  n = paste0("N = ", .),
  x = rnorm(.)
)) 
sim_dat$n <- fct_inorder(factor(sim_dat$n))
ggplot(sim_dat, aes(x = x, y = after_stat(density))) +
  geom_histogram(bins = 40) +
  geom_function(fun = dnorm, 
                args = list(mean = 0, sd = 1), 
                inherit.aes = FALSE) +
  labs(x = "Simulated data") +
  facet_wrap("n", as.table = TRUE, ncol = 2)


## ----sim1----------------------------------------------------------------------------
df_sim <- tibble(
  simulated = rnorm(100, mean = 0, sd = 1)
)
ggplot(df_sim) +
  geom_histogram(aes(x = simulated, y = after_stat(density)), bins = 40) +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  labs(x = "Simulated data") 


## ---- echo=FALSE---------------------------------------------------------------------
vjust1 <- 1.05
vjust2 <- 2.2
data("fhch2010")
pnon0_dist <- fhch2010 %>% 
  summarise(m = mean(rt), sd = sd(rt))
pnon0 <- fhch2010 %>% 
  ggplot(aes(rt, y = after_stat(density))) +
  geom_histogram(binwidth = 0.1, boundary = 0) +
  geom_function(fun = dnorm, 
                args = list(mean = pnon0_dist$m, sd = pnon0_dist$sd), 
                inherit.aes = FALSE) +
  labs(x = "Response time (seconds)") +
    geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(pnon0_dist$m, digits = 2, 
                                       format = "f")), 
                x = Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = 1.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(pnon0_dist$sd, digits = 2, 
                                       format = "f")), 
                x = Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = 1.05, vjust = vjust2, parse = TRUE)
data("absenteeism", package = "openintro")
pnon1_dist <- absenteeism %>%
  summarise(m = mean(days), sd = sd(days))
pnon1 <- absenteeism %>%
  ggplot(aes(days, y = after_stat(density))) +
  geom_histogram(binwidth = 2.5, boundary = 0) +
  labs(x = "Days not in school") +
      geom_function(fun = dnorm, 
                args = list(mean = pnon1_dist$m, sd = pnon1_dist$sd), 
                inherit.aes = FALSE) +
    geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(pnon1_dist$m, digits = 2, 
                                       format = "f")), 
                x = Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = 1.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(pnon1_dist$sd, digits = 2, 
                                       format = "f")), 
                x = Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = 1.05, vjust = vjust2, parse = TRUE)
# data("Wage", package = "ISLR")
# pnon2_dist <- Wage %>% 
#   summarise(m = mean(wage), sd = sd(wage))
# pnon2 <- Wage %>% 
#   ggplot(aes(wage, y = after_stat(density))) +
#   geom_histogram(binwidth = 10, boundary = 100) +
#   labs(x = "Yearly income ($1000)") +
#     geom_function(fun = dnorm, 
#                 args = list(mean = pnon2_dist$m, sd = pnon2_dist$sd), 
#                 inherit.aes = FALSE) +
#     geom_label(data=data.frame(), 
#             aes(label = paste0("italic(m) == ", 
#                                formatC(pnon2_dist$m, digits = 2, 
#                                        format = "f")), 
#                 x = Inf, y = Inf), 
#             inherit.aes = FALSE,
#             hjust = 1.05, vjust = vjust1, parse = TRUE) +
#   geom_label(data=data.frame(), 
#             aes(label = paste0("italic(sd) == ", 
#                                formatC(pnon2_dist$sd, digits = 2, 
#                                        format = "f")), 
#                 x = Inf, y = Inf), 
#             inherit.aes = FALSE,
#             hjust = 1.05, vjust = vjust2, parse = TRUE)
cowplot::plot_grid(pnon0, pnon1, labels = "auto", align = "hv")


## ----prob1, echo=FALSE---------------------------------------------------------------
p6_dist <- height_f %>% 
  summarise(m = mean(height_cm), sd = sd(height_cm))
pprob1 <- height_f %>% 
  mutate(half = factor(ifelse(height_cm > p6_dist$m, "upper", "lower"))) %>% 
  ggplot(aes(x = height_cm, y = after_stat(density))) +
  geom_histogram(binwidth = 2.5, 
                 mapping = aes(fill = half, 
                               x = height_cm, y = after_stat(density / 2))) +
  geom_function(fun = dnorm, 
                args = list(mean = p6_dist$m, sd = p6_dist$sd), 
                inherit.aes = FALSE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(m) == ", 
                               formatC(p6_dist$m, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust1, parse = TRUE) +
  geom_label(data=data.frame(), 
            aes(label = paste0("italic(sd) == ", 
                               formatC(p6_dist$sd, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = Inf), 
            inherit.aes = FALSE,
            hjust = -0.05, vjust = vjust2, parse = TRUE) +
  labs(x = "Student height (f, cm)") +
  coord_cartesian(xlim = c(120, 200)) +
  ggthemes::scale_fill_tableau(palette = "Color Blind") +
  theme(legend.position = "none")


tmp_cols <- ggthemes::tableau_color_pal(palette = "Color Blind")(2)

pprob2 <- height_f %>% 
  ggplot(aes(x = height_cm)) +
  geom_function(fun = dnorm, 
                args = list(mean = p6_dist$m, sd = p6_dist$sd), 
                inherit.aes = FALSE) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean = p6_dist$m, sd = p6_dist$sd),
                xlim = c(min(height_f$height_cm), p6_dist$m), 
                fill = tmp_cols[1]) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean = p6_dist$m, sd = p6_dist$sd),
                xlim = c(p6_dist$m, max(height_f$height_cm)),
                fill = tmp_cols[2]) +
  coord_cartesian(xlim = c(120, 200))
cowplot::plot_grid(pprob1, pprob2, labels = "auto", align = "hv")


## ----prob2, echo=FALSE---------------------------------------------------------------
height_f %>% 
  ggplot(aes(x = height_cm)) +
  geom_function(fun = dnorm, 
                args = list(mean = p6_dist$m, sd = p6_dist$sd), 
                inherit.aes = FALSE) +
  stat_function(fun = dnorm,
                geom = "area",
                args = list(mean = p6_dist$m, sd = p6_dist$sd),
                xlim = c(160, 175), 
                fill = tmp_cols[1]) +
  coord_cartesian(xlim = c(120, 200))


## ------------------------------------------------------------------------------------
data("speed_gender_height", package = "openintro")
height_f <- speed_gender_height %>% 
  filter(gender == "female") %>% 
  mutate(height_cm = height * 2.54) %>% 
  filter(!is.na(height))
height_stat <- height_f %>% 
  summarise(m = mean(height_cm), sd = sd(height_cm))
pnorm(175, mean = height_stat$m, sd = height_stat$sd) - 
  pnorm(160, mean = height_stat$m, sd = height_stat$sd)



## ------------------------------------------------------------------------------------
data("laptop_urry")
res1 <- aov_car(overall ~ condition + Error(pid), laptop_urry)
res1


## ------------------------------------------------------------------------------------
emmeans(res1, "condition") %>% 
  pairs()


## ------------------------------------------------------------------------------------
afex_plot(res1, "condition", data_geom = ggbeeswarm::geom_quasirandom)


## ------------------------------------------------------------------------------------
laptop_urry$residuals <- resid(res1)
head(laptop_urry)


## ------------------------------------------------------------------------------------
laptop_urry %>% 
  ggplot(aes(residuals)) +
  geom_histogram()


## ------------------------------------------------------------------------------------
library("gganimate")
library("emmeans")
theme_set(theme_bw(base_size = 25) + 
            theme(legend.position="bottom"))

nsim <- 50
res_sim <- vector("list", nsim)
em_sim <- vector("list", nsim)
for (i in seq_len(nsim)) {
  laptop_urry[[paste0("sim", i)]] <- extraDistr::rlst(
    n = nrow(laptop_urry), df = nrow(laptop_urry) - 2, 
    mu = coef(res1$lm)[1], 
    sigma = sigma(res1$lm)
  )
  res_sim[[i]] <- aov_ez(id = "pid", dv = paste0("sim", i),
                         data = laptop_urry, between = "condition")
  em_sim[[i]] <-  as.data.frame(emmeans(res_sim[[i]], "condition"))
  em_sim[[i]]$simulation <- paste0("sim", i)
}

sim_long <- laptop_urry %>% 
  select(-c(overall, factual, conceptual, residuals)) %>% 
  pivot_longer(cols = starts_with("sim"), 
               names_to = "simulation", values_to = "overall")
em_sim <- bind_rows(em_sim) %>% 
  rename(overall = emmean)
em_diff <- em_sim %>% 
  group_by(simulation) %>% 
  summarise(diff = overall[1] - overall[2])

psim1 <- ggplot(data = sim_long, aes(x = condition, y = overall)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.3, size = 3) +
  geom_pointrange(data = em_sim, mapping = aes(
    ymax = upper.CL, ymin = lower.CL
  ), size = 1.5) + 
  geom_label(data = em_diff, 
             aes(label = paste0("italic(d) == ", 
                               formatC(diff, digits = 2, 
                                       format = "f")), 
                x = -Inf, y = -Inf), 
            inherit.aes = FALSE, size = 8,
            #label.padding = unit(0.5, "lines"),
            label.size = 0.5,
            hjust = -0.05, vjust = -0.1, parse = TRUE) +
  transition_manual(frames = simulation) +
  ggtitle('Simulation {frame} of {nframes}')
#anim_save(filename = "out1.gif", animation = psim1)
df_ani <- em_diff %>% 
  split(.$simulation) %>% 
  accumulate(~ bind_rows(.x, .y)) %>% 
  bind_rows(.id = "simulation") 
psim2 <- ggplot(df_ani, aes(x = diff)) +
  geom_histogram(binwidth = 1, boundary = 0.5) + 
  geom_vline(xintercept = coef(res1$lm)[2] * 2) +
  transition_manual(simulation) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade() +
  ggtitle('Distribution of differences')
#psim2


## ------------------------------------------------------------------------------------
library(magick)
a_gif <- animate(psim1, 
                 fps = 10, 
                 duration = nsim,
        renderer = gifski_renderer("output/animation1.gif"))
b_gif <- animate(psim2, 
                 fps = 10, 
                 duration = nsim,
        renderer = gifski_renderer("output/animation2.gif"))

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)
new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
for(i in 2:nsim){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}
image_write_gif(new_gif, path = "output/sim1.gif", delay = 1/2)


## ------------------------------------------------------------------------------------
desc_urry <- laptop_urry %>% 
  group_by(condition) %>% 
  summarise(m = mean(overall),
            sd = sd(overall), 
            n = n())

get_null_dist_t <- function(nsim, mean, sd, ns) {
  y <- extraDistr::rlst(n = nsim * sum(ns), df = sum(ns) - 2, 
                        mu = mean, sigma = sd)
  group <- letters[rep(c(1,2), ns)]
  sim <- factor(rep(seq_len(nsim), sum(ns)))
  #browser()
  ysplit <- split(x = y, f = sim)
  vapply(ysplit, function(x) diff(tapply(x, INDEX = group, mean)), 
         FUN.VALUE = 0)
}
nulldist_diff_t <- get_null_dist_t(
  nsim = 100000, 
  mean = coef(res1$lm)[1], 
  sd = sigma(res1$lm), 
  ns = desc_urry$n
)
#str(nulldist_diff_t)
full_sim_urry <- tibble(
  diff = nulldist_diff_t
)
full_sim_urry$frame <- rep(1:100, each = length(nulldist_diff_t)/100)
df_full_ani <- full_sim_urry %>% 
  split(.$frame) %>% 
  accumulate(~ bind_rows(.x, .y)) %>% 
  bind_rows(.id = "frame") %>% 
  mutate(frame = as.integer(frame))

hist_full <- ggplot(df_full_ani, aes(x = diff)) +
  geom_histogram(binwidth = 0.2, boundary = 0.5) + 
  geom_vline(xintercept = coef(res1$lm)[2] * 2) +
  transition_manual(frame) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade() +
  ggtitle('Distribution of differences')

anim_save(filename = "output/full_urry.gif", animation = hist_full)


## ------------------------------------------------------------------------------------
tp1 <-  ggplot(full_sim_urry, aes(x = diff)) +
  geom_histogram(binwidth = 0.2, boundary = 0.5) + 
  geom_vline(xintercept = coef(res1$lm)[2] * 2)

tp2 <- full_sim_urry %>% 
  mutate(side = 
           factor(ifelse(diff > coef(res1$lm)[2] * 2, 
                         "larger", "smaller"))) %>% 
  ggplot(aes(x = diff, fill = side)) +
  geom_histogram(binwidth = 0.2, boundary = 0.5) + 
  geom_vline(xintercept = coef(res1$lm)[2] * 2) +
  ggthemes::scale_fill_tableau(palette = "Color Blind") +
  theme(legend.position = "none")
cowplot::plot_grid(tp1, tp2)

mean(df_full_ani$diff > coef(res1$lm)[2] * 2) * 2

