
library("afex")
library("tidyverse")
library("broom")
library("binom")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))

d1a <- readxl::read_xlsx("Experiment1_Raw.xlsx") %>% 
  mutate(subno = factor(paste0("e1a_", subno)),
         response = factor(response),  #levels = c("reject", "accept")
         condition = round(condition, 2)) %>% 
  separate("condition", c("gain_c", "loss_c"), remove = FALSE) %>% 
  mutate(gain_c = factor(gain_c), 
         loss_c = factor(loss_c, labels = c("20", "40"))) %>% 
  mutate(condition = factor(condition))

d1b <- readxl::read_xlsx("Experiment1b_Raw.xlsx") %>% 
  mutate(subno = factor(paste0("e1b_", subno)),
         response = factor(response),  #levels = c("reject", "accept")
         condition = round(condition, 2)) %>% 
  separate("condition", c("gain_c", "loss_c"), remove = FALSE) %>% 
  mutate(gain_c = factor(gain_c), 
         loss_c = factor(loss_c, labels = c("20", "40"))) %>% 
  mutate(condition = factor(condition))
  
d1 <- bind_rows(d1a, d1b) %>% 
  mutate(gamble = factor(case_when(
    gain == loss ~ "EV = 0",
    gain < loss ~ "EV negative",
    gain > loss ~ "EV positive",
    TRUE ~ "error"
  ))) %>% 
  mutate(
    gamble2 = factor(paste0("g", gain, ":l", loss))
  )

str(d1)

d1 %>% 
  filter(loss == gain, loss %in% c(12, 16, 20)) %>%
  group_by(loss_c, gain_c) %>% 
  summarise(n = length(unique(subno)), 
            acc = mean(resp),
            succ = sum(resp), 
            n = n())

### analysis "normal condition" (i.e., 40-gain, 20-loss) as in Tom et al.

d1 %>% 
  filter(gain_c == "40", loss_c == "20") %>% 
  filter(gamble == "EV = 0") %>% 
  group_by(gamble2) %>% 
  summarise(acc = mean(resp), 
            succ = sum(resp), 
            n = n())

d1 %>% 
  filter(gain_c == "40", loss_c == "20") %>% 
  group_by(gamble) %>% 
  summarise(acc = mean(resp), 
            succ = sum(resp), 
            n = n())


### old stuff

da <- d1 %>%
  group_by(condition, loss, gain) %>% 
  summarise(acc = mean(resp))

da %>% 
  ggplot(aes(x = interaction(loss, gain), y = acc)) +
  geom_point() +
  facet_wrap("condition")

da %>% 
  ggplot(aes(x = condition, y = acc)) +
  geom_point() +
  facet_grid(rows = vars(loss), cols = vars(gain))

da %>% 
  group_by(loss, gain) %>% 
  mutate(n = n()) %>% 
  filter(n == 4) %>% 
  ggplot(aes(x = condition, y = acc)) +
  geom_point() +
  facet_grid(rows = vars(loss), cols = vars(gain), labeller = label_both)


da2 <- d1 %>%
  mutate(gamble = factor(case_when(
    gain == loss ~ "EV = 0",
    gain < loss ~ "EV negative",
    gain > loss ~ "EV positive",
    TRUE ~ "error"
  ))) %>% 
  filter((gain %in% c(12, 16, 20)) & loss %in% c(12, 16, 20)) %>% 
  group_by(condition, gamble) %>% 
  summarise(acc = mean(resp), 
            succ = sum(resp), 
            n = n()) %>% 
  ungroup

da2$lower <- binom.confint(x = da2$succ, n = da2$n, methods = "profile")$lower
da2$upper <- binom.confint(x = da2$succ, n = da2$n, methods = "profile")$upper

da2 %>% 
  ggplot(aes(x = condition, y = acc)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap("gamble")
ggsave("shared_gamble_choices.png", dpi = 500)

d1l <- d1 %>% 
  filter((gain %in% c(12, 16, 20)) & loss %in% c(12, 16, 20)) %>%
  group_by(gain_c, loss_c, condition) %>% 
  nest() %>% 
  mutate(glm = map(data, ~tidy(glm(response ~ gain + loss, ., 
                                   family = binomial))))
d1r <- d1l %>% 
  unnest(glm) %>% 
  select(condition:estimate) %>% 
  select(-data) %>% 
  pivot_wider(names_from = "term", values_from = "estimate") %>% 
  mutate(loss_aversion = loss / -gain) %>% 
  filter(loss_aversion > -5, loss_aversion < 5) %>% 
  rename(Gain = gain_c,
         Loss = loss_c)
d1r


d1ls <- d1 %>% 
  filter((gain %in% c(12, 16, 20)) & loss %in% c(12, 16, 20)) %>%
  group_by(gain_c, loss_c, condition) %>% 
  nest() %>% 
  mutate(glm = map(data, ~tidy(glm(response ~ 0 + gain + loss, ., 
                                   family = binomial))))
d1rs <- d1ls %>% 
  unnest(glm) %>% 
  select(condition:estimate) %>% 
  select(-data) %>% 
  pivot_wider(names_from = "term", values_from = "estimate") %>% 
  mutate(loss_aversion = loss / -gain) %>% 
  filter(loss_aversion > -5, loss_aversion < 5) %>% 
  rename(Gain = gain_c,
         Loss = loss_c)
d1rs


