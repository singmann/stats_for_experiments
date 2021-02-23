
setwd("scripts/")
source("01_read_data_demog.R")

library("tidyverse")

summary(mydata$obj_raw)
summary(mydata$open_raw)

summary(mydata$obj_index)
summary(mydata$open_index)

laptop <- mydata %>% 
  select(ID, condition_label, whichtalklabel, obj_index, open_index) %>% 
  mutate(performance = obj_index + open_index) %>% 
  mutate(pid = factor(ID)) %>% 
  mutate(condition = factor(condition_label, 
                            levels = c("laptop", "longhand")),
         talk = factor(whichtalklabel)) %>% 
  select(pid, condition, talk, performance)

ggplot(laptop, aes(x = condition, y = performance)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter()

save(laptop, file = "../../../data/urry2021_data.rda")

