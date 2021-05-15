
setwd("scripts/")
source("01_read_data_demog.R")

library("tidyverse")

summary(mydata$obj_raw)
summary(mydata$open_raw)

summary(mydata$obj_index_prop)
summary(mydata$open_index_prop)

laptop_urry <- mydata %>% 
  #select(ID, condition_label, whichtalklabel, obj_index, open_index) %>% 
  mutate(overall = (obj_index_prop + open_index_prop)/2 * 100) %>% 
  mutate(factual = obj_index_prop * 100,
         conceptual = open_index_prop * 100) %>% 
  mutate(pid = factor(ID)) %>% 
  mutate(condition = factor(condition_label, 
                            levels = c("laptop", "longhand")),
         talk = factor(whichtalklabel)) %>% 
  select(pid, condition, talk, overall, factual, conceptual)

nrow(laptop_urry)

laptop_urry %>% 
  group_by(condition) %>% 
  count()

ggplot(laptop_urry, aes(x = condition, y = overall)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1)

save(laptop_urry, file = "../../../data/laptop_urry.rda")

