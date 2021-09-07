
library("tidyverse")

earworm <- haven::read_spss("Earworm_PSG_OSF.sav")

earworm <- earworm %>% 
  mutate(across(
    .cols = c(
      Group, Gender, RaceEthnicity, NativeEnglishSpeaker, Bilingual, 
      #starts_with("Earworm"),
      MusicExperience_Any_YN, MusicExperience_3orMoreyears, 
      MEQ_Type
    ), 
    .fns = as_factor))

colnames(earworm) <- tolower(colnames(earworm))
glimpse(earworm)

earworm_select <- earworm %>% 
  select(id:earwormrightnow, n1_se, n1_tstmin, 
         va2_relaxed, va2_sleepy) %>% 
  rename(
    sleep_efficiency = n1_se,
    sleep_time = n1_tstmin,
    earworm_falling_asleep = earwormlastnight,
    earworm_night = earwormwaso,
    earworm_morning = earwormthismorning,
    earworm_control = earwormrightnow,
    relaxed = va2_relaxed,
    #stressed = va2_stressed, 
    sleepy = va2_sleepy
  ) %>% 
  select(id, group, relaxed, sleepy, 
         starts_with("earworm"), starts_with("sleep_"), everything())

earworm_select

earworm_select %>% 
  select(starts_with("Earworm"))

earworm_select %>% 
  group_by(group) %>% 
  summarise(across(starts_with("Earworm"), mean))

earworm_select %>% 
  group_by(group) %>% 
  summarise(across(starts_with("sleep_"), .fns = c(mean = mean, sd = sd)))

earworm_select %>% 
  group_by(group) %>% 
  summarise(across(c(stressed, relaxed, sleepy), .fns = c(mean = mean, sd = sd)))

earworm_select %>% 
  group_by(group, gender) %>% 
  summarise(across(starts_with("sleep_"), .fns = c(mean = mean, sd = sd)))

earworm_select %>% 
  group_by(raceethnicity) %>% 
  summarise(across(starts_with("sleep_"), 
                   .fns = c(n = ~n(), mean = mean, sd = sd), 
                   .names = ))


write_csv(earworm_select, "earworm_study.csv")

library("afex")

aov_ez("id", "sleep_efficiency", earworm_select, between = "group")

aov_ez("id", "sleep_time", earworm_select, between = "group")

glimpse(earworm)

aov_ez("id", "n1_se", earworm, between = "meq_type")
aov_ez("id", "n1_tstmin", earworm, between = "meq_type")

earworm %>% 
  mutate(bmi = weight_lbs / height_inches^2 * 703) %>% 
  select(n1_se, age, bmi, psqi_global, 
         epworth:ravens) %>% 
  GGally::ggpairs()
