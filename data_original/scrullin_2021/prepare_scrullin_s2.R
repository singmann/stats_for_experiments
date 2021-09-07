
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
  select(id:earwormrightnow, n1_se, n1_tstmin) %>% 
  rename(
    sleep_efficiency = n1_se,
    sleep_time = n1_tstmin,
    earworm_falling_asleep = earwormlastnight,
    earworm_night = earwormwaso,
    earworm_morning = earwormthismorning,
    earworm_control = earwormrightnow
  ) %>% 
  select(id, group, starts_with("earworm"), starts_with("sleep_"), everything())

earworm_select

earworm_select %>% 
  select(starts_with("Earworm"))

earworm_select %>% 
  group_by(group) %>% 
  summarise(across(starts_with("Earworm"), mean))

earworm_select %>% 
  group_by(group) %>% 
  summarise(across(starts_with("sleep_"), .fns = c(mean = mean, sd = sd)))

write_csv(earworm_select, "earworm_study.csv")
