
library("tidyverse")
source("fun_willoughby_prep.R")

din <- read_csv("POL_ATT_public_data.csv", lazy = FALSE)[]
str(din)
table(din$INELIG)
any(din$INELIG, na.rm = TRUE)

din <- din %>% 
  filter(is.na(INELIG))

colnames(din) <- tolower(colnames(din))
din <- din %>% 
  select(-relig_d_in)

str_subset(colnames(din), "egalit")
str_subset(colnames(din), "icar") 
str_subset(colnames(din), "relig") 
str_subset(colnames(din), "politic") 

dsel <- din %>% 
  select(
    id, 
    matches(str_subset(colnames(din), "egalit")),
    str_subset(colnames(din), "icar"),
    str_subset(colnames(din), "relig"),
    str_subset(colnames(din), "politic"),
    idsex, ethnic,
    str_subset(colnames(din), "age"),
    str_subset(colnames(din), "^ed_yrs"),
    famid, idsc, idab, idfamab, idfamsex
  ) 

colnames(dsel) <- str_remove(colnames(dsel), "_f3")
colnames(dsel) <- str_replace(colnames(dsel), "_d", "_f")

dsel <- dsel %>% 
  mutate(
    idab = factor(idab, 
                  levels = c(2, 1), 
                  labels = c("biological","adopted")),
    idsex = factor(idsex, levels = c(1, 2, 3, 9), 
                   labels = c("male", "female", "indeterminate", "missing")),
    ethnic = factor(
      ethnic, 
      levels = c(1,2,3,4,5,6,7,8,9,20), 
      labels = c("White", "Asian", "Black", "East Indian", "Hispanic", 
                 "Native American", "South American", "Pacific Islander", 
                 "Mixed", "Other"))
  )

dsel <- dsel %>% 
  mutate(
    egalit_p = make_parent_score(egalit_m, egalit_f),
    egalit_p_code = make_parent_code(egalit_m, egalit_f),
    icar_p = make_parent_score(icar_m, icar_f),
    icar_p_code = make_parent_code(icar_m, icar_f),
    relig_p = make_parent_score(relig_m, relig_f),
    relig_p_code = make_parent_code(relig_m, relig_f),
    politic_p = make_parent_score(politic_m, politic_f),
    politic_p_code = make_parent_code(politic_m, politic_f),
    ed_yrs_p = make_parent_score(ed_yrs_m, ed_yrs_f),
    ed_yrs_p_code = make_parent_code(ed_yrs_m, ed_yrs_f),
  )

mean(is.na(dsel$egalit))
mean(is.na(dsel$egalit_p))
mean(is.na(dsel$politic_p))


dsel <- dsel %>% 
  filter(!is.na(egalit), !is.na(egalit_p),
         !is.na(icar), !is.na(icar_p),
         !is.na(relig), !is.na(relig_p),
         !is.na(politic), !is.na(politic_p))

str(dsel)

GGally::ggpairs(dsel, columns = c(2,3,4,5,6,7, 8, 9, 10))

GGally::ggpairs(dsel, columns = c(2, 5, 8, 11))

GGally::ggpairs(dsel, columns = 
                  c("egalit", "egalit_p", "politic_p", 
                    "icar_p", "relig_p", "ed_yrs_p"))

afex::set_sum_contrasts()
m_egal <- lm(egalit ~ egalit_p*idab, dsel)
summary(m_egal)

m_relig <- lm(relig ~ relig_p*idab, dsel)
summary(m_relig)

m_icar <- lm(icar ~ icar_p*idab, dsel)
summary(m_icar)

dsel %>% 
  group_by(idab) %>% 
  summarise(
    egalit = cor(egalit, egalit_p),
    relig = cor(relig, relig_p),
    politic = cor(politic, politic_p),
    icar = cor(icar, icar_p),
  )

pred_egalit_1 <- lm(egalit ~ egalit_p, dsel)
summary(pred_egalit_1)

pred_egalit_1c <- lm(egalit ~ politic_p, dsel)
summary(pred_egalit_1c)

pred_egalit_1b <- lm(egalit ~ egalit_p + politic_p, dsel)
summary(pred_egalit_1b)

pred_egalit_2 <- lm(egalit ~ egalit_p + icar_p, dsel)
summary(pred_egalit_2)

pred_egalit_3 <- lm(egalit ~ relig_p, dsel)
summary(pred_egalit_3)

dsel3 <- dsel %>% 
  select(id, egalit, politic, egalit_p, politic_p, 
         egalit_p_code, politic_p_code, 
         idsex, ethnic, age, idab, famid)
write_csv(dsel3, "../../data/willoughby_2021_1.csv")

dsel2 <- dsel %>% 
  select(id, egalit, politic, relig, icar, 
         egalit_p, politic_p, relig_p, icar_p,
         egalit_p_code, politic_p_code, relig_p_code, 
         icar_p_code, 
         idsex, ethnic, age, idab, famid)
write_csv(dsel2, "../../data/willoughby_2021_2.csv")

