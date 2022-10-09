## ----rstudio1, fig.cap='Screenshot of `RStudio` window with empty `R` script and default layout. The script is shown top left, the `R` console bottom left, the Environment pane in the top right, and the Files and Plot viewer in the bottom right.', echo=FALSE----
knitr::include_graphics("figures/rstudio-window.png")


## ----rstudio2, fig.cap='Screenshot of `RStudio` window with empty `R` script and alternative layout. In this image, console and environment pane have switched their position which allows both `R` script and console to occupy a larger space.', echo=FALSE----
knitr::include_graphics("figures/rstudio-window2.png")


## ----------------------------------------------------------------------
df1a <- read.csv("data/ws2015_exp1a.csv")


## ---- eval=FALSE-------------------------------------------------------
## df1a <- read.csv("https://github.com/singmann/stats_for_experiments/raw/master/data/ws2015_exp1a.csv")


## ----------------------------------------------------------------------
str(df1a)


## ----------------------------------------------------------------------
df1a$subno <- factor(df1a$subno)
df1a$response <- factor(df1a$response, levels = c("reject", "accept"))
df1a$condition <- factor(
  df1a$condition, 
  levels = c(40.2, 20.2, 40.4, 20.4), 
  labels = c("-$20/+$40", "-$20/+$20", "-$40/+$40", "-$40/+$20")
)
str(df1a)


## ----------------------------------------------------------------------
head(df1a)


## ---- eval=FALSE-------------------------------------------------------
## install.packages("tidyverse")


## ----------------------------------------------------------------------
library("tidyverse")


## ----------------------------------------------------------------------
tbl1a <- as_tibble(df1a)
tbl1a


## ----------------------------------------------------------------------
tbl1a <- read_csv("data/ws2015_exp1a.csv")
tbl1a


## ----------------------------------------------------------------------
tbl1a <- read_csv("data/ws2015_exp1a.csv", show_col_types = FALSE)
tbl1a


## ----------------------------------------------------------------------
# step 1: get minimum and maximum and save in temporary variables
tmp <- range(tbl1a$loss)
tmp # print minimum and maximum, to check everything is okay
#step 2: calculate mean of minimum and maximum:
mean(tmp)


## ----------------------------------------------------------------------
mean(range(tbl1a$loss))


## ----------------------------------------------------------------------
tbl1a$loss %>% 
  range() %>% 
  mean()


## ----------------------------------------------------------------------
loss_midpoint <- tbl1a$loss %>%
  range() %>% 
  mean()
loss_midpoint


## ----pipe-screenshot, fig.cap='Comparison of execution order without pipe (upper part) or with pipe (lower part). From: https://evalsp21.classes.andrewheiss.com/projects/01_lab/slides/01_lab.html#116', echo=FALSE----
knitr::include_graphics("figures/pipe_example.jpg")


## ---- message=FALSE----------------------------------------------------
tbl1a <- read_csv("data/ws2015_exp1a.csv")
tbl1a <- tbl1a %>% 
  mutate(
    subno = factor(subno),
    response = factor(response, levels = c("reject", "accept")),
    condition = factor(
      condition, 
      levels = c(40.2, 20.2, 40.4, 20.4), 
      labels = c("-$20/+$40", "-$20/+$20", "-$40/+$40", "-$40/+$20")
    )
  )
tbl1a
 


## ----------------------------------------------------------------------
tbl1a %>% 
  summarise(mean_acc = mean(resp))


## ----------------------------------------------------------------------
tbl1a %>% 
  summarise(
    mean_acc = mean(resp),
    sd_acc = sd(resp),  ## sd() returns the standard deviation
    mean_pot_loss = mean(loss),
    mean_pot_gain = mean(gain)
  )


## ----------------------------------------------------------------------
tbl1a %>%
  filter(condition == "-$20/+$40") %>% 
  summarise(
    mean_acc = mean(resp),
    sd_acc = sd(resp),
    mean_pot_loss = mean(loss),
    mean_pot_gain = mean(gain)
  )


## ----------------------------------------------------------------------
symm1 <- tbl1a %>%
  filter(loss == gain)
symm1


## ----------------------------------------------------------------------
symm2 <- tbl1a %>%
  filter(!(loss != gain))
all.equal(symm1, symm2)


## ----------------------------------------------------------------------
tbl1a %>%
  filter(loss == gain) %>% 
  select(loss, gain, condition) %>% 
  unique() %>% 
  arrange(condition) %>% 
  print(n = Inf)


## ----------------------------------------------------------------------
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  select(loss, gain, condition) %>% 
  unique() %>% 
  arrange(condition) 


## ----------------------------------------------------------------------
## "loss aversion" condition:
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  filter(condition == "-$20/+$40") %>% 
  summarise(mean_acc = mean(resp))

## "loss seeking" condition:
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  filter(condition == "-$40/+$20") %>% 
  summarise(mean_acc = mean(resp))


## ----------------------------------------------------------------------
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  group_by(condition) %>% 
  summarise(mean_acc = mean(resp))


## ----------------------------------------------------------------------
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  group_by(condition, loss, gain) %>% 
  summarise(mean_acc = mean(resp)) %>% 
  print(n = Inf)


## ----------------------------------------------------------------------
tbl1a %>%
  filter(loss == gain, loss %in% c(12, 16, 20)) %>% 
  group_by(condition, loss, gain) %>% 
  summarise(mean_acc = mean(resp)) %>% 
  ungroup()


## ----------------------------------------------------------------------
tbl1a %>% 
  group_by(condition, subno) %>% 
  summarise(n = n())

tbl1a %>% 
  count(condition, subno)


## ----------------------------------------------------------------------
tbl1a %>% 
  group_by(condition, subno) %>% 
  summarise(n = n()) %>% 
  group_by(condition) %>% 
  summarise(
    n_participants = n(),
    all_64 = all(n == 64)
    )

