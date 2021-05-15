library(tidyverse)
library(tidymodels)
library(workflows)
library(tune)

dat <- read.csv("winequalityred.csv")

ggplot(dat) +
  geom_histogram(aes(quality))

dat<- dat %>%
  mutate(quality_label = 
           if_else(quality > 6.5, "good", "bad"))

dat <- dat[,-12]

ggplot(dat)+ 
  geom_violin(aes(fixed.acidity, quality_label))

dat <- 
  (rename(dat, quality.label = quality_label ))

sum(is.na(dat))

dat_split <- initial_split(dat)

dat_train <- training(dat_split)
dat_test <- testing(dat_split)

dat_recipe <- 
  recipe(quality.label ~ ., data = dat) %>%
  step_normalize(all_numeric())

log_r_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

log_r_workflow <-
  workflow() %>%
  add_recipe(dat_recipe) %>%
  add_model(log_r_model)

log_r_fit <- 
  log_r_workflow %>%
  last_fit(dat_split)
  
  
our_metrics <- metric_set(accuracy())

test_predictions <- collect_predictions(log_r_fit)

our_metrics(test_predictions, quality.label, .pred_class)