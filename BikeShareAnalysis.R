library(tidyverse)
library(tidymodels)
library(vroom)

#read in the training data
bike_tr <- vroom("./KaggleBikeShare/train.csv")
bike_te <- vroom("./KaggleBikeShare/test.csv")


#isolating problematic weather value and changing to 3
isolated_weather <- bike_tr %>% filter(weather == 4) %>% mutate(weather = 3)
#replacing row in original dataset
bike_tr <- bind_rows(bike_tr %>% filter(weather != 4), isolated_weather)

#isolating problematic weather value and changing to 3
isolated_weather <- bike_te %>% filter(weather == 4) %>% mutate(weather = 3)
#replacing row in original dataset
bike_te <- bind_rows(bike_te %>% filter(weather != 4), isolated_weather)


bike_tr %>% mutate(weather_cat = as_factor(weather)) %>% select(weather_cat)

my_recipe <- recipe(count ~ datetime + season + holiday + workingday
                    + weather + temp + humidity,
                     data=bike) %>% # Set model formula
  step_mutate(weather_cat = as_factor(weather)) %>% #changes weather from numeric to factor
  step_poly(temp, degree=2) %>% #Create polynomial expansion of temperature
  step_time(datetime, features=c("hour", "minute")) %>% #break out datetime
  step_dummy(all_nominal_predictors()) %>% #create dummy variables (for weather)
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_rm(weather) #removes weather since it is now categorized
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using recipie
bake(prepped_recipe, new_data = bike_te) 

my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_tr)

bike_predictions <- predict(bike_workflow, new_data = bike_te)
#set all negative numbers so that they are zero
bike_predictions[bike_predictions < 0] <- 0 
bike_predictions <- floor(bike_predictions)

sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred") %>% mutate(datetime = as.character(datetime))


vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

