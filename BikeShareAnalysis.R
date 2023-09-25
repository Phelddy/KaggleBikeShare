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

#recipe for putting together the first count or filtering
my_recipe <- recipe(count ~ datetime + season + holiday + workingday
                    + weather + temp + humidity,
                     data=bike_tr) %>% # Set model formula
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
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))


vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

#Poisson regression work
library(poissonreg)

#poisson modifiations
pois_mod <- poisson_reg() %>%
  set_engine("glm")

#workflow adding together this previous recipie
bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bike_tr)

#predictions
bike_predictions <- predict(bike_pois_workflow,
                            new_data = bike_te)

#setting everything to zero (unnecessary for poisson)
bike_predictions[bike_predictions < 0] <- 0 
bike_predictions <- floor(bike_predictions)

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

logbike_tr <- bike_tr %>% mutate(count = log(count))

#penalized code
#recipe for putting together the first count or filtering
my_recipe <- recipe(count ~ datetime + season + holiday + workingday
                    + weather + temp + humidity,
                    data=logbike_tr) %>% # Set model formula
  step_mutate(weather_cat = as_factor(weather)) %>% #changes weather from numeric to factor
  step_poly(temp, degree=2) %>% #Create polynomial expansion of temperature
  step_time(datetime, features=c("hour", "minute")) %>% #break out datetime
  step_dummy(all_nominal_predictors()) %>% #create dummy variables (for weather)
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_rm(weather, datetime) %>% #removes weather since it is now categorized
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using recipie
bake(prepped_recipe, new_data = bike_te)   

preg_model <- linear_reg(penalty = .05, mixture = .5) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = logbike_tr)

predict(preg_wf, new_data = bike_te)

#predictions
bike_predictions <- exp(predict(preg_wf,
                            new_data = bike_te))

#setting everything to zero (unnecessary for poisson)
bike_predictions[bike_predictions < 0] <- 0 
bike_predictions <- floor(bike_predictions)

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

#FRIDAY
#additional testing on (9-22-2023)
logbike_tr <- bike_tr %>% mutate(count = log(count))
logbike_tr <- logbike_tr %>% select(-casual, -registered)

#penalized code
#recipe for putting together the first count or filtering
my_recipe <- recipe(count ~ ., data=logbike_tr) %>% # Set model formula
  step_mutate (weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rm(datetime)
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using recipie
bake(prepped_recipe, new_data = bike_te)
bake(prepped_recipe, new_data = logbike_tr)

preg_model <- linear_reg(penalty = .01, mixture = 0) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = logbike_tr)

predict(preg_wf, new_data = bike_te)

#predictions
bike_predictions <- exp(predict(preg_wf,
                                new_data = bike_te))

#setting everything to zero (unnecessary for poisson)
bike_predictions[bike_predictions < 0] <- 0 
bike_predictions <- floor(bike_predictions)

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")


#TUNING PARAMETERS 9/25/2023
library(tidymodels)
library(poissonreg)


#penalized regression model
preg_model <- linear_reg(penalty = tune(),
                         mixture = tune()) %>%
  set_engine("glmnet")


preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model)

#creating the tuning grid. Choosing five levels for each for 25 options
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

#kfold is vfold in tidy models
folds <- vfold_cv(logbike_tr, v = 5, repeats = 1)

#This will actually create all results for CV
CV_results <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq))

#plots CV results
collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color = factor(mixture))) +
  geom_line()

#this allows for the running of the best possible model
bestTune <- CV_results %>%
  select_best("rmse")

final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=logbike_tr)

bike_predictions <- exp(predict(final_wf,
                                new_data = bike_te))

#setting everything to zero (unnecessary for poisson)
bike_predictions[bike_predictions < 0] <- 0 
bike_predictions <- floor(bike_predictions)

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

