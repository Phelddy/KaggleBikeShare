library(tidyverse)
library(tidymodels)
library(vroom)
library(stacks)

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
logbike_tr <- bike_tr %>% mutate(count = log(count))
logbike_tr <- logbike_tr %>% select(-casual, -registered)
# CV and control grid
folds <- vfold_cv(logbike_tr, v = 5, repeats = 1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()


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

my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

lin_reg_model <-
  fit_resamples(
    bike_workflow,
    resamples = folds,
    metrics = metric_set(rmse, mae, rsq),
    control = tunedModel)

# bike_predictions <- predict(bike_workflow, new_data = bike_te)
#set all negative numbers so that they are zero
# bike_predictions[bike_predictions < 0] <- 0 
# bike_predictions <- floor(bike_predictions)
# 
# sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
# sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
#   mutate(datetime = as.character(format(datetime)))
# 
# 
# vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

#Poisson regression work
library(poissonreg)

#poisson modifiations
pois_mod <- poisson_reg() %>%
  set_engine("glm")

#workflow adding together this previous recipie
bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod)

pois_reg_model <-
  fit_resamples(
    bike_pois_workflow,
    resamples = folds,
    metrics = metric_set(rmse, mae, rsq),
    control = tunedModel
  )

# #predictions
# bike_predictions <- predict(bike_pois_workflow,
#                             new_data = bike_te)
# 
# #setting everything to zero (unnecessary for poisson)
# bike_predictions[bike_predictions < 0] <- 0 
# bike_predictions <- floor(bike_predictions)
# 
# #formats submissions properly
# sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
# sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
#   mutate(datetime = as.character(format(datetime)))
# 
# #writes onto a csv
# vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")
# 
# logbike_tr <- bike_tr %>% mutate(count = log(count))

#penalized code
#recipe for putting together the first count or filtering

# preg_model <- linear_reg(penalty = .05, mixture = .5) %>%
#   set_engine("glmnet")
# 
# preg_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(preg_model) %>%
#   fit(data = logbike_tr)
# 
# predict(preg_wf, new_data = bike_te)
# 
# #predictions
# bike_predictions <- exp(predict(preg_wf,
#                             new_data = bike_te))

# #setting everything to zero (unnecessary for poisson)
# bike_predictions[bike_predictions < 0] <- 0 
# bike_predictions <- floor(bike_predictions)
# 
# #formats submissions properly
# sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
# sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
#   mutate(datetime = as.character(format(datetime)))
# 
# #writes onto a csv
# vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

#FRIDAY
#additional testing on (9-22-2023)
# logbike_tr <- bike_tr %>% mutate(count = log(count))
# logbike_tr <- logbike_tr %>% select(-casual, -registered)
# 
# #penalized code
# #recipe for putting together the first count or filtering
# my_recipe <- recipe(count ~ ., data=logbike_tr) %>% # Set model formula
#   step_mutate (weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
#   step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
#   step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
#   step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
#   step_time(datetime, features="hour") %>%
#   step_mutate(datetime_hour = as.factor(datetime_hour)) %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_rm(datetime)
# prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using recipie
# bake(prepped_recipe, new_data = bike_te)
# bake(prepped_recipe, new_data = logbike_tr)
# 
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
tuning_grid_1 <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

#kfold is vfold in tidy models
folds <- vfold_cv(logbike_tr, v = 5, repeats = 1)




#This will actually create all results for CV
CV_results <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid_1,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedModel)

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

###9-27-2023
###Regression Trees
library(tidyverse)
library(tidymodels)
library(vroom)

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

tree_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(), 
                        min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

## Create Workflow
tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(tree_mod)

## Set up tuning Grid
tuning_grid <- grid_regular(tree_depth(range = c(20, 30)), 
                            cost_complexity(range = c(-10, -1)), 
                            min_n(range = c(20, 30)))

## Set up K-fold CV
#kfold is vfold in tidy models
folds <- vfold_cv(logbike_tr, v = 5, repeats = 1)

#This will actually create all results for CV
CV_results <- tree_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq))

## Find best tuning parameters
bestTune <- CV_results %>%
  select_best("rmse")


## Finalize workflow and predict
final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=logbike_tr)

bike_predictions <- exp(predict(final_wf,
                                new_data = bike_te))

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

###9-29-2023
###Random Forests
library(tidyverse)
library(tidymodels)
library(vroom)

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

forest_mod <- rand_forest(mtry = tune(),
                          min_n = tune(), 
                          trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Create Workflow
forest_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(forest_mod)

## Set up tuning Grid
tuning_grid <- grid_regular(mtry(range = c(1, 15)),
                            min_n(),
                            levels = 2)

## Set up K-fold CV
#kfold is vfold in tidy models
folds <- vfold_cv(logbike_tr, v = 5, repeats = 1)

#This will actually create all results for CV
CV_results_forest <- forest_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq),
            control = untunedModel)

## Find best tuning parameters
bestTune <- CV_results %>%
  select_best("rmse")


## Finalize workflow and predict
final_wf <-
  forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=logbike_tr)

bike_predictions <- exp(predict(final_wf,
                                new_data = bike_te))

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")

my_stack <- stacks() %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(pois_reg_model) %>%
  add_candidates(CV_results) %>%
  add_candidates(CV_results_forest)

stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()

stackData <- as_tibble(my_stack)

bike_predictions <- exp(predict(stack_mod,
                                new_data = bike_te))

#formats submissions properly
sumbission <- bind_cols(bike_te %>% select(datetime), bike_predictions)
sumbission <- sumbission %>% rename("datetime" = "datetime", "count" = ".pred")  %>% 
  mutate(datetime = as.character(format(datetime)))

#writes onto a csv
vroom_write(sumbission, "./KaggleBikeShare/submission.csv", col_names = TRUE, delim = ", ")
