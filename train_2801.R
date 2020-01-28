library(ranger)
library(parsnip)
library(recipes)
library(tidyverse)
library(lubridate)
library(rsample)
library(yardstick)

TRAIN = "training"
SCRATCH = "scratch"
MODEL = "model"

# Util functions --------------------------------------------------------------
add_response = function(tables){
  dates = bind_rows(
    tables$condition_occurrence %>% 
      select(person_id, date = condition_end_date),
    tables$drug_exposure %>% 
      select(person_id, date = drug_exposure_start_date) ,
    tables$measurement %>% 
      select(person_id, date = measurement_date),
    tables$observation_period %>% 
      select(person_id, date = observation_period_end_date) ,
    tables$observation %>% 
      select(person_id, date = observation_date) ,
    tables$procedure_occurrence %>% 
      select(person_id, date = procedure_date),    
    tables$visit_occurrence %>% 
      select(person_id, date = visit_end_date),
    tables$visit_occurrence %>% 
      select(person_id, date = visit_start_date),
    tables$condition_occurrence %>% 
      select(person_id, date = condition_start_date),
    tables$observation_period %>% 
      select(person_id, date = observation_period_start_date)
  )
  
  dates = dates %>% 
    arrange(person_id, desc(date)) %>% 
    distinct(person_id, .keep_all = TRUE)
  
  dates = dates %>%
    left_join(tables$person %>% select(person_id, year_of_birth, month_of_birth, day_of_birth), by = "person_id") %>%
    mutate(time_of_birth = as_date(str_c(year_of_birth,"-", month_of_birth,"-", day_of_birth)))
  
  dates$last_known_age <- floor(time_length(difftime(as.Date(dates$date),as.Date(dates$time_of_birth)), "years"))
  
  tables$response = dates %>% 
    left_join(tables$death %>% select(person_id, death_date), by = "person_id") %>% 
    mutate(death_date = coalesce(death_date, as_date("2030-01-01"))) %>% 
    mutate(response = factor(death_date - date < 183,labels = c("No", "Yes"))) %>% 
    select(person_id, response, last_known_age)
  
  return(tables)
}

# Read data -------------------------------------------------------------------
training = list(
  condition_occurrence = read_csv(str_glue("{TRAIN}/condition_occurrence.csv"), col_types = cols_only(person_id = col_double(), condition_concept_id = col_double(),condition_start_date = col_date(),condition_end_date = col_date())),
  procedure_occurrence = read_csv(str_glue("{TRAIN}/procedure_occurrence.csv"), col_types = cols_only(person_id = col_double(), procedure_concept_id = col_double(), procedure_date = col_date())),
  observation = read_csv(str_glue("{TRAIN}/observation.csv"), col_types = cols_only(person_id = col_double(), observation_concept_id = col_double(), observation_date = col_date())),
  drug_exposure = read_csv(str_glue("{TRAIN}/drug_exposure.csv"), col_types = cols_only(person_id = col_double(), drug_concept_id = col_double(), drug_exposure_start_date = col_date())),
  measurement = read_csv(str_glue("{TRAIN}/measurement.csv"), col_types = cols_only(person_id = col_double(), measurement_date = col_date())),
  visit_occurrence = read_csv(str_glue("{TRAIN}/visit_occurrence.csv"), col_types = cols_only(person_id = col_double(),visit_start_date = col_date(), visit_end_date = col_date())),
  death = read_csv(str_glue("{TRAIN}/death.csv"), col_types = cols_only(person_id = col_double(), death_date = col_date())), 
  observation_period = read_csv(str_glue("{TRAIN}/observation_period.csv"), col_types = cols_only(person_id = col_double(),observation_period_start_date = col_date(), observation_period_end_date = col_date())),
  person = read_csv(str_glue("{TRAIN}/person.csv"), col_types = cols_only(person_id = col_double(), year_of_birth = col_double(),
                                                                          month_of_birth = col_double(),day_of_birth = col_double())),
  person_char = read_csv(str_glue("{TRAIN}/person.csv"), col_types = cols_only(person_id = col_double(),
                                                                               gender_concept_id = col_double(),
                                                                               race_concept_id = col_double()))
)

# Create response variable ----------------------------------------------------
training = add_response(training)

# Create features -------------------------------------------------------------
gender = training$person_char %>% 
  select(person_id, gender_concept_id) %>%
  mutate(gender_concept_id = str_c("Gender", gender_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = gender_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

race = training$person_char %>% 
  select(person_id, race_concept_id) %>%
  mutate(race_concept_id = str_c("Race", race_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = race_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))


condition = training$condition_occurrence %>% 
  select(person_id, condition_concept_id) %>% 
  mutate(condition_concept_id = str_c("Condition",condition_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = condition_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

observation = training$observation %>% 
  select(person_id, observation_concept_id) %>% 
  mutate(observation_concept_id = str_c("Observation",observation_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = observation_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

#Finding important variables (observations, conditions, drugs)-----------------
#Conditions

cond_lr_data = training$response %>% 
  right_join(training$person, by = "person_id") %>% 
  mutate_at(.vars = vars(starts_with("response")), .funs = ~ coalesce(., as.factor("No")))

cond_lr_data = cond_lr_data %>% 
  left_join(condition, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Condition")), .funs = ~ coalesce(., 0))
cond_lr_data = cond_lr_data %>% select(-month_of_birth, -day_of_birth,-year_of_birth, -person_id)
cond_lr_data = na.omit(cond_lr_data)
cond_lr_data = cond_lr_data[,!(names(cond_lr_data) %in% c("NA"))]

mylogit <- glm(response ~ ., data=cond_lr_data, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.5
tmp <- cond_lr_data %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.05
tmp <- tmp %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.005
tmp <- tmp %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.0005
tr <- tr %>% select(names(keep[keep])[-1])

condlogit <- names(keep[keep])
save(condlogit, file = "condlogit.RData")

condition = condition %>% select(condlogit, "person_id")
#Observations
obs_lr_data = training$response %>% 
  right_join(training$person, by = "person_id") %>% 
  mutate_at(.vars = vars(starts_with("response")), .funs = ~ coalesce(., as.factor("No")))

obs_lr_data = obs_lr_data %>% 
  left_join(observation, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Observation")), .funs = ~ coalesce(., 0))
obs_lr_data = obs_lr_data %>% select(-month_of_birth, -day_of_birth,-year_of_birth, -person_id)
obs_lr_data = na.omit(obs_lr_data)
obs_lr_data = obs_lr_data[,!(names(obs_lr_data) %in% c("NA"))]

mylogit <- glm(response ~ ., data=obs_lr_data, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.5
tmp <- obs_lr_data %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.05
tmp <- tmp %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.005
tmp <- tmp %>% select(names(keep[keep])[-1],"response")

mylogit <- glm(response ~ ., data=tmp, family = "binomial")


keep <- coef(summary(mylogit))[,4]<0.0005
tr <- tr %>% select(names(keep[keep])[-1])

obslogit <- names(keep[keep])
save(obslogit, file = "obslogit.RData")

observation = observation %>% select(obslogit, "person_id")
#Binding data -----------------------------------------------------------------

data = training$response %>% 
  right_join(training$person, by = "person_id") %>% 
  mutate_at(.vars = vars(starts_with("response")), .funs = ~ coalesce(., as.factor("No")))


data = data %>% 
  left_join(gender, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Gender")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(race, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Race")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(condition, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Condition")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(observation, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Observation")), .funs = ~ coalesce(., 0))

#Fixing data --------------------------------------------------------------------
data = data %>% select(-month_of_birth, -day_of_birth)
data <- data[!is.na(data$response),]
data <- data[!is.na(data$last_known_age),]

# Train model -----------------------------------------------------------------
data <- data %>% select(-person_id)
set.seed(2020)

#Separation of died patients and alive patients
data_yes <- data[data$response == "Yes",]
data_no <-  data[data$response == "No",]
#"Yes"-weight for weighted random forrest
correction <- floor(nrow(data_no)/nrow(data_yes))
data_no <- data_no[sample(1:nrow(data_no),size = correction*nrow(data_yes)),]
# Calculate the size of each of the data sets:
data_set_sizey <- floor(nrow(data_yes)*0.7)
data_set_sizen <- floor(nrow(data_no)*0.7)
# Generate a random sample of "data_set_size" indexes
indexes_yes <- sample(1:nrow(data_yes), size = data_set_sizey)
indexes_no <- sample(1:nrow(data_no), size = data_set_sizen)
# Assign the data to the correct sets
training <- rbind(data_yes[indexes_yes,], data_no[indexes_no,])
validation <- rbind(data_yes[-indexes_yes,],data_no[-indexes_no,])

model = ranger(response ~ .,
               data=training, num.trees = 1024 ,mtry = 12,
               class.weights = c("No" = 1, "Yes"=correction))

# Save model ------------------------------------------------------------------
features = setdiff(colnames(data), "response")
save(model, features, file = str_glue("{MODEL}/model.RData"))









