library(parsnip)
library(recipes)
library(tidyverse)
library(ranger)

# INFER = "Data/2019-10-07/OMOPchallenge/evaluation/"
# SCRATCH = "Data/2019-10-17/SCRATCH/"
# MODEL = "Data/2019-10-22/scratch/"
# OUTPUT = "Data/2019-10-17/SCRATCH/"

INFER = "infer"
SCRATCH = "scratch"
MODEL = "model"
OUTPUT = "output"

# Read model ------------------------------------------------------------------
load(str_glue("{MODEL}/model.RData"), verbose = TRUE)

# Read evaluation data --------------------------------------------------------
training = list(
  condition_occurrence = read_csv(str_glue("{INFER}/condition_occurrence.csv"), col_types = cols_only(person_id = col_double(), condition_concept_id = col_double(),condition_start_date = col_date(),condition_end_date = col_date())),
  procedure_occurrence = read_csv(str_glue("{INFER}/procedure_occurrence.csv"), col_types = cols_only(person_id = col_double(), procedure_concept_id = col_double(), procedure_date = col_date())),
  observation = read_csv(str_glue("{INFER}/observation.csv"), col_types = cols_only(person_id = col_double(), observation_concept_id = col_double(), observation_date = col_date())),
  drug_exposure = read_csv(str_glue("{INFER}/drug_exposure.csv"), col_types = cols_only(person_id = col_double(), drug_concept_id = col_double(), drug_exposure_start_date = col_date())),
  measurement = read_csv(str_glue("{INFER}/measurement.csv"), col_types = cols_only(person_id = col_double(), measurement_date = col_date())),
  visit_occurrence = read_csv(str_glue("{INFER}/visit_occurrence.csv"), col_types = cols_only(person_id = col_double(),visit_start_date = col_date(), visit_end_date = col_date())),
  death = read_csv(str_glue("{INFER}/death.csv"), col_types = cols_only(person_id = col_double(), death_date = col_date())), 
  observation_period = read_csv(str_glue("{INFER}/observation_period.csv"), col_types = cols_only(person_id = col_double(),observation_period_start_date = col_date(), observation_period_end_date = col_date())),
  person = read_csv(str_glue("{INFER}/person.csv"), col_types = cols_only(person_id = col_double(), year_of_birth = col_double(),
                                                                          month_of_birth = col_double(),day_of_birth = col_double())),
  person_char = read_csv(str_glue("{INFER}/person.csv"), col_types = cols_only(person_id = col_double(),
                                                                               gender_concept_id = col_double(),
                                                                               race_concept_id = col_double()))
)

# Create features ------------------------------------------------------------
# Using dates

dates = bind_rows(
  training$condition_occurrence %>% 
    select(person_id, date = condition_end_date),
  training$drug_exposure %>% 
    select(person_id, date = drug_exposure_start_date) ,
  training$measurement %>% 
    select(person_id, date = measurement_date),
  training$observation_period %>% 
    select(person_id, date = observation_period_end_date) ,
  training$observation %>% 
    select(person_id, date = observation_date) ,
  training$procedure_occurrence %>% 
    select(person_id, date = procedure_date),    
  training$visit_occurrence %>% 
    select(person_id, date = visit_end_date) 
)


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
dates = dates %>% select(person_id, last_known_age)

# Create dummy 

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


#add drugs

load("condlogit.RData")
load("obslogit.RData")

condition = condition %>% select(condlogit, "person_id")
observation = observation %>% select(obslogit, "person_id")
#drug = ...


# Harmonize with training data features


data = training$person %>% 
  right_join(dates, by = "person_id")

data = data %>% 
  left_join(observation, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Observation")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(condition, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Condition")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(gender, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Gender")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(race, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Race")), .funs = ~ coalesce(., 0))

features_not_present = setdiff(features, c(colnames(observation),colnames(condition),colnames(dates),
                                           colnames(gender),colnames(race),
                                           colnames(person)))
if(length(features_not_present) > 0){
  supplement = matrix(0, nrow = nrow(data), ncol = length(features_not_present), dimnames = list(NULL, features_not_present)) %>% 
    as_tibble()
  
  data= data %>% 
    bind_cols(supplement)
}

features <- c(features, "person_id")
data = data %>% 
  magrittr::extract(features)

# Predict ---------------------------------------------------------------------
res = tibble(
  person_id = diagnosis$person_id,
  score = model %>%
    predict(data, type = "prob") %>% 
    pull(.pred_Yes)
)

write_csv(res, path = str_glue("{OUTPUT}/predictions.csv"))





