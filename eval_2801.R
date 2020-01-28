library(parsnip)
library(recipes)
library(tidyverse)
library(ranger)
library(rsample)
library(yardstick)
library(lubridate)

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
tables = list(
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

gender = tables$person_char %>% 
  select(person_id, gender_concept_id) %>%
  mutate(gender_concept_id = str_c("Gender", gender_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = gender_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

race = tables$person_char %>% 
  select(person_id, race_concept_id) %>%
  mutate(race_concept_id = str_c("Race", race_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = race_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))


condition = tables$condition_occurrence %>% 
  select(person_id, condition_concept_id) %>% 
  mutate(condition_concept_id = str_c("Condition",condition_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = condition_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

observation = tables$observation %>% 
  select(person_id, observation_concept_id) %>% 
  mutate(observation_concept_id = str_c("Observation",observation_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = observation_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))


drug = tables$drug_exposure %>% 
  select(person_id, drug_concept_id) %>% 
  mutate(drug_concept_id = str_c("Drug",drug_concept_id)) %>% 
  mutate(value = 1) %>% 
  pivot_wider(id_cols = person_id, names_from = drug_concept_id, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))


# load("obslogit.RData")
# load("condlogit.RData")
# load("druglogit.RData")

obslogit <- c( "Observation4214956",  "Observation4015724",  "Observation0",
               "Observation4167217",  "Observation440922" ,  "Observation40485017",
               "Observation440927",   "Observation2617267",  "Observation2720581",
               "Observation2108553",  "Observation40479430", "Observation40479343",
               "Observation4149299",  "Observation2514491",  "Observation440121",
              "Observation4090651", "Observation2106386",  "Observation2108782",
               "Observation2614664",  "Observation4014023",  "Observation2617827",
               "Observation4253306",  "Observation2102836",  "Observation437175",
              "Observation4019957",  "Observation4150253",  "Observation2106328",
               "Observation4171434",  "Observation2106342",  "Observation2102833",
               "Observation4060094",  "Observation4059465",  "Observation2106182",
               "Observation4305831",  "Observation4192270",  "Observation4058861",
               "Observation40757059", "Observation4219847",  "Observation2617883",
               "Observation438872",   "Observation2109560",  "Observation2108688",
               "Observation2617820",  "Observation2614675",  "Observation2617808",
              "Observation2101873",  "Observation2101879", "Observation442424",
               "Observation2101934",  "Observation2108529",  "Observation2110380",
               "Observation2106382",  "Observation4058284",  "Observation4117957",
               "Observation2414352",  "Observation2101911",  "Observation4030415",
               "Observation438331",   "Observation437449",  "Observation4060088",
               "Observation2108611",  "Observation442423",   "Observation433946",
               "Observation2107558",  "Observation194800",   "Observation4081758",
               "Observation2614661",  "Observation435151",   "Observation196462",
               "Observation2106844",  "Observation433376",   "Observation436868",
               "Observation2617805",  "Observation441764",   "Observation2101838",
               "Observation2108583",  "Observation4145111",  "Observation4094126",
               "Observation2108585",  "Observation2106289",  "Observation2108776",
               "Observation2614673",  "Observation2617831",  "Observation4329840",
               "Observation2101906",  "Observation433393",  "Observation439371",
               "Observation2108708",  "Observation440308",   "Observation442936",
               "Observation2106843",  "Observation4052171",  "Observation2106711"
)

condlogit <- c( "Condition198700",   "Condition201337",   "Condition201531",
                "Condition444099",  "Condition438739",   "Condition435577",
                "Condition40486896", "Condition135215",   "Condition78227",
                "Condition4181482",  "Condition4261933",  "Condition201603",
                "Condition376552",   "Condition379021",   "Condition73847",
                "Condition198184",   "Condition381446",   "Condition78282",
                "Condition439920",   "Condition73289",    "Condition132344",
                "Condition138876",   "Condition40480431", "Condition376107",
                "Condition433180",   "Condition434766",   "Condition134569",
                "Condition138690",   "Condition4119258",  "Condition321726",
                "Condition77373",    "Condition80197",    "Condition4303970",
                "Condition138455"
)

druglogit <- c( "Drug19029027", "Drug0",        "Drug40165762", "Drug19133768", "Drug1314006",
                "Drug19003829", "Drug1551170",  "Drug19074672", "Drug2213440",  "Drug1518606",
                "Drug2213483",  "Drug40171917", "Drug1363059",  "Drug956877",   "Drug40222592",
                "Drug736021",   "Drug903963",   "Drug790255",   "Drug19010040", "Drug19075308",
                "Drug1126750",  "Drug2213438",  "Drug40164141", "Drug1790982",  "Drug19090761",
                "Drug1750501",  "Drug40222092", "Drug19135233", "Drug40168549", "Drug40175215",
                "Drug40168370", "Drug19019338", "Drug40174491", "Drug19096776", "Drug19030828",
                "Drug40162439", "Drug40163754", "Drug40169706", "Drug40162871", "Drug19021074",
                "Drug40227085", "Drug40168499", "Drug40169695", "Drug40243198", "Drug42708517",
                "Drug40162484", "Drug40171683", "Drug19049685", "Drug40165453", "Drug1156715",
                "Drug19058442", "Drug19078868", "Drug1586370",  "Drug42898215", "Drug19095118",
                "Drug40226155", "Drug19124005", "Drug42800815", "Drug1134473",  "Drug788065",
                "Drug42873728", "Drug40162275", "Drug19052941", "Drug40227730", "Drug40234555",
                "Drug40222791", "Drug961085",   "Drug42800292", "Drug1733766",  "Drug19132662",
                "Drug40238948", "Drug40164860", "Drug40168924", "Drug19125455", "Drug43012043",
                "Drug40071653", "Drug40167416", "Drug40161865", "Drug19034478", "Drug40164178",
                "Drug40232658", "Drug905412",   "Drug19113353"
)

observation = observation %>% select(intersect(obslogit, names(observation)), "person_id")
condition = condition %>% select(intersect(condlogit, names(condition)), "person_id")
drug = drug %>% select(intersect(druglogit, names(drug)), "person_id")

# Harmonize with training data features


data = tables$person %>% 
  right_join(dates, by = "person_id")

data = data %>% 
  left_join(observation, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Observation")), .funs = ~ coalesce(., 0))

data = data %>% 
  left_join(drug, by = "person_id") %>%
  mutate_at(.vars = vars(starts_with("Drug")), .funs = ~ coalesce(., 0))

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
                                           colnames(gender),colnames(race),colnames(drug),
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
probabilities = predict(model, data = data, type = "response")
res = tibble(
  person_id = data$person_id,
  score = probabilities$predictions[,2]
)

write_csv(res, path = str_glue("{OUTPUT}/predictions.csv"))





