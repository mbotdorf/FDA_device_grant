#' associates demographic feature with a
#' cohort tbl
#'
#' @param cohort
#' @param demog_feature
#'
#' @return two column tbl
#' with person_id and the demographic
#' feature of interest
#'

get_demographic_feature <- function(cohort,
                                    demog_feature) {
  
  person_join <-
    cdm_tbl('person') %>%
    right_join(cohort,by='person_id') %>%
    select(person_id,!! demog_feature) %>%
    distinct()
  
}


#' function to compute all demographics
#'
#' @param cohort the cohort of patients included
#' @return A tbl with coded demographic features
#'
#' sex,race,age_in_days


get_demographics <- function(cohort) {
  
  rslt = list()
  rslt$sex <-
    get_demographic_feature(cohort=cohort,
                            demog_feature='gender_concept_id') %>%
    mutate(sex =
             case_when(gender_concept_id == 8532 ~ as.character('Female'),
                       gender_concept_id == 8507 ~ as.character('Male'),
                       TRUE ~ as.character('Other'))) %>%
    distinct(person_id,sex)
  
  rslt$race <-
    get_demographic_feature(cohort=cohort,
                            demog_feature='race_concept_id') %>%
    mutate(race=case_when(race_concept_id == 8516 ~ as.character('Black'),
                          race_concept_id == 8527 ~ as.character('White'),
                          race_concept_id == 8657 ~
                            as.character('American Indian or Alaskan Native'),
                          race_concept_id == 8515 ~ as.character('Asian'),
                          race_concept_id == 44814659 ~
                            as.character('Multiple Race'),
                          race_concept_id == 8557 ~
                            as.character('Native Hawaiian or Other Pacific Islander'),
                          TRUE ~ as.character('Other/Unknown'))) %>%
    distinct(person_id,race)
  
  rslt$age <- cohort %>%
    dates_to_ages() %>% 
    rename(cohort_start_age=age) %>%
    distinct(person_id, cohort_start_age)
  
  rslt$age_category <- rslt$age %>%
    mutate(age_category =
             case_when(cohort_start_age < 366 ~ as.character('0-<1'),
                       cohort_start_age < 365.25 * 6 ~ as.character('1-<6'),
                       cohort_start_age < 365.25 * 11 ~ as.character('6-<11'),
                       cohort_start_age < 365.25 * 20 ~ as.character('11-<20'),
                       TRUE ~ as.character('20+'))) %>%
    select(person_id, age_category)
  
  reduce(rslt, ~left_join(.x, .y, by = 'person_id'))
}
