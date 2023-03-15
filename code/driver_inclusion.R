congenital <- load_codeset('congenital_scoliosis')
neuro <- load_codeset('neuromusc_scoliosis')
idio <- load_codeset('idiopathic_scoliosis')
duane <- load_codeset('duane_scoliosis')
tissue <- load_codeset('tissue_scoliosis')
skeletal <- load_codeset('skeletal_dysplasia_scoliosis')

#fx to unite codesets
unite_codesets(congenital,neuro,idio,duane,tissue,skeletal)


scoliosis <- congenital %>% union(neuro) %>%
  union(idio) %>% union(duane) %>% union(tissue) %>% union(skeletal)

# Has systolic and diastolic measured on the same day
#scoliosis_cohort <- cdm_tbl('person') %>% inner_join(cdm_tbl('condition_occurrence'))
cohort<-cdm_tbl('condition_occurrence') %>% 
  inner_join(scoliosis, by = c("condition_concept_id" = "concept_id")) %>% 
  inner_join(cdm_tbl('person'), by = c("person_id", "provider_id", "site"))

#count number of patients
cohort %>% summarise(n_patients = n_distinct(person_id)) 


#age in years and then days
cohort<-cohort %>% mutate(age=condition_start_age_in_months/12) %>% mutate(cohort_start_age=age*365)

cohort %>% summarise(meanage = mean(age))
cohort %>% summarise(sdage = sd(age))

cohort<-cohort %>% mutate(age_category =
           case_when(cohort_start_age < 366 ~ as.character('0-<1'),
                     cohort_start_age < 365.25 * 6 ~ as.character('1-<6'),
                     cohort_start_age < 365.25 * 11 ~ as.character('6-<11'),
                     cohort_start_age < 365.25 * 20 ~ as.character('11-<20'),
                     TRUE ~ as.character('20+'))) #%>%
#  select(person_id, age_category)

write.csv(scoliosis, "~/Desktop/scoliosis_codeset_R.csv")
