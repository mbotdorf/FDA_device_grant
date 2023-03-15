#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'


#fx to unite codesets
unite_codesets<-function(cs1, cs2, cs3, cs4, cs5, cs6) {cs1 %>% union(cs2) %>%
    union(cs3) %>% union(cs4) %>% union(cs5) %>% union(cs6)}