# exclude

library(tidyverse)
library(edwr)
library(icd)

dir_raw <- "data/raw/mbo"

eligible_pts <- read_rds("data/final/eligible.Rds")

patients <- list(eligible = eligible_pts$millennium.id)

raw_diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    tidy_data()

# pregnant ---------------------------------------------
# exclude any pregnant patients

excl_preg_diagnosis <- raw_diagnosis %>%
    check_pregnant()

excl_preg_labs <- read_data(dir_raw, "labs-preg", FALSE) %>%
    as.labs() %>%
    # tidy_data() %>%
    check_pregnant()

excl_preg <- bind_rows(excl_preg_diagnosis, excl_preg_labs) %>%
    distinct(millennium.id)

patients$exclude_pregnant = excl_preg$millennium.id

include <- anti_join(eligible_pts, excl_preg, by = "millennium.id")

# icu admission ----------------------------------------
# exclude patients who were first admitted to the PICU

picu <- "Hermann 9 Pediatric Intensive Care Unit"
ed <- "HC Virtual Pedi Emergency Dept"

excl_locations <- read_data(dir_raw, "locations") %>%
    as.locations() %>%
    tidy_data() %>%
    filter((unit.count == 1 & location == picu) |
               (unit.count == 2 & location == picu & lag(location) == ed)) %>%
    distinct(pie.id)

patients$exclude_icu = excl_locations$pie.id

include <- anti_join(include, excl_locations, by = "pie.id")

# other indications ------------------------------------
# exclude if croup or anaphylactic shock

alt_diag9 <- list(croup = icd_children("464.4"),
                  anaphylaxis = icd_children(c("995", "999.4")))

alt_diag10 <- list(croup = icd_children("J05.0"),
                   anaphylaxis = icd_children(c("T78.0", "T78.2", "T80.5", "T88.6")))

excl_diag9 <- raw_diagnosis %>%
    semi_join(include, by = "pie.id") %>%
    icd_comorbid(alt_diag9, "pie.id", "diag.code", FALSE, return_df = TRUE) %>%
    filter(croup == TRUE | anaphylaxis == TRUE)

excl_diag10 <- raw_diagnosis %>%
    semi_join(include, by = "pie.id") %>%
    icd_comorbid(alt_diag10, "pie.id", "diag.code", FALSE, return_df = TRUE) %>%
    filter(croup == TRUE | anaphylaxis == TRUE)

excl_diag <- bind_rows(excl_diag9, excl_diag10) %>%
    distinct(pie.id)

patients$exclude_alternate_indication = excl_diag$pie.id

include <- anti_join(include, excl_diag, by = "pie.id")

# both steroids ----------------------------------------
# exclude patients who received both dexamethasone and prednisone/prednisolone

excl_steroids <- read_data(dir_raw, "meds_freq") %>%
    as.meds_freq() %>%
    semi_join(include, by = "pie.id") %>%
    distinct(pie.id, med) %>%
    mutate(med = factor(med),
           value = TRUE) %>%
    group_by(pie.id) %>%
    select(pie.id, med, value) %>%
    spread(med, value) %>%
    filter(dexamethasone == TRUE,
           (prednisone == TRUE | prednisolone == TRUE))

patients$exclude_multiple_steroids = excl_steroids$pie.id

include <- anti_join(include, excl_steroids, by = "pie.id")

# racemic epi ------------------------------------------

excl_racepi <- read_data(dir_raw, "meds_sched") %>%
    as.meds_sched() %>%
    filter(med == "racepinephrine") %>%
    distinct(pie.id)

patients$exclude_racemic_epi = excl_racepi$pie.id

include <- anti_join(include, excl_racepi, by = "pie.id")

# er visits -----------------------------------------
# remove any ER encounters

raw_demographics <- read_data(dir_raw, "demographics") %>%
    as.demographics()

excl_er <- raw_demographics %>%
    semi_join(include, by = "pie.id") %>%
    filter(visit.type == "Emergency") %>%
    distinct(pie.id)

patients$exclude_er_visit = excl_er$pie.id

include <- anti_join(include, excl_er, by = "pie.id")

# readmissions -----------------------------------------
# remove any subsequent encounters

raw_encounters <- read_data(dir_raw, "encounters") %>%
    as.encounters() %>%
    semi_join(include, by = "pie.id")

first_encounters <- raw_demographics %>%
    semi_join(include, by = "pie.id") %>%
    left_join(raw_encounters[c("pie.id", "admit.datetime")], by = "pie.id") %>%
    group_by(person.id) %>%
    arrange(admit.datetime) %>%
    summarize(pie.id = first(pie.id))

patients$exclude_readmission = first_encounters$pie.id

include <- semi_join(include, first_encounters, by = "pie.id")

# included patients ------------------------------------

patients$include = include$pie.id

write_rds(patients, "data/final/patients.Rds")
