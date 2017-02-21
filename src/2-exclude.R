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

# picu <- "Hermann 9 Pediatric Intensive Care Unit"
picu <- "HC PICU"
# ed <- "HC Virtual Pedi Emergency Dept"
ed <- c("HC EDPD", "HC VUPD", "HH ADMT")

excl_locations <- read_data(dir_raw, "location", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter((unit.count == 1 & location == picu) |
               (unit.count == 2 & location == picu & lag(location) %in% ed)) %>%
    distinct(millennium.id)

patients$exclude_icu = excl_locations$millennium.id

include <- anti_join(include, excl_locations, by = "millennium.id")

# other indications ------------------------------------
# exclude if croup or anaphylactic shock

alt_diag9 <- list(croup = icd_children("464.4"),
                  anaphylaxis = icd_children(c("995", "999.4")))

alt_diag10 <- list(croup = icd_children("J05.0"),
                   anaphylaxis = icd_children(c("T78.0", "T78.2", "T80.5", "T88.6")))

excl_diag9 <- raw_diagnosis %>%
    semi_join(include, by = "millennium.id") %>%
    icd_comorbid(alt_diag9, "millennium.id", "diag.code", FALSE, return_df = TRUE) %>%
    filter(croup == TRUE | anaphylaxis == TRUE)

excl_diag10 <- raw_diagnosis %>%
    semi_join(include, by = "millennium.id") %>%
    icd_comorbid(alt_diag10, "millennium.id", "diag.code", FALSE, return_df = TRUE) %>%
    filter(croup == TRUE | anaphylaxis == TRUE)

excl_diag <- bind_rows(excl_diag9, excl_diag10) %>%
    distinct(millennium.id)

patients$exclude_alternate_indication = excl_diag$millennium.id

include <- anti_join(include, excl_diag, by = "millennium.id")

# both steroids ----------------------------------------
# exclude patients who received both dexamethasone and prednisone/prednisolone

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

excl_steroids <- raw_meds %>%
    filter(med %in% c("dexamethasone", "methylprednisolone", "prednisolone", "prednisone")) %>%
    semi_join(include, by = "millennium.id") %>%
    distinct(millennium.id, med) %>%
    mutate(value = TRUE) %>%
    spread(med, value) %>%
    filter(dexamethasone == TRUE & (prednisone == TRUE | prednisolone == TRUE | methylprednisolone == TRUE))

patients$exclude_multiple_steroids = excl_steroids$millennium.id

include <- anti_join(include, excl_steroids, by = "millennium.id")

# racemic epi ------------------------------------------

excl_racepi <- raw_meds %>%
    filter(med == "racepinephrine") %>%
    distinct(millennium.id)

patients$exclude_racemic_epi = excl_racepi$millennium.id

include <- anti_join(include, excl_racepi, by = "millennium.id")

# er visits -----------------------------------------
# remove any ER encounters
# no longer needed, ER encounters removed in include script

raw_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()
#
# excl_er <- raw_demographics %>%
#     semi_join(include, by = "millennium.id") %>%
#     filter(visit.type == "Emergency") %>%
#     distinct(millennium.id)
#
# patients$exclude_er_visit = excl_er$millennium.id
#
# include <- anti_join(include, excl_er, by = "millennium.id")

# readmissions -----------------------------------------
# remove any subsequent encounters

raw_encounters <- read_data(dir_raw, "encounters") %>%
    as.encounters() %>%
    semi_join(include, by = "millennium.id")

first_encounters <- raw_demographics %>%
    semi_join(include, by = "millennium.id") %>%
    left_join(raw_encounters[c("millennium.id", "person.id", "admit.datetime")], by = "millennium.id") %>%
    group_by(person.id) %>%
    arrange(admit.datetime) %>%
    summarize(millennium.id = first(millennium.id))

patients$exclude_readmission = first_encounters$millennium.id

include <- semi_join(include, first_encounters, by = "millennium.id")

# included patients ------------------------------------

patients$include = include$millennium.id

write_rds(patients, "data/final/patients.Rds")
