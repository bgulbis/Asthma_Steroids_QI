# exclude

library(tidyverse)
library(edwr)

dir_raw <- "data/raw"

eligible_pie <- read_rds("data/final/eligible.Rds")

patients <- list(eligible = eligible_pie$pie.id)

# pregnant ---------------------------------------------
# exclude any pregnant patients

excl_preg_diagnosis <- read_data(dir_raw, "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data() %>%
    check_pregnant()

excl_preg_labs <- read_data(dir_raw, "labs_preg") %>%
    as.labs() %>%
    tidy_data() %>%
    check_pregnant()

excl_preg <- bind_rows(excl_preg_diagnosis, excl_preg_labs)

patients$exclude_pregnant = excl_preg$pie.id

include <- anti_join(eligible_pie, excl_preg, by = "pie.id")

# icu admission ----------------------------------------
# exclude patients who were first admitted to the PICU

picu <- "Hermann 9 Pediatric Intensive Care Unit"
ed <- "HC Virtual Pedi Emergency Dept"

locations <- read_edw_data(dir.patients, "locations") %>%
    tidy_data("locations")

excl.location <- locations %>%
    filter((unit.count == 1 & location == picu) |
               (unit.count == 2 & location == picu & lag(location) == ed)) %>%
    distinct(pie.id)

patients$exclude_icu = excl.location$pie.id

include <- anti_join(include, excl.location, by = "pie.id")

# other indications ----
# exclude if croup or anaphylactic shock

ref.icd9 <- find_icd_codes("(anaphyl|croup)") %>%
    filter(icd.code != "V13.81") %>%
    transmute(disease.state = ifelse(icd.code == "464.4", "croup", "anaphylaxis"),
              type = "ICD",
              code = icd.code)

excl.icd9 <- read_edw_data(dir.patients, "icd9") %>%
    semi_join(include, by = "pie.id") %>%
    tidy_data("icd9", ref.data = ref.icd9)

ref.icd10 <- find_icd_codes("(anaphyl|croup)", TRUE) %>%
    filter(icd.code != "Z87.892") %>%
    transmute(disease.state = ifelse(icd.code == "J05.0", "croup", "anaphylaxis"),
              type = "ICD",
              code = icd.code)

excl.icd10 <- read_edw_data(dir.patients, "icd10") %>%
    semi_join(include, by = "pie.id") %>%
    tidy_data("icd10", ref.data = ref.icd10)

excl.indication <- bind_rows(excl.icd9, excl.icd10)
rm(excl.icd9, excl.icd10)

patients$exclude_alternate_indication = excl.indication$pie.id

include <- anti_join(include, excl.indication, by = "pie.id")

# both steroids ----
# exclude patients who received both dexamethasone and prednisone/prednisolone

excl.steroids <- read_edw_data(dir.patients, "meds_freq", "meds_sched_freq") %>%
    semi_join(include, by = "pie.id") %>%
    distinct(pie.id, med) %>%
    mutate(med = factor(med),
           value = TRUE) %>%
    group_by(pie.id) %>%
    select(pie.id, med, value) %>%
    spread(med, value) %>%
    filter(dexamethasone == TRUE,
           (prednisone == TRUE | prednisolone == TRUE))

patients$exclude_multiple_steroids = excl.steroids$pie.id

include <- anti_join(include, excl.steroids, by = "pie.id")

# racemic epi ----

excl.racepi <- read_edw_data(dir.patients, "meds_sched") %>%
    semi_join(include, by = "pie.id") %>%
    filter(med == "racepinephrine") %>%
    distinct(pie.id)

patients$exclude_racemic_epi = excl.racepi$pie.id

include <- anti_join(include, excl.racepi, by = "pie.id")

# readmissions ----
# remove any subsequent encounters

# get list of unique person id's to use for identifying re-encounters
raw.demographics <- read_edw_data(dir.patients, "demographics") %>%
    semi_join(include, by = "pie.id")

excl.er <- filter(raw.demographics, visit.type == "EC Emergency Center")

patients$exclude_er_visit = excl.er$pie.id

include <- anti_join(include, excl.er, by = "pie.id")

tmp <- semi_join(raw.demographics, include, by = "pie.id") %>%
    distinct(person.id)

# use for EDW query "Encounters - by Person ID"
concat_encounters(tmp$person.id)

raw.encounters <- read_edw_data(dir.patients, "encounters")

tmp.visits <- inner_join(raw.demographics,
                         raw.encounters[c("pie.id", "admit.datetime")],
                         by = "pie.id") %>%
    group_by(person.id) %>%
    arrange(admit.datetime) %>%
    summarize(pie.id = first(pie.id))

excl.readmit <- anti_join(include, tmp.visits, by = "pie.id")

patients$exclude_readmission = excl.readmit$pie.id

include <- semi_join(include, tmp.visits, by = "pie.id")

# included patients ----

patients$include = include$pie.id

save_rds(dir.save, "^patients")
