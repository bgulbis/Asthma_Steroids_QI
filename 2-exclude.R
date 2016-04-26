# exclude

source("0-library.R")

tmp <- get_rds(dir.save)

include <- data_frame(pie.id = patients$eligible)

# get list of unique person id's to use for identifying re-encounters
raw.persons <- read_edw_data(dir.patients, "demographics") %>%
    select(person.id) %>%
    distinct

concat_encounters(raw.persons$person.id)

# pregnant ----
# exclude any pregnant patients

preg.icd9 <- read_edw_data(dir.patients, "icd9") %>%
    semi_join(include, by = "pie.id")
preg.icd10 <- read_edw_data(dir.patients, "icd10") %>%
    semi_join(include, by = "pie.id")
preg.labs <- read_edw_data(dir.patients, "labs_preg", "labs") %>%
    semi_join(include, by = "pie.id")

excl.preg <- check_pregnant(preg.labs, preg.icd9, preg.icd10)

patients$exclude_pregnant = excl.preg$pie.id

include <- anti_join(include, excl.preg, by = "pie.id")

# icu admission ----
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
    distinct(pie.id)

patients$exclude_racemic_epi = excl.racepi$pie.id

include <- anti_join(include, excl.racepi, by = "pie.id")

# included patients ----

patients$include = include$pie.id

save_rds(dir.save, "^patients")
