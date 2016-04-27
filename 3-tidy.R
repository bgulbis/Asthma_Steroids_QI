# tidy

source("0-library.R")

tmp <- get_rds(dir.save)

include <- data_frame(pie.id = patients$include)

# demographics ----

data.demographics <- read_edw_data(dir.patients, "demographics") %>%
    semi_join(include, by = "pie.id")

# groups ----

tmp.steroids <- read_edw_data(dir.patients, "meds_freq", "meds_sched_freq") %>%
    semi_join(include, by = "pie.id")

data.groups <- tmp.steroids %>%
    mutate(dex = ifelse(med == "dexamethasone", TRUE, FALSE)) %>%
    group_by(pie.id) %>%
    summarize(group = ifelse(sum(dex) >= 1, "dexamethasone", "prednisone"))

# primary diagnosis ----

tmp.icd9 <- read_edw_data(dir.patients, "icd9") %>%
    semi_join(include, by = "pie.id") %>%
    filter(diag.type == "Final",
           diag.seq == 1)

tmp <- icd_description(tmp.icd9$diag.code)

tmp.icd9 <- inner_join(tmp.icd9, tmp, by = c("diag.code" = "icd.code"))

tmp.icd10 <- read_edw_data(dir.patients, "icd10") %>%
    semi_join(include, by = "pie.id") %>%
    filter(diag.type == "Final",
           diag.seq == 1)

tmp <- icd_description(tmp.icd10$diag.code, icd10 = TRUE)

tmp.icd10 <- inner_join(tmp.icd10, tmp, by = c("diag.code" = "icd.code"))

data.primary.diagnosis <- bind_rows(tmp.icd9, tmp.icd10)

# measures ----

tmp.measures <- read_edw_data(dir.patients, "measures") %>%
    semi_join(include, by = "pie.id")

tmp.height <- filter(tmp.measures, measure == "Height",
                     measure.units == "cm") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(height = first(measure.result))

data.measures <- filter(tmp.measures, measure == "Weight",
                        measure.units == "kg") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(weight = first(measure.result)) %>%
    left_join(tmp.height, by = "pie.id")

# asthma assessment ----

data.asthma <- read_edw_data(dir.patients, "scores", "events") %>%
    mutate(post = ifelse(str_detect(event, "post"), TRUE, FALSE),
           event = str_replace_all(event, "  ", " "),
           event = str_replace_all(event, "(rc|pre|post| asthma assess|asthma treatment )", ""),
           event = str_trim(event, "both"),
           event = str_replace_all(event, " ", "_")) %>%
    filter(event.result != "") %>%
    group_by(pie.id, event.datetime, post, event) %>%
    summarize(event.result = first(event.result)) %>%
    group_by(pie.id, event.datetime, post) %>%
    spread(event, event.result)

# steroids ----

data.steroids <- tmp.steroids %>%
    group_by(pie.id, med) %>%
    arrange(med.datetime) %>%
    summarize(num.doses = n(),
              first.dose = first(med.dose),
              first.freq = first(freq),
              total.dose = sum(med.dose),
              first.datetime = first(med.datetime),
              duration = difftime(last(med.datetime), first(med.datetime),
                                  units = "hours"))

# adjunct medications ----

tmp.first.steroid <- data.steroids %>%
    group_by(pie.id) %>%
    summarize(first.datetime = first(first.datetime))

tmp.meds <- read_edw_data(dir.patients, "meds_sched") %>%
    semi_join(include, by = "pie.id") %>%
    filter(med %in% c("albuterol", "ipratropium", "albuterol-ipratropium",
                      "ipratropium nasal", "magnesium sulfate",
                      "magnesium oxide", "terbutaline")) %>%
    inner_join(tmp.first.steroid, by = "pie.id") %>%
    group_by(pie.id, med) %>%
    arrange(med.datetime)

tmp.meds.adjunct <- tmp.meds %>%
    filter(med.datetime > first.datetime) %>%
    group_by(pie.id, med, med.dose.units) %>%
    summarize(total.dose.after = sum(med.dose))

data.meds.adjunct <- tmp.meds %>%
    group_by(pie.id, med, med.dose.units) %>%
    summarize(total.dose = sum(med.dose)) %>%
    full_join(tmp.meds.adjunct, by = c("pie.id", "med", "med.dose.units"))

# readmissions ----

tmp.encounters <- read_edw_data(dir.patients, "encounters")

tmp.index <- tmp.encounters%>%
    group_by(person.id) %>%
    arrange(admit.datetime) %>%
    left_join(data.demographics[c("pie.id", "age")], by = "pie.id") %>%
    filter(!is.na(age)) %>%
    select(person.id, index.datetime = admit.datetime)

tmp.readmit <- inner_join(tmp.encounters, tmp.index, by = "person.id") %>%
    filter(admit.datetime > index.datetime,
           visit.type %in% c("Inpatient", "OBS Observation Patient")) %>%
    group_by(person.id) %>%
    mutate(readmit.days = difftime(admit.datetime, index.datetime,
                                   units = "days")) %>%
    summarize(readmit.days = min(readmit.days))

data.readmit <- left_join(data.demographics[c("pie.id", "person.id")],
                          tmp.readmit[c("person.id", "readmit.days")],
                          by = "person.id") %>%
    mutate(readmit.30days = ifelse(readmit.days <= 30, TRUE, FALSE),
           readmit.7days = ifelse(readmit.days <= 7, TRUE, FALSE)) %>%
    select(-readmit.days, -person.id) %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), contains("readmit"))


save_rds(dir.save, "^data")

