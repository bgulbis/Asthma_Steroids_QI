# tidy

library(tidyverse)
library(stringr)
library(edwr)
library(icd)

dir_raw <- "data/raw"
patients <- read_rds("data/final/patients.Rds")
include <- tibble(pie.id = patients$include)

# demographics -----------------------------------------

data_demographics <- read_data(dir_raw, "demographics") %>%
    as.demographics() %>%
    semi_join(include, by = "pie.id") %>%
    select(-person.id)

# groups -----------------------------------------------

ref <- tibble(name = c("dexamethasone", "prednisone", "prednisolone"),
              type = "med",
              group = "sched")

tmp_steroids <- read_data(dir_raw, "meds_freq") %>%
    as.meds_freq() %>%
    tidy_data(ref) %>%
    semi_join(include, by = "pie.id")

data_groups <- tmp_steroids %>%
    mutate(dex = med == "dexamethasone") %>%
    group_by(pie.id) %>%
    summarize(group = sum(dex) >= 1) %>%
    mutate(group = if_else(group, "dexamethasone", "prednisone"))

# primary diagnosis ------------------------------------

data_primary_diagnosis <- read_data(dir_raw, "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data() %>%
    semi_join(include, by = "pie.id") %>%
    filter(diag.type == "Final",
           diag.seq == 1) %>%
    by_row(~icd_explain(.x$diag.code), .to = "descr", .collate = "rows")

# measures ---------------------------------------------

tmp_measures <- read_data(dir_raw, "measures") %>%
    as.measures() %>%
    semi_join(include, by = "pie.id")

tmp_height <- tmp_measures %>%
    filter(measure == "height",
           measure.units == "cm") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(height = first(measure.result))

data_measures <- tmp_measures %>%
    filter(measure == "weight",
           measure.units == "kg") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(weight = first(measure.result)) %>%
    left_join(tmp_height, by = "pie.id")

# asthma assessment ------------------------------------

data_asthma <- read_data(dir_raw, "scores") %>%
    as.events() %>%
    semi_join(include, by = "pie.id") %>%
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

# steroids ---------------------------------------------

data_steroids <- tmp_steroids %>%
    group_by(pie.id, med) %>%
    arrange(med.datetime) %>%
    summarize(num.doses = n(),
              first.dose = first(med.dose),
              first.freq = first(freq),
              total.dose = sum(med.dose),
              first.datetime = first(med.datetime),
              last.datetime = last(med.datetime),
              duration = difftime(last(med.datetime), first(med.datetime),
                                  units = "hours"))

# adjunct medications ----

tmp.first.steroid <- data.steroids %>%
    group_by(pie.id) %>%
    summarize(first.datetime = first(first.datetime))

tmp.meds.sched <- read_edw_data(dir.patients, "meds_sched") %>%
    semi_join(include, by = "pie.id")

tmp.meds <- tmp.meds.sched %>%
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

ref.cont.meds <- data_frame(name = "albuterol", type = "med", group = "cont")

tmp.meds.cont <- edwr::read_edw_data(dir.patients, "meds_cont_detail") %>%
    semi_join(include, by = "pie.id") %>%
    tidy_data("meds_cont", ref.data = ref.cont.meds, sched.data = tmp.meds.sched)

concat_encounters(tmp.meds.cont$order.id, 900)

tmp.meds.cont.dc <- edwr::read_edw_data(dir.patients, "order_detail") %>%
    filter(action.type == "Discontinue",
           ingredient == "albuterol") %>%
    distinct(pie.id, order.id, action.datetime) %>%
    rename(dc.datetime = action.datetime)

tmp.meds.cont.order <- edwr::read_edw_data(dir.patients, "order_detail") %>%
    filter(action.type == "Order",
           ingredient == "albuterol")

tmp.meds.cont.alb <- tmp.meds.cont %>%
    filter(str_detect(event.tag, "Begin Bag")) %>%
    full_join(tmp.meds.cont.order, by = c("pie.id", "order.id")) %>%
    full_join(tmp.meds.cont.dc, by = c("pie.id", "order.id")) %>%
    select(pie.id:med, route, event.tag, ingredient.dose, ingredient.unit,
           dc.datetime) %>%
    inner_join(tmp.first.steroid, by = "pie.id") %>%
    distinct(pie.id, order.id, .keep_all = TRUE) %>%
    group_by(pie.id) %>%
    mutate(dc.datetime = coalesce(dc.datetime, lead(med.datetime)),
           after.steroid = dc.datetime > first.datetime,
           overlap = first.datetime %within% interval(med.datetime, dc.datetime),
           rate = ingredient.dose / 250 * 30,
           duration.before = if_else(
               after.steroid == TRUE & overlap == TRUE,
               as.numeric(difftime(first.datetime, med.datetime, units = "hours")),
               if_else(after.steroid == TRUE,
                       0,
                       as.numeric(difftime(dc.datetime, med.datetime, units = "hours"))
               )
           ),
           duration.after = if_else(
               after.steroid == TRUE & overlap == TRUE,
               as.numeric(difftime(dc.datetime, first.datetime, units = "hours")),
               if_else(after.steroid == TRUE,
                       as.numeric(difftime(dc.datetime, med.datetime, units = "hours")),
                       0
               )
           ),
           duration.before = if_else(duration.before < 0, 0, duration.before),
           duration.after = if_else(duration.after < 0, 0, duration.after),
           dose.before = rate * duration.before,
           dose.after = rate * duration.after) %>%
    group_by(pie.id, med, ingredient.unit) %>%
    summarize(total.dose = sum(dose.before) + sum(dose.after),
              total.dose.after = sum(dose.after)) %>%
    mutate(total.dose = coalesce(total.dose, 0),
           total.dose.after = coalesce(total.dose.after, 0)) %>%
    select(pie.id, med, med.dose.units = ingredient.unit, total.dose, total.dose.after) %>%
    filter(total.dose > 0)

data.meds.adjunct <- bind_rows(data.meds.adjunct, tmp.meds.cont.alb) %>%
    mutate(total.dose.after = coalesce(total.dose.after, 0))

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
    mutate(readmit.30days = if_else(readmit.days <= 30, TRUE, FALSE, FALSE),
           readmit.7days = if_else(readmit.days <= 7, TRUE, FALSE, FALSE)) %>%
    select(-readmit.days, -person.id)

# icu admission ----

tmp.locations <- read_edw_data(dir.patients, "locations") %>%
    semi_join(include, by = "pie.id") %>%
    tidy_data("locations") %>%
    filter(location == "Hermann 9 Pediatric Intensive Care Unit") %>%
    distinct(pie.id, .keep_all = TRUE)

data.picu <- left_join(data.demographics["pie.id"],
                       tmp.locations[c("pie.id", "location")],
                       by = "pie.id") %>%
    mutate(picu.admit = ifelse(!is.na(location), TRUE, FALSE)) %>%
    select(-location)

# vomiting ----

tmp.emesis <- read_edw_data(dir.patients, "vomit", "events") %>%
    semi_join(include, by = "pie.id") %>%
    inner_join(data.steroids[c("pie.id", "first.datetime", "last.datetime")],
               by = "pie.id") %>%
    mutate(event.result = as.numeric(event.result)) %>%
    filter(event.result > 0,
           event.datetime > first.datetime,
           event.datetime < last.datetime + hours(24)) %>%
    distinct(pie.id, .keep_all = TRUE)

data.emesis <- left_join(data.demographics["pie.id"],
                         tmp.emesis[c("pie.id", "event.result")],
                         by = "pie.id") %>%
    mutate(emesis = ifelse(!is.na(event.result), TRUE, FALSE)) %>%
    select(-event.result)

# identifiers ----

data.identifiers <- read_edw_data(dir.patients, "identifiers", "id") %>%
    semi_join(include, by = "pie.id")

# save data files

save_rds(dir.save, "^data")

