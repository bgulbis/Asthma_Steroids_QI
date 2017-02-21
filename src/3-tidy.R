# tidy

library(tidyverse)
library(stringr)
library(lubridate)
library(edwr)
library(icd)

dir_raw <- "data/raw/mbo"
patients <- read_rds("data/final/patients.Rds")
include <- tibble(millennium.id = patients$include)

# demographics -----------------------------------------

data_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics() %>%
    semi_join(include, by = "millennium.id")

# groups -----------------------------------------------

# ref <- tibble(name = c("dexamethasone", "prednisone", "prednisolone"),
#               type = "med",
#               group = "sched")
#

raw_meds_inpt <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

tmp_steroids <- raw_meds_inpt %>%
    filter(med %in% c("dexamethasone", "methylprednisolone", "prednisolone", "prednisone")) %>%
    # tidy_data(ref) %>%
    semi_join(include, by = "millennium.id") %>%
    dmap_at("order.parent.id", ~ na_if(.x, 0)) %>%
    mutate(order.parent.id = coalesce(order.parent.id, order.id))

data_groups <- tmp_steroids %>%
    mutate(dex = med == "dexamethasone") %>%
    group_by(millennium.id) %>%
    summarize(group = sum(dex) >= 1) %>%
    mutate(group = if_else(group, "dexamethasone", "prednisone"))

# primary diagnosis ------------------------------------

data_primary_diagnosis <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    tidy_data() %>%
    semi_join(include, by = "millennium.id") %>%
    filter(diag.type == "FINAL",
           diag.seq == "Primary") %>%
    by_row(~icd_explain(.x$diag.code), .to = "icd.description", .collate = "rows")

# measures ---------------------------------------------

tmp_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.measures() %>%
    semi_join(include, by = "millennium.id")

tmp_height <- tmp_measures %>%
    filter(measure == "height",
           measure.units == "cm") %>%
    group_by(millennium.id) %>%
    arrange(measure.datetime) %>%
    summarize(height = first(measure.result))

data_measures <- tmp_measures %>%
    filter(measure == "weight",
           measure.units == "kg") %>%
    group_by(millennium.id) %>%
    arrange(measure.datetime) %>%
    summarize(weight = first(measure.result)) %>%
    left_join(tmp_height, by = "millennium.id")

# asthma assessment ------------------------------------

data_asthma <- read_data(dir_raw, "events_asthma_scores", FALSE) %>%
    as.events() %>%
    semi_join(include, by = "millennium.id") %>%
    mutate(post = ifelse(str_detect(event, "post"), TRUE, FALSE),
           event = str_replace_all(event, "  ", " "),
           event = str_replace_all(event, "(rc|pre|post| asthma assess|asthma treatment )", ""),
           event = str_trim(event, "both"),
           event = str_replace_all(event, " ", "_")) %>%
    filter(event.result != "") %>%
    group_by(millennium.id, event.datetime, post, event) %>%
    summarize(event.result = first(event.result)) %>%
    group_by(millennium.id, event.datetime, post) %>%
    spread(event, event.result)

# steroids ---------------------------------------------

tmp_freq <- read_data(dir_raw, "orders-details", FALSE) %>%
    as.order_detail() %>%
    distinct(millennium.id, order.id, freq)

data_steroids <- tmp_steroids %>%
    left_join(tmp_freq, by = c("millennium.id", "order.parent.id" = "order.id")) %>%
    group_by(millennium.id, med) %>%
    arrange(med.datetime) %>%
    summarize(num.doses = n(),
              first.dose = first(med.dose),
              first.freq = first(freq),
              total.dose = sum(med.dose),
              first.datetime = first(med.datetime),
              last.datetime = last(med.datetime),
              duration = difftime(last(med.datetime), first(med.datetime),
                                  units = "hours"))

# adjunct medications ----------------------------------

tmp_first_steroid <- data_steroids %>%
    group_by(millennium.id) %>%
    summarize(first.datetime = first(first.datetime))

# ref <- tibble(name = c("albuterol", "ipratropium", "albuterol-ipratropium",
#                        "ipratropium nasal", "magnesium sulfate",
#                        "magnesium oxide", "terbutaline"),
#               type = "med",
#               group = "sched")

adj_meds <- c("albuterol", "ipratropium", "albuterol-ipratropium",
              "ipratropium nasal", "magnesium sulfate", "magnesium oxide",
              "terbutaline")

raw_meds_sched <- raw_meds_inpt %>%
    filter(med %in% adj_meds) %>%
    # tidy_data(ref) %>%
    semi_join(include, by = "millennium.id")

tmp_meds <- raw_meds_sched %>%
    inner_join(tmp_first_steroid, by = "millennium.id") %>%
    group_by(millennium.id, med) %>%
    arrange(med.datetime)

tmp_meds_adjunct <- tmp_meds %>%
    filter(med.datetime > first.datetime) %>%
    group_by(millennium.id, med, med.dose.units) %>%
    summarize(total.dose.after = sum(med.dose))

data_meds_adjunct <- tmp_meds %>%
    group_by(millennium.id, med, med.dose.units) %>%
    summarize(total.dose = sum(med.dose)) %>%
    full_join(tmp_meds_adjunct, by = c("millennium.id", "med", "med.dose.units"))

# ref_cont <- tibble(name = "albuterol", type = "med", group = "cont")

raw_albuterol_cont <- raw_meds_inpt %>%
    filter(!is.na(event.tag),
           med == "albuterol") %>%
    # tidy_data(ref_cont, raw_meds_sched) %>%
    semi_join(include, by = "millennium.id")

# concat_encounters(tmp.meds.cont$order.id, 900)

tmp_albuterol_dc <- read_data(dir_raw, "orders-actions", FALSE) %>%
    as.order_action() %>%
    semi_join(include, by = "millennium.id") %>%
    filter(action.type == "Discontinue") %>%
    distinct(millennium.id, order.id, action.datetime) %>%
    rename(dc.datetime = action.datetime)

tmp_albuterol_order <- read_data(dir_raw, "orders-edw-detail") %>%
    as.order_detail() %>%
    filter(ingredient == "albuterol",
           action.type == "Order") %>%
    dmap_at("ingredient.dose", as.numeric)

tmp_albuterol_cont <- raw_albuterol_cont %>%
    filter(str_detect(event.tag, "Begin Bag")) %>%
    full_join(tmp_albuterol_order, by = "order.id") %>%
    full_join(tmp_albuterol_dc, by = c("millennium.id", "order.id")) %>%
    select(millennium.id:med, route, event.tag, ingredient.dose, ingredient.unit,
           dc.datetime) %>%
    inner_join(tmp_first_steroid, by = "millennium.id") %>%
    distinct(millennium.id, order.id, .keep_all = TRUE) %>%
    group_by(millennium.id) %>%
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
    group_by(millennium.id, med, ingredient.unit) %>%
    summarize(total.dose = sum(dose.before) + sum(dose.after),
              total.dose.after = sum(dose.after)) %>%
    mutate(total.dose = coalesce(total.dose, 0),
           total.dose.after = coalesce(total.dose.after, 0)) %>%
    select(millennium.id, med, med.dose.units = ingredient.unit, total.dose, total.dose.after) %>%
    filter(total.dose > 0)

data_meds_adjunct <- bind_rows(data_meds_adjunct, tmp_albuterol_cont) %>%
    dmap_at("total.dose.after", ~ coalesce(.x, 0))

# readmissions -----------------------------------------

raw_encounters <- read_data(dir_raw, "encounters") %>%
    as.encounters()

tmp_index <- raw_encounters%>%
    group_by(person.id) %>%
    arrange(admit.datetime) %>%
    left_join(data_demographics[c("millennium.id", "age")], by = "millennium.id") %>%
    filter(!is.na(age)) %>%
    select(person.id, index.datetime = admit.datetime)

tmp_readmit <- raw_encounters %>%
    inner_join(tmp_index, by = "person.id") %>%
    filter(admit.datetime > index.datetime,
           visit.type %in% c("Inpatient", "OBS Observation Patient")) %>%
    group_by(person.id) %>%
    mutate(readmit.days = difftime(admit.datetime, index.datetime,
                                   units = "days")) %>%
    summarize(readmit.days = min(readmit.days))

raw_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

data_readmit <- data_demographics %>%
    left_join(raw_id, by = "millennium.id") %>%
    left_join(tmp_readmit, by = "person.id") %>%
    mutate(readmit.30days = if_else(readmit.days <= 30, TRUE, FALSE, FALSE),
           readmit.7days = if_else(readmit.days <= 7, TRUE, FALSE, FALSE)) %>%
    select(millennium.id, readmit.days:readmit.7days)

# icu admission ----

raw_locations <- read_data(dir_raw, "location", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(include, by = "millennium.id") %>%
    filter(location == "HC PICU") %>%
    distinct(millennium.id, .keep_all = TRUE)

data_picu <- left_join(data_demographics["millennium.id"],
                       raw_locations[c("millennium.id", "location")],
                       by = "millennium.id") %>%
    mutate(picu.admit = ifelse(!is.na(location), TRUE, FALSE)) %>%
    select(-location)

# vomiting ---------------------------------------------

raw_emesis <- read_data(dir_raw, "output-emesis") %>%
    as.events() %>%
    left_join(raw_id, by = "pie.id") %>%
    semi_join(include, by = "millennium.id") %>%
    inner_join(data_steroids[c("millennium.id", "first.datetime", "last.datetime")],
               by = "millennium.id") %>%
    mutate(event.result = as.numeric(event.result)) %>%
    filter(event.result > 0,
           event.datetime > first.datetime,
           event.datetime < last.datetime + hours(24)) %>%
    distinct(millennium.id, .keep_all = TRUE)

data_emesis <- left_join(data_demographics["millennium.id"],
                         raw_emesis[c("millennium.id", "event.result")],
                         by = "millennium.id") %>%
    mutate(emesis = ifelse(!is.na(event.result), TRUE, FALSE)) %>%
    select(-event.result)

# identifiers ------------------------------------------

data_identifiers <- raw_id %>%
    semi_join(include, by = "millennium.id")

dirr::save_rds("data/tidy", "data_")
