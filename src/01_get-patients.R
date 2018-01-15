# include

library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/megan"

# step 1 -----------------------------------------------
# run MBO query:
#   * Patients - By Medication (Generic) - Administration Date
#       - Medication (Generic): albuterol
#       - Facility (Curr): HC Children's
#       - Date Only - Admit

# filter data
raw_patients <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age <= 17,
           visit.type != "Outpatient",
           visit.type != "Emergency",
           discharge.datetime <= mdy("12/31/2017", tz = "US/Central"))

mbo_pie <- concat_encounters(raw_patients$millennium.id)

# step 2 -----------------------------------------------
# limit to patients receiving albuterol between desired date ranges

# run MBO query
#   * Medications - Inpatient - All

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_albuterol <- raw_meds %>%
    filter(med == "albuterol",
           (med.datetime >= mdy("9/22/2016", tz = "US/Central") &
                med.datetime <= mdy("10/19/2016", tz = "US/Central")) |
               (med.datetime >= mdy("9/22/2017", tz = "US/Central") &
                    med.datetime <= mdy("10/19/2017", tz = "US/Central"))) %>%
    mutate(orig.order.id = if_else(order.parent.id == 0, order.id, order.parent.id))

mbo_alb <- concat_encounters(unique(meds_albuterol$millennium.id))

mbo_order <- concat_encounters(unique(meds_albuterol$orig.order.id), 1000)

# step 3 -----------------------------------------------
# run the following MBO queries using mbo_alb
#   * Demographics
#   * Diagnosis Codes (ICD-9/10-CM) - All
#   * Measures
#   * Vitals - BP

# run the following MBO query using mbo_order
#   * Orders Meds - Details - by Order id
#   * Orders Meds - Details - by Order id, Volume

# find ashtma patients; ICD 10 = J45.*

diag_asthma <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis() %>%
    filter(diag.type == "FINAL",
           str_detect(diag.code, "J45"))

pts_asthma <- raw_patients %>%
    semi_join(diag_asthma, by = "millennium.id") %>%
    mutate(group = year(discharge.datetime)) %>%
    select(millennium.id, group)

raw_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

raw_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE)

raw_vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals()

pts_all <- raw_demographics %>%
    left_join(pts_asthma, by = "millennium.id")

# step 4 -----------------------------------------------

raw_orders <- read_data(dir_raw, "orders-details_", FALSE) %>%
    as.order_detail()

raw_orders_vol <- read_data(dir_raw, "orders-details-vol", FALSE) %>%
    rename(`Order Strength Dose` = `Order Volume Dose`,
           `Order Strength Dose Unit` = `Order Volume Dose Unit`) %>%
    as.order_detail()
    # anti_join(raw_orders, c("millennium.id", "order.id"))

orders <- bind_rows(raw_orders, raw_orders_vol) %>%
    arrange(millennium.id, order.id, order.datetime) %>%
    filter(ingredient.unit %in% c("mg", "puff")) %>%
    select(millennium.id, order.id, order.datetime, freq, prn) %>%
    distinct()

meds <- meds_albuterol %>%
    left_join(orders, by = c("millennium.id", "orig.order.id" = "order.id"))

meds_albuterol_cont <- meds %>%
    filter(is.na(order.datetime))
    # filter(!is.na(event.tag) | route == "IV")

mbo_cont <- concat_encounters(unique(meds_albuterol_cont$orig.order.id), 1000)

raw_orders_cont <- read_data(dir_raw, "orders-details-cont", FALSE) %>%
    rename(`Order Strength Dose` = `Order Volume Dose`,
           `Order Strength Dose Unit` = `Order Volume Dose Unit`) %>%
    mutate(Frequency = NA_character_,
           `PRN Indicator` = NA_character_) %>%
    as.order_detail(extras = list(clinical.display = "`Complete Clinical Display Line`"))


# run MBO query
#   * Orders Meds - Details - by Order Id, Cont

meds_albuterol_mdi <- meds %>%
    filter(med.dose.units == "puff")


# run the following queries:
#   * Encounters - by Person ID

# raw_meds_sched <- read_data(dir_raw, "meds_sched") %>%
#     as.meds_sched()
#
# ref <- tibble(name = "albuterol", type = "med", group = "cont")
#
# raw_albuterol <- read_data(dir_raw, "meds_cont_asthma") %>%
#     as.meds_cont() %>%
#     tidy_data(ref, raw_meds_sched) %>%
#     distinct(order.id)
#
# edw_order <- concat_encounters(raw_albuterol$order.id)

# step 5 -----------------------------------------------
# run the following queries:
#   * Orders - from Clinical Event Id - Prompt
