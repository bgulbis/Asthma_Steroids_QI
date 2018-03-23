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
    mutate(asthma = TRUE) %>%
    select(millennium.id, asthma)
    # mutate(group = year(discharge.datetime)) %>%
    # select(millennium.id, group)

raw_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

raw_measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE)

raw_vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals()

pts_all <- raw_demographics %>%
    semi_join(pts_asthma, by = "millennium.id")

# orders -----------------------------------------------

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

# albuterol --------------------------------------------
meds <- meds_albuterol %>%
    left_join(orders, by = c("millennium.id", "orig.order.id" = "order.id")) %>%
    semi_join(pts_asthma, by = "millennium.id")

meds_albuterol_cont <- meds %>%
    filter(!is.na(event.tag) | med.dose.units == "microgram")
    # filter(is.na(order.datetime))
    # filter(!is.na(event.tag) | route == "IV")

meds_albuterol_cont_run <- meds_albuterol %>%
    filter(!is.na(event.tag) | med.dose.units == "microgram") %>%
    mutate_at("med.rate", funs(na_if(., 0L))) %>%
    mutate_at("med.rate", funs(coalesce(., med.dose))) %>%
    mutate_at("med.rate.units", funs(coalesce(., med.dose.units))) %>%
    calc_runtime()

meds_albuterol_cont_dosing <- summarize_data(meds_albuterol_cont_run)

mbo_cont <- concat_encounters(unique(meds_albuterol_cont$orig.order.id), 1000)

raw_orders_cont <- read_data(dir_raw, "orders-details-cont", FALSE) %>%
    rename(`Order Strength Dose` = `Order Volume Dose`,
           `Order Strength Dose Unit` = `Order Volume Dose Unit`) %>%
    mutate(Frequency = NA_character_,
           `PRN Indicator` = NA_character_) %>%
    as.order_detail(extras = list(clinical.display = "Complete Clinical Display Line"))

# run MBO query
#   * Orders Meds - Details - by Order Id, Cont

meds_albuterol_mdi_run <- meds_albuterol %>%
    filter(med.dose.units == "puff") %>%
    calc_runtime(cont = FALSE) %>%
    mutate(orig.order.id = if_else(order.parent.id == 0, order.id, order.parent.id)) %>%
    left_join(orders, by = c("millennium.id", "orig.order.id" = "order.id"))

meds_albuterol_mdi_dosing <- summarize_data(meds_albuterol_mdi_run, cont = FALSE)

meds_albuterol_neb_run <- meds_albuterol %>%
    filter(is.na(event.tag),
           med.dose.units != "microgram",
           med.dose.units != "puff") %>%
    calc_runtime(cont = FALSE) %>%
    mutate(orig.order.id = if_else(order.parent.id == 0, order.id, order.parent.id)) %>%
    left_join(orders, by = c("millennium.id", "orig.order.id" = "order.id"))

meds_albuterol_neb_dosing <- summarize_data(meds_albuterol_neb_run, cont = FALSE)

# other ---------------------------------------------

meds_steroids_run <- raw_meds %>%
    semi_join(pts_asthma, by = "millennium.id") %>%
    filter(med %in% c("prednisone", "prednisolone", "methylprednisolone", "dexamethasone")) %>%
    calc_runtime(cont = FALSE)

meds_steroids_dosing <- summarize_data(meds_steroids_run, cont = FALSE)

meds_mag <- raw_meds %>%
    semi_join(pts_asthma, by = "millennium.id") %>%
    filter(str_detect(med, "magnesium"),
           route %in% c("IV", "IVPB"))

meds_mag_cont <- meds_mag %>%
    filter(!is.na(event.tag))

if (nrow(meds_mag_cont) > 0) {
    meds_mag_cont_run <- meds_mag_cont %>%
        mutate_at("med.rate", funs(na_if(., 0L))) %>%
        mutate_at("med.rate", funs(coalesce(., med.dose))) %>%
        mutate_at("med.rate.units", funs(coalesce(., med.dose.units))) %>%
        calc_runtime()

    meds_mag_cont_dosing <- summarize_data(meds_mag_cont_run)
}

meds_mag_run <- meds_mag %>%
    filter(is.na(event.tag)) %>%
    calc_runtime(cont = FALSE)

meds_mag_dosing <- summarize_data(meds_mag_run, cont = FALSE)

# measures ---------------------------------------------

measures <- raw_measures %>%
    filter(!is.na(event.result.units)) %>%
    arrange(millennium.id, event.datetime) %>%
    group_by(millennium.id, event, event.result.units) %>%
    mutate_at("event.result", as.numeric) %>%
    summarize_at("event.result", first) %>%
    ungroup() %>%
    select(-event.result.units) %>%
    spread(event, event.result)

# data sets --------------------------------------------

group <- meds_albuterol %>%
    # mutate(year = floor_date(med.datetime, "year")) %>%
    mutate(year = year(med.datetime)) %>%
    distinct(millennium.id, year)

data_demographics <- pts_all %>%
    semi_join(meds_albuterol, by = "millennium.id") %>%
    left_join(group, by = "millennium.id") %>%
    left_join(pts_asthma, by = "millennium.id") %>%
    mutate_at("asthma", funs(coalesce(., FALSE))) %>%
    left_join(measures, by = "millennium.id") %>%
    select(-disposition, -visit.type, -facility)

write.csv(data_demographics, "data/external/demographics.csv", row.names = FALSE)
write.csv(meds_albuterol_cont_run, "data/external/albuterol_cont_all.csv", row.names = FALSE)
write.csv(meds_albuterol_cont_dosing, "data/external/albuterol_cont_summary.csv", row.names = FALSE)
write.csv(meds_albuterol_mdi_run, "data/external/albuterol_mdi_all.csv", row.names = FALSE)
write.csv(meds_albuterol_mdi_dosing, "data/external/albuterol_mdi_summary.csv", row.names = FALSE)
write.csv(meds_albuterol_neb_run, "data/external/albuterol_neb_all.csv", row.names = FALSE)
write.csv(meds_albuterol_neb_dosing, "data/external/albuterol_neb_summary.csv", row.names = FALSE)
write.csv(meds_steroids_run, "data/external/steroids_all.csv", row.names = FALSE)
write.csv(meds_steroids_dosing, "data/external/steroids_summary.csv", row.names = FALSE)

if (exists("meds_mag_cont_run")) write.csv(meds_mag_cont_run, "data/external/magnesium_cont_all.csv", row.names = FALSE)
if (exists("meds_mag_cont_dosing")) write.csv(meds_mag_cont_dosing, "data/external/magnesium_cont_summary.csv", row.names = FALSE)

write.csv(meds_mag_run, "data/external/magnesium_all.csv", row.names = FALSE)
write.csv(meds_mag_dosing, "data/external/magnesium_summary.csv", row.names = FALSE)
