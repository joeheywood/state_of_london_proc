# Project: State of London
# Title: Crime chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Sophie Deakin
# Date: May 2023

library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(tidyxl)
library(unpivotr)
library(readxl)
# library(vroom)

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Crime and Safety/Crime and Safety Chapter - November 2023 - Data File.xlsx")

# Function: Inserts data for the Crime chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_crime_updates <- function(dt_fl, db_fl) {
    stopifnot(file.exists(dt_fl))
    
    xfl <- "insert_sol_crime"
    log <- ""
    
    if(!file.exists(db_fl)) warning("DB file looks wrong")
    
    
    
    #### TNO ####
    tryCatch({
        read_excel(dt_fl, "TNO") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_tno", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    
    
    #### ASB Calls ####
    tryCatch({
        read_excel(dt_fl, "ASB_Calls") %>%
            select(xvardt = `Month-Year`, yval = `ASB Calls`) %>%
            mutate(dataset = "sol_asb", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Robbery Offences ####
    tryCatch({
        read_excel(dt_fl, "Robbery_Offences") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_rob", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Theft Person Offences ####
    tryCatch({
        read_excel(dt_fl, "Theft_Person_Offences") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_tpo", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Theft from Motor Vehicles Offences ####
    tryCatch({
        read_excel(dt_fl, "TfMV_Offences") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_tfmv", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Fraud ####
    tryCatch({
        read_excel(dt_fl, "Fig11 - Fraud") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_fraud", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Non DA VWI ####
    tryCatch({
        read_excel(dt_fl, "Non-DA VWI") %>%
            select(xvardt = `Month-Year`, yval = `Offences`) %>%
            filter(!is.na(yval)) |>
            mutate(dataset = "sol_vwi", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Domestic Abuse ####
    tryCatch({
        read_excel(dt_fl, "Domestic_Abuse") %>%
            select(xvardt = `Month-Year`, yval = `Offences`) %>%
            filter(!is.na(yval)) |>
            mutate(dataset = "sol_da", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Homicide ####
    tryCatch({
        read_excel(dt_fl, "Homicide") %>%
            select(xvardt = `Month-Year`, yval = `Homicide`) %>%
            mutate(dataset = "sol_hmcd", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Youth Knife Crime ####
    tryCatch({
        read_excel(dt_fl, "AwI_Knife_U25") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            mutate(dataset = "sol_knife", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Harassment ####
    tryCatch({
        read_excel(dt_fl, "Harassment") %>%
            select(xvardt = `Month-Year`, yval = Offences) %>%
            filter(!is.na(yval)) |>
            mutate(dataset = "sol_hrsm", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"),
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Sexual Offences ####
    tryCatch({
        read_excel(dt_fl, "Sexual Offences") %>%
            select(`Month-Year`:`Rape Offences`) |>
            pivot_longer(-`Month-Year`, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_sxoff", xwhich = 2, xvarchar = "", 
                   xvardt = format(`Month-Year`, "%Y-%m-%d"), text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Victim Satisfaction ####
    tryCatch({
        rgx <- "Q(\\d{1}) (\\d{2})/(\\d{2})"
        read_excel(dt_fl, "Victim_Satisfaction_USS") %>%
            mutate(y1 = str_replace(Quarter, rgx, "20\\2"),
                   y2 = str_replace(Quarter, rgx, "20\\3"),
                   q = as.numeric(str_replace(Quarter, rgx, "\\1"))) %>%
            mutate(
                xvarchar = case_when(
                    q %in% 1:3 ~ paste0(y1, "Q", (q + 1)),
                    q %in% 4 ~ paste0(y2, "Q1")
                )
            ) %>%
            select(xvarchar, 2:4) %>%
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_vctsat", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Fair Treatment ####
    tryCatch({
        read_excel(dt_fl, "Fair Treatment") %>%
            mutate(dataset = "sol_fair", xwhich = 2, xvarchar = "", 
                   yval = Proportion, xvardt = format(`R12 to Date`, "%Y-%m-%d"), 
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### Burglary Offences ####
    tryCatch({
        read_excel(dt_fl, "Burglary_Offences") %>%
            select(xvardt = 1, `Residential Burglary`, `Business and Community Burglary`) |>
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_brglry", xwhich = 2, xvarchar = "", 
                   xvardt = format(xvardt, "%Y-%m-%d"), text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
    
    #### After Dark ####
    tryCatch({
        rgx <- "Q(\\d{1}) (\\d{2})-(\\d{2})"
        read_excel(dt_fl, "Safety_After_Dark") %>%
            rename(Quarter = 1) %>%
            mutate(y1 = str_replace(Quarter, rgx, "20\\2"),
                   y2 = str_replace(Quarter, rgx, "20\\3"),
                   q = as.numeric(str_replace(Quarter, rgx, "\\1"))) %>%
            mutate(
                xvarchar = case_when(
                    q %in% 1:3 ~ paste0(y1, "Q", (q + 1)),
                    q %in% 4 ~ paste0(y2, "Q1")
                )
            ) %>%
            select(xvarchar, 
                   Male = `Feel Safe (Male)`, 
                   Female = `Feel Safe (Female)`, 
                   All = `Feel Safe (All)`) %>%
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_dark", xwhich = 1, xvardt = NA, 
                   text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Crime")}  )
}