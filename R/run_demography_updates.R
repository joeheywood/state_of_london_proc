# Project: State of London
# Title: Demography chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Ben Corr
# Date: May 2023

library(purrr)
library(dplyr)
library(glue)
library(tidyxl)
library(unpivotr)
library(readxl)
# library(vroom)

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Demography/demography_chapter_nov23.xlsx")

# Function: Inserts data for Demography chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_population_updates <- function(dt_fl, dbfl = "") {
    stopifnot(file.exists(dt_fl))
    log <- ""
    if(!file.exists(dbfl)) print("NO DB FILE")
    
    xfl <- "insert_sol_demography"
    excel_sheets(dt_fl)
    
    #### Fig 1. London Population #### 
    tryCatch({
        read_excel(dt_fl, "fig1 london population") %>%
            mutate(dataset = "sol_lpop", xwhich = 2, xvardt = date, xvarchar = "",
                   yvllb = source, yval = value,
                   text = ifelse(yvllb == "Census estimates", "dotted", "solid")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 2. London population cob - UK/non-UK ####
    tryCatch({
        read_excel(dt_fl, "fig2 pop cob uk non-uk") %>%
            mutate(dataset = "sol_cob_uk", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yvllb = cob, text = "dotted", yval = value) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 3. London population cob ####
    tryCatch({
        read_excel(dt_fl, "fig3 pop cob") %>%
            mutate(dataset = "sol_cob", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yvllb = cob, text = "dotted", yval = value) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 4. London Births ####
    tryCatch({
        read_excel(dt_fl, "fig4 london births") %>%
            mutate(dataset = "sol_ann_births", xwhich = 2, 
                   xvardt = year_ending_date, 
                   xvarchar = "", yval = annual_births, text = "", 
                   yvllb = type) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    
    #### Fig 5. Annual births - mother's region ####
    tryCatch({
        read_excel(dt_fl, "fig5 births mcob uk non-uk") %>%
            mutate(dataset = "sol_mcob_uk", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yval = value, text = "", yvllb = mcob) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 6. Annual births - mother's place of birth ####
    tryCatch({
        read_excel(dt_fl, "fig6 births mcob") %>%
            mutate(dataset = "sol_mcob", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yval = value, text = "", yvllb = mcob) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    TRUE
}