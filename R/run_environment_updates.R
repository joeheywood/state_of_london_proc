# Project: State of London
# Title:  Environment chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Sarah Willis
# Date: May 2023

library(purrr)
library(dplyr)
library(glue)
library(tidyxl)
library(unpivotr)
library(readxl)
# library(vroom)

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 3 (June 2023)/Data/", 
                   "Environment/Environment_April2023.xlsx")

# Function: Inserts data for YP/Education chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_environment_updates <- function(dt_fl, dbfl = "") {
    xfl <- "insert_sol_environment"
    log <- ""
    if(!file.exists(dbfl)) { 
        message("...")
    }
    
    #### Greenhouse Gas Emmission ####
    tryCatch({
        xlsx_cells(dt_fl, "GGasEmissions") %>%
            # filter(row > 9) %>%
            behead("N", "xvarchar") %>%
            behead("W", "yvllb") %>%
            mutate(dataset = "sol_greenhouse", xwhich = 1, xvardt = NA, 
                   yval = numeric, text = "") %>%
            filter(!is.na(yval), !is.na(yvllb), !str_detect(yvllb, "^Total")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Energy Performance ####
    tryCatch({
        read_excel(dt_fl, "ALL Energy performance AC", skip = 3) %>%
            select(xvarchar = 15, yval = 16) %>%
            mutate(xvarchar = str_replace_all(xvarchar, "/", "Q"), yvllb = "",
                   dataset = "epc", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    
    #### Renewable Electricity Generation ####
    tryCatch({
        read_excel(dt_fl, "Electricity generation", col_names = FALSE) %>%
            select(xvarchar = 1, yval = 2) %>%
            mutate(dataset = "bio", xwhich = 1, xvardt = NA, text = "", yvllb = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Household Waste ####
    tryCatch({
        read_excel(dt_fl, "Household waste", skip = 1) |>
            select(xvarchar = 1, yvllb = 2, yval = 3) |>
            filter(!is.na(yval)) |>
            mutate(dataset = "wst", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    
}