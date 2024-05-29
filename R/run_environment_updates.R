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
library(vroom)

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/", 
                   "IU - Shared Projects/State of London report/", 
                   "Version 5 (June 2024)/Data/Environment/Environment v5.xlsx")

aq_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Environment/airquality_data.csv")
# Function: Inserts data for YP/Education chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_environment_updates <- function(dt_fl,  dbfl = "") {
    xfl <- "insert_sol_environment"
    log <- ""
    if(!file.exists(dbfl)) { 
        message("...")
    }
    
    #### Greenhouse Gas Emmission ####
    tryCatch({
        run_wide(dt_fl, "Fig 1 GHG emissions", xwch = 1, dcode = "sol_greenhouse") |>
            insert_db(log = log, excfl = xfl)
    #     env_lkp <- c(
    #         `Domestic Energy` = "Domestic Energy",
    #         `Industrial & commercial` = "Industrial & Commercial", 
    #         `STATIONARY ENERGY: FUGITIVE` = "Stationary Energy",
    #         `Transport` = "Transport", 
    #         `Waste` = "Waste", 
    #         `Industrial processes and product use (IPPU)` = "Industrial processes and product use", 
    #         `Agriculture, forestry, and other land use (AFOLU)`="Agriculture, forestry, and other land use" 
    #     )
    #     xlsx_cells(dt_fl, "GGasEmissions") %>%
    #         # filter(row > 9) %>%
    #         behead("N", "xvarchar") %>%
    #         behead("W", "yvllb") %>%
    #         mutate(yvllb = env_lkp[yvllb]) |>
    #         mutate(dataset = "sol_greenhouse", xwhich = 1, xvardt = NA, 
    #                yval = numeric, text = "") %>%
    #         filter(!is.na(yval), !is.na(yvllb), !str_detect(yvllb, "^Total")) %>%
    #         insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Greenhouse Gas per capita ####
    tryCatch({
        run_wide(dt_fl, "Fig 2 consumption GHG emissions", xwch = 1, dcode = "sol_pc_greenhouse") |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Energy Performance ####
    tryCatch({
        run_wide(dt_fl, "Fig 10 Energy performance", xwch = 1, dcode = "epc") |>
            mutate(xvarchar = str_replace_all(xvarchar, "/", "Q")) |>
            insert_db(log = log, excfl = xfl)
        #     
        # read_excel(dt_fl, "ALL Energy performance AC", skip = 3) %>%
        #     select(xvarchar = 15, yval = 16) %>%
        #     mutate(xvarchar = str_replace_all(xvarchar, "/", "Q"), yvllb = "",
        #            dataset = "epc", xwhich = 1, xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    
    #### Renewable Electricity Generation ####
    tryCatch({
        run_wide(dt_fl, "Fig 12 Renewable energy gen", xwch = 1, dcode = "bio") |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_fl, "Electricity generation", col_names = FALSE) %>%
        #     select(xvarchar = 1, yval = 2) %>%
        #     filter(!is.na(yval)) |>
        #     mutate(dataset = "bio", xwhich = 1, xvardt = NA, text = "", 
        #            yvllb = "", xvarchar = as.character(xvarchar)) %>%
        #     filter(!is.na(xvarchar)) |>
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Household Waste ####
    tryCatch({
        run_wide(dt_fl, "Fig 8 Household waste", xwch = 1, dcode = "wst") |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_fl, "Household waste", skip = 1) |>
        #     select(xvarchar = 1, yvllb = 2, yval = 3) |>
        #     filter(!is.na(yval)) |>
        #     mutate(dataset = "wst", xwhich = 1, xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Recycling ####
    tryCatch({
        run_wide(dt_fl, "Fig 9 Recycling rates", xwch = 1, dcode = "rcyc") |>
            insert_db(log = log, excfl = xfl)
        # xlsx_cells(dt_fl, "Recycling rate") |>
        #     filter(row > 5) |>
        #     behead("N", xvarchar) |>
        #     behead("W", "yvllb") |>
        #     filter(!is.na(xvarchar), !is.na(yvllb))  |>
        #     # select(xvardt, yvllb, yval = numeric)
        #     mutate(dataset = "rcyc", xwhich = 1, xvardt = NA, text = "",
        #            yval = numeric) %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Air Quality #### 
    tryCatch({
        nrd <- read_excel(dt_fl, "NO2 roadside") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_no2", 
                   chart = "Roadside", text = "")
        nub <- read_excel(dt_fl, "NO2 UB") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_no2", 
                   chart = "Urban Background", text = "")
        bind_rows(nrd, nub) |>
            insert_db(log = log, excfl = xfl) 
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Air Quality #### 
    tryCatch({
        nrd <- read_excel(dt_fl, "PM10 roadside") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_pm10", 
                   chart = "Roadside", text = "")
        nub <- read_excel(dt_fl, "PM10 UB") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_pm10", 
                   chart = "Urban Background", text = "")
        bind_rows(nrd, nub) |>
            insert_db(log = log, excfl = xfl) 
    }, error = function(e){error_log(e, "SoL - Env")}  )
    
    #### Air Quality #### 
    tryCatch({
        nrd <- read_excel(dt_fl, "PM25 roadside") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_pm25", 
                   chart = "Roadside", text = "")
        nub <- read_excel(dt_fl, "PM25 UB") |>
            select(xvardt = Month, yvllb = Category, yval = Value) |>
            mutate(xwhich = 2, xvarchar = "", dataset = "aq_pm25", 
                   chart = "Urban Background", text = "")
        bind_rows(nrd, nub) |>
            insert_db(log = log, excfl = xfl) 
    }, error = function(e){error_log(e, "SoL - Env")}  )
}