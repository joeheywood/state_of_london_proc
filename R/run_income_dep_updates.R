# Project: State of London
# Title: Young people and education chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Rachel Leeser
# Date: 9 May 2023

#library(resdata)
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(tidyxl)
library(unpivotr)
library(vroom)
library(readxl)

# dt_pth <- file.path("C:/Users/joheywood/Greater London Authority/",
#                    "IU - State of London report/Version 4 (January 2024)/Data/",
#                    "Income, poverty and destitution/Data for IPD charts.xlsx")

dt_pth <- file.path("C:/Users/joheywood/Greater London Authority/", 
                    "IU - Shared Projects/State of London report/", 
                    "Version 5 (June 2024)/Data/Income, poverty and destitution/", 
                    "IPD data using JH template.xlsx")
# inc_prev <- "C:/Users/joheywood/Greater London Authority/IU - State of London report/Version 3 (June 2023)/Data/Income, poverty and destitution/Data for IPD charts v3.xlsx"

hbuc <- file.path("C:/Users/joheywood/Greater London Authority/", 
                  "IU - Shared Projects/State of London report/", 
                  "Version 5 (June 2024)/Data/", 
                  "Income, poverty and destitution/hh_uc.csv")

ucws <- file.path("C:/Users/joheywood/Greater London Authority/", 
                  "IU - Shared Projects/State of London report/", 
                  "Version 5 (June 2024)/Data/", 
                  "Income, poverty and destitution/pucws.csv") 

pchb <-file.path("C:/Users/joheywood/Greater London Authority/", 
                 "IU - Shared Projects/State of London report/", 
                 "Version 5 (June 2024)/Data/",  
                 "Income, poverty and destitution/pchb.csv") 

# Function: Inserts data for Income/Deprivation chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_income_dep_updates <- function(dt_pth, hbuc, ucws,pchb, dbfl = "") {
    
    xfl <- "insert_sol_pov_dest"
    log <- ""
    
    if(!file.exists(dbfl)) { print("no db file") }
    
    #### Disposable Household Income ####
    tryCatch({
        run_wide(dt_pth, "2", xwch = 1, dcode = "sol_dhhinc") |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "2", skip = 2) %>%
        #     select( xvarchar = 1, `0.1`, median, `0.9`) %>%
        #     pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_dhhinc", xwhich = 1, xvardt = NA,
        #            text = "") %>%
        #     insert_db(log = log, excfl = xfl)

    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Universal Credit/Housing Benefits Households ####
    tryCatch({
        vroom(hbuc) |>
            mutate(xwhich = 2, xvarchar = "", xvardt = format(xd, "%Y-%m-%d"),
                   yvllb = b, yval = y, text = "") |>
            insert_db(log = log, excfl = xfl )
        
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Universal Credit/Work Status ####
    tryCatch({
        vroom(ucws) |>
            mutate(dataset = "sol_ucws", xwhich = 2, xvarchar = "",
                   xvardt = format(xd, "%Y-%m-%d"),
                   yvllb = b, yval = y, text = "") |>
            insert_db(log = log, excfl = xfl )
        
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Pensioners  ####
    
    tryCatch({
        lkp <- c(
            `PC only` = "Pension Credit only",
            `PC and HB` = "Pension Credit and Housing Benefit",
            `HB only` = "Housing Benefit only")
        vroom(pchb) |>
            mutate(xvardt = as.Date(paste0("1 ", xvardt), format = "%d %b-%y")) |>
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") |>
            mutate(xvardt = format(xvardt, "%Y-%m-%d"), dataset = "sol_pensmeans",
                   xwhich = 2, xvarchar = "", text = "",
                   yvllb = lkp[yvllb]) |>
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    
    #### Relative Poverty ####
    tryCatch({
        a <- run_wide(dt_pth, "6", xwch = 1, dcode = "sol_relpov" ) |>
            filter(xvarchar > "07/08-09/10" & xvarchar < "9") |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 7", skip = 2) %>%
        #     filter(Year > "07/08-09/10" & Year < "9") %>%
        #     pivot_longer(-Year, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_relpov", xwhich = 1, xvarchar = Year,
        #            xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    ### Persisent Poverty ####
    tryCatch({
        run_wide(dt_pth, "7", xwch = 1, dcode = "sol_prspov" ) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 8", skip = 2) %>%
        #     select(xvarchar = 1, London, UK) %>%
        #     pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_prspov", xwhich = 1, xvardt = NA,
        #            # text = ifelse(yvllb == "London", "Chart_left", "Chart_right")) %>%
        #            text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    ####### ADJUSTED ###########################
    tryCatch({
        run_wide(dt_pth, "8", xwch = 2, dcode = "sol_struggfin" ) |>
            insert_db(log = log, excfl = xfl)
        # xlsx_cells(dt_pth, "Fig 9") %>%
        #     filter(row > 27) %>%
        #     behead("N", "xvardt") %>%
        #     behead("W", "longlab") %>%
        #     behead("W", "yvllb") %>%
        #     filter(!is.na(xvardt), !is.na(numeric))%>%
        #     mutate(dataset = "sol_struggfin", xwhich = 2,
        #            xvarchar = "", text = "", yval = numeric) %>%
        #     filter(!is.na(yvllb)) |> 
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd strugg fin")}  )
    
    ### Material deprivation children ####
    tryCatch({
        run_wide(dt_pth, "9", xwch = 1, dcode = "sol_matdep_c" ) |>
            insert_db(log = log, excfl = xfl)
        # xlsx_cells(dt_pth, "Fig 10") |>
        #     filter(row > 4) |>
        #     behead("W", "xvarchar") |>
        #     behead("N", "cat") |>
        #     mutate(yvllb = case_when(
        #         cat == 0.5 ~ "Severe low income",
        #         cat == 0.6 ~ "Low income",
        #         cat == 0.7 ~ "Marginal low income")
        #         ) |>
        #     mutate(dataset = "sol_matdep_c", xwhich = 1, xvardt =  NA,
        #            yval = numeric, text = "") |>
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Material deprivation elderly ####
    tryCatch({
        run_wide(dt_pth, "10", xwch = 1, dcode = "sol_matdep_o" ) |>
            mutate(yvllb = ifelse(str_detect(xvarchar, "London"), "London", "Other")) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 11", skip = 2) %>%
        #     select(xvarchar = 1, yval = 2) %>%
        #     mutate(yval = as.numeric(yval)) %>%
        #     filter(!is.na(yval)) %>%
        #     mutate(dataset = "sol_matdep_o", xwhich = 1,
        #            yvllb = ifelse(str_detect(xvarchar, "London"), "London", "Other"),
        #            xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Fuel Poverty ####
    tryCatch({
        run_wide(dt_pth, "11", xwch = 1, dcode = "sol_fuelpov" ) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 12", skip = 3) %>% 
        #     pivot_longer(-Year, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_fuelpov", xwhich = 1, xvarchar = Year,
        #            xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Food Security ####
    tryCatch({
        run_wide(dt_pth, "12", xwch = 1, dcode = "sol_foodsec" ) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 13", skip = 3) %>% 
        #     filter(!is.na(Low)) %>%
        #     pivot_longer(-`Region/Country`, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_foodsec", xwhich = 1, xvarchar = `Region/Country`,
        #            xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
    #### Personal insolvencies ####
    tryCatch({
        run_wide(dt_pth, "13", xwch = 1, dcode = "sol_persins" ) |>
            mutate(xvardt = as.Date(paste0(xvarchar, "-01-01")), xwhich = 2) |>
            mutate(xvarchar = "") |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_pth, "Fig 14", skip = 3) %>% 
        #     pivot_longer(-Year, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_persins", xwhich = 2, xvarchar = "",
        #            xvardt = as.Date(paste0(Year, "-01-01")), text = "") %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - sol ipd")}  )
    
}