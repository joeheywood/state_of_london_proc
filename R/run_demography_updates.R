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

# dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
#                    "IU - State of London report/Version 4 (January 2024)/Data/",
#                    "Demography/demography_chapter_Dec23.xlsx")


# ddr <- file.path("C:/Users/joheywood/Greater London Authority/", 
#                  "IU - Shared Projects/State of London report/", 
#                  "Version 5 (June 2024)/Data/Demography/")
# 
# f1 <- file.path(ddr, "Fig1_population_ons_London_2022.csv")
# f2 <- file.path(ddr, "Fig2_(oldFig1)_populationChange_ed.csv")
# f3 <- file.path(ddr, "Fig3_(oldFig2)_COBChange_UKnonUK.csv" )
# f4 <- file.path(ddr, "Fig4_(oldFig3)_COBChange_nonUK_Region.csv")
# f5 <- file.path(ddr, "Fig5_Births_change.csv")
# f6 <- file.path(ddr, "Fig6_(oldFig5)_BirthsChange_MothersUKnonUK.csv")
# f7 <- file.path(ddr, "Fig7_(oldFig6)_BirthsChange_nonUK_MothersRegionOB.csv")
# f8 <- file.path(ddr, "Fig8_London_Boroughs_IntMig.csv")
# f9 <- file.path(ddr, "Fig9_london_domMig_longit_corrected.csv")
# f10 <- file.path(ddr, "Fig10_London_dom_mign_net_bySYA_2022.csv")

# Function: Inserts data for Demography chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_population_updates <- function(f1,f2,f3,f4,f5,f6,f7,f10, dbfl = "") {
    library
    # stopifnot(file.exists(dt_fl))
    log <- ""
    if(!file.exists(dbfl)) print("NO DB FILE")
    
    # xfl <- as.character(dt_fl)
    # excel_sheets(dt_fl, )
    
    #### New Fig 1 ?? ####
    tryCatch({
        vroom(f1) |>
            mutate(dataset = "sol_pop_age", xvarchar = age_numeric, xwhich = 1, xvardt = NA, yvllb = sex, 
                   yval = popn_tot,
                   text = "" ) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_fl, "Fig1_populationChange_ed") %>%
        #     mutate(dataset = "sol_lpop", xwhich = 2, xvardt = date, xvarchar = "",
        #            yvllb = source, yval = value,
        #            text = ifelse(yvllb == "Census estimates", "dotted", "solid")) %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 2. London Population #### 
    tryCatch({
        vroom(f2) |>
            mutate(xvardt = as.Date(date, format = "%d/%m/%Y"),dataset = "sol_lpop",  
                   xwhich = 2, xvarchar = "", yvllb = source, yval = value,
                   text = ifelse(yvllb == "Census estimates", "dotted", "solid") ) |>
            insert_db(log = log, excfl = xfl)
        # read_excel(dt_fl, "Fig1_populationChange_ed") %>%
        #     mutate(dataset = "sol_lpop", xwhich = 2, xvardt = date, xvarchar = "",
        #            yvllb = source, yval = value,
        #            text = ifelse(yvllb == "Census estimates", "dotted", "solid")) %>%
        #     insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 3. London population cob - UK/non-UK ####
    tryCatch({
        # read_excel(dt_fl, "fig2 pop cob uk non-uk") %>%
        vroom(f3) |>
            mutate(dataset = "sol_cob_uk", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yvllb = cob, text = "dotted", yval = value) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 4. London population cob ####
    tryCatch({
        vroom(f4) |>
        # read_excel(dt_fl, "fig3 pop cob") %>%
            mutate(dataset = "sol_cob", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yvllb = cob, text = "dotted", yval = value) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 4. London Births ####
    tryCatch({
        vroom(f5) |>
        # read_excel(dt_fl, "fig4 london births") %>%
            mutate(dataset = "sol_ann_births", xwhich = 2,
                   xvardt = year_ending_date,
                   xvarchar = "", yval = annual_births, text = "",
                   yvllb = type) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    
    #### New Fig 5 ?? ####
    
    #### Fig 5. Annual births - mother's region ####
    tryCatch({
        vroom(f6) |>
        # read_excel(dt_fl, "fig5 births mcob uk non-uk") %>%
            mutate(dataset = "sol_mcob_uk", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yval = value, text = "", yvllb = mcob) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### Fig 6. Annual births - mother's place of birth ####
    tryCatch({
        vroom(f7) |>
        # read_excel(dt_fl, "fig6 births mcob") %>%
            mutate(dataset = "sol_mcob", xwhich = 1, xvardt = NA, 
                   xvarchar = year, yval = value, text = "", yvllb = mcob) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    #### New Fig 8 ?? ####
    
    
    #### New Fig 9 ?? ####
    
    
    #### New Fig 10 ?? ####
    tryCatch({
        vroom(f10) |>
            # read_excel(dt_fl, "fig6 births mcob") %>%
            mutate(dataset = "sol_net_dom_age", xwhich = 1, xvardt = NA, 
                   xvarchar = age, yval = dommig_netK, text = "", yvllb = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Demography")}  )
    
    TRUE
}