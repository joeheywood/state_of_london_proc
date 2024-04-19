# Project: State of London
# Title: Transport and infrastructure chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Abi
# Date: May 2023

library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(tidyxl)
library(unpivotr)
library(readxl)
# library(vroom)
dt_fl <- "C:/Users/joheywood/OneDrive - Greater London Authority/SoL/Transport & Infrastructure Template.xlsx"
source("R/reformat_datasets.R")


# Function: Inserts data for Transport and Infrastructure chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_transport_infrastructure_updates <- function(dt_fl, db_fl = "") {
    if(!file.exists(dt_fl)) stop("NO. That transport data file doesn't exist!")
    if(!file.exists(db_fl)) message("DB file doesn't exist. Probably fine")
    log <- ""
    
    #### Mode Split ####
    tryCatch({
        run_wide(dt_fl, "2.Active mode share", xwch = 1, "sol_modesplit") |> 
            insert_db2(excfl = dt_fl, tab = "2.Active mode share")
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Active Travel ####
    tryCatch({
        run_wide(dt_fl, "3. Active travel", 1, "sol_actvtrav") |> 
            insert_db2(excfl = dt_fl, tab = "3. Active travel")
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - roads ####
    tryCatch({
        run_wide(dt_fl, "4. Safety (roads)", 1, "sol_rd_safety") |> 
            insert_db2(excfl = dt_fl, tab = "4. Safety (roads)" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - buses ####
    tryCatch({
        run_wide(dt_fl, "5. Safety (buses)", 1, "sol_bus_safety") |> 
            insert_db2(excfl = dt_fl, tab = "5. Safety (buses)" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    ### Accessibility ####
    tryCatch({
        run_wide(dt_fl, "6. Accessibility (step free)", 1, "sol_accessibility") |> 
            insert_db2(excfl = dt_fl, tab = "6. Accessibility (step free)" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Bus ####
    tryCatch({
        run_wide(dt_fl, "8. Bus", 1, "sol_bus") |> 
            insert_db2(excfl = dt_fl, tab = "8. Bus" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Tube ####
    tryCatch({
        run_wide(dt_fl, "9. Tube", 1, "sol_tube") |> 
            insert_db2(excfl = dt_fl, tab = "9. Tube" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Trasnsport Affordability ####
    tryCatch({
        run_wide(dt_fl, "7. Affordability", 1, "sol_transp_aff") |> 
            insert_db2(excfl = dt_fl, tab = "7. Affordability" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    
    #### Road traffic ####
    tryCatch({
        run_wide(dt_fl, "10. Roadtraffic", 1, "sol_traffic") |> 
            insert_db2(excfl = dt_fl, tab = "10. Roadtraffic" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Full fibre availability ####
    tryCatch({
        run_wide(dt_fl, "11. Full fibre availability", 1, "sol_flfb") |> 
            insert_db2(excfl = dt_fl, tab = "11. Full fibre availability" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    #### Not spots ####
    tryCatch({
        run_wide(dt_fl, "12. Superfast unavailability", 1, "sol_notspot") |> 
            insert_db2(excfl = dt_fl, tab = "12. Superfast unavailability" )
    }, error = function(e){error_log2(e, "SoL - Transport/Infrastructure")}  )
    
    TRUE
}