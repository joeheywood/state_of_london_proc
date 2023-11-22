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

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                    "IU - State of London report/Version 3 (June 2023)/Data/",
                   "Transport and Infrastructure", "Transport & Infrastructure.xlsx") 

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
        read_excel(dt_fl, "2.Active mode share", skip = 2) %>% 
            select(xvarchar = Year,yval = 2) %>%
            # pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_modesplit", xwhich = 1, xvardt = NA, yvllb = "",
                   text = "") %>%
            filter(!is.na(xvarchar)) %>% 
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Active Travel ####
    tryCatch({
        read_excel(dt_fl, "3. Active travel", skip = 1) %>% 
            mutate(dataset = "sol_actvtrav", xwhich = 1, xvardt = NA,
                   xvarchar = Year, yval = `Achieving 20 minutes`, yvllb = "",
                   text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - roads ####
    tryCatch({
        read_excel(dt_fl, "4. Safety (roads)", skip = 1) %>% 
            select(xvarchar = 1, yval = 2) %>%
            filter(!is.na(yval)) %>%
            mutate(dataset = "sol_rd_safety", xwhich = 1, xvardt = NA,
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - buses ####
    tryCatch({
        read_excel(dt_fl, "5. Safety (buses)", skip = 2) %>% 
            select(xvarchar = 1, yval = 2) %>%
            filter(!is.na(yval)) %>%
            mutate(dataset = "sol_bus_safety", xwhich = 1, xvardt = NA,
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    ### Accessibility ####
    tryCatch({
        read_excel(dt_fl, "6. Accessibility (step free)", skip = 1) %>%
            select(xvarchar = Year, yval = 2) %>%
            filter(xvarchar < "2030", !is.na(yval)) %>%
            mutate(dataset = "sol_accessibility", xwhich = 1, xvardt = NA,
                   yvllb = "", text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")

    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Bus ####
    tryCatch({
        read_excel(dt_fl, "8. Bus") %>% 
            select(xvarchar = 1, yval = 2) |>
            filter(!is.na(yval)) |>
            mutate(dataset = "sol_bus", xwhich = 1, xvardt = NA,
                   text = "", yvllb = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Tube ####
    tryCatch({
        read_excel(dt_fl, "9. Tube", skip = 1) %>% 
            filter(str_detect(Year, "^20")) |>
            rename(xvarchar = Year) |>
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") |>
            mutate(dataset = "sol_tube", xwhich = 1, xvardt = NA,
                   text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Trasnsport Affordability ####
    tryCatch({
        read_excel(dt_fl, "7. Affordability", skip = 1) %>% 
            select(yvllb = 1, England, London) %>%
            pivot_longer(-yvllb, names_to = "xvarchar", values_to = "yval") %>%
            mutate(dataset = "sol_transp_aff", xwhich = 1, xvardt = NA,
                   text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    
    #### Road traffic ####
    tryCatch({
        ## This one is a massive pain. There are 13 periods in a financial year. 
        ## So I start with the first day of the financial year, get the period #
        ## and multiply that by 365/13 and add it in days to the first day of the FY (!!)
        prd <- 365 / 13
        read_excel(dt_fl, "10. Roadtraffic", skip = 2) |>
            select(`Central London`, `Inner London`, 
                   `Outer London`, `Period end`) |>
            mutate(xvardt = as.Date(`Period end`)) |>
            select(-`Period end`) %>%
            filter(!is.na(xvardt)) %>%
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_traffic", xwhich = 2, xvarchar = "", text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Full fibre availability ####
    tryCatch({
        read_excel(dt_fl, "11. Full fibre availability", skip = 3) %>%
            select(xvardt = 1, London, `Rest of the UK`) %>%
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_flfb", xwhich = 2, xvarchar = "", 
                   text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Not spots ####
    tryCatch({
        read_excel(dt_fl, "12. Superfast unavailability", skip = 3) %>%
            select(xvardt = 1, London, `Rest of the UK`) %>%
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_notspot", xwhich = 2, xvarchar = "", 
                   text = "") %>%
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    TRUE
}
