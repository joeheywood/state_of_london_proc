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
                   "IU - Shared Projects/State of London report/", 
                   "Version 5 (June 2024)/Data/Transport and infrastructure/", 
                   "Transport & infrastructure.xlsx")

# Function: Inserts data for Transport and Infrastructure chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_transport_infrastructure_updates <- function(dt_fl, db_fl = "") {
    if(!file.exists(dt_fl)) stop("NO. That transport data file doesn't exist!")
    if(!file.exists(db_fl)) message("DB file doesn't exist. Probably fine")
    log <- ""
    
    
    #### Demand #### 
    tryCatch({
        run_wide(dt_fl, "1. Demand for transport" , xwch = 2, dcode = "tfl") |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Mode Split ####
    tryCatch({
        run_wide(dt_fl, "2. Active mode share", xwch = 1, dcode = "sol_modesplit") |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
        # read_excel(dt_fl, "2.Active mode share", skip = 2) %>% 
        #     select(xvarchar = Year,yval = 3) %>%
        #     # pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_modesplit", xwhich = 1, xvardt = NA, yvllb = "",
        #            text = "") %>%
        #     filter(!is.na(xvarchar)) %>% 
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Active Travel ####
    tryCatch({
        run_wide(dt_fl, "3. Active travel", xwch = 1, dcode = "sol_actvtrav") |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "3. Active travel", skip = 1) %>% 
        #     select(xvarchar = Year, yval = Actuals) |>
        #     filter(!is.na(xvarchar)) |>
        #     mutate(dataset = "sol_actvtrav", xwhich = 1, xvardt = NA,
        #            yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - roads ####
    tryCatch({
        run_wide(dt_fl, "4. Safety (roads)", xwch = 1, dcode = "sol_rd_safety") |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "4. Safety (roads)", skip = 1) %>% 
        #     select(xvarchar = Year, yval = `KSIs (road traffic collisions)`) %>%
        #     filter(!is.na(xvarchar)) %>%
        #     mutate(dataset = "sol_rd_safety", xwhich = 1, xvardt = NA,
        #            yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Road Safety - buses ####
    tryCatch({
        run_wide(dt_fl, "5. Safety (buses)", xwch = 1, dcode = "sol_bus_safety")  |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "5. Safety (buses)", skip = 2) %>% 
        #     select(xvarchar = Year, yval = `KSIs (London buses)`) %>%
        #     filter(!is.na(xvarchar)) %>%
        #     mutate(dataset = "sol_bus_safety", xwhich = 1, xvardt = NA,
        #            yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    ### Accessibility ####
    tryCatch({
        run_wide(dt_fl, "6. Accessibility (step free)", xwch = 1, dcode = "sol_accessibility")  |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "6. Accessibility (step free)", skip = 1) %>%
        #     select(xvarchar = Year, yval = 3) %>%
        #     filter(xvarchar < "2030", !is.na(yval)) %>%
        #     mutate(dataset = "sol_accessibility", xwhich = 1, xvardt = NA,
        #            yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")

    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Bus ####
    tryCatch({
        run_wide(dt_fl, "8. Bus", xwch = 1, dcode = "sol_bus")  |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "8. Bus", skip = 1) %>% 
        #     select(xvarchar = 2, yval = 3) |>
        #     filter(!is.na(yval)) |>
        #     mutate(dataset = "sol_bus", xwhich = 1, xvardt = NA,
        #            text = "", yvllb = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Tube ####
    tryCatch({
        run_wide(dt_fl, "9. Tube", xwch = 1, dcode = "sol_tube")  |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "9. Tube", skip = 1) %>% 
        #     filter(str_detect(Year, "^20")) |>
        #     select(xvarchar = Year, yval = `Operated kilometres (%)`) |>
        #     mutate(dataset = "sol_tube", xwhich = 1, xvardt = NA,
        #            text = "", yvllb = "only") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Trasnsport Affordability ####
    tryCatch({
        # read_excel(dt_fl, "7. Affordability", skip = 1) %>% 
        #     select(yvllb = 2, England, London) %>%
        #     filter(yvllb != "Total") |>
        #     pivot_longer(-yvllb, names_to = "xvarchar", values_to = "yval") %>%
        #     mutate(dataset = "sol_transp_aff", xwhich = 1, xvardt = NA,
        #            text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    
    #### Road traffic ####
    tryCatch({
        ## This one is a massive pain. There are 13 periods in a financial year. 
        ## So I start with the first day of the financial year, get the period #
        ## and multiply that by 365/13 and add it in days to the first day of the FY (!!)
        # read_excel(dt_fl, "10. Roadtraffic", skip = 1) |>
        #     select(xvarchar = Year, `Central London`:`Great Britain`) |>
        #     filter(!is.na(xvarchar)) %>%
        #     pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_traffic", xwhich = 1, xvardt = NA, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Full fibre availability ####
    tryCatch({
        run_wide(dt_fl, "11. Full Fibre availability", xwch = 2, dcode = "sol_flfb")  |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "11. Full fibre availability", skip = 3) %>%
        #     select(xvardt = Date, London, `Rest of the UK`) %>%
        #     mutate(xvardt = as.Date(xvardt)) |>
        #     mutate(xvardt = format(xvardt, "%Y-%m-%d")) |>
        #     filter(!is.na(xvardt)) |>
        #     pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_flfb", xwhich = 2, xvarchar = "", 
        #            text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    #### Not spots ####
    tryCatch({
        run_wide(dt_fl, "12. Superfast unavailability", xwch = 2, dcode = "sol_notspot") |>
            insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        # read_excel(dt_fl, "12. Superfast unavailability", skip = 3) %>%
        #     select(xvardt = Date, London, `Rest of the UK`) %>%
        #     mutate(xvardt = as.Date(xvardt)) |>
        #     mutate(xvardt = format(xvardt, "%Y-%m-%d")) |>
        #     filter(!is.na(xvardt)) |>
        #     pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_notspot", xwhich = 2, xvarchar = "", 
        #            text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Transport/Infrastructure")
        
    }, error = function(e){error_log(e, "SoL - Transport/Infrastructure")}  )
    
    TRUE
}
