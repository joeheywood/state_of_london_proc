# Inserts data for the Economy (Chapter one) section of the State of London Report
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(readxl)
library(stringr)
# library(vroom)
library(resdata)

dt_pth <- file.path("C:/Users/joheywood/Greater London Authority/", 
                    "IU - State of London report/Version 4 (January 2024)/Data/",
                    "The Economy and Labour Market/Skills_data.xlsx")

jbs_fl <- file.path("C:/Users/joheywood/Greater London Authority/", 
                    "IU - State of London report/Version 4 (January 2024)/Data/",
                    "The Economy and Labour Market/2023-12-01_jobs.xlsx")

prv_jbs <- "C:/Users/joheywood/Greater London Authority/IU - State of London report/Version 3 (June 2023)/Data/The Economy and Labour Market/2023-05-04_jobs_skills.xlsx"

econ_fl <- file.path("C:/Users/joheywood/Greater London Authority/", 
                    "IU - State of London report/Version 4 (January 2024)/Data/",
                    "The Economy and Labour Market/Econ_business_charts.xlsx")


run_economy_updates <- function(dt_pth, jbs_fl, econ_fl, dbfl, prv_jbs) {
    xfl <- dt_pth
    log <- ""
    if(!file.exists(dbfl)) print("NO DB FILE")
    
    
    #### Highest Qualification ####
    tryCatch({
        read_excel(dt_pth, "Figure 1_Highest qual", skip = 1) |>
            pivot_longer(-London, names_to = "yvllb", values_to = "yval") |>
            mutate(dataset = "sol_highqual", xwhich = 1, xvarchar = London, 
                   xvardt = NA, text = "") |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    #### FE/Skills ####
    tryCatch({
        xlsx_cells(dt_pth, "Figure 2_FE and skills ") |>
            filter(row > 2) |>
            behead("N", "xvarchar") |>
            behead("W", "yvllb") |>
            filter(yvllb != "Devolution", !is.na(xvarchar)) |>
            mutate(dataset = "sol_fe_skills", xwhich = 1, xvardt = NA, yval = numeric,
                   text = ifelse(yvllb == "London (left axis)", "Chart_top", "Chart_bottom")) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    #### SSV ####
    tryCatch({
        read_excel(dt_pth, "Figure 3_SSV", skip = 1) |>
            rename(xvarchar = 1) |>
            mutate(xvarchar = ifelse(is.na(xvarchar), "", xvarchar)) |>
            # filter(!is.na(xvarchar)) |>
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") |>
            mutate(dataset = "sol_ssv", xwhich = 1, xvardt = NA, text = "") |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    #### Workforce Jobs ####
    tryCatch({
        read_excel(jbs_fl, "total workforce jobs") |>
            mutate(xvardt = as.Date(date)) |>
            mutate(dataset = "sol_workforce", xwhich = 2, xvarchar = "", 
                   yval = obs_value, yvllb = "", # geography_name, 
                   text = "", xvardt = format(xvardt, "%Y-%m-%d")) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    #### Workforce Jobs by profile ####
    ### NEED TO CHANGE NAMES HERE ###
    tryCatch({
        read_excel(jbs_fl, "broad sector wfj") |> 
            mutate(xvardt = as.Date(paste0(date, "-01"))) |>
            filter(!is.na(xvardt)) |>
            select(xvardt,  `production_utilities`:`business_other_services`) |>
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") |> # %>%
            mutate(dataset = "sol_wfprof", xwhich = 2, xvarchar = "",  
                   text = "", xvardt = format(xvardt, "%Y-%m-%d")) |>
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "SoL - Business")}  )
    
    #### Employment ####
    tryCatch({
        read_excel(jbs_fl, "employment rate") %>%
            mutate(dataset = "sol_empl", xwhich = 2, xvarchar = "", 
                   xvardt = format(date, "%Y-%m-%d"), 
                   yval = obs_value, yvllb = geography_name, 
                   text = "") %>% 
            # mutate(xvardt = as.Date(date)) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Micro")}  )
    
    #### Inactivity fig 9####
    tryCatch({
        read_excel(jbs_fl, "inactivity rate") %>%
            mutate(dataset = "sol_inact", xwhich = 2, xvarchar = "", 
                   xvardt = format(date, "%Y-%m-%d"),
                   yval = obs_value, yvllb = geography_name, 
                   text = "") %>% 
            mutate(xvardt = as.Date(date)) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Micro")}  )
    
    #### Unemployment ####
    tryCatch({
        read_excel(jbs_fl, "unemployment rate") %>%
            mutate(dataset = "sol_unempl", xwhich = 2, xvarchar = "", 
                   xvardt = format(date, "%Y-%m-%d"),
                   yval = obs_value, yvllb = geography_name, 
                   text = "") %>% 
            mutate(xvardt = as.Date(date)) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Micro")}  )
    
    tryCatch({
        read_excel(jbs_fl, "employment gaps") |>
            filter(!is.na(rate), geography_name == "London") |>
            select(xvardt = date, yvllb = gap, yval = rate) |>
            mutate(dataset = "sol_empgap", xwhich = 2, 
                   xvarchar = "", text = "", xvardt = format(xvardt, "%Y-%m-%d")) |>
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Micro")}  )
    
    #### Spending #### 
    ## New chart - as discussed with James W in late April '23
    tryCatch({
        read_excel(econ_fl, "Spending", skip = 2) |>
            select(xvardt = 3, Weekdays = 4, Weekend =  5) |> 
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") |>
            mutate( dataset = "sol_spending", xwhich = 2, xvarchar = "", text = "",
                    xvardt = format(xvardt, "%Y-%m-%d")) |>  
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    
    # #### FDI data ####
    tryCatch({
        read_excel(econ_fl, "FDI data") %>%
            pivot_longer(-Quarter, names_to = "yvllb", values_to = "yval") %>%
            mutate(text = ifelse(yvllb == "Projects", "Chart_top", "Chart_bottom"),
                   dataset = "sol_fdi", xvarchar = Quarter, xwhich = 1,
                   xvardt = NA) %>%
            filter(!is.na(yval)) |>
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "SoL - Econ/Business")}  )
    
    
    #### GVA ####
    # For this one, there had been separate lines for GLA projections, but has
    # since been simplified.
    tryCatch({
        dat <- read_excel(econ_fl, "GVA_data") %>%
            select(xvarchar = `Quarter`, `UK` = 3, `London` = 4 ) %>%
            filter(!is.na(xvarchar))
        dat %>% pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            filter(!is.na(yval)) %>%
            mutate(dataset = "sol_gva", xwhich = 1, xvardt = NA, text = "") %>% 
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Economy")}  )
    
    #### Conf ####
    tryCatch({
        read_excel(econ_fl, "Conf_data") %>%
            select(xvardt = 1, London, UK) %>%
            pivot_longer(-xvardt, names_to = "yvllb", values_to = "yval") %>%
            filter(!is.na(yval) ) %>%
            mutate(dataset = "sol_conf", xwhich = 2, xvarchar = "", text = "",
                   xvardt = format(xvardt, "%Y-%m-%d")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Economy")}  )
    
    #### Business Births/Deaths ####
    tryCatch({
        read_excel(econ_fl, "Births&Deaths", skip = 5) |>
            tidyr::separate(col = Quarter, into = c("Q", "Y"), sep = " ") |>
            mutate(xvarchar = paste0(Y, Q)) |>
            select(xvarchar, Births, Closures) |>
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            filter(!is.na(yval) ) %>%
            mutate(dataset = "sol_busbd", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Economy")}  )
    
    
    #### London Living Wage ####
    tryCatch({
        read_excel(prv_jbs, "below living wage", skip = 1) |> 
            select(xvarchar = Year, yvllb = Region, yval = 4) |>
            mutate(dataset = "sol_llw", xwhich = 1, xvardt = NA, 
                   text = "") |> # %>% 
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "SoL - Micro")}  )
    
    
    #### Insecure work ####
    tryCatch({
        read_excel(prv_jbs, "insecure") |> 
            select(xvarchar = 1, yvllb = geography, yval = share) |>
            mutate(dataset = "sol_insemp", xwhich = 1, xvardt = NA, text = "") |>
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "SoL - Business")}  )
    
    
}