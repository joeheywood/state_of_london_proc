# Inserts data for the Communities section of the State of London Report
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(tidyxl)
library(unpivotr)
library(readxl)
library(stringr)
# library(vroom)

cm_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 3 (June 2023)/Data/",
                   "Community Participation/Communities State of London data.xlsx")

res_dash <- "Q:/Teams/D&PA/Social Policy/COVID-19 data/Recovery Dashboard data/BF data for Resilience Dashboard March 2021.xlsx"

run_communities_updates <- function(cm_fl, res_dash, dbfl) {
    log <- ""
    if(!file.exists(dbfl)) {
        print("no db file")
    }
    comms_dat <- xlsx_cells(cm_fl, "Overall") %>%
        filter(row > 17) %>%
        behead("NNW", "yvllb") %>%
        behead("N", "xvarchar") %>%
        behead("W", "ix") %>%
        behead("W", "indicator") %>%
        behead("W", "title") %>%
        select(ix, xvarchar, indicator, title, yvllb, yval = numeric)
    
    sing_dat <- xlsx_cells(cm_fl, "Overall") %>%
        filter(row > 34) %>%
        behead("N", "xvarchar") %>%
        behead("W", "ix") %>%
        behead("W", "indicator") %>%
        behead("W", "title") %>%
        select(ix, xvarchar, indicator, title, yval = numeric)
    
    part_dat <- xlsx_cells(cm_fl, "Participation") %>%
        filter(row %in% 1:3) %>%
        behead("N", "xvarchar") %>%
        behead("W", "indicator") %>%
        select(xvarchar, indicator, yval = numeric)
    
    #### Formal volunteering ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 1) %>%
            mutate(dataset = "fmlvol", xwhich = 1, xvardt = NA,  text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    
    
    #### Voter Registration
    tryCatch({
        read_excel(cm_fl, "VoterReg", skip = 17) %>% 
            select(xvarchar = 1, England, London) %>%
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "votereg", xwhich = 1, xvardt = NA,  text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Informal volunteering ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 2) %>%
            mutate(dataset = "infvol", xwhich = 1, xvardt = NA,  text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Social Action ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 4) %>%
            mutate(dataset = "socact", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Neighbourhood belonging ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 5) %>%
            mutate(dataset = "nghbel", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Neighbourhood Trust ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 6) %>%
            mutate(dataset = "nghbtrst", xwhich = 1, xvardt = NA, text = "") %>%
            filter(!is.na(yval)) %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Talking to neighbours ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 7) %>%
            mutate(dataset = "sol_nghb", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Influencing local decisions ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 8) %>%
            mutate(dataset = "infloc", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Social isolation ####
    tryCatch({
        comms_dat %>% 
            filter(ix == 11) %>%
            mutate(dataset = "sol_hlp", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Participation in Culture ####
    tryCatch({
        part_dat %>%
            filter(indicator == "Participation in culture") %>%
            mutate(dataset = "sol_culture", xwhich = 1, xvardt = NA, 
                   yvllb = "", text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Participation in Sport ####
    tryCatch({
        part_dat %>%
            filter(indicator == "Participation in sport ") %>%
            mutate(dataset = "sol_sport", xwhich = 1, xvardt = NA, 
                   yvllb = "", text = "") %>%
            insert_db(log, "SoL - Communities")
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    
    #### Perception of hate crime ####
    tryCatch({
        dat <- read_excel(cm_fl, "PerceptionHateCrime") %>%
            mutate(yr1 = str_replace_all(Year, "^(\\d{4}).*", "\\1"),
                   dataset = "sol_phtcrm", xwhich = 1, xvardt = NA,
                   yvllb = "") %>%
            mutate(q = as.numeric(str_replace_all(Quarter, "Q", ""))) %>%
            mutate(xvarchar = ifelse(q == 4, 
                                     paste0((as.numeric(yr1) + 1), "Q1"), 
                                     paste0(yr1, "Q", (q + 1) ) )
            ) %>%
            select(dataset, xwhich, xvarchar, xvardt, yval = 3, yvllb)
        lnbrk <- which(is.na(dat$yval))
        dat$text <- NA
        dat$text[1:min(lnbrk) - 1] <- "solid1"
        dat$text[(max(lnbrk) + 1):nrow(dat)] <- "solid2"
        
        dat %>% insert_db(log, "SoL - Communities")
        
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Mobility ####
    tryCatch({
        read_excel(cm_fl, "MobilityCAZ") %>% 
            mutate(dataset = "sol_o2mobility", xwhich = 2, xvarchar = "", 
                   xvardt = count_date, yval = people, yvllb = area, text = "") %>% 
            insert_db(log, "SoL - Communities")
        
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    #### Trust in Institutions ####
    tryCatch({
        xlsx_cells(cm_fl, "Trust - tracked graphs") %>% 
            filter(row > 46) %>%
            behead("W", "yvllb") %>%
            behead("N", "xvardt") %>%
            mutate(dataset = "sol_insttrst", xwhich = 2, xvarchar = "", 
                   yval = numeric, text = "") %>%
            insert_db(log, "SoL - Communities")
        
    }, error = function(e){error_log(e, "SoL - Communities")})
    
    ##### Neighbourhood cohesion ####
    tryCatch({
        xlsx_cells(res_dash, "12") |>
            filter(col != 3) |>
            behead("N", xvardt) |>
            filter(!is.na(xvardt)) |>
            select(xvardt, yval = numeric) |>
            mutate(dataset = "coh", xwhich = 2, xvarchar = "", text = "", yvllb = "" ) |>
            insert_db(log, "SoL - Communities")
            
    })
}