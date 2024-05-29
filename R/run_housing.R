library(readxl)
library(dplyr)
library(stringr)
# library(resdata)
library(tidyr)

hs_fl <- file.path("C:/Users/joheywood/Greater London Authority/IU - Shared Projects/State of London report/Version 5 (June 2024)/Data/Housing/SOL v4 Housing data workbook.xlsx")  

log <- "logfile.txt"
a <- "x"

run_housing_updates <- function(hs_fl, db_fl) {
    log <- ""
    if(!file.exists(db_fl)) message("No DB file!")
    
    tryCatch({
        run_wide(hs_fl, "New build EPCs", xwch = 1, dcode = "sol_nh_epc") |> 
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "New build EPCs") %>%
        #     mutate(dataset = "sol_nh_epc", xwhich = 1, xvardt = NA, 
        #            xvarchar = str_replace_all(Quarter, "/", "Q"),  
        #            yval = `Annualised total`, yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### New Build Planning permissions ####
    tryCatch({
        run_wide(hs_fl, "Planning permissions", xwch = 1, dcode = "sol_nh_pln") |> 
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "Planning permissions") %>%
        #     pivot_longer(-Quarter, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_nh_pln", xwhich = 1, xvardt = NA,
        #            xvarchar = str_replace_all(Quarter, "/", "Q"), 
        #            text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### House Price ####
    tryCatch({
        run_wide(hs_fl, "House Price Index", xwch = 2, dcode = "sol_hpi") |> 
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "Private rent affordability") %>%
        #     pivot_longer(-Date, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_rnt_aff", xwhich = 2, xvarchar = "", 
        #            xvardt = format(Date, "%Y-%m-%d"),  text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### Rent Affordability ####
    tryCatch({
        run_wide(hs_fl, "Private rent affordability", xwch = 2, dcode = "sol_rnt_aff") |> 
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "Private rent affordability") %>%
        #     pivot_longer(-Date, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_rnt_aff", xwhich = 2, xvarchar = "", 
        #            xvardt = format(Date, "%Y-%m-%d"),  text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### EPC ####
    tryCatch({
    #     read_excel(hs_fl, "Energy efficiency") %>%
    #         select(xvarchar = `Quarter`, yval = `A+B as%`) %>%
    #         mutate(dataset = "sol_epc", xwhich = 1, xvardt = NA, yvllb = "", 
    #                text = "", xvarchar = str_replace_all(xvarchar, "/", "Q")) %>%
    #         insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### Possession claims ####
    tryCatch({
        run_wide(hs_fl, "Possession claims", xwch = 1, dcode = "sol_poss_clm") |> 
            mutate(xvarchar = str_replace_all(xvarchar, " ", "")) |>
            insert_db(log = log, excfl = "SoL - Housing (pc)")
        # read_excel(hs_fl, "Possession claims") %>%
        #     mutate(xvarchar = str_replace_all(Quarter, " ", ""), yval = Claims,
        #            dataset = "sol_poss_clm", xwhich = 1, xvardt = NA, 
        #            yvllb = "", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
    }, error = function(e){error_log(e, "SoL - Housing (poss clms)")}  )
    
    #### Homeless Decisions ####
    tryCatch({
        run_wide(hs_fl, "Homelessness decisions", xwch = 1, dcode = "sol_hmlsdec") |> 
            mutate(xvarchar = str_replace_all(xvarchar, " ", "")) |>
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "Homelessness decisions") %>%
        #     select(-2) %>%
        #     mutate(Category = str_replace(Category, " ", "")) %>%
        #     pivot_longer(-Category, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_hmlsdec", xwhich = 1, xvardt = NA, 
        #            xvarchar = Category, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
        
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### Rough Sleeping ####
    tryCatch({
        run_wide(hs_fl, "Rough sleeping", xwch = 1, dcode = "sol_rghslp") |> 
            mutate(xvarchar = str_replace_all(xvarchar, " ", "")) |>
            filter(yvllb != "Total") |>
            insert_db(log = log, excfl = "SoL - Housing")
        # read_excel(hs_fl, "Rough sleeping") %>%
        #     select(-Total) %>%
        #     mutate(Quarter = str_replace(Quarter, " ", "")) %>%
        #     pivot_longer(-Quarter, names_to = "yvllb", values_to = "yval") %>%
        #     mutate(dataset = "sol_rghslp", xwhich = 1, xvardt = NA, 
        #            xvarchar = Quarter, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Housing")
        
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
    #### Temporary Accommodation ####
    tryCatch({
        read_excel(hs_fl, "Temporary accommodation") %>%
            select(Year:`Other private sector accommodation`) |>
            # fill(Year) %>%
            unite("xvarchar", Year, Quarter, sep = "") %>%
            pivot_longer(-xvarchar, names_to = "yvllb", values_to = "yval") %>%
            mutate(dataset = "sol_tempacc", xwhich = 1, xvardt = NA, text = "") %>%
            insert_db(log = log, excfl = "SoL - Housing")
        
    }, error = function(e){error_log(e, "SoL - Housing")}  )
    
}


