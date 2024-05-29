# Project: State of London
# Title: Young people and education chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Ellie Bloom
# Date: 9 May 2023

library(purrr)
library(dplyr)
library(glue)
library(tidyxl)
library(unpivotr)
library(readxl)
# library(vroom)

dt_fl <- file.path("C:/Users/joheywood/Greater London Authority", 
                   "IU - Shared Projects/State of London report", 
                   "Version 5 (June 2024)/Data/Young people and education", 
                   "24_04 - Young People and Education - data for figures v0.1.xlsx")

# Function: Inserts data for YP/Education chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_yp_education_updates <- function(dt_fl, dbfl = "") {
    if(!file.exists(dt_fl)) {
        return(glue("File doesn't exist: {dt_fl}"))
    }
    
    if(!file.exists(dbfl)) { 
        message("...")
    }
    #### Infant Mortality ####
    tryCatch({
        run_wide(dt_fl, "1_Infant_mortality", xwch = 1, dcode = "sol_inf_mort") |>
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "1. Child mortality rate") %>%
        #     filter(row > 5, is_blank == FALSE) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_chmort", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Child Mortality ####
    tryCatch({
        run_wide(dt_fl, "2_Child_mortality", xwch = 1, dcode = "sol_chmort") |>
            insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    
    #### Child obesity ####
    tryCatch({
        run_wide(dt_fl, "3_Obesity", xwch = 1, dcode = "sol_obs")  |>
            separate(yvllb, into = c("yvllb", "ctg"), sep = " ", extra = "merge") |>
            mutate(text = ifelse(ctg == "Reception", "solid", "dotted")) |>
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "3_Obesity ") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "prev") %>%
        #     separate(prev, into = c("yvllb", "ctg"), sep = " - ") %>%
        #     mutate(text = ifelse(ctg == "reception", "solid", "dotted")) %>%
        #     mutate(dataset = "sol_obs", xwhich = 1, xvardt = NA, 
        #            yval = numeric) %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Happiness ####
    tryCatch({
        run_wide(dt_fl, "4_Happiness", xwch = 1, dcode = "sol_ylstsf")  |> # sol_ylstsf??
            insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    # #### Life Satisfaction #### 
    # tryCatch({
    #     xlsx_cells(dt_fl, "3. Life satisfaction") %>%
    #         filter(row > 4) %>%
    #         behead("N", "xvarchar") %>%
    #         behead("W", "yvllb") %>%
    #         mutate(dataset = "sol_ylstsf", xwhich = 1, xvardt = NA, 
    #                yval = numeric, text = "") %>%
    #         insert_db(log = log, excfl = "SoL - Education")
    # }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Mental disorder ####
    tryCatch({
        run_wide(dt_fl, "5_Mental_disorder", xwch = 1, dcode = "sol_ment_dis")  |> 
            insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### Early Years Providers ####
    tryCatch({
        run_wide(dt_fl, "7_EYR_providers", xwch = 2, dcode = "sol_eyp")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # read_excel(dt_fl, "7_EYR_providers", skip = 5) %>%
        #     select(x = 1, yval = 2) %>%
        #     mutate(dataset = "sol_eyp", xwhich = 2, xvarchar = "", yvllb = "", 
        #            text = "",
        #            xvardt = as.Date(paste0(x, " 1"), format = "%Y - %b %d")) %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Free Early Education Entitlement ####
    tryCatch({
        run_wide(dt_fl, "8_FEEE", xwch = 1, dcode = "sol_feee")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "7. FEEE") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_feee", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     filter(!is.na(xvarchar), !is.na(yval)) |>
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### EY Ofsted ####
    tryCatch({
        run_wide(dt_fl, "9_EY_ofsted", xwch = 1, dcode = "sol_ofsted")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "5. EY Ofsted") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_ofsted", xwhich = 1, xvardt = NA, 
        #            xvarchar = as.character(xvarchar), yval = numeric, text = "") %>%
        #     filter(!is.na(xvarchar)) %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### GLD Aged 5 ####
    
    tryCatch({
        run_wide(dt_fl, "10_GLD_Aged_5", xwch = 1, dcode = "sol_eydev")  |> ## NEW??
            insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### Mother's empl ####
    tryCatch({
        run_wide(dt_fl, "11_Mothers_paid_work", xwch = 1, dcode = "sol_mthr")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "8. Mothers empl") %>%
        #     filter(row > 3) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_mthr", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Eng Maths ####
    tryCatch({
        run_wide(dt_fl, "12_GCSE_pass", xwch = 1, dcode = "sol_engmat")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     filter(!is.na(xvarchar)) %>%
        #     mutate(dataset = "sol_engmat", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Attainment 8 ####
    tryCatch({
        run_wide(dt_fl, "13_Attainment_8", xwch = 1, dcode = "sol_att8")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "10. Attainment 8") %>%
        #     filter(row > 3) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     filter(!is.na(yvllb)) %>%
        #     mutate(dataset = "sol_att8", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Suspensions #### 
    tryCatch({
        run_wide(dt_fl, "14_Suspension", xwch = 1, dcode = "sol_susp")  |> ### NEW ONE?
            insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Perm Excl ####
    tryCatch({
        run_wide(dt_fl, "15_Permanent_exclusion", xwch = 1, dcode = "sol_prmexcl")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "11. Perm excl") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(text = ifelse(xvarchar >= "2019-20", "dotted", "solid")) %>%
        #     mutate(text = ifelse(xvarchar == "2018-19", "solid_join", text )) %>%
        #     mutate(dataset = "sol_prmexcl", xwhich = 1, xvardt = NA, 
        #            yval = numeric ) %>%
        #     filter(!is.na(xvarchar)) |>
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### EHCP ####
    tryCatch({
        run_wide(dt_fl, "16_EHC_SEN_statement", xwch = 1, dcode = "sol_ehcp")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "13. EHCP") %>%
        #     filter(row > 4) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_ehcp", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    # #### FT Excl ####
    # tryCatch({
    #     xlsx_cells(dt_fl, "12. FT excl") %>%
    #         filter(row > 3) %>%
    #         behead("N", "xvarchar") %>%
    #         behead("W", "yvllb") %>%
    #         mutate(text = ifelse(xvarchar >= "2019-20", "dotted", "solid")) %>%
    #         mutate(text = ifelse(xvarchar == "2018-19", "solid_join", text )) %>%
    #         mutate(dataset = "sol_ftexcl", xwhich = 1, xvardt = NA, 
    #                yval = numeric) %>%
    #         filter(!is.na(xvarchar)) |>
    #         insert_db(log = log, excfl = "SoL - Education")
    # }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### Level 3 ####
    tryCatch({
        run_wide(dt_fl, "17_Level_3_qual", xwch = 1, dcode = "sol_lvl3")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "14. level 3") %>%
        #     filter(row > 3) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_lvl3", xwhich = 1, xvardt = NA, 
        #            yval = numeric, text = "") %>%
        #     filter(!is.na(xvarchar)) |>
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    #### Apprenticeships ####
    tryCatch({
        run_wide(dt_fl, "18_Apprenticeship", xwch = 1, dcode = "sol_app")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        
        # xlsx_cells(dt_fl, "15. apprenticeships") %>%
        #     filter(row > 4 , row < 10) %>%
        #     behead("N", "xvarchar") %>%
        #     behead("W", "yvllb") %>%
        #     mutate(dataset = "sol_app", xwhich = 1, xvardt = NA, 
        #            yval = numeric) %>%
        #     mutate(
        #         text = case_when(
        #             xvarchar > "2018/19" ~ "dotted",
        #             xvarchar == "2018/19" ~ "solid_join",
        #             xvarchar < "2018/19" ~ "solid" )
        #         #  yvllb = str_replace_all(yvllb, " - (Starts|Completions)", "")
        #     ) %>%
        #     filter(!is.na(xvarchar), !is.na(yval)) %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    
    
    #### Safety ####
    tryCatch({
        run_wide(dt_fl, "19_Safety", xwch = 1, dcode = "sol_sfty")  |> 
            insert_db(log = log, excfl = "SoL - Education")
        # xlsx_cells(dt_fl, "17. Crime data") %>%
        #     filter(row > 3, row < 6) %>%
        #     behead("N", "xvarchar") %>%
        #     filter(xvarchar != "London", xvarchar < "2023-24") %>%
        #     mutate(dataset = "sol_sfty", xwhich = 1, xvardt = NA, 
        #            yval = numeric, yvllb = "only", text = "") %>%
        #     insert_db(log = log, excfl = "SoL - Education")
    }, error = function(e){error_log(e, "SoL - Education")}  )
    TRUE
}


