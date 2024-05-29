# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("r2d3svg", "rsvg", "glue", "purrr", "readxl",
               "dplyr", "tidyr", "readr", "git2r", "tidyxl", "unpivotr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

trgt_dbfl <- "data/sol_v5_test.db"
if(!file.exists(trgt_dbfl)) {
    trgt_dbfl <- setup_db(trgt_dbfl, clear = TRUE, mtd = "data/meta.csv")
}

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# dbfl <- setup_db(
#     "data/sol_v4.db",
#     clear = TRUE,
#     mtd = "data/meta.csv")


econ_dir <- file.path("C:/Users/joheywood/Greater London Authority/", 
                      "IU - Shared Projects/State of London report/", 
                      "Version 5 (June 2024)/Data/The Economy and Labour Market/")
#
e_bus <- file.path(econ_dir, "Economy and business data in template/")
bd <- file.path(e_bus, "Companies Birth and Deaths.xlsx")
cc <- file.path(e_bus, "Consumer confidence.xlsx")
fdi <- file.path(e_bus, "Foreign direct investment (FDI).xlsx")
gva <- file.path(e_bus, "GVA chart.xlsx")
spnd <- file.path(e_bus, "Spending data and chart.xlsx")
wt <- file.path(e_bus, "world trade chart.xlsx")

e_jobs <- file.path(econ_dir, "Jobs data in template")
llw <- file.path(e_jobs, "Employees below LW LDN-UK.xlsx")
emp <- file.path(e_jobs, "Employment rates and gaps.xlsx")
ins <- file.path(e_jobs, "Insecure work - final.xlsx")
wkj <- file.path(e_jobs, "WKJ by broad sector London.xlsx")
wkf <- file.path(e_jobs, "Workforce Jobs London.xlsx")

skills <- file.path(econ_dir, "Skills data in template", "Skills_June 2024.xlsx")

ddr <- file.path("C:/Users/joheywood/Greater London Authority/", 
                 "IU - Shared Projects/State of London report/", 
                 "Version 5 (June 2024)/Data/Demography/")

d1 <- file.path(ddr, "Fig1_population_ons_London_2022.csv")
d2 <- file.path(ddr, "Fig2_(oldFig1)_populationChange_ed.csv")
d3 <- file.path(ddr, "Fig3_(oldFig2)_COBChange_UKnonUK.csv" )
d4 <- file.path(ddr, "Fig4_(oldFig3)_COBChange_nonUK_Region.csv")
d5 <- file.path(ddr, "Fig5_Births_change.csv")
d6 <- file.path(ddr, "Fig6_(oldFig5)_BirthsChange_MothersUKnonUK.csv")
d7 <- file.path(ddr, "Fig7_(oldFig6)_BirthsChange_nonUK_MothersRegionOB.csv")
d8 <- file.path(ddr, "Fig8_London_Boroughs_IntMig.csv")
d9 <- file.path(ddr, "Fig9_london_domMig_longit_corrected.csv")
d10 <- file.path(ddr, "Fig10_London_dom_mign_net_bySYA_2022.csv")



# source("other_functions.R") # Source other scripts as needed. # nolint
data_dir <- file.path("C:/Users/joheywood/Greater London Authority/", 
                      "IU - Shared Projects/State of London report/", 
                      "Version 5 (June 2024)/Data")
#Replace the target list below with your own:
list(
    ### dbfl ###
    tar_target(db_fl, "data/sol_v4.db", format = "file"),
    ### Environment chapter ### 
    tar_target(
        env_fl,
        file.path(data_dir, "Environment/Environment v5.xlsx"),
        format = "file"),
    # tar_target(
    #     aq_fl,
    #     file.path(data_dir, "Environment/airquality_data.csv"),
    #     format = "file"),
    tar_target(run_env, command = run_environment_updates(env_fl, db_fl)),
    ### Communities Chapter ###
    tar_target(comm_fl, 
               file.path(data_dir, 
                         "Community Participation/Communities data using JH template.xlsx"),
               format = "file"),
    tar_target(run_comm, command = run_communities_updates(comm_fl, db_fl)),
    ### Transport and Infrastructure
    tar_target(trans_inf_fl, 
               file.path(data_dir, 
                         "Transport and Infrastructure/Transport & Infrastructure.xlsx"),
               format = "file"),
    tar_target(run_trans_inf, run_transport_infrastructure_updates(trans_inf_fl, db_fl)),
    
    ### Income/Deprivation Chapter ###
    tar_target(dep_fl,
               file.path(data_dir,
                         "Income, poverty and destitution/IPD data using JH template.xlsx"),
               format = "file"),
    tar_target(hh_uc,
               file.path(data_dir, "Income, poverty and destitution/hh_uc.csv"),
               format = "file"),
    tar_target(ucws,
               file.path(data_dir, "Income, poverty and destitution/pucws.csv"),
               format = "file"),
    tar_target(pchb,
               file.path(data_dir, "Income, poverty and destitution/pchb.csv"),
               format = "file"),
    tar_target(
        run_inc_dep,
        command = run_income_dep_updates(dep_fl, hh_uc, ucws, pchb, db_fl)
    ),
    ### Demography ###
    tar_target(dem1, d1, format = "file"),
    tar_target(dem2, d2, format = "file"),
    tar_target(dem3, d3, format = "file"),
    tar_target(dem4, d4, format = "file"),
    tar_target(dem5, d5, format = "file"),
    tar_target(dem6, d6, format = "file"),
    tar_target(dem7, d7, format = "file"),
    # tar_target(dem8, d8, format = "file"),
    # tar_target(dem9, d9, format = "file"),
    tar_target(dem10, d10, format = "file"),
    tar_target(run_dem, 
               command = run_population_updates(dem1,dem2,dem3,dem4,dem5,dem6,dem7,
                                                dem10, db_fl)),
    ### Economy ###
    tar_target(ebd, bd, format = "file"),
    tar_target(ecc, cc, format = "file"),
    tar_target(efdi, fdi, format = "file"),
    tar_target(egva, gva, format = "file"),
    tar_target(espnd, spnd, format = "file"),
    tar_target(ewt, wt, format = "file"),
    tar_target(ellw, llw, format = "file"),
    tar_target(eemp, emp, format = "file"),
    tar_target(eins, ins, format = "file"),
    tar_target(ewkj, wkj, format = "file"),
    tar_target(ewkf, wkf, format = "file"),
    tar_target(ec_skills_fl, skills, format = "file"),
    # tar_target(
    #     ec_jobs_fl,
    #     file.path(data_dir, "The Economy and Labour Market/2023-12-01_jobs.xlsx"),
    #     format = "file"),
    # tar_target(
    #     ec_bus_fl,
    #     file.path(data_dir, "The Economy and Labour Market/Econ_business_charts.xlsx"),
    #     format = "file"),
    # tar_target(
    #     prv_jbs,
    #     "C:/Users/joheywood/Greater London Authority/IU - State of London report/Version 3 (June 2023)/Data/The Economy and Labour Market/2023-05-04_jobs_skills.xlsx",
    #     format = "file"),
    tar_target(run_econ, run_economy_updates(ebd, ecc, efdi, egva, espnd, ewt,
                                              ellw, eemp, eins, ewkj, ewkf,
                                              skills, db_fl)),
    ### Housing ###
    tar_target(hs_fl,
               file.path(data_dir, "Housing/SOL v4 Housing data workbook.xlsx"),
               format = "file"),
    tar_target(run_housing, run_housing_updates(hs_fl, db_fl)),
    ### Health/Wellbeing ### 
    tar_target(hlth_fl,
               file.path(data_dir,
                         "Health and wellbeing",
                         "V5/SOL Health in template.xlsx"),
               format = "file"),
    tar_target(run_hw, run_health_wellbeing_updates(hlth_fl, db_fl)),
    ### Crime ###
    tar_target(crm_fl,
               file.path(data_dir,
                         "Crime and Safety/Crime and Safety Chapter -April 2024 - Data File.xlsx"),
               format = "file"),
    tar_target(run_crm, run_crime_updates(crm_fl, db_fl)),
    ### Children and Young people ###
    tar_target(cyp_fl,
               file.path(data_dir,
                         "Young people and education", 
                         "24_04 - Young People and Education - data for figures v0.1.xlsx"),
               format = "file"),
    tar_target(run_cyp, run_yp_education_updates(cyp_fl, db_fl))
    
)
