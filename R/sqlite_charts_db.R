library(RSQLite)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(rsvg)
library(r2d3svg)
source("R/get_chart_data.R")

shrpnt_root <- file.path("C:/Users/joheywood/Greater London Authority/",
                    "IU - State of London report/Version 4 (January 2024)/Data/")

run_charts_db <- function() {
    fl <- "data/sol_v4_charts.db"
    closeAllConnections()
    file.remove(fl)
    cn <- dbConnect(RSQLite::SQLite(), fl)
    tbls <- dbListTables(cn)
    if(!"chartdat" %in% tbls) {
        dbSendQuery(cn, "create table chartdat(dtst text, xd text, b text, y real, text text, chart text);")
        dbSendQuery(cn, "create table chartmeta(dtst text primary key, chartmeta json);")
        dbSendQuery(cn, "create table meta(dtst text primary key, chapter text, title text, sub text, meta json);")
        dbSendQuery(cn, "create table updates(dtst text primary key, lastmax text, newvals text, timestamp text, execfile text, latest_newvals text);")
    } 
    chart_sqlite_environment(cn)
    chart_sqlite_communities(cn)
    chart_sqlite_trans_infr(cn)
    chart_sqlite_demography(cn)
    # chart_sqlite_inc_pov_dest(cn)  
    # chart_sqlite_health_wellbeing(cn)
    # chart_sqlite_crime(cn)
    # ingest_econ_business(cn)
    # ingest_housing(cn)
    # chart_sqlite_yp_education(cn)  
    
    dbDisconnect(cn)
    
}

convert_all_to_svg <- function() {
    fls <- dir("data/RDA", full.names = TRUE)
    map_lgl(fls, convert_to_svg)
    
}

convert_to_svg <- function(fl) {
    tryCatch({
        load(fl)
        shrpnt <- file.path(shrpnt_root, d$m$sol_chapter, "svg")
        if(!dir.exists(shrpnt)) dir.create(shrpnt)
        ttl <- d$m$ttl |> 
            str_replace_all("[:/]", "") |>
            trimws()
        
        save_d3_svg(d$chart, file = glue("{shrpnt}/{ttl}.svg"),  delay = 2 )
        remove_div(glue("{shrpnt}/{ttl}.svg"))
        print(glue("Chart added: {ttl}"))
        TRUE
        
    }, error = function(e) {
        print(glue("Didn't work for ", fl))
        print(e)
        FALSE
    }  )
}

save_chart <- function(dtst, opts = list(), mopts = list(), preproc = list(), conn) {
    tryCatch({
        ### combine opts, mopts above with defaults?
        d <- get_obs_chart(dtst, opts, pp = preproc)# add mopts here eventually
        save(d, file = glue("data/RDA/{d$m$ttl}"))
        d$d <- d$d |> rename(dtst = dataset)
        chart_meta <- data.frame(dtst = dtst,
                                 chartmeta = as.character(
                                     toJSON(d$co, auto_unbox = T,
                                            pretty = TRUE)),
                                 stringsAsFactors = FALSE)
        
        meta <- data.frame(dtst = dtst,
                           chapter = d$m$sol_chapter,
                           title = d$m$ttl,
                           sub = d$m$sol_sub,
                           meta = as.character(
                               toJSON(d$m, auto_unbox = TRUE,
                                      pretty = TRUE)))
        # conn <- dbConnect(RSQLite::SQLite(), cdbfl)
        dbSendQuery(conn, glue("delete from chartdat where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from chartmeta where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from meta where dtst = '{dtst}'"))
        # dbSendQuery(conn, glue("delete from updates where dtst = '{dtst}'"))
        if(!"chart" %in% names(d$d)) d$d$chart <- ""
        dbAppendTable(
            conn, "chartdat", 
            d$d |> select(dtst, xd, y, b, text, chart))
        dbAppendTable(conn, "chartmeta", chart_meta)
        dbAppendTable(conn, "meta", meta)
        # dbAppendTable(conn, "updates", uu)
        TRUE
    }, error = function(e){
        print(glue("Not working: {dtst}: {e}"))
        FALSE
    })
    
}


chart_sqlite_environment <- function(cn) {
    save_chart("bio", conn = cn)
    save_chart("sol_greenhouse", conn = cn)
    save_chart("epc", conn = cn)
    save_chart("wst", conn = cn)
    save_chart("rcyc", conn = cn)
    
}


chart_sqlite_communities <- function(cn) {
    save_chart("fmlvol", conn = cn)
    save_chart("votereg", conn = cn)
    save_chart("infvol", conn = cn)
    save_chart("socact", conn = cn)
    save_chart("nghbel", conn = cn)
    save_chart("nghbtrst", conn = cn)
    save_chart("sol_nghb", conn = cn)
    save_chart("infloc", conn = cn)
    ordr <- c( "2018-19", "2021-22", "2022 (Feb)", "2022 (May)", "2022 (Aug)", 
               "2022 (Nov)", "2023 (Feb)", "2023 (May)", "2023 (Aug)", 
               "2023 (Nov)" )
    save_chart("sol_culture", preproc = list(orderxd = ordr), conn = cn)
    save_chart("sol_sport", preproc = list(orderxd = ordr), conn = cn)
    save_chart("coh", conn = cn)
    # save_chart("htcrm", conn = cn)
    save_chart("sol_phtcrm", conn = cn)
    # save_chart("sol_o2mobility", conn = cn)
    save_chart("sol_hlp", conn = cn)
    save_chart("sol_insttrst", conn = cn)
}


chart_sqlite_trans_infr <- function(cn) {
    # save_for_SoL("tfl",   mvav = 7, section = sctn, cnn = cn)
    save_chart("sol_modesplit", conn = cn)
    # save_chart("tfl_wlkcyc", conn = cn)
    save_chart("sol_actvtrav", conn = cn)
    save_chart("sol_rd_safety", conn = cn)
    save_chart("sol_bus_safety", conn = cn)
    save_chart("sol_accessibility", conn = cn)
    save_chart("sol_bus", conn = cn)
    save_chart("sol_tube", conn = cn)
    save_chart("sol_traffic", conn = cn)
    save_chart("sol_flfb", conn = cn)
    save_chart("sol_notspot", conn = cn)
    save_chart("sol_transp_aff", conn = cn)
}



chart_sqlite_demography <- function(cn, sctn = "Demography") {
    # save_for_SoL("sol_lpop", 
    #              leg = list(o = "top"),
    #              forcecol = list(`ONS mid-year estimates` = "#6da7de",
    #                              `GLA estimates`= "#9e0059",
    #                              `Census estimates` = "#AAAAAA"),
    #              mrkrs = list("Census estimates"), 
    #              # section = "", retChart = TRUE)
    #              section = sctn, cnn = cn) ## 
    # 
    # cob_or <- c("Europe", "Africa", "Middle East and Asia", 
    #             "The Americas and the Caribbean", "Other")
    # save_for_SoL("sol_cob", 
    #              mrkrs = list("all_categories"), 
    #              ordr = cob_or, section = sctn, cnn = cn) ## 
    # 
    # save_for_SoL("sol_cob_uk", mrkrs = list("all_categories"), 
    #              section = sctn, cnn = cn) ##
    save_chart("sol_ann_births", conn = cn) ##
    save_chart("sol_mcob_uk", conn = cn) ##
    save_chart("sol_mcob", conn = cn) ##
}


# add_to_sqlite <- function(d, cm, m, uu, conn) {
#     dtst <- cm$dtst[1]
#     # print(dtst)
#     # print(glue("delete from chartdat where dataset = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from chartdat where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from chartmeta where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from meta where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from updates where dtst = '{dtst}'"))
#     dbAppendTable(conn, "chartdat", d )
#     dbAppendTable(conn, "chartmeta", cm)
#     dbAppendTable(conn, "meta", m)
#     dbAppendTable(conn, "updates", uu)
#     
# }
# 


remove_div <- function(fl) {
    code <- readLines(fl) %>% paste(collapse = "")
    code <- str_replace(code, "^<div[a-z =\"]+>", "")
    code <- str_replace(code, "</div>$", "")
    writeLines(code, fl)
    TRUE
}
