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

get_buttons_list <- function(dd) {
    xn <- names(dd)
    xx <- as.character(dd)
    out <- list()
    map(1:length(xn), function(i) {
        list(id = xn[i], button_text = xx[i])
    })
}

sol_buttons <- function(opts_dict, radio = c()) {
    # opts <- list(
    #     list(
    #         id = "neets",
    #         button_text = "NEETS"
    #     ),
    #     list(
    #         id = "sol_noloqual",
    #         button_text = "No or low Quals"
    #     ),
    #     list(
    #         id = "sol_inact",
    #         button_text = "Inactivity")
    # )
    inplist <- list(buttons_opts = get_buttons_list(opts_dict) )
    incs <-  c("bchart", "viewof buttons_select", "style")
    if(length(radio) > 0) {
        incs <- c(incs, "viewof luk")
        inplist$inpopts = radio
    }
    robservable(
        "@joe-heywood-gla/sol-sqlite",
        include = incs,
        input = inplist)
    
}

sol_aq <- function() {
    incs <-  c("aqchart", "viewof aq_buttons_select", "viewof aq_radio", "style")
    robservable(
        "@joe-heywood-gla/sol-sqlite",
        include = incs)
}


sol_dashboard <- function(dtst) {
    incs <-  c("for_dashboard", "style")
    robservable(
        "@joe-heywood-gla/sol-sqlite",
        include = incs,
        input = list(dashboard_chart = dtst))
    
}

run_charts_db <- function() {
    fl <- "data/sol_v4_charts.db"
    closeAllConnections()
    file.remove(fl)
    cn <- dbConnect(RSQLite::SQLite(), fl)
    tbls <- dbListTables(cn)
    if(!"chartdat" %in% tbls) {
        dbSendQuery(cn, "create table chartdat(dtst text, xd text, b text, y real, text text, chart text);")
        dbSendQuery(cn, "create table chartmeta(dtst text primary key, chartmeta json);")
        dbSendQuery(cn, "create table meta(dtst text primary key, chapter text, title text, sub text, last_update text, last_check text, max_xval text, meta json);")
        dbSendQuery(cn, "create table updates(dtst text primary key, lastmax text, newvals text, timestamp text, execfile text, latest_newvals text);")
    } 
    chart_sqlite_environment(cn)
    chart_sqlite_communities(cn)
    chart_sqlite_trans_infr(cn)
    chart_sqlite_demography(cn)
    chart_sqlite_inc_dep(cn)
    chart_econ(cn)
    chart_housing(cn)
    chart_health_wellbeing(cn)
    chart_sqlite_crime(cn)
    chart_sqlite_yp_education(cn)  
    
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
        file.remove(fl)
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
        d <- get_obs_chart(dtst, opts, mopts, pp = preproc)# add mopts here eventually
        if("filter_chart" %in% names(preproc)) {
            d$m$ttl <- paste0(d$m$ttl, "_", preproc$filter_chart)
            # d$m$ttl <- paste0(d$m$ttl, "_", preproc$filter_chart)
            dtst <- paste0(dtst, "_", preproc$filter_chart)
            d$d$dataset <- dtst
        }
        save(d, file = glue("data/RDA/{d$m$ttl}"))
        d$d <- d$d |> rename(dtst = dataset)
        chart_meta <- data.frame(dtst = dtst,
                                 chartmeta = as.character(
                                     toJSON(d$co, auto_unbox = T,
                                            pretty = TRUE)),
                                 stringsAsFactors = FALSE)
        if(nrow(d$u) == 0) {
            d$u <- data.frame(
                last_update = Sys.time(),
                last_check = Sys.time(),
                max_xval = max(d$d$xd)
            )
        }
        
        meta <- data.frame(dtst = dtst,
                           chapter = d$m$sol_chapter,
                           title = d$m$ttl,
                           sub = d$m$sol_sub,
                           last_update = format(d$u$last_update, "%a %H:%m %d %b %Y"),
                           last_check = format(d$u$last_check, "%a %H:%m %d %b %Y"),
                           max_xval = d$u$max_xval,
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
    save_chart("aq_no2", conn = cn, preproc = list(filter_chart = "Roadside"))
    save_chart("aq_no2", conn = cn, preproc = list(filter_chart = "Urban Background"))
    save_chart("aq_pm10", conn = cn, preproc = list(filter_chart = "Roadside"))
    save_chart("aq_pm10", conn = cn, preproc = list(filter_chart = "Urban Background"))
    save_chart("aq_pm25", conn = cn, preproc = list(filter_chart = "Roadside"))
    save_chart("aq_pm25", conn = cn, preproc = list(filter_chart = "Urban Background"))
    save_chart("aq_pm25", conn = cn)
    
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
    save_chart("htcrm", conn = cn)
    save_chart("sol_phtcrm", conn = cn)
    # save_chart("sol_o2mobility", conn = cn)
    save_chart("sol_hlp", conn = cn)
    save_chart("sol_insttrst", opts = list(ticksOnValues = TRUE), conn = cn)
    save_chart("sol_footfall", conn = cn)
}


chart_sqlite_trans_infr <- function(cn) {
    # save_for_SoL("tfl",   mvav = 7, section = sctn, cnn = cn)
    save_chart("sol_modesplit", opts = list(forceYDomain = list(0, 1)), conn = cn)
    # save_chart("tfl_wlkcyc", conn = cn)
    save_chart("sol_actvtrav", conn = cn)
    save_chart("sol_rd_safety", opts = list(forceYDomain = list(0, 6000)), conn = cn)
    save_chart("sol_bus_safety", conn = cn)
    save_chart("sol_accessibility", conn = cn)
    save_chart("sol_bus", opts = list(forceYDomain = list(0, 12)), conn = cn)
    save_chart("sol_tube", conn = cn)
    save_chart("sol_traffic", opts = list(leglab = "legend"), conn = cn)
    save_chart("sol_flfb", conn = cn)
    save_chart("sol_notspot", conn = cn)
    save_chart("sol_transp_aff", opts = list(silent_x = FALSE), conn = cn)
}



chart_sqlite_demography <- function(cn, sctn = "Demography") {
    save_chart("sol_lpop", 
               opts = list(
                   inc_mark = "all_categories",
                   forcecols  = list(`ONS mid-year estimates` = "#6da7de",
                                     `GLA estimates`= "#9e0059",
                                     `Census estimates` = "#AAAAAA")), conn = cn)
    save_chart("sol_cob", opts = list(inc_mark = "all_categories"), conn = cn)
    
    save_chart("sol_cob_uk", opts = list(inc_mark = "all_categories"), 
               conn = cn) ##
    save_chart("sol_ann_births", conn = cn) ##
    save_chart("sol_mcob_uk", conn = cn) ##
    save_chart("sol_mcob", conn = cn) ##
}
chart_sqlite_inc_dep <- function(cn) {
    ons_order <- c("North East", "North West", "Yorkshire and the Humber",
                   "East Midlands", "West Midlands", "East", "East of England", "London", 
                   "Inner London", "Outer London", "South East", "South West",
                   "Wales", "Scotland", "Northern Ireland")
    
    llg <- list(o = "top", 
                `0.1` = "10th percentile",
                `median` = "Median",
                `0.9` = "90th percentile")
    
    save_chart("sol_dhhinc", 
               opts = list(
                   leglab = "legend",
                   lgg = llg
               ),
               preproc = list(wrap_text = TRUE),
               conn = cn) ## 
    save_chart("sol_relpov", 
               opts = list( leglab = "legend"),
               preproc = list(
                   wrap_text = TRUE,
                   orderctg = c("All people", 
                                "Children", 
                                "Working Age", 
                                "Pensioners")),
               conn = cn )
    fcl <- list(
        `Working Age Households on HB` = "#6da7de",
        `Working Age Households on HB and UC` = "#943fa6",
        `Working Age Households on UC` = "#d82222",
        `Working Age Households on UC, not in payment` = "#888888"  
    )
    ucordr <- c("Working Age Households on HB", 
                "Working Age Households on HB and UC", 
                "Working Age Households on UC", 
                "Working Age Households on UC, not in payment")  
    save_chart("sol_uchh", 
               opts = list(forcecols = fcl, cheight = 700), 
               mopts = list(cheight = 700),
               preproc = list(orderctg = ucordr),
               conn = cn)
    
    fcl <- list(
        `Not currently expected to work` = "#888888")
    pu_ordr <- c( "Searching for work",
                  "Looking for more work",
                  "Working but on low earnings",
                  "Not currently expected to work" )
    save_chart("sol_ucws", 
               opts = list( forcecols = fcl,cheight = 700 ),
               mopts = list(cheight = 700),
               preproc = list( orderctg = pu_ordr),
               conn = cn)
    
    
    
    fcl <- list(
        `Pension Credit only` = "#6da7de",
        `Pension Credit and Housing Benefit` = "#943fa6",
        `Housing Benefit only` = "#d82222"
    )
    pord <- c("Pension Credit only", "Pension Credit and Housing Benefit",
              "Housing Benefit only" )
    save_chart("sol_pensmeans", 
               opts = list( forcecols = fcl, cheight = 700 ),
               mopts = list(cheight = 700),
               preproc = list( orderctg = pord),
               conn = cn)
    
    cordr <- c(
        "Going without or relying on debt",
        "Struggling making ends meet", 
        "Just about managing",
        "Coping okay", 
        "Comfortable financially",
        "Prefer not to say",
        "Do not know"   
    )
    fcol = list(
        `Going without or relying on debt` = "#6da7de",
        `Struggling making ends meet` = "#9e0059",
        `Just about managing` = "#dee000",
        `Coping okay` = "#d82222",
        `Comfortable financially` = "#5ea15d",
        `Prefer not to say` = "#AAAAAA",
        `Do not know` = "#888888" 
    )
    
    save_chart("sol_struggfin", 
               opts = list(
                   forcecols = fcol,
                   cheight = 700
               ),
               mopts = list( cheight = 700 ), 
               preproc = list(orderctg = cordr),
               conn = cn  
    )
    
    save_chart("sol_persins", conn = cn )
    hoz_ordr <- c(rev(ons_order))
    ordr2 <- c("Severe low income", "Low income", "Marginal low income")
    fcmdep <- list(
        `Severe low income` = "#00163e",
        `Low income` = "#065d8e",
        `Marginal low income` = "#6da7de"
    )
    save_chart("sol_matdep_c", 
                 preproc = list(orderctg = ordr2),
                 conn = cn)
    
    
    
    # save_for_SoL("sol_matdep_o", 
    #              hh = 700, ordr = hoz_ordr, section = sctn, cnn = cn )
}


chart_econ <- function(cn) {
    save_chart("sol_spending", conn = cn)
    save_chart("sol_gva", conn = cn)
    save_chart("sol_conf", conn = cn)
    save_chart("sol_fdi", opts = list(leglab = "legend"), 
               mopts = list(cheight = 700, multichart = "vsplit_line"), 
               conn = cn)
    # save_chart("sol_fdi", conn = cn, hh= 800)
    save_chart("sol_busbd", conn = cn)
    save_chart("sol_llw", conn = cn)
    save_chart("sol_workforce", conn = cn)
    save_chart("sol_insemp", conn = cn)
    save_chart("sol_empgap", conn = cn)
    # save_chart("sol_noloqual", conn = cn)
    # save_chart("sol_neets", conn = cn)
    save_chart("sol_wfprof", conn = cn)
    save_chart("sol_empl", conn = cn)
    save_chart("sol_unempl", conn = cn)
    save_chart("sol_inact", conn = cn) 
    save_chart("sol_highqual", conn = cn)
    save_chart("sol_fe_skills", 
               opts = list(leglab = "legend"),
               mopts = list(cheight = 700, multichart = "vsplit_line"), 
               conn = cn)
    ssv_ordr <- c("Total", "", "Education", "Transport & Storage", "Construction", 
                  "Information & Communications","Arts & Other Services",
                  "Manufacturing", "Health & Social Work","Wholesale & Retail",
                  "Hotels & Restaurants", "Business Services", "Financial Services")
    save_chart("sol_ssv", 
               opts = list(cheight = 700, horiz = TRUE), 
               mopts = list(cheight = 700, horiz = TRUE), 
               preproc = list(orderxd = rev(ssv_ordr)),
               conn = cn)
}

chart_housing <- function(cn) {
    save_chart("sol_nh_epc", conn = cn)
    save_chart("sol_rnt_aff", conn = cn)
    save_chart("sol_poss_clm", conn = cn)
    save_chart("sol_hmlsdec", conn = cn)
    ordr <- c(
        "No second night out",
        "Second night out but not living on the streets",
        "Joined living on the streets population" ) 
    save_chart("sol_rghslp", preproc = list(orderctg = ordr), conn = cn)
    
    save_chart("sol_nh_pln", opts = list(leglab = "legend"), conn = cn )
    save_chart("sol_tempacc", conn = cn)
    save_chart("sol_epc", conn = cn)
}

chart_health_wellbeing <- function(cn) {
    save_chart("sol_lbw", conn = cn)
    semh_ordr <- c(
        "Mixed",
        "White",
        "Any other ethnic groups, and unclassified",
        "Asian",
        "All pupils"
    )
    save_chart("sol_semh", 
               opts = list(suffix = "%", horiz = TRUE), 
               preproc = list(orderctg = c("London", "England"),
                              orderxd = semh_ordr),
               conn = cn)     
    save_chart("sol_smoke", 
               opts = list(suffix = "%", horiz = TRUE), 
               preproc = list(orderctg = c("London", "England")),
               conn = cn)
    lfexpopts <- list(leglab = "legend", xFontsize = "11pt", yFontsize = "11pt")
    lfexpmopts <- list(multichart = "lines_horiz", subheads = list("Female (Years)", "Male (Years)"))
    save_chart("sol_life_exp02", opts = lfexpopts, mopts = lfexpmopts, conn = cn)
    save_chart("sol_life_exp64", opts = lfexpopts, mopts = lfexpmopts, conn = cn)
    o75o <- c("__by_xd__",
              "Decile 1 (most deprived)",
              "Decile 2",
              "Decile 3",
              "Decile 4",
              "Decile 5",
              "Decile 6",
              "Decile 7",
              "Decile 8",
              "Decile 9",
              "Decile 1 (least deprived)"
    )
    save_chart("sol_75_mort", opts = list(horiz = TRUE), preproc = list(orderxd = o75o), conn = cn)
    save_chart("sol_adult_mort", opts = list(horiz = TRUE), 
               preproc = list(orderctg = list("Female", "Male")), conn = cn)
    save_chart("sol_prev_disease", opts = list(horiz = TRUE, suffix = "%"), conn = cn)
    anxmopts <- list(multichart = "lines_horiz", subheads = list("Anxiety", "Satisfaction"))
    save_chart("sol_adult_anx", opts = lfexpopts, mopts = anxmopts, conn = cn)
    # save_for_SoL("sol_semh", section = "Health and Wellbeing", 
    #              ordr = c(
    #                  "__by_xd__", 
    #                  "All pupils",
    #                  "Asian",
    #                  "Any other ethnic groups, and unclassified",
    #                  "Black",
    #                  "White",
    #                  "Mixed"
    #              ), suff = "%")
    # 
    # save_for_SoL("sol_cancdep", section = "Health and Wellbeing", 
    #              leg = list(o = "top"),
    #              # ["#","#BBD0E7","#A0B8D8","#87A2C3","#718FAF","#5E7997","#4E667F","#3E5166","#313F4E","#252C34"]
    #              forcecol = list(
    #                  `1 - most deprived` = "#BBD0E7",
    #                  `2` = "#87A2C3",
    #                  `3` = "#5E7997",
    #                  `4` = "#3E5166",
    #                  `5 - least deprived` = "#252C34"
    #              )) ## 
    cancols <- list(
        `1 - most deprived` = "#BBD0E7",
        `2` = "#87A2C3",
        `3` = "#5E7997",
        `4` = "#3E5166",
        `5 - least deprived` = "#252C34"
    )
    save_chart("sol_cancdep", opts = list(forcecols = cancols), conn = cn)
    
}

chart_sqlite_crime <- function(cn) {
    save_chart("sol_tno", conn = cn)
    save_chart("sol_vwi", conn = cn)
    save_chart("sol_da", conn = cn)
    save_chart("sol_sxoff", conn = cn)
    save_chart("sol_knife", conn = cn)
    save_chart("sol_hmcd", conn = cn)
    save_chart("sol_rob", conn = cn)
    save_chart("sol_brglry", conn = cn)
    save_chart("sol_tfmv", conn = cn)
    save_chart("sol_tpo", conn = cn)
    save_chart("sol_fraud", conn = cn)
    save_chart("sol_asb", conn = cn)
    save_chart("sol_hrsm", conn = cn)
    save_chart("sol_dark", opts = list(silent_x = TRUE), conn = cn)
    save_chart("sol_vctsat", conn = cn)
    save_chart("sol_fair", conn = cn)
}

chart_sqlite_yp_education <- function(cn) {
    save_chart("sol_chmort", conn = cn)
    llg <- list(dots_London = "Year 6 London", 
                dots_England = "Year 6 England", solid_London = "Reception London",
                solid_England = "Reception England")
    save_chart("sol_obs", opts = list(leglab = "legend", lgg = llg), conn = cn)
    save_chart("sol_ylstsf", conn = cn)
    save_chart("sol_eyp", conn = cn)
    save_chart("sol_feee", conn = cn)
    save_chart("sol_feee", conn = cn) 
    save_chart("sol_mthr", conn = cn)
    save_chart("sol_eydev", conn = cn)
    
    ordr <- c("2018", "2019", "2020", "2021", "2022", "2023")
    save_chart("sol_ofsted", 
               opts = list(leglab = "legend"), 
               preproc = list(orderctg = ordr), 
               conn = cn)
    save_chart("sol_engmat", conn = cn)
    save_chart("sol_att8", conn = cn)
    save_chart("sol_ftexcl", conn = cn)
    save_chart("sol_prmexcl", conn = cn)
    save_chart("sol_ehcp", opts = list(cheight = 600), mopts = list(cheight = 600), 
               conn = cn)
    save_chart("sol_lvl3", conn = cn) # not working?
    forceCol <- list(o = "top", 
                     `Under 19s - Starts` = "#963760", 
                     `19-24 - Starts` = "#d397ab", 
                     `Under 19s - Completions` = '#7f9cbc', 
                     `19-24 - Completions` = '#b8cce5')
    save_chart("sol_app", 
                opts = list(forcecols = forceCol), 
                conn = cn)
    save_chart("sol_sfty", opts = list(cheight = 600), mopts = list(cheight = 600), 
               conn = cn)
    
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
