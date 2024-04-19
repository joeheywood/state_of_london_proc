#### D3 next stab ###
library(dplyr)
library(tidyr)
library(r2d3)
library(jsonlite)
library(stringr)
library(purrr)
library(data.table)
library(zoo)
library(robservable)

library(glue)
library(RSQLite)

## 
get_obs_chart <- function(dtst, opts, mopts = list(), pp = list()) {
    if("filter_chart" %in% names(pp)) {
        print("Applying filter chart..")
        dat <- get_data_from_sqlite(dtst, "data/sol_v4.db", chart = pp$filter_chart) 
    } else {
        dat <- get_data_from_sqlite(dtst, "data/sol_v4.db") 
    }
    if("orderctg" %in% names(pp)) { dat$d <- orderctg(dat$d, pp$orderctg) }
    if("orderxd" %in% names(pp)) { dat$d <- orderxd(dat$d, pp$orderxd) }
    
    if("wrap_text" %in% names(pp)) {
        # stack$d$upper_xd <- stack$d$xd
        dat$d <- dat$d |> 
            separate(xd, sep = "(?<=[-])", remove = FALSE, 
                     into = c("upper_xd", "lower_xd"))
    }
    
    
    chart_opts <- c("chrt")
    tickf <- ifelse(dat$m$tickformat %in% "%", ".0%", 
                    ifelse(is.na(dat$m$tickformat),".1f", dat$m$tickformat))
    ctype <- ifelse(dat$m$bar == 1, "bar", "line")
    
    optsx <- list(
        ytickformat = tickf, 
        type = dat$m$type,
        charttype = ctype, 
        stack= dat$m$stack,
        high = dat$m$highlight
    )
    if("quarters" %in% names(dat$m)) {
        optsx$quarters <- TRUE
    }
    for(n in names(opts)) {
        print(glue("ADDING: {n} = {opts[[n]]}"))
        optsx[[n]] <- opts[[n]]
    }
    if(dat$m$stack %in% TRUE) {
        optsx$stack <- TRUE
        optsx$stackgroup <- "stack"
        if(!"silent_x" %in% names(opts) && dat$m$type %in% c("date", "quarter")) {
            optsx$silent_x <- TRUE
        }
        optsx$convert_wide <- TRUE
    } else {
        optsx$stackgroup <- "group"
        optsx$stack <- FALSE
    }
    mopts$ttl <- dat$m$charttitle
    mopts$title <- dat$m$charttitle
    mopts$sub <- dat$m$subtitle
    mopts$sol_chapter <- dat$m$sol_chapter
    mopts$sol_sub <- dat$m$sol_subsection
    
    if("filter_chart" %in% names(pp)) {
        if(pp$filter_chart == "keep") {
            # chart_opts <- c("chrt")
            chart_opts <- c("viewof luk", "chrt")
            button_opts <- unique(dat$d$chart)
            
        }
    }
    chrt <- robservable(
        "@joe-heywood-gla/gla-dpa-chart",
        include = chart_opts,
        input = list(unempl = dat$d, chartopts = optsx, metaopts = mopts, 
                     inpopts = list())
    )
    
    list(d = dat$d, co = optsx, m = mopts, chart = chrt, u = dat$u)
    
}

# get_aq_chart


orderxd <- function(df, xfact) {
    if(is.null(xfact)) {
        df
    } else {
        bbx <- factor(df$xd, levels = xfact)
        df <- df[order(bbx), ]
    }
}

orderctg <- function(df, ctgfact) {
    if(is.null(ctgfact)) {
        df
    } else {
        bbx <- factor(df$b, levels = ctgfact)
        df <- df[order(bbx), ]
    }
}


get_data_from_sqlite <- function(dtst, sqlite_dpth, chart = NULL) {
    # dpth <- Sys.getenv("DASH_DATAPATH")
    # sqlite_dpth <- "data/sol_v4.db"
    conn <- dbConnect(SQLite(), sqlite_dpth)
    m <- tbl(conn, "mtd") %>% 
        filter(dataset == dtst) %>% 
        collect() 
    
    upd <- tbl(conn, "updates") %>% 
        filter(dataset == dtst) %>% 
        collect() 
    
    
    if(!is.null(chart) && !chart %in% "keep") {
        dat <- dbGetQuery(conn, glue("SELECT * FROM ind_dat WHERE dataset = '{dtst}' and chart = '{chart}'")) 
    } else {
        
        dat <- dbGetQuery(conn, glue("SELECT * FROM ind_dat WHERE dataset = '{dtst}'")) 
    }
    
    dbDisconnect(conn)
    # save(dat, upd, m, dtst, file = glue("debug_{dtst}.Rda"))
    if(nrow(upd) > 0) {
        upd$timestamp <- as.POSIXct(upd$timestamp, origin = "1970-1-1")
        if(any(upd$newvals > 0, na.rm = TRUE)) {
            last_update <- max(upd$timestamp[which(upd$newvals > 0)])
            
        } else {
            last_update <- upd$timestamp[1]
        }
        last_check <- max(upd$timestamp)
        max_xval <- ifelse(dat$xwhich[1] == 1, max(dat$xvarchar), max(dat$xvardt))
        upds <- data.frame(dataset = dtst, last_update = last_update, 
                           last_check = last_check, max_xval = max_xval)
    } else {
        upds <- data.frame()
    }
    
    if(dat$xwhich[1] == 1) {
        m$type <- "character"
        dat$xd <- dat$xvarchar
    } else {
        m$type <- "date"
        dat$xd <- dat$xvardt
        
    }
    
    if(!is.null(chart) && chart %in% "keep") {
        outdat <- dat
    } else {
        outdat <- dat |> select(dataset, xd, b = yvllb, y = yval, text)
    }
        
    list(d = outdat, 
         m = m,
         u = upds) |> 
        convert_q_date()
    
}

convert_q_date <- function(obj) {
    dat <- obj$d
    opts <- obj$m
    if("xd" %in% names(dat) == TRUE && 
       all(str_detect(dat$xd, "20\\d{2} ?Q[1-4]+"))) {
        dat <- dat %>%
            mutate(xd = str_replace(xd, " ", "")) %>%
            separate(xd, c("yr", "q"), sep = "Q") %>%
            mutate(xd = as.Date(paste0(yr, "-",
                                       (as.numeric(q)*3),
                                       "-01"))) 
        # opts$dc <- 2
        # opts$type <- "date"
        opts$type <- "quarter"
        opts$xtick <- "Quarter ending %b %Y"
        # opts$dodge <- dodge_1(dat)
        opts$frm <- min(dat$xd) - 20
        opts$tt <- max(dat$xd) + 2
        opts$quarters <- TRUE
        datrange <- difftime(max(dat$xd), min(dat$xd), units = "weeks")
        uyr <- datrange > 140
        opts$useyear <- uyr
        dat$xd <- format(dat$xd, "%Y-%m-%d")
    }
    list(d = dat, m = opts, u = obj$u)
}


