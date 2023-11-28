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
get_obs_chart <- function(dtst, opts, mopts = list()) {
    dat <- get_data_from_sqlite(dtst, "data/sol_v4.db")
    chart_opts <- c("chrt")
    tickf <- ifelse(dat$m$tickformat %in% "%", ".0%",
        ifelse(is.na(dat$m$tickformat), ".1f", dat$m$tickformat)
    )
    ctype <- ifelse(dat$m$bar == 1, "bar", "line")

    optsx <- list(
        ytickformat = tickf,
        type = dat$m$type,
        charttype = ctype,
        stack = dat$m$stack,
        high = dat$m$highlight
    )
    mopts$ttl <- dat$m$charttitle
    mopts$sol_chapter <- dat$m$sol_chapter
    mopts$sol_sub <- dat$m$sol_subsection
    chrt <- robservable(
        "@joe-heywood-gla/gla-dpa-chart",
        include = chart_opts,
        input = list(
            unempl = dat$d, chartopts = optsx, metaopts = mopts,
            inpopts = list()
        )
    )

    list(d = dat$d, co = optsx, m = mopts, chart = chrt)
}


get_data_from_sqlite <- function(dtst, sqlite_dpth) {
    # dpth <- Sys.getenv("DASH_DATAPATH")
    # sqlite_dpth <- "E:/project_folders/apps/db/dashboard_files/sqlite/sol_v4.db"
    conn <- dbConnect(SQLite(), sqlite_dpth)
    m <- tbl(conn, "mtd") %>%
        filter(dataset == dtst) %>%
        collect()

    dat <- dbGetQuery(conn, glue("SELECT * FROM ind_dat WHERE dataset = '{dtst}'"))
    dbDisconnect(conn)
    if (dat$xwhich[1] == 1) {
        m$type <- "character"
        dat$xd <- dat$xvarchar
    } else {
        m$type <- "date"
        dat$xd <- dat$xvardt
    }


    list(
        d = dat |>
            select(dataset, xd, b = yvllb, y = yval, text),
        m = m
    ) |>
        convert_q_date()
}

convert_q_date <- function(obj) {
    dat <- obj$d
    opts <- obj$m
    if ("xd" %in% names(dat) == TRUE &&
        all(str_detect(dat$xd, "20\\d{2} ?Q[1-4]+"))) {
        dat <- dat %>%
            mutate(xd = str_replace(xd, " ", "")) %>%
            separate(xd, c("yr", "q"), sep = "Q") %>%
            mutate(xd = as.Date(paste0(
                yr, "-",
                (as.numeric(q) * 3),
                "-01"
            )))
        # opts$dc <- 2
        opts$type <- "date"
        opts$xtick <- "Quarter ending %b %Y"
        # opts$dodge <- dodge_1(dat)
        opts$frm <- min(dat$xd) - 20
        opts$tt <- max(dat$xd) + 2
        opts$quarters <- TRUE
        datrange <- difftime(max(dat$xd), min(dat$xd), units = "weeks")
        uyr <- datrange > 140
        opts$useyear <- uyr
    }
    list(d = dat, m = opts)
}
