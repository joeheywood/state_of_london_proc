library(readxl)
library(stringr)
library(dplyr)
library(tidyr)

run_narrow <- function() {
    
}

# fl <- "C:/Users/joheywood/OneDrive - Greater London Authority/SoL/Transport & Infrastructure Template.xlsx"
# sheet <- "2.Active mode share"
# dcode = "sol_actmode"
# xwch <- 2
# 
# sheet <- "3. Active travel"
# dcode = "sol_acttrav"
# xwch <- 2
# 
# 
# sheet <- "4. Safety (roads)"
# dcode = "sol_rdsft"
# xwch <- 2
# 
# run_wide(fl, "4. Safety (roads)", dcode = "sol_rdsft", xwch = 2)
# run_wide(fl, "4. Safety (roads)", dcode = "sol_rdsft", xwch = 2)



run_wide <- function(fl, sheet, xwch, dcode, dtfmt = "%Y", rmNAx = TRUE, rmNAy = FALSE) {
    ## select/get data
    # save(fl, sheet, xwhch, dcode, dtmft, rmNAx, rmNAy, file = "deb.Rda")
    x <- read_excel(fl, sheet)
    
    ### find blank cols
    blank_cols <- which(str_detect(names(x), "^\\.{3}\\w"))
    if(blank_cols[2] == blank_cols[1] + 1) {
        names(x)[1:(blank_cols[1] - 1)]
        x <- x[, 1:(blank_cols[1] - 1)]
    }
    
    ## pivot to long format
    x <- x |>  pivot_longer(-1, names_to = "yvllb", values_to = "yval") 
    
    
    if(xwch == 1) {
        names(x)[1] <- "xvarchar"
        x$xvardt <- NA
        ### if only one, then set yvllb to "only"?
        if(rmNAx) {
            x <- x |> filter(!is.na(xvarchar))
        }
    } else {
        names(x)[1] <- "xvardt"
        x$xvarchar <- ""
        if(!is.null(dtfmt)) {
            x$xvardt <- as.Date(x$xvardt, format = dtfmt)
            x$xvardt <- format(x$xvardt, "%Y-%m-%d")
        }
        if(rmNAx) { 
            x <- x |> filter(!is.na(xvardt))
        }
    }
    
    if(rmNAy) {
        x <- x |> filter(!is.na(yval))
    }
    ### filter on missing values:
    ## add other args
    x |>
        mutate(dataset = dcode, text = "", xwhich = xwch) |>
        select(dataset, xwhich, xvarchar, xvardt, yvllb, yval, text)
        
}

run_nested <- function() {
    
}


