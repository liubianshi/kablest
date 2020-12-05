genstar <- function(pvalue,
                    starcut = c(0.1, 0.05, 0.01),
                    starsymbol = c("*", "**", "***"),
                    outfmt = "text") {
    stopifnot(is.numeric(starcut))
    stopifnot(max(starcut) < 1 && min(starcut) > 0)
    starcut <- unique(sort(starcut, decreasing = TRUE))

    stopifnot(is.character(starsymbol))
    stopifnot(length(starsymbol) >= length(starcut))
    if (outfmt != "text") {
        starsymbol <- escape(starsymbol, chars = "*^_`~")
        starsymbol <- paste0("^", starsymbol, "^")
    }

    if (!is.numeric(pvalue)) return(pvalue)
    stopifnot(max(pvalue, na.rm = TRUE) < 1)
    stopifnot(min(pvalue, na.rm = TRUE) >= 0)

    star <- ifelse(is.na(pvalue), NA, "") 
    for (i in seq_along(starcut)) {
        star <- ifelse(pvalue <= starcut[i], starsymbol[i], star)
    }
    star
}

getesti <- function(coefname, reglist, vars, fmt = NULL)  {
    dt_coef <- lapply(reglist,
        function(l) {
            broom::tidy(l) %>%
                setDT() %>%
                .[vars, ..coefname, on = "term"]
        })
    dt_coef <- do.call("cbind", dt_coef)
    setDT(dt_coef)[, term := ..vars]
    setcolorder(dt_coef, "term")
    setnames(dt_coef, c("term", paste0("V", seq_along(reglist))))

    if (is.null(fmt)) {
        dt_coef
    } else {
        fmt = parse_c(fmt)
        stopifnot(length(fmt) == 3)
        coef2str(dt_coef, fmt)
    }
}

getstat <- function(statname, reglist, fmt = NULL) {
    switch(statname, 
        obs,
        N = 

    )
    nobs()
}

switch(
    "x"
)

coef2str <- function(data, fmt) {
    # for example: c("(", "2", ")")
    stopifnot(length(fmt) == 3)
    l_par = fmt[1]
    r_par = fmt[3]
    digits = as.integer(fmt[2])

    coef_dt <- copy(data) %>% setDT()
    regnames <- names(coef_dt)[purrr::map_lgl(coef_dt, is.numeric)]
    fm <- function(x, digits = NULL, l_par, r_par) {
        y <- lbs::stformat(x, digits = digits, na.replace = "") %>%
            trimws()
        paste0(l_par, y, r_par)
    }

    coef_dt[,
        (regnames) := lapply(.SD, fm,
                             digits = ..digits,
                             l_par = l_par,
                             r_par = y_par),
        .SDcols = c(regnames) 
    ]
}


parse_c <- function(char) {
    if (is.null(char) || is.na(char) || length(char) == 0L)
        return(NULL)
    if (length(char) == 0L) 
    stopifnot(length(char) == 1L)
    stopifnot(nchar(char) %in% c(1, 3))
    fmt <- if (nchar(char) == 1L) {
        c("", char, "")
    } else {
        strsplit(char, "")[[1]]
    }
    stopifnot(grepl("^\\d$", fmt[2]))
    stopifnot(!any(grepl("^[A-Za-z0-9]$", fmt[c(1,3)])))
    fmt
}

escape <- function(x, chars = "*\\") {
    stopifnot(length(chars) == 1)

    exit_esc = grepl("\\\\", chars)
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    chars = gsub("\\", "", chars, fixed = TRUE)

    char_list = strsplit(chars, "")[[1]]
    for (ch in char_list) {
        x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
    }
    x
}

dfplus_col <- function(df_x, df_y, ignore_col = NULL, sep = " ") {
    stopifnot(all.equal(dim(df_x), dim(df_y)))
    for (i in seq_along(df_x)) {
        if (i %in% ignore_col) next
        df_x[[i]] <- paste(df_x[[i]], df_y[[i]], sep = sep)
    }
    df_x
}

#> 逐行合并数据框
dfplus_row <- function(..., list = NULL, common_col = NULL) {
    l <- c(list(...), list)
    stopifnot(length(l) >= 2L)
    rownumlist <- l %>% purrr::map_int(nrow)
    colnumlist <- l %>% purrr::map_int(ncol)
    stopifnot(max(rownumlist) == min(rownumlist))
    stopifnot(max(rownumlist) == min(rownumlist))
    colnum <- colnumlist[1] 
    rownum <- rownumlist[1]

    for (i in seq_along(l)) {
        l[[i]][["_ori"]] = seq_len(rownum)
        l[[i]][["_index"]] = i
    }

    combined <- data.table::rbindlist(l, use.names = FALSE)
    setorderv(combined, c("_ori", "_index"))
    if (!is.null(common_col)) {
        for (j in common_col) {
            combined[[j]] = ifelse(
                combined[["_index"]] == 1, combined[[j]], "")
        }
    }
    combined[, -c("_ori", "_index")]
}

