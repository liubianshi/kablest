# adjstar: adjust star vetor --------------------------------------------------
adjstar <- function(star, outfmt = "text") {
    starcut <- star$cut
    starsymbol <- star$symbol

    stopifnot(is.numeric(starcut))
    stopifnot(max(starcut) < 1 && min(starcut) > 0)
    starcut <- unique(sort(starcut, decreasing = TRUE))

    stopifnot(is.character(starsymbol))
    stopifnot(all(!grepl("[0-9A-Za-z]", starsymbol)))
    stopifnot(length(starsymbol) >= length(starcut))
    starsymbol <- starsymbol[seq_along(starcut)] 

    if (outfmt != "text") {
        starsymbol <- escape(starsymbol, chars = "*^_`~")
        starsymbol <- paste0("^", starsymbol, "^")
    }
    list(cut = starcut, symbol = starsymbol)
}

# genstar: gen star vector from p-value vector --------------------------------
genstar <- function(pvalue, star) {
    starcut <- star$cut
    starsymbol <- star$symbol

    if (!is.numeric(pvalue)) return(pvalue)
    stopifnot(max(pvalue, na.rm = TRUE) < 1)
    stopifnot(min(pvalue, na.rm = TRUE) >= 0)

    star <- ifelse(is.na(pvalue), NA, "") 
    for (i in seq_along(starcut)) {
        star <- ifelse(pvalue <= starcut[i], starsymbol[i], star)
        star %<>% rempty("")
    }
    star
}

# genheader: gen header list from reglist -------------------------------------
genheader <- function(reglist, header) {
    if (is.null(header)) return(NULL)

    nulltrue <- function(x, true) {
        if (is.null(x) || isFALSE(x)) return(NULL)
        if (isTRUE(x)) return(true)
        x
    }
    header$indep %<>% nulltrue(purrr::map_chr(reglist, getindep))
    header$regname %<>% nulltrue(names(reglist))
    header$regno %<>% nulltrue(paste0("(", seq_along(reglist), ")"))

    h <- header[!purrr::map_lgl(header, is.null)]
    format_header <- function(x, l = length(reglist)) {
        if (!is.null(names) && all(grepl("^\\d+$", x))) 
            x <- rep(names(x), x)
        rep_len(x, l)
    }
    h <- lapply(h, format_header)
    h
}


# getindep: get independent variable name form reg
getindep <- function(reg) {
    indep <- names(reg$model)[1]
    indep
}


# getesti: get estimate result from reglist ------------------------------------
#
getesti <- function(coefname, reglist, vars, fmt = NULL)  {
    stopifnot(length(coefname) == 1L)
    nameslist <- purrr::map(reglist, ~ names(broom::tidy(.x)))
    if (coefname == "term") return(NULL)
    if (!all(purrr::map_lgl(nameslist, ~ coefname %in% .x))) return(NULL)

    dt_coef <- purrr::map(reglist, ~ broom::tidy(.x) %>%
            data.table::setDT() %>%
            .[vars, ..coefname, on = "term"]) %>%
            do.call("cbind", .) %>%
            .[, term := ..vars] %>%
            data.table::setcolorder("term") %>%
            data.table::setnames(c("term", names(reglist)))

    if (is.null(fmt)) return(dt_coef)
    coef2str(dt_coef, parse_c(fmt))
}



# length_equal: whether two object's length is equal --------------------------
length_equal <- function(x, y) {
    if (length(x) == length(y)) return(TRUE)
    else return(FALSE)
}

# ifthen: scalar version of ifelse --------------------------------------------
ifthen <- function(x, then, otherwise = x, fun = is.null) {
    result <- do.call(fun, list(x))
    stopifnot(isTRUE(result) || isFALSE(result))
    if (result) then else otherwise
}

# rempty: replace empty with specific value -----------------------------------
rempty <- function(x, r, empty = NULL) {
    stopifnot(length(r) == 1L || length(r) == length(x))
    stopifnot(typeof(x) == typeof(r))
    x <- ifelse(is.na(x) | x %in% empty, r, x)
    x
}


# coef2str: transform estimate to string --------------------------------------
coef2str <- function(data, fmt) {
    # for example: c("(", "2", ")")
    stopifnot(length(fmt) == 3)
    l_par = fmt[1]
    r_par = fmt[3]
    digits = as.integer(fmt[2])

    regnames <- names(data)[purrr::map_lgl(data, is.numeric)]
    fm <- function(x, digits = NULL, l_par, r_par) {
        y <- lbs::stformat(x, digits = digits, na.replace = "") %>%
            trimws()
        ifelse(y == "", "", paste0(l_par, y, r_par))
    }
    for (i in seq_along(regnames))
        data[[regnames[i]]] %<>% fm(digits, l_par, r_par)
    data
}

# parse_c: parse numeric format -----------------------------------------------
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

# escape: escape specific chars -----------------------------------------------
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

# dfplus_element: add two data.frame by element -------------------------------
dfplus_element <- function(df_x, df_y, ignore_col = NULL, sep = " ") {
    stopifnot(all.equal(dim(df_x), dim(df_y)))
    for (i in seq_along(df_x)) {
        if (names(df_x)[i] %in% ignore_col) next
        df_x[[i]] <- paste(df_x[[i]], df_y[[i]], sep = sep)
    }
    df_x
}

# dfplus_row: append multi data.frame by position -----------------------------
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
    data.table::setorderv(combined, c("_ori", "_index"))
    if (!is.null(common_col)) {
        for (j in common_col) {
            combined[[j]] = ifelse(
                combined[["_index"]] == 1, combined[[j]], "")
        }
    }
    combined[, -c("_ori", "_index")]
}

# translate: translate specific key words -------------------------------------
translate <- function(x, lang = "en_US") {
    if (lang %in% c("zh_cn", "zh_CN", "ZH_cn", "ZH_CN")) {
        x <- gsub("^term|vari|variable$", "变量", x)
        x <- gsub("^N|obs|nobs$", "观测数", x)
        x <- gsub("^r2|R2|r.squared$", "R^2", x)
        x <- gsub("^ar2|AR2|adj.r.squared$", "调整R^2^", x)
    }
    x
}

# getstat: get stats from estimate result -------------------------------------
getstat <- function(statname, reglist, digits) {
    if (grepl("^N|obs$", statname)) statname <- "nobs"
    if (grepl("^r2|R2$", statname)) statname <- "r.squared"
    if (grepl("^ar2|AR2$", statname)) statname <- "adj.r.squared"

    stat <- lapply(reglist, broom::glance) %>%
        data.table::rbindlist()
    stat <- stat[[statname]]
    if (is.null(stat)) return("")
    stat <- lbs::stformat(stat, digits = digits)
    stat
}

