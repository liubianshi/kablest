# adjstar: adjust star vetor --------------------------------------------------
adjstar <- function(star, outfmt = "text") {
    if (is.null(star)) return(NULL)
    names(star) = complete_names(star, c("cut", "symbol"))
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

# adjvari: adjust variable list -----------------------------------------------
adjvari <- function(vari, reglist) {
    vari %<>% ifthen(list(name = NULL, label = NULL))
    names(vari) <- complete_names(vari, c("name", "label"))
    vars <- purrr::map(reglist, ~ names(.x$coefficients)) %>%
        purrr::flatten_chr() %>%
        unique()
    vari$name %<>% ifthen(vars)
    vari$label %<>% ifthen(vars)
    vari$label <- if (is.character(vari$label)) {
        stopifnot(length_equal(vars, vari$label))
        vari$label
    } else if (is.list(vari$label)) {
        purrr::map_chr(vari$name, ~ ifthen(vari$label[[.x]], .x))
    } else {
        stop("vari$label only accep character vector of list")
    }
    vari
}

# coef2df: translate coefficients of regression to data.frame -----------------
coef2df <- function(reg) {
    reg_coef <- summary(reg)$coefficients
    coef_df <- as.data.frame(reg_coef, stringsAsFactors = FALSE)
    coefnames <- c("estimate", "std.error", "statistic", "p.value")
    names(coef_df) <- coefnames
    coef_df$term <- row.names(reg_coef)
    coef_df[c("term", coefnames)]
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
        y <- lbs::stformat(x, digits = digits, na.replace = "") %>% trimws()
        ifelse(y == "", "", paste0(l_par, y, r_par))
    }
    for (i in seq_along(regnames))
        data[[regnames[i]]] %<>% fm(digits, l_par, r_par)
    data
}

# complete_names: complete object names ---------------------------------------
complete_names <- function(obj, n) {
    nms <- names(obj)
    nms_null <- if (is.null(nms)) seq_along(obj) else which(nms == "")
    nms_not_null <- nms[which(nms != "")]

    n <- n[!(n %in% nms_not_null)]
    nms[nms_null] <- n[seq_along(nms_null)]
    nms[is.na(nms)] <- ""
    nms
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

# genbody: generate body data.table from estimate result ----------------------
genbody <- function(esti, reglist, vari, star, outfmt) {
    vari <- adjvari(vari, reglist)
    estilist <- genestimate(reglist, esti$fun, esti$fun.args)

    l.esti <- local({
        n <- names(esti)[!purrr::map_lgl(esti, is.null)]
        n <- n[!n %in% c("singlerow", "fun", "fun.args")]
        purrr::map(n, ~ getesti(.x, estilist, vari$name, esti[[.x]]))
    })

    if (!is.null(star)) {
        l.esti[[1]] <- local({
            stardf <- getesti("p.value", estilist, vari$name) %>%
                purrr::map_dfc(genstar, star = adjstar(star, outfmt))
            estidf <- l.esti[[1]]
            for (i in seq_along(stardf)) {
                if (i == 1L) next
                estidf[[i]] <- paste0(estidf[[i]], stardf[[i]])
            }
            estidf
        })
    }

    # body: combined
    body <- if (length(l.esti) <= 1L) {
        l.esti[[1]]
    } else {
        if (isTRUE(esti$singlerow)) {
            purrr::reduce(l.esti, dfplus_element, ignore_col = "term")
        } else {
            dfplus_row(list = l.esti, common_col = "term")
        }
    }
    body$term <- vari$label[match(body$term, vari$name)] %>% rempty("")
    body
}

# genestimate: generate estimate data.table from esitmates result -------------
genestimate <- function(reglist, fun = NULL, fun.args = NULL) {
    len <- length(reglist)

    fun %<>% ifthen(coef2df)
    fun <- c(fun)
    fun <- fun[rep_len(seq_along(fun), len)]

    fun.args %<>% ifthen(list())

    fun.args <- if (length(fun.args) == 0L) {
        fun.args[rep_len(1, len)]
    } else {
        fun.args[rep_len(seq_along(fun.args), len)]
    }
    fun.args <- purrr::map2(reglist, fun.args, ~ c(list(.x), .y))
    estilist <- purrr::map2(fun, fun.args, do.call)
    names(estilist) <- names(reglist)
    estilist
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


# gennotelist: generate notlist -----------------------------------------------
gennotelist <- function(note, star, digits = 3L, lang = "en_US") {
    if (is.null(note) || isFALSE(note)) return(NULL)
    note <- if (isTRUE(note)) {
        star <- adjstar(star, "text")
        star$symbol %<>% escape("*^_`~")
        note <- paste0(star$symbol, " p <", format(star$cut, digits = digits))
        note <- paste(note, collapse = ", ")
    }
    note.list <- list()
    if (is.character(note)) {
        note.list$general = note
        note.list$general_title = translate("Note ", lang)
    }
    note.list
}

# getindep: get independent variable name form reg ----------------------------
getindep <- function(reg) {
    indep <- names(reg$model)[1]
    indep
}


# getstat: get stats from estimate result -------------------------------------
getstat <- function(stat, reglist, digits, lang = "en_US") {
    if (is.null(stat)) return(NULL)
    stat.add <- stat[-match(c("name", "label", ""), names(stat),
                    nomatch = length(stat) + 1)]

    stat_df <- if (is.null(stat$name) || all(lbs::isempty(stat$name))) {
        NULL
    } else {
        stat$label %<>% ifthen(translate(stat$name, lang))
        stopifnot(length_equal(stat$name, stat$label))
        local({
            l <- lapply(stat$name, getstat_byname, reglist, digits)
            d <- do.call(rbind, l) %>% as.data.frame()
            d$term <- stat$label
            data.table::setcolorder(d, "term")
        })
    }
 
    if (length(stat.add) != 0) {
        add2str <- function(x, len = 0L) {
            if (is.numeric(x))
                x <- lbs::stformat(.x, digits = digits, na.replace = "")
            if (len > 0L)
                x <- rep_len(x, len)
            x
        }
        stat.add <- local({
            l <- lapply(stat.add, add2str, len = length(reglist))
            d <- do.call(rbind, l) %>% as.data.frame()
            d$term <- names(stat.add)
            data.table::setcolorder(d, "term")
        })
        stat_df <- data.table::rbindlist(list(stat.add, stat_df))
    }

    if (!is.null(stat_df)) {
        data.table::setnames(stat_df, c("term", names(reglist)))
    }
    stat_df
}

# getstat_byname: get specific stats form regression --------------------------
getstat_byname <- function(statname, reglist, digits) {
    if (grepl("^N|obs$", statname)) statname <- "nobs"
    if (grepl("^r2|R2$", statname)) statname <- "r.squared"
    if (grepl("^ar2|AR2$", statname)) statname <- "adj.r.squared"
    stopifnot(length(reglist) > 0)

    if (statname == "nobs") {
        stat <- purrr::map_int(reglist, ~ ifthen(nrow(.x$model), NA))
    } else {
        stat <- purrr::map_dbl(reglist, ~ ifthen(summary(.x)[[statname]], NA))
    }
    if (is.null(stat)) return(rep("", length(reglist)))
    stat <- lbs::stformat(stat, digits = digits, na.replace = "")
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

# getesti: get estimate result ------------------------------------
getesti <- function(coefname, estilist, vars, fmt = NULL)  {
    stopifnot(length(coefname) == 1L)
    nameslist <- purrr::map(estilist, names)

    if (coefname == "term") return(NULL)
    if (!all(purrr::map_lgl(nameslist, ~ coefname %in% .x))) return(NULL)

    dt_coef <- estilist %>%
        purrr::map(data.table::as.data.table) %>%
        purrr::map(~ .x[vars, ..coefname, on = "term"]) %>%
        do.call("cbind", .) %>%
        .[, term := ..vars] %>%
        data.table::setcolorder("term") %>%
        data.table::setnames(c("term", names(estilist)))

    if (is.null(fmt)) return(dt_coef)
    coef2str(dt_coef, parse_c(fmt))
}

# ifthen: scalar version of ifelse --------------------------------------------
ifthen <- function(x, then, otherwise = x, fun = is.null) {
    result <- do.call(fun, list(x))
    stopifnot(isTRUE(result) || isFALSE(result))
    if (result) then else otherwise
}

# length_equal: whether two object's length is equal --------------------------
length_equal <- function(x, y) {
    if (length(x) == length(y)) return(TRUE)
    else return(FALSE)
}

# outtext: output result in raw textformat ------------------------------------
outtext <- function(body, stat, ...) {
    ft <- flextable::flextable()
    rbind(body, stat)
}

# outflextable: output result in raw flextable format -------------------------
outflextable <- function(body, stat, header, caption, note, flextable.args, ...) {
    ft <- rbind(body, stat) %>%
        flextable::flextable()
    if (!is.null(header)) {
        ft <- delete_part(x = ft, part = "header")
    }

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

# rempty: replace empty with specific value -----------------------------------
rempty <- function(x, r, empty = NULL) {
    stopifnot(length(r) == 1L || length(r) == length(x))
    stopifnot(typeof(x) == typeof(r))
    x <- ifelse(is.na(x) | x %in% empty, r, x)
    x
}


# translate: translate specific key words -------------------------------------
translate <- function(x, lang = "en_US") {
    if (lang %in% c("zh_cn", "zh_CN", "ZH_cn", "ZH_CN")) {
        x <- gsub("^Note: $",    "注释：",     x)
        x <- gsub("^term|vari|variable$",    "变量",     x)
        x <- gsub("^N|obs|nobs$",            "观测数",   x)
        x <- gsub("^r2|R2|r.squared$",       "R^2^",     x)
        x <- gsub("^ar2|AR2|adj.r.squared$", "调整R^2^", x)
    } else if (lang %in% c("en_US", "en_us")) {
        x <- gsub("^term|vari|variable$",    "Variable", x)
        x <- gsub("^N|obs|nobs$",            "N",        x)
        x <- gsub("^r2|R2|r.squared$",       "R^2^",     x)
        x <- gsub("^ar2|AR2|adj.r.squared$", "Adj R^2^", x)
    }
    x
}

