# adjalign: adjust parameter align --------------------------------------------
adjalign <- function(align, length) {
    if (is.null(align)) return(c("l", rep("c", length - 1)))
    if (length(align) == 1L) {
        align <- strsplit(align, "")[[1]]
    }
    stopifnot(all(align %in% c("l", "c", "r")))
    rep_len(align, length)
}

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

    if (outfmt %in% c("flextable", "docx", "word")) {
        starsymbol <- paste0("^", starsymbol, "^")
    } else if (outfmt %in% c("html", "pdf", "kable")) {
        starsymbol <- escape(starsymbol, chars = "*^_`~")
        starsymbol <- paste0("^", starsymbol, "^")
    }
    list(cut = starcut, symbol = starsymbol)
}

# adjvari: adjust variable list -----------------------------------------------
adjvari <- function(vari, reglist) {
    vari %<>% ifthen(list(name = NULL, label = NULL))
    names(vari) <- complete_names(vari, c("name", "label"))
    vars <- lapply(reglist, getindepvars) %>%
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
        y <- strformat(x, digits = digits, na.replace = "") %>% trimws()
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

# dropbyname: drop vector and list element by name ----------------------------
dropbyname <- function(x, name = NULL) {
    names_x <- names(x)
    if (is.null(names_x) || is.null(name)) return(x)
    x[! names_x %in% name]
}

# keepbyname: keep subset vector and list element by name ----------------------------
keepbyname <- function(x, name = NULL) {
    names_x <- names(x)
    if (is.null(names_x) || is.null(name)) return(x)
    x[names_x %in% name]
}

# genbody: generate body data.table from estimate result ----------------------
genbody <- function(esti, reglist, vari, star, outfmt) {
    vari <- adjvari(vari, reglist)
    estilist <- genestimate(reglist, esti$fun, esti$fun.args)

    l.esti <- local({
        n <- names(esti)[!purrr::map_lgl(esti, is.null)]
        n <- n[n %in% c("estimate", "std.error", "statistic", "p.value")]
        purrr::map(n, ~ getesti(.x, estilist, vari$name, esti[[.x]]))
    })

    if (!is.null(star)) {
        l.esti[[1]] <- local({
            stardf <- getesti("p.value", estilist, vari$name) %>%
                purrr::map_dfc(genstar, star = star)
            estidf <- l.esti[[1]]
            for (i in seq_along(stardf)[-1])
                estidf[[i]] <- paste0(estidf[[i]], stardf[[i]])
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
    if ("dep" %in% header$name)
        header$dep <- purrr::map_chr(reglist, getdepvar)
    if ("reg" %in% header$name)
        header$reg <- names(reglist)
    if ("no" %in% header$name)
        header$no <- paste0("(", seq_along(reglist), ")")
    header$name <- NULL

    format_header <- function(x, l = length(reglist)) {
        if (!is.null(names(x)) && all(grepl("^\\d+$", x))) 
            x <- rep(names(x), x)
        rep_len(x, l)
    }

    header[!purrr::map_lgl(header, is.null)] %>% lapply(format_header)
}


# gennote: generate notlist -----------------------------------------------
gennote <- function(note, star, digits = 3L, lang = "en_US") {
    if (is.null(note) || isFALSE(note)) return(NULL)
    if (isTRUE(note)) {
        star <- adjstar(star, "text")
        note <- paste0(star$symbol, " p <", format(star$cut, digits = digits))
        note <- paste(note, collapse = ", ")
    }
    note.list <- list()
    if (is.character(note)) {
        note.list$general = note
        note.list$general_title = translate("Note: ", lang)
    }
    note.list
}

# getdepvar: get dependent variable name form reg ----------------------------
getdepvar <- function(reg) {
    model <- reg$model
    stopifnot(!is.null(model))
    while (inherits(model, "call") || inherits(model, "formula")) {
        model <- model[[2]]
    }
    depvar <- if (inherits(model, "data.frame")) {
        names(reg$model)[1]
    } else if (inherits(model, "name")) {
        as.character(model)
    } else {
        stop("Can not get regression's independent variable")
    }
    depvar
}


# getindepvars: get all variable names from reglist ---------------------------
getindepvars <- function(reg) {
    depvars <- rownames(summary(reg)$coefficients)
    stopifnot(!is.null(depvars))
    depvars
}

# getobsnumber: get observation number from regression model ------------------
getobsnumber <- function(reg) {
    ifthen(reg$N, length(reg$residuals))
}

# genstat: gen stats from estimate result -------------------------------------
genstat <- function(stat, reglist, digits, lang = "en_US") {
    if (is.null(stat)) return(NULL)
    names(stat) <- complete_names(stat, c("name", "label"))
    stat.add <- dropbyname(stat, c("name", "label", ""))
    stat_df <- if (hasName(stat, "name") && is.null(stat$name)) {
        NULL
    } else {
        stat$name %<>% ifthen(c("N", "r2"))
        stat$label %<>% ifthen(stat$name) %>% translate(lang)
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
                x <- strformat(.x, digits = digits, na.replace = "")
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
        stat <- purrr::map_int(reglist, ~ ifthen(getobsnumber(.x), NA))
    } else {
        stat <- purrr::map_dbl(reglist, ~ ifthen(summary(.x)[[statname]], NA))
    }
    if (is.null(stat)) return(rep("", length(reglist)))
    stat <- strformat(stat, digits = digits, na.replace = "")
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

# genreglist: generate regression list ----------------------------------------
genreglist <- function(l) {
    names_l <- ifthen(names(l), rep("", length(l)))
    r <- list()
    n <- c()

    while (length(l) != 0) {
        reg <- shift(l)
        name <- shift(names_l)
        if (is.null(reg)) next
        if (inherits(reg, "list")) {
            r <- c(r, reg)
            push(n, ifthen(names(reg), rep("", length(reg))))
        } else {
            push(r, reg)
            push(n, name)
        }
    }

    if (length(r) == 0) stop("Didn't setting regression!")
    names(r) <- ifelse(n == "", paste0("R", seq_along(r)), n)
    r
}

# genstyle: Constuct style list for output function ---------------------------
genstyle <- function(fmt, arglist, style) {
    switch(fmt,
        flextable = genstyle_flextable(arglist, style),
        genstyle_default(arglist, style)
    )
}

genstyle_default <- function(arglist, style) {
    style %<>% ifthen(list())
    for (n in names(arglist))
        style[[n]] <- arglist[[n]]
    style
}

genstyle_flextable <- function(arglist, style) {
    style <- genstyle_default(arglist, style)
    style$merge_header     %<>% ifthen(TRUE)
    style$multicolumn_line %<>% ifthen(TRUE)
    style$empty_col        %<>% ifthen(TRUE)
    style$fpmin            %<>% ifthen(officer::fp_border(width = 1))
    style$fpmax            %<>% ifthen(officer::fp_border(width = 1.5))
    style
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

# `%//%`: replace NULL with default value -------------------------------------
`%//%` <- function(x, then) {
    if (is.null(x)) then else x 
}


# length_equal: whether two object's length is equal --------------------------
length_equal <- function(x, y) {
    if (length(x) == length(y)) return(TRUE)
    else return(FALSE)
}

# mdlist2chunk: translate parsed markdown list to flextable chunk -------------
mdlist2chunk <- function(l) {
    if (length(l) == 0) return(list(flextable::as_chunk("")))
    l_normal <- l[!purrr::map_lgl(l, is.list)]
    l_list <- l[purrr::map_lgl(l, is.list)]
    if (length(l_normal) == 0)
        return(c(mdlist2chunk(l[[1]]), mdlist2chunk(l[-1])))
    if (length(l_list) != 0L) 
        return(c(mdlist2chunk(l_normal), mdlist2chunk(l_list)))
    if (is.null(l$s)) return(NULL) 
    chunk <- flextable::as_chunk(l$s)
    if (isTRUE(l$i))   chunk$italic           <- TRUE
    if (isTRUE(l$b))   chunk$bold             <- TRUE
    if (isTRUE(l$sup)) chunk$vertical.align   <- "superscript"
    if (isTRUE(l$sub)) chunk$vertical.align   <- "subscript"
    list(chunk)
}

# outpipe: output result in pandoc pipe format ---------------------------------
outpipe <- function(body, stat, header, caption, align, ...) {
    args <- list(
        x = outtext(header, body, stat),
        format = "pipe",
        caption = caption,
        col.names = NA,
        align = align 
    )
    do.call(knitr::kable, args)
}

# outflextable: output result in raw flextable format -------------------------
outflextable <- function(body, stat, header, star, caption, note, lang,
                         style, ...) {
    hline <- nrow(body)
    key_cols <- names(body)
    body <- rbind(body, stat)

    if (!is.null(header)) {
        header <- purrr::map(header[length(header):1], ~ c("", .x))
        header[[1]][1] <- translate("variable", lang)
        if (isTRUE(style$merge_header) && isTRUE(style$empty_col)) {
            header_name <- insertemptycolumn(header, key_cols)
            header <- header_name[[1]]
            key_cols  <- header_name[[2]]
            rm(header_name)
        }
    }

    ft <- flextable::flextable(body, key_cols)
    for (i in seq_along(body)) {
        n <- names(body)[i]
        if (i == 1L) {
            ft %<>% flextable::compose(j = n,
                                       value = str2paragraph(body[[i]]))
        } else {
            ft %<>% flextable::compose(
                j = n,
                value = str2paragraph(body[[i]], syntax = "sup")
            )
        }
    }
    if (!is.null(header)) {
        ft <- flextable::delete_part(ft, part = "header")
        for (h in header) {
            ft %<>% flextable::add_header_row(values = h)
            if (isTRUE(style$merge_header)) {
                ft %<>% flextable::merge_h(i = 1, part = "header") 
                if (!isFALSE(style$multicolumn_line)) {
                    hh <- squeeze(h)
                    hh <- cumsum(hh)[which(hh > 1L) - 1] + 1
                    ft %<>% flextable::hline(i = 1, j = hh,
                        border = style$fpmin, part = "header")
                    rm(hh)
                }
            }
        }
    }

    if (!is.null(note)) {
        note <- paste0(note$general_title, note$general, ".") %>%
            str2paragraph()
        ft %<>% flextable::footnote(value = note, ref_symbols = "" )
    }

    ft %<>% flextable::hline_top(border = style$fpmax, part = "header") %>%
        flextable::hline_bottom(border = style$fpmin, part = "header") %>%
        flextable::hline(i = hline, border = style$fpmin) %>%
        flextable::autofit()
    ft
}

# outtext: output result in raw textformat ------------------------------------
outtext <- function(header, body, stat, ...) {
    header_first_col <- rep("", length(header))
    header %<>% lapply(function(x) {
        x[x == data.table::shift(x)] <- ""
        x
    })
    header_df <- do.call(rbind, header) %>%
        cbind(header_first_col, .) %>%
        data.table::as.data.table()
    names(header_df) <- names(body)
    dt <- rbind(header_df, body, stat)
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

# parse_md: parse string in markdown syntax -----------------------------------
parse_md <- function(s, syntax = c("sup", "i", "b", 'sub'), a = NULL) {
    if (is.null(s) || is.na(s) || s == "") return(NULL)
    if (length(syntax) == 0L) return(s)

    md_syntax <- supported_md_syntax()
    md_syntax_names <- names(md_syntax)
    if (!all(syntax %in% md_syntax_names)) 
        stop("Only support '", paste(md_syntax_names, collapse = ", "), "'.")

    index <- purrr::map_int(syntax,
        ~ regexpr(md_syntax[[.x]]$p, s, perl = TRUE)[[1]])
    if (all(index == -1L))
        return(if (is.null(a)) list(list(s = s)) else c(s = s, a))
    syntax_match_first <- syntax[index > 0L][which.min(index[index > 0L])]

    a.new <- a
    a.new[[syntax_match_first]] <- TRUE
    ss <- sub(md_syntax[[syntax_match_first]]$p,
              md_syntax[[syntax_match_first]]$r, s, perl = TRUE)
    ss <- strsplit(ss, "\t")[[1]]

    c(parse_md(ss[1], syntax, a),
      list(parse_md(ss[2], syntax, a.new)),
      parse_md(ss[3], syntax, a))
}

# push: perl-stly push --------------------------------------------------------
push <- function(x, values) {
    # From:
    #   mpettis/push-pop-shift-unshift.R
    #   https://gist.github.com/mpettis/b7bfeff282e3b052684f
    outer_x <- as.character(substitute(x))
    if (inherits(x, "list")) values <- list(values)
    assign(outer_x, c(x, values), parent.frame())
    invisible(get(outer_x, parent.frame()))
}

# rempty: replace empty with specific value -----------------------------------
rempty <- function(x, r, empty = NULL) {
    stopifnot(length(r) == 1L || length(r) == length(x))
    stopifnot(typeof(x) == typeof(r))
    x <- ifelse(is.na(x) | x %in% empty, r, x)
    x
}

# shift: perl style shift -----------------------------------------------------
shift <- function(x, drop = TRUE) {
    # Inspired by:
    #   mpettis/push-pop-shift-unshift.R
    #   https://gist.github.com/mpettis/b7bfeff282e3b052684f
    if (length(x) == 0) return(NA)
    shiftret <- if (inherits(x, "list") && isTRUE(drop)) {
        x[[1]]
    } else {
        x[1, drop = drop]
    }
    assign(as.character(substitute(x)), x[-1], parent.frame())
    return(shiftret)
}

# squeeze: squeeze vector -----------------------------------------------------
squeeze <- function(x) {
    num <- c(1, which(x != data.table::shift(x)))
    y <- diff(c(num, length(x) + 1))
    names(y) <- x[num]
    y
}

# star2sup: transform star to supscript ---------------------------------------
star2sup <- function(x, symbol = "*") {
    if (length(symbol) == 1L && nchar(symbol[1]) == 1L) {
        symbol <- sub("^", "\\^", symbol, fixed = TRUE) %>%
            paste0("([", ., "]+)")
    } else {
        symbol <- strsplit(symbol, "") %>%
            purrr::map(~ sub("^", "\\^", .x, fixed = TRUE)) %>%
            purrr::map(~paste0("[", .x, "]")) %>%
            purrr::map(paste, collapse = "") %>%
            paste(collapse = "|") %>%
            paste0("(", ., ")")
    }
    if (all(!grepl(paste0(symbol, "$"), x))) return(x)

    x <- sub(paste0(symbol, "*$"), "\t\\1", x)
    x <- strsplit(x, "\t")
    x <- purrr::map(x, ~ {
        if (length(.x) == 1L) .x[2] = ""
        .x
    })
    x <- do.call(rbind, x)
    star_chunk <- flextable::as_sup(x[, 2])
    flextable::as_paragraph(x[,1], star_chunk)
}

# str2paragraph: translate markdown string vector to flextable paragrap -------
str2paragraph <- function(x, syntax = c("b", "i", "sup", "sub"), a = NULL) {
    l_list <- purrr::map(as.character(x), parse_md, syntax, a)
    chunk_list <- purrr::map(l_list, mdlist2chunk)
    para_list <- purrr::map(chunk_list, ~
            flextable::as_paragraph(list_values = .x)[[1]]
    )
    para_list
}

# insertemptycolumn: insert empty column for flextable ------------------------
insertemptycolumn <- function(l, name) {
    # insert space before and after continuous value, for example
    # form list(c("a", "b", "b", "c", "c"), c("d", "d", "e", "e", "e"))
    # to list(c("a", "", "b", "b", "b", "", "c" "c"),
    #         c("d", "d", "d", "", "e", "e", "e", "e"))
    stopifnot(all(purrr::map_int(l, length) == length(name)))

    empty = purrr::map(l, ~ which(.x != data.table::shift(.x))) %>%
        purrr::flatten_int() %>%
        unique() %>%
        sort()

    f <- function(v, empty) {
        if (length(empty) == 1L)
            c(v[1:(empty - 1)], "  ", v[empty:length(v)])
        else
            f(f(v, empty[1]), empty[-1] + 1)
    }
    name %<>% f(empty)
    name <- ifelse(name == "  ", paste0("..", seq_along(name)), name)
    l %<>% purrr::map(~ {
        l0 <- f(.x, empty)
        e0 <- empty[-match(which(.x != data.table::shift(.x)), empty)]
        l0[e0 + match(e0, empty) - 1] <- .x[e0]
        l0
    })
    list(l, name)
}

# supported_md_syntax: currently supported syntax -----------------------------
supported_md_syntax <- function() {
    md <- list(
        b   = list(p = "[*]{2}([^*]+)[*]{2}|[_]{2}([^_]+)[_]{2}", r = "\t\\1\\2\t"),
        i   = list(p = "[_]([^_]+)[_]|[*]([^*]+)[*]",             r = "\t\\1\\2\t"),
        sup = list(p = "\\^([^^]+)\\^",                           r = "\t\\1\t"),
        sub = list(p = "[~]([^~]+)[~]",                           r = "\t\\1\t")
    )
    md
}

# translate: translate specific key words -------------------------------------
translate <- function(x, lang = "en_US") {
    if (lang %in% c("zh_cn", "zh_CN", "ZH_cn", "ZH_CN")) {
        x <- gsub("^Note: $",    "注释：",     x)
        x <- gsub("^term|vari|variable$",    "变量",     x)
        x <- gsub("^N|obs|nobs$",            "观测数",   x)
        x <- gsub("^r2|R2|r.squared$",       "*R*^2^",     x)
        x <- gsub("^ar2|AR2|adj.r.squared$", "调整*R*^2^", x)
    } else if (lang %in% c("en_US", "en_us")) {
        x <- gsub("^term|vari|variable$",    "Variable", x)
        x <- gsub("^N|obs|nobs$",            "N",        x)
        x <- gsub("^r2|R2|r.squared$",       "*R*^2^",     x)
        x <- gsub("^ar2|AR2|adj.r.squared$", "Adj *R*^2^", x)
    }
    x
}

# strformat: format numeric vector --------------------------------------------
strformat <- function(x, digits = 3L, nsmall = 3L, width = NULL,
                      big.mark = ",", na.replace = "") {
    stopifnot(is.numeric(x))
    if (is.integer(x)) return(as.character(x))
    one <- function(z, nsmall, width, digits, na.replace, big.mark) {
        stopifnot(is.numeric(z) && length(z) == 1L)
        if (is.na(z)) return(na.replace)
        if (is.integer(z)) return(format(z, digits = 0, nsmall = 0,
                                         width = width, big.mark = big.mark))
        t <- abs(z)
        n <- if (t == 0) {
                format(z, digits = 0, nsmall = nsmall, width = width, big.mark = big.mark)
            } else if (t < 1) {
                format(z, nsmall = nsmall, width = width,
                       digits = max(0, digits - as.integer(log10(1/t))),
                       , big.mark = big.mark)
            } else if (t < 10) {
                format(z, nsmall = nsmall, width = width, digits = digits,
                       big.mark = big.mark )
            } else if (log10(t) < digits + 1) {
                format(z, digits = digits - as.integer(log10(t)),
                       nsmall = max(0, nsmall - as.integer(log10(t))),
                       width = width, big.mark = big.mark)
            } else {
                format(z, digits = 0, width = width, big.mark = big.mark)
            }
    }
    as.character(lapply(x, one, nsmall, width, digits, na.replace, big.mark))
}
