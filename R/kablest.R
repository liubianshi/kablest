#' Match
#' 
#' @param x A character vector.
#' @param sub_x A character vector.
#' @param method A string, used to set matching method, default is "exact".
#' @return The position \code{sub_x} in \code{x}
#' @examples
#' x <- c("A", "B", "C", "D", "A")
#' subx <- c("A", "C")
#' fStringMatch(x, subx)
fStringMatch <- function(x, sub_x, method = "exact") {
    if (!is.character(x))
        stop("The first augment must be character")
    if (method == "exact") {
        if (anyNA(match(sub_x, x)))
            stop(paste0(sub_x[is.na(match(sub_x, x))], collapse = " "),
                 ": not exists!")
        match(sub_x, x)    
    } else if (method == "regex") {
        map(sub_x, ~ str_which(x, .)) %>% flatten_int() %>% unique()
    } else {
        stop("Currently, only support `exact` and `regex` method")
    }

}

#' zip character vector
#'
#' @param x A character vector
#' @return A named integer vector, the name represents elements if \code{x}
#'     which is different from former elements, the number represents number  
#'     of consecutive occurrences.
#' @examples
#' x <- c("A", "B", "B", "D", "D", "A", "A" ,"E")
#' fZipChar(x)
fZipChar <- function(x) {
    if (length(x) == 1) return(x) 
    name <- x[c(TRUE, x[2:length(x)] != x[1:length(x)-1])]
    y <- rep(0, length(name))
    names(y) <- name
    j = 1
    for (i in seq_along(y)) {
        while (j <= length(x)) {
            if (x[j] == names(y)[i]) {
                y[i] <- y[i] + 1
            } else {
                break
            }
            j = j + 1
        }
    }
    y
}

methods("lag")

nobs.felm <- function(objects) {
    objects$N
}

fGetDep <- function(objects) {
    str_match(deparse(objects$call),
          "^[^(]+\\(formula\\s*=\\s*(\\w+)\\s*~.*$")[1, 2]
}
 
fPrint <- function(z, digits = getOption("digits"), nsmall = 3,
                   width = 7, big.mark = ",") {
        if (is.na(z)) 
            return("")
        t <- abs(z)
        nsmall <- min(digits, nsmall)
        if (t == 0) {
            x <- format(z, digits = 0, 
                        nsmall = nsmall, width = width, scientific = FALSE)
        } else if (t < 1) {
            x <- format(z, digits = max(0, digits - as.integer(log10(1/t))), 
                        nsmall = nsmall, width = width, scientific = FALSE)
        } else if (t < 10) {
            x <- format(z, digits = digits, nsmall = nsmall,
                        width = width, scientific = FALSE)
        } else if (log10(t) < digits + 1) {
            x <- format(z, digits = digits - as.integer(log10(t)),
                        nsmall = max(0, nsmall - as.integer(log10(t))),
                        width = width, scientific = FALSE, big.mark = big.mark)
        } else {
            x <- format(z, digits = 0, scientific = FALSE, big.mark = big.mark)
        }
        x
}

kableReg <- function(
    reg.list, style = "text", caption = NULL, label = NULL, align = NULL,
    escape = TRUE, note = NULL, LANG = NULL, column.name = NULL,
    var.drop = NULL, var.drop.method = "exact",
    var.keep = NULL, var.keep.method = "exact", 
    var.order = NULL, var.order.method = "exact",
    var.label = NULL, single.row = FALSE,
    coef.alter = NULL, coef.se = TRUE, coef.t = FALSE, coef.p = FALSE,
    coef.star = c("***.01", "**.05", "*.1"),
    digits.coef = 3L, digits.se = 3L, digits.p = 3L, digits.t = 2L,
    bracket.se = "()", bracket.t = "()", bracket.p = "[]",
    stat.obs = TRUE, stat.r2 = TRUE, stat.ar2 = FALSE, stat.other = NULL,
    stat.label = NULL, add.lines = list(),
    header.add.args = list(), multicolumn = TRUE, 
    header.model = TRUE, header.model.args = list(),
    header.dependent = FALSE, header.dependent.args = list(),
    header.number = FALSE, header.number.args = list(),
    kable.args = list(), kable.style.args = list(), ...) {

#> handle variable 
    variable <- map(reg.list, ~ tidy(.)[["term"]]) %>%
                flatten_chr() %>% unique()
    if (!is.null(var.drop)) {          # 处理变量删除 
        k.t <- fStringMatch(variable, var.drop, var.drop.method)
        if (length(k.t) != 0) 
            variable <- variable[-k.t]
    }
    if (!is.null(var.keep)) {          # 处理变量保留 
        k.t <- fStringMatch(variable, var.keep, var.keep.method)
        if (length(k.t) != 0) 
            variable <- variable[k.t]
    }
    if (!is.null(var.order)) {          # 处理变量保留 
        k.t <- fStringMatch(variable, var.order, var.order.method)
        if (length(k.t) != 0) 
            variable <- c(variable[k.t], variable[-k.t])
    }
    variable <- tibble(term = variable)

#> handle coef 
    if (is.null(coef.alter))
        k.coef.data.list <- c("estimate")
    if (coef.se)
        k.coef.data.list <- c(k.coef.data.list, "std.error")
    if (coef.t)
        k.coef.data.list <- c(k.coef.data.list, "statistic")
    if (coef.p || !is.null(coef.star))
        k.coef.data.list <- c(k.coef.data.list, "p.value")
    if (!is.null(coef.star))
        k.coef.data.list <- c(k.coef.data.list, "star")
    coef.data.list <- vector("list", length(k.coef.data.list))
    names(coef.data.list) <- k.coef.data.list
    for (i in seq_along(k.coef.data.list)) {
        if (k.coef.data.list[i] == "star")
            next 
        coef.data.list[[i]] <- reg.list %>%
            map_dfc(~ tidy(.) %>%
                      select(c("term", k.coef.data.list[i])) %>%
                      right_join(variable, by = "term") %>%
                      select(c(k.coef.data.list[i]))) %>%
                      bind_cols(variable, .)
        names(coef.data.list[[i]])[-1] <- c(str_c("V", seq_along(reg.list)))
    }

#> handle star, star must comply with specific formats 
    if (!is.null(coef.star)) {
        d.t <- str_match(coef.star, "^([^.]+)\\.([0-9]+)$")[, -1] %>%
            as_tibble() %>%
            rename(symbol = "V1", cut = "V2") %>%
            mutate(cut = as.double(str_c("0.", cut))) %>%
            arrange(cut)
        d.s <- d.t[[1]]
        d.c <- d.t[[2]]
        coef.data.list[["star"]] <- coef.data.list[["p.value"]][, -1] %>%
            map_dfc(~ {
                k.t <- ifelse(is.na(.), 1L, .)
                temp <- rep("", length(k.t))
                for (i in seq_along(d.c)) {
                    temp <- ifelse(temp == "" & k.t <= d.c[i], d.s[i], temp)
                }
                temp                    
            }) %>% bind_cols(variable, .)
    }

#> Convert numeric data to string 
    #> coef
    coef.data.list[["estimate"]][, -1] <-
        coef.data.list[["estimate"]][, -1] %>%
            map_dfc(~ map_chr(., fPrint, digits = digits.coef) %>% str_trim())
    #> se
    if (coef.se) {
        coef.data.list[["std.error"]][, -1] <-
            coef.data.list[["std.error"]][, -1] %>%
                map_dfc(~ {
                    temp <- map_chr(., fPrint, digits = digits.se)
                    temp <- str_trim(temp) 
                    ifelse(temp == "", "",
                           str_c(str_sub(bracket.se, 1,1), temp,
                                 str_sub(bracket.se, 2,2)))
                }) 
    }
    #> t-statistic
    if (coef.t) {
        coef.data.list[["statistic"]][, -1] <-
            coef.data.list[["statistic"]][, -1] %>%
                map_dfc(~ {
                    temp <- map_chr(., fPrint, digits = digits.t)
                    temp <- str_trim(temp) 
                    ifelse(temp == "", "",
                           str_c(str_sub(bracket.t, 1,1), temp,
                                 str_sub(bracket.t, 2,2)))
                })
    }
    #> p-value
    if (coef.p) {
        coef.data.list[["p.value"]][, -1] <-
            coef.data.list[["p.value"]][, -1] %>%
                map_dfc(~ {
                    temp <- map_chr(., fPrint, digits = digits.p)
                    temp <- str_trim(temp) 
                    ifelse(temp == "", "",
                           str_c(str_sub(bracket.p, 1,1), temp,
                                 str_sub(bracket.p, 2,2)))
                })
    }

#> combine coef, t, p and star
    #> coef and star
    if (!is.null(coef.star))
        if (style == "text") {
            body <- map2_dfc(coef.data.list[["estimate"]][, -1],
                             coef.data.list[["star"]][, -1], str_c) %>% 
                    bind_cols(variable, .)
        } else if (style %in% c("latex", "html", "markdown")) {
            body <- coef.data.list[["star"]][, -1] %>%
                    map_dfc(function(x) {
                        ifelse(x == "", "", str_c("$^{", x, "}$"))
                    }) %>%
                    map2_dfc(coef.data.list[["estimate"]][, -1], ., str_c) %>% 
                    bind_cols(variable, .)
        }
    if (single.row == TRUE) {
        if (coef.se)
            body <- map2_dfc(body[, -1], coef.data.list[["std.error"]][, -1],
                             str_c, sep = " ") %>% bind_cols(variable, .)
        if (coef.t)
            body <- map2_dfc(body[, -1], coef.data.list[["statistic"]][, -1],
                             str_c, sep = " ") %>% bind_cols(variable, .)
        if (coef.p)
            body <- map2_dfc(body[, -1], coef.data.list[["p.value"]][, -1],
                             str_c, sep = " ") %>% bind_cols(variable, .)
    } else {
        body <- mutate(body, ori = row_number(), index = 1)
        if (coef.se) 
            body <- coef.data.list[["std.error"]] %>%
                mutate(ori = row_number(), index = 2) %>%
                bind_rows(body, .) %>% arrange(ori, index)
        if (coef.t)
            body <- coef.data.list[["statistic"]] %>%
                mutate(ori = row_number(), index = 3) %>%
                bind_rows(body, .) %>% arrange(ori, index)
        if (coef.p)
            body <- coef.data.list[["p.value"]] %>%
                mutate(ori = row_number(), index = 4) %>%
                bind_rows(body, .) %>% arrange(ori, index)
        body <- body %>%
            mutate(term = ifelse(index ==1, term, "")) %>%
            select(-c("ori", "index"))
        k.hline <- dim(body)[1]
    }

#> handle variable label
    if (!is.null(var.label)) {
        k.variable <- flatten_chr(variable)
        l.variable <- vector("list", length(k.variable))
        names(l.variable) <- k.variable
        if (is.character(var.label)) {
            if (length(var.label) != length(k.variable))
                stop(str_c("var.label: using list instead",
                           "or make vector length equal ",
                           "reserved variables"))
            for (i in seq_along(k.variable)) {
                l.variable[[i]] <- var.label[i]
            }
        } else if (is.list(var.label)) {
            for (i in seq_along(k.variable)) {
                l.variable[[i]] <- ifelse(is.null(var.label[[k.variable[i]]]),
                                          k.variable[i],
                                          var.label[[k.variable[i]]])
            }
        }
        body <- body %>%
            mutate(term = map_chr(term, ~ ifelse(. ==  "", "",
                                                 l.variable[[.]])))
    }

#> handle add lines
    if (length(add.lines) != 0) {
        k.temp <- names(add.lines)
        add.lines <- add.lines %>%
            map(~ if (is.numeric(.)) { 
                      map_chr(., fPrint, digits = coef.digits) %>%
                      str_trim()
                } else {
                    .
                })
        line.list <- bind_cols(add.lines) %>% t() %>% as_tibble(rownames = "term")
        body <- bind_rows(body, line.list)
    }

#> handle statistic
    if (stat.obs || stat.r2 || stat.ar2 || (! is.null(stat.other))) {
        if (stat.obs)
            k.stat.list <- c("obs.number")
        if (stat.r2)
            k.stat.list <- c(k.stat.list, "r.squared")
        if (stat.ar2)
            k.stat.list <- c(k.stat.list, "adj.r.squared")
        if (! is.null(stat.other))
            k.stat.list <- c(k.stat.list, stat.other)
        stat.list <- vector("list", length(k.stat.list))
        names(stat.list) <- k.stat.list
        for (i in seq_along(k.stat.list)) {
            if (k.stat.list[[i]] == "obs.number") {
                stat.list[[i]] <- reg.list %>% map_int(nobs)
                next
            }        
            stat.list[[i]] <- reg.list %>% map_dbl(~ glance(.)[[k.stat.list[i]]])
        }
        stat.list <- bind_cols(stat.list) %>% t() %>%
            as_tibble(rownames = "term")
        stat.list[,-1] <- stat.list[,-1] %>%
            map_dfc(~ map_chr(., fPrint, digits = digits.coef) %>%
            str_trim())
        if (!is.null(stat.label)) {
            l.stat.list <- vector("list", length(k.stat.list))
            names(l.stat.list) <-k.stat.list 
            if (is.character(stat.label)) {
                if (length(stat.label) != length(k.stat.list))
                    stop(str_c("stat.label: using list instead",
                            "or make vector length equal ",
                            "reserved variables"))
                for (i in seq_along(k.stat.list)) {
                    l.stat.list[[i]] <- stat.label[i]
                }
            } else if (is.list(stat.label)) {
                for (i in seq_along(k.stat.list)) {
                    l.stat.list[[i]] <- ifelse(
                        is.null(stat.label[[k.stat.list[i]]]),
                        k.stat.list[i], stat.label[[k.stat.list[i]]])
                }
            }
            stat.list <- stat.list %>%
                mutate(term = map_chr(term, ~ ifelse(. ==  "", "",
                                                 l.stat.list[[.]])))
        }
        body <- bind_rows(body, stat.list)
    }    

#> handle column.name 
    if (!is.null(column.name) && length(column.name) != length(reg.list))
        stop("length of header not equal reg number")
    if (is.null(column.name)) {
        names(body) <- c("variable", str_c("(", seq_along(reg.list), ")"))
    } else {
        names(body) <- c("variable", column.name)
    }


#> Adjust output format
    if (style == "text") {
        print(as.data.frame(body))
        invisible(body)
    } else {
    #> handle language
        if (LANG == "zh_CN") {
            body$variable <- str_replace(body$variable, "^obs\\.number$",
                                         "观测数")
            body$variable <- str_replace(body$variable, "^r\\.squared$",
                                         "$R^2$")
            body$variable <- str_replace(body$variable, "^adj\\.r\\.squared$",
                                         "调整 $R^2$")
            names(body)[1] <- "变量"
            general.title <- "注释: "
        }
        kable.args[["x"]] <- body
        kable.args[["format"]] <- style
        kable.args[["caption"]] <- caption 
        kable.args[["escape"]] <- escape 
        kable.args[["align"]] <- align
        kable.args[["label"]] <- label
        if (is.null(kable.args[["booktabs"]]))
            kable.args[["booktabs"]] <- TRUE
        if (is.null(kable.args[["linesep"]]))
            kable.args[["linesep"]] <- "" 
        out.kable <- do.call(kable, kable.args)

        kable.style.args[["kable_input"]] <- out.kable
        if (is.null(kable.style.args[["position"]]))
            kable.style.args[["position"]] = "center"
        out.kable <- do.call(kable_styling, kable.style.args) %>%
            row_spec(k.hline, hline_after = TRUE)

        #> handle header
        k.temp <- c(header.model, header.dependent, header.number)
        if (sum(k.temp) != 0) {
            header <- vector("list", sum(k.temp))
            names(header) <- c("model", "dependent", "number")[k.temp]
            header.args <- vector("list", sum(k.temp))
            names(header.args) <- c("model", "dependent", "number")[k.temp]
            if (header.number) {
                header[["number"]] <- str_c("(", seq_along(reg.list), ")")
                header.args[["number"]] <- header.number.args
                if (is.null(header.number.args))
                    header.args[["number"]] <- header.add.args
            }
            if (header.dependent) {
                header[["dependent"]] <- map_chr(reg.list, fGetDep)
                header.args[["dependent"]] <- header.dependent.args
                if (is.null(header.dependent.args))
                    header.args[["dependent"]] <- header.add.args
            }
            if (header.model) {
                header[["model"]] <- map_chr(reg.list, ~ class(.)[1])
                header.args[["model"]] <- header.model.args
                if (is.null(header.model.args))
                    header.args[["model"]] <- header.add.args
            }
            if (multicolumn) {
                header <- header %>% map(~ c("", fZipChar(.)))
            } else {
                header <- header %>% map(~ c("", .))
            }
            for (i in seq(header)) {
                header.args[[i]][["kable_input"]] <- out.kable
                header.args[[i]][["header"]] <- header[[i]] 
                out.kable <- do.call(add_header_above, header.args[[i]])
            }
        }

        #> handle notes
        if (is.null(note) || !is.na(note)) {
            if (is.null(note))
                note <- str_c(d.s, " p < ", d.c, collapse = "; ")
            out.kable <- footnote(out.kable, general = note,
                                  escape = escape,
                                  general_title = general.title,
                                  title_format = "bold",
                                  footnote_as_chunk = TRUE)
        }
    }
    out.kable
}


