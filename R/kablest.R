#' Get number of felm regression observations
#'
#' @param object An object created by lfe::felm
#' @return An integer 
#' @export
nobs.felm <- function(object) {
    object$N
}

#' Print formated number depents on the number size
#' 
#' The \code{fPrint} function format number depends on its own size.
#'
#' @param z a number.
#' @param digits the maximum number of digits to the right of  
#'      the decimal point
#' @inheritParams base::format 
#' @return A formated character representations of \code{z} 
#' @examples
#' fPrint(NA)
#' fPrint(0)
#' fPrint(0, digits = 3)
#' fPrint(0.12345)
#' fPrint(0.1)
#' fPrint(0.012345)
#' fPrint(1.121234)
#' fPrint(11.121234)
#' fPrint(111.12123)
#' fPrint(11111)
fPrint <- function(z, digits = getOption("digits"),
                   width = NULL, big.mark = ",") {
        if (is.null(width)) width <- digits + 3L
        if (is.na(z)) 
            return("")
        t <- abs(z)
        if (t == 0) {
            x <- format(z, digits = 0, nsmall = digits, width = width)
        } else if (t < 1) {
            x <- format(z, digits = max(0, digits - as.integer(log10(1/t))), 
                        nsmall = digits, width = width, scientific = FALSE)
        } else if (t < 10) {
            x <- format(z, digits = digits, nsmall = digits,
                        width = width, scientific = FALSE)
        } else if (round(t) < 10^digits) {
            width <- ifelse(round(t) < 10^3, width, width -1)
            x <- format(z, digits = digits - as.integer(log10(round(t))),
                        nsmall = max(0, digits - as.integer(log10(round(t)))),
                        width = width, scientific = FALSE, big.mark = big.mark)
        } else {
            x <- format(z, digits = 0, width = width - length(big.mark),
                        scientific = FALSE,
                        big.mark = big.mark)
        }
        x
}

#' Convert tidy regress model results to academic table
#'
#' Based on tidy tibble form regress model results converted by \code{broom}
#'      package, \code{kablest} created an academic table including: coef or
#'      other similar variable provided by \code{broom::tidy}, se and/or t 
#'      statistic, significant symbol, model observations numbers, and/or 
#'      other model statistics provided by \code{broom:glance}. 
#'
#' @param reg.list a list of regression results. Regresssion model needs to
#'      be handled by \code{broom} package.
#' @param format a string for output format, which can be \code{text}(default)
#'      \code{markdown} or \code{latex}.
#' @param caption the table caption.
#' @param label The table reference label. By default, the label is obtained
#'      from ‘knitr::opts_current$get('label')’.
#' @param align Column alignment: a character vector consisting of \code{l} 
#'      (left), \code{c} (center) and/or \code{r} (right). By default or
#'      if \code{align = NULL}, numeric columns are right-aligned, and
#'      other columns are left-aligned. 
#' @param escape Boolean; whether to escape special characters when producing
#'      HTML or LaTeX tables.
#' @param note the table note, by default or if \code{note = NULL}, note is
#'      the meaning of significant symbol.
#' @param LANG the language used to automatically generate default names 
#' @param column.name a string vector for column names of output kabel.
#'      by default or if \code{column.name = NULL}, the column names if 
#'      regression number.
#' @param var.drop a string vector for variables needed to drop out.
#' @param var.drop.method \code{exact} (default) or \code{regex}; the 
#'      method used to match string.
#' @param var.keep a string vector for variables needed to keep.
#' @param var.keep.method \code{exact} (default) or \code{regex}; the 
#'      method used to match string.
#' @param var.order a variable name vector for sort variables.
#' @param var.order.method \code{exact} (default) or \code{regex}; the 
#'      method used to match string.
#' @param var.label a string vector or a list. If \code{var.label = NULL},
#'      using varname; if \code{var.label} is a string vector, its length 
#'      and order must consistent with the remaining variables; if 
#'      \code{var.label} is a list, element name must in remainning variables.
#' @param single.row T/F, whether to display \code{coef}, \code{se} and \code{t}
#'      in one line.
#' @param coef.alter a string; by the default or \code{coef.alter = NULL}, the
#'      output kable will display \code{coef} first. Otherwise, the variable  
#'      corresponding to the string will be displayed. The string must be a  
#'      variable of \code{broom::tidy} output.
#' @param coef.se T/F; whether display \code{se} (standard error). The default
#'      is \code{TRUE}.
#' @param coef.t T/F; whether display \code{t} (t or z statistic). The default
#'      is \code{FALSE}.
#' @param coef.p T/F; whether display \code{p} (p-value). The default
#'      is \code{FALSE}.
#' @param coef.star a string vector that satisifies a specific format. The    
#'      element needed to be match by patten \code{"^([^.]+)\\.([0-9]+)$"}.
#'      If \code{coef.star = NULL}, the \code{coef.star} will not be displayed.
#' @param digits.coef the maximum number of digits to the right of  
#'      the decimal point
#' @param digits.se the maximum number of digits to the right of  
#'      the decimal point
#' @param digits.t the maximum number of digits to the right of  
#'      the decimal point
#' @param bracket.se the bracket surround the \code{se}
#' @param bracket.t the bracket surround the \code{t}
#' @param bracket.p the bracket surround the \code{p}
#' @param stat.obs T/N; wheter to display observation number
#' @param stat.r2 T/N; wheter to display r.squared 
#' @param stat.ar2 T/N; wheter to display adjusted r.squared 
#' @param stat.other T/N; wheter to display other statistics, which must be  
#'      contained in \code{broom::glance} output.
#' @param stat.label a string vector of list. If it is a string vector, the
#'      length and order must be identical to remaining \code{stat}; if it is
#'      a list, the element name must be a remaining \code{stat}.
#' @param add.lines a list of vectors (one vector per line) containing
#'      additional lines to be included in the table. Each element name wil be
#'      treated as a \code{stat}. Each element of the listed vectors will be
#'      put into a separate column. The length each listed vectors must be
#'      equal to the length of \code{reg.list}.
#' @param header.model T/F, whether to display the regression model.
#' @param header.model.args parameter list, which will be passed to
#'      \code{kableExtra::add_header_above}.
#' @param header.dependent T/F, whether to display the regression dependent
#'      variable. The default is \code{FALSE}. Currently the function to get
#'      the dependent variable is not muture, pluse use it wich caution. 
#' @param header.dependent.args parameter list, which will be passed to
#'      \code{kableExtra::add_header_above}.
#' @param header.number T/F, whether to display the regression number. The
#'      default is \code{FALSE}, because the default \code{column.namel} will
#'      display regression number. Consider turning this option on after
#'      setting \code{column.namel} to other. 
#' @param header.number.args parameter list, which will be passed to
#'      \code{kableExtra::add_header_above}.
#' @param header.add.args parameter list, which will be passed to
#'      \code{kableExtra::add_header_above}. When \code{header.model.args},
#'      \code{header.dependent.args} and/or \code{header.number.args} is
#'      \code{list()}, these parameter will use \code{header.add.args} as
#'      default.
#' @param multicolumn T/N; whether to merge header cells when the value of
#'      adjacent column are the same. 
#' @param kable.args parameter list which will be passed to \code{knitr::kable}
#'      to control the display of the \code{latex} and/or \code{markdown}
#'      format 
#'      output. When a \code{kable.args} conflicts with the of \code{kablest},
#'      the setting of \code{kablest} will prevail.
#' @param kable.style.args parameter list which will be passed to
#'      \code{kableExtra::kable_styling}
#'      to control the display of the \code{latex} and/or \code{markdown} style
#'      output. When a \code{kable.style.args} conflicts with the of
#'      \code{kablest}, the setting of \code{kablest} will prevail.
#' @return A kable. If \code{style = "text"}, the \code{kablest} will print
#'      the output as a \code{data.frame} and return a character \code{tibble}.
#'      If \code{style} is \code{markdown}, \code{latex} or \code{html},
#'      \code{kablest} will print and return a formated kable which can handled
#'      by \code{kableExtra}.
#' @importFrom magrittr `%>%`
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,80,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#' glm.1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' glm.2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
#' l.reg <- list(lm.D9, lm.D90, glm.1, glm.2)
#' kablest(l.reg)
#' kablest(l.reg, coef.star= c("+.001", "*.01"))
#' kablest(l.reg, format = "latex")
#' kablest(l.reg, var.keep = "group.*", var.keep.method = "regex")
#' kablest(l.reg,  var.keep = c("groupTrt", "log(u)"))
#' kablest(l.reg,  var.keep = c("groupTrt", "log(u)"),
#'         var.label = list("log(u)" = "log_u"))
#' kablest(l.reg,  var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"))
#' kablest(l.reg,  var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"),
#'         add.lines = list(FE = rep("N", 4)))
#' kablest(l.reg, var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"),
#'         coef.t = TRUE, bracket.t = "[]",
#'         add.lines = list(FE = rep("N", 4)))
#' kablest(l.reg, var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"),
#'         single.row = TRUE,
#'         add.lines = list(FE = rep("N", 4)))
#' kablest(l.reg, var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"),
#'         single.row = TRUE, coef.se = FALSE,
#'         add.lines = list(FE = rep("N", 4)))
#' kablest(l.reg, var.keep = c("groupTrt", "log(u)"),
#'         var.label = c("Trt", "log_u"),
#'         digits.se = 4L, digits.coef = 4L,
#'         add.lines = list(FE = rep("N", 4)))
#' @export
kablest <- function(
    reg.list, format = "text", caption = NULL, label = NULL, align = NULL,
    escape = TRUE, note = NULL, LANG = "en_US", column.name = NULL,
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
    header.model = TRUE, header.model.args = list(),
    header.dependent = FALSE, header.dependent.args = list(),
    header.number = FALSE, header.number.args = list(),
    header.add.args = list(), multicolumn = TRUE, 
    kable.args = list(), kable.style.args = list(), ...
) {
#> handle variable
    variable <- purrr::map(reg.list, ~ broom::tidy(.)[["term"]]) %>%
                purrr::flatten_chr() %>% unique()
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
    variable <- tibble::tibble(term = variable)
    
#> handle coef 
    k.coef <-  if (is.null(coef.alter)) "estimate" else coef.alter
    if (coef.se) k.coef <- c(k.coef, "std.error")
    if (coef.t) k.coef <- c(k.coef, "statistic")
    if (coef.p || !is.null(coef.star)) k.coef <- c(k.coef, "p.value")
    if (!is.null(coef.star)) k.coef <- c(k.coef, "star")
    l.coef <- vector("list", length(k.coef))
    names(l.coef) <- k.coef
    for (i in seq_along(k.coef)) {
        if (k.coef[i] == "star")
            next 
        l.coef[[i]] <- reg.list %>%
            purrr::map_dfc(~
                broom::tidy(.) %>%
                dplyr::select(c("term", k.coef[i])) %>%
                dplyr::right_join(variable, by = "term") %>%
                dplyr::select(c(k.coef[i]))
            ) %>%
            dplyr::bind_cols(variable, .)
        names(l.coef[[i]])[-1] <- c(stringr::str_c("V", seq_along(reg.list)))
    }

#> handle star, star must comply with specific formats 
    if (!is.null(coef.star)) { 
        coef.star <- c("***.00001", coef.star) 
        d.t <- stringr::str_match(coef.star, "^([^.]+)\\.([0-9]+)$")[, -1] %>%
            as.data.frame(stringsAsFactors = FALSE) %>% .[-1,] %>%
            dplyr::rename(symbol = "V1", cut = "V2") %>%
            dplyr::mutate(cut = as.double(stringr::str_c("0.", cut))) %>%
            dplyr::arrange(cut)
        d.s <- d.t[[1]]
        d.c <- d.t[[2]]
        l.coef[["star"]] <- l.coef[["p.value"]][, -1] %>%
            purrr::map_dfc(~ {
                k.t <- ifelse(is.na(.), 1L, .)
                temp <- rep("", length(k.t))
                for (i in seq_along(d.c)) {
                    temp <- ifelse(temp == "" & k.t <= d.c[i], d.s[i], temp)
                }
                temp                    
            }) %>% dplyr::bind_cols(variable, .)
    }

#> Convert numeric data to string 
    #> coef
    if (is.null(coef.alter)) {
        l.coef[["estimate"]][, -1] <- l.coef[["estimate"]][, -1] %>%
            purrr::map_dfc(~ purrr::map_chr(., fPrint, 
                                            digits = digits.coef) %>%
            stringr::str_trim())
    } else {
        l.coef[[coef.alter]][, -1] <- l.coef[[coef.alter]][, -1] %>%
            purrr::map_dfc(~ purrr::map_chr(., fPrint, 
                                            digits = digits.coef) %>%
            stringr::str_trim())
    }
    #> se
    if (coef.se) {
        l.coef[["std.error"]][, -1] <- l.coef[["std.error"]][, -1] %>%
            purrr::map_dfc(~ {
                temp <- purrr::map_chr(., fPrint, digits = digits.se)
                temp <- stringr::str_trim(temp) 
                ifelse(temp == "", "",
                        stringr::str_c(stringr::str_sub(bracket.se, 1, 1),
                                        temp,
                                        stringr::str_sub(bracket.se, 2, 2)))
            }) 
    }
    #> t-statistic
    if (coef.t) {
        l.coef[["statistic"]][, -1] <- l.coef[["statistic"]][, -1] %>%
            purrr::map_dfc(~ {
                temp <- purrr::map_chr(., fPrint, digits = digits.t)
                temp <- stringr::str_trim(temp) 
                ifelse(temp == "", "",
                       stringr::str_c(stringr::str_sub(bracket.t, 1, 1), temp,
                                      stringr::str_sub(bracket.t, 2, 2)))
            })
    }
    #> p-value
    if (coef.p) {
        l.coef[["p.value"]][, -1] <- l.coef[["p.value"]][, -1] %>%
            purrr::map_dfc(~ {
                temp <- purrr::map_chr(., fPrint, digits = digits.p)
                temp <- stringr::str_trim(temp) 
                ifelse(temp == "", "",
                       stringr::str_c(stringr::str_sub(bracket.p, 1, 1), temp,
                                      stringr::str_sub(bracket.p, 2, 2)))
            })
    }
#> combine coef, t, p and star
    #> coef and star
    if (!is.null(coef.star))
        if (format == "text") {
            body <- if(is.null(coef.alter)) {
                purrr::map2_dfc(l.coef[["estimate"]][, -1],
                                l.coef[["star"]][, -1], stringr::str_c) %>%
                dplyr::bind_cols(variable, .)
            } else {
                purrr::map2_dfc(l.coef[[coef.alter]][, -1],
                                l.coef[["star"]][, -1], stringr::str_c) %>%
                dplyr::bind_cols(variable, .)
            }
        } else if (format %in% c("latex", "html", "markdown")) {
            body <- l.coef[["star"]][, -1] %>%
                purrr::map_dfc(function(x) {
                    ifelse(x == "", "", stringr::str_c("$^{", x, "}$"))
                }) %>%
                purrr::map2_dfc(l.coef[[
                        if (is.null(coef.alter)) "estimate" else coef.alter
                    ]][, -1], ., stringr::str_c) %>% 
                dplyr::bind_cols(variable, .)
        }
    if (single.row == TRUE) {
        if (coef.se)
            body <- body[, -1] %>%
                purrr::map2_dfc(l.coef[["std.error"]][, -1],
                                stringr::str_c, sep = " ") %>%
                dplyr::bind_cols(variable, .)
        if (coef.t)
            body <- body[, -1] %>%
                purrr::map2_dfc(l.coef[["statistic"]][, -1],
                                stringr::str_c, sep = " ") %>%
                dplyr::bind_cols(variable, .)
        if (coef.p)
            body <- body[, -1] %>%
                purrr::map2_dfc(l.coef[["p.value"]][, -1],
                                stringr::str_c, sep = " ") %>%
                dplyr::bind_cols(variable, .)
    } else {
        body <- dplyr::mutate(body, ori = dplyr::row_number(), index = 1)
        if (coef.se) 
            body <- l.coef[["std.error"]] %>%
                dplyr::mutate(ori = dplyr::row_number(), index = 2) %>%
                dplyr::bind_rows(body, .) %>%
                dplyr::arrange(ori, index)
        if (coef.t)
            body <- l.coef[["statistic"]] %>%
                dplyr::mutate(ori = dplyr::row_number(), index = 3) %>%
                dplyr::bind_rows(body, .) %>%
                dplyr::arrange(ori, index)
        if (coef.p)
            body <- l.coef[["p.value"]] %>%
                dplyr::mutate(ori = row_number(), index = 4) %>%
                dplyr::bind_rows(body, .) %>%
                dplyr::arrange(ori, index)
        body <- body %>%
            dplyr::mutate(term = ifelse(index == 1, term, "")) %>%
            dplyr::select(-c("ori", "index"))
        k.hline <- dim(body)[1]
    }
#> handle variable label
    if (!is.null(var.label)) {
        k.variable <- purrr::flatten_chr(variable)
        l.variable <- vector("list", length(k.variable))
        names(l.variable) <- k.variable
        if (is.character(var.label)) {
            if (length(var.label) != length(k.variable))
                stop(stringr::str_c("var.label: using list instead",
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
            dplyr::mutate(term = purrr::map_chr(
                term, ~ ifelse(. ==  "", "", l.variable[[.]])))
    }

#> handle add lines
    if (length(add.lines) != 0) {
        k.temp <- names(add.lines)
        add.lines <- add.lines %>%
            purrr::map(~
                if (is.numeric(.)) { 
                    purrr::map_chr(., fPrint, digits = coef.digits) %>%
                    stringr::str_trim()
                } else {
                    .
                })
        body <- dplyr::bind_cols(add.lines) %>%
            t() %>%
            tibble::as_tibble(rownames = "term") %>%
            dplyr::bind_rows(body, .)
    }
#> handle statistic
    if (stat.obs || stat.r2 || stat.ar2 || (!is.null(stat.other))) {
        k.stat <- vector("character") 
        if (stat.obs) k.stat <- c("obs.number")
        if (stat.r2)  k.stat <- c(k.stat, "r.squared")
        if (stat.ar2) k.stat <- c(k.stat, "adj.r.squared")
        if (!is.null(stat.other)) k.stat <- c(k.stat, stat.other)
        l.stat <- vector("list", length(k.stat))
        names(l.stat) <- k.stat
        for (i in seq_along(k.stat)) {
            if (k.stat[[i]] == "obs.number") {
                l.stat[[i]] <- reg.list %>% purrr::map_int(nobs)
                next
            }        
            l.stat[[i]] <- reg.list %>%
                purrr::map_dbl(~ if(is.null(broom::glance(.)[[k.stat[i]]])) NA
                else broom::glance(.)[[k.stat[i]]])
        }
        l.stat <- dplyr::bind_cols(l.stat) %>% t() %>%
            tibble::as_tibble(rownames = "term")
        l.stat[, -1] <- l.stat[, -1] %>%
            purrr::map_dfc(~
                purrr::map_chr(., fPrint, digits = digits.coef) %>%
                stringr::str_trim()
            )
        if (!is.null(stat.label)) {
            l.stat.label <- vector("list", length(k.stat))
            names(l.stat.label) <- k.stat 
            if (is.character(stat.label)) {
                if (length(stat.label) != length(k.stat))
                    stop(stringr::str_c("stat.label: using list instead",
                                        "or make vector length equal ",
                                        "reserved variables"))
                for (i in seq_along(k.stat)) {
                    l.stat.label[[i]] <- stat.label[i]
                }
            } else if (is.list(stat.label)) {
                for (i in seq_along(k.stat)) {
                    l.stat.label[[i]] <- ifelse(
                        is.null(stat.label[[k.stat[i]]]),
                        k.stat[i], stat.label[[k.stat[i]]])
                }
            }
            l.stat <- l.stat %>% dplyr::mutate(
                term = purrr::map_chr(
                    term, ~ ifelse(. ==  "", "", l.stat.label[[.]])
                )
            )
        }
        body <- dplyr::bind_rows(body, l.stat)
    }    
#> handle column.name 
    if (!is.null(column.name) && length(column.name) != length(reg.list))
        stop("length of header not equal reg number")
    if (is.null(column.name)) {
        names(body) <- c("variable", stringr::str_c("(", seq_along(reg.list), ")"))
    } else {
        names(body) <- c("variable", column.name)
    }
#> Adjust output format
    if (format == "text") {
        print(as.data.frame(body), right = FALSE)
        invisible(body)
    } else {
    #> handle language
        k.note.title <- "Note: "
        if (LANG == "zh_CN") {
            body$variable <- body$variable %>%
                stringr::str_replace("^obs\\.number$", "观测数") %>%
                stringr::str_replace("^r\\.squared$", "$R^2$") %>%
                stringr::str_replace("^adj\\.r\\.squared$", "调整 $R^2$")
            names(body)[1] <- "变量"
            k.note.title <- "注释: "
        }
        kable.args[["x"]]       <- body
        kable.args[["format"]]  <- format 
        kable.args[["caption"]] <- caption
        kable.args[["escape"]]  <- escape
        kable.args[["align"]]   <- align
        kable.args[["label"]]   <- label
        if (is.null(kable.args[["booktabs"]]))
            kable.args[["booktabs"]] <- TRUE
        if (is.null(kable.args[["linesep"]]))
            kable.args[["linesep"]] <- "" 
        out.kable <- do.call(knitr::kable, kable.args)
        kable.style.args[["kable_input"]] <- out.kable
        if (is.null(kable.style.args[["position"]]))
            kable.style.args[["position"]] = "center"
        out.kable <- do.call(kableExtra::kable_styling, kable.style.args) %>%
            kableExtra::row_spec(k.hline, hline_after = TRUE)
        #> handle header
        k.temp <- c(header.model, header.dependent, header.number)
        if (sum(k.temp) != 0) {
            l.header <- vector("list", sum(k.temp))
            names(l.header) <- c("model", "dependent", "number")[k.temp]
            l.header.args <- vector("list", sum(k.temp))
            names(l.header.args) <- c("model", "dependent", "number")[k.temp]
            if (header.number) {
                l.header[["number"]] <- stringr::str_c(
                    "(", seq_along(reg.list), ")")
                l.header.args[["number"]] <- header.number.args
                if (is.null(header.number.args))
                    l.header.args[["number"]] <- header.add.args
            }
            if (header.dependent) {
                l.header[["dependent"]] <- purrr::map_chr(reg.list, fGetDep)
                l.header.args[["dependent"]] <- header.dependent.args
                if (is.null(header.dependent.args))
                    l.header.args[["dependent"]] <- header.add.args
            }
            if (header.model) {
                l.header[["model"]] <- purrr::map_chr(reg.list, ~ class(.)[1])
                l.header.args[["model"]] <- header.model.args
                if (is.null(header.model.args))
                    l.header.args[["model"]] <- header.add.args
            }
            if (multicolumn) {
                l.header <- l.header %>% purrr::map(~ c("", fZipChar(.)))
            } else {
                l.header <- l.header %>% purrr::map(~ c("", .))
            }
            for (i in seq(l.header)) {
                l.header.args[[i]][["kable_input"]] <- out.kable
                l.header.args[[i]][["header"]] <- l.header[[i]] 
                out.kable <- do.call(kableExtra::add_header_above,
                                     l.header.args[[i]])
            }
        }

        #> handle notes
        if (is.null(note) || !is.na(note)) {
            if (is.null(note))
                note <- stringr::str_c(d.s, " p < ", d.c, collapse = "; ")
            out.kable <- kableExtra::footnote(
                out.kable, general = note,
                escape = escape, threeparttable = TRUE,
                general_title = k.note.title, title_format = "bold",
                footnote_as_chunk = TRUE
            )
        }
        out.kable
    }
}


