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
#' @param path Path or connection to write to.     
#' @param append If `FALSE`, will overwrite existing file. If `TRUE`, will
#'      append to existing file. In both cases, if file does not exist a
#'      new file is created.
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
kablest <- function(..., reglist = NULL, outfmt = "text",
    path = NULL, append = FALSE,
    caption = NULL, align = NULL, lang = "en_US",
    vari = list(name = NULL, label = NULL),
    esti = list(estimate = 3L, std.error = "(3)", statistic = NULL,
                p.value = NULL, singlerow = FALSE,
                fun = NULL, fun.args = NULL),
    star = list(cut = c(0.1, 0.05, 0.01), symbol = c("*", "**", "***")),
    stat = list(name = c("N", "r2"), label = c("N", "R^2^")),
    header = list(indep = TRUE, regname = TRUE, regno = TRUE),
    note = TRUE,
    header.args = list(top = TRUE, multicolumn = TRUE),
    flextable.args = list(empty_col = TRUE, multicolumn_line = TRUE),
    kable.args = list(), kable.style.args = list()
) {
    # digits for float number
    digits <- ifthen(as.integer(esti$estimate), 3L)

    # reglist and header
    reglist <- c(list(...), if(!is.null(reglist)) reglist)
    stopifnot(length(reglist) > 0)
    names(reglist) %<>% ifthen(paste0("R", seq_along(reglist)))
    vari <- adjvari(vari, reglist)
    star <- adjstar(star, outfmt)
    header <- genheader(reglist, header)
    body <- genbody(esti, reglist, vari, star, outfmt)
    stat <- getstat(stat, reglist, digits, lang)


    # output
    out <- local({
        outargs <- list(body = body,
                        stat = stat,
                        header = header,
                        star = star,
                        caption = caption,
                        note = gennotelist(note, star, digits, lang),
                        align = align,
                        lang = lang,
                        flextable.args = flextable.args,
                        header.args = header.args,
                        kable.args = kable.args,
                        kable.style.args = kable.args)
        do.call(paste0("out", outfmt), outargs)
    })

    # export
    if (!is.null(path)) write(out, path, append = append)
    invisible(out)
}


