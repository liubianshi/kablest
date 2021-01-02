#' Tabulating Regresssion for Report and Further Processing
#'
#' Convert regress model results to a `data.frame`, `kable`, `flextable`,
#' or other similar object. The output object can be printed directly to pdf,
#' html or docx file. `tabreg` provides some common settings.
#' The output object can be further precessed, such as adding lines and
#' footnote, merging cells, adjusting output format, etc. In additional,
#' the way of procesing model results and output format can also be customised.
#'
#' @param reglist a list of regression results.
#' @param caption the table caption.
#' @param outfmt a string for output format, which can be `text`(default),
#'      `kable`, `flextable`, `markdown`, or `pandoc`.
#' @param vari a named list indicating variable name and label. If `name` is `NULL`,
#'      all regressors are used. If `label` is `NULL`, variable names are used.
#'      `name` needed to be a character vector, while `label` can be a character
#'      vector or named list. If `label` is a vector, the length must equal 
#'      to the number of uesd variables.
#' @param esti a named list indicating which data to extract and in what
#'      way to present them.
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
#' tabreg(l.reg)
#' @export
tabreg <- function(reglist, caption = NULL, outfmt = "text",
    vari = list(name = NULL, label = NULL),
    esti = list(estimate = 3L, std.error = "(3)", statistic = NULL,
                p.value = NULL, singlerow = FALSE,
                fun = NULL, fun.args = NULL),
    star = list(cut = c(0.1, 0.05, 0.01), symbol = c("*", "**", "***")),
    stat = list(name = c("N", "r2"), label = c("N", "*R*^2^")),
    header = list(name = c("indep", "reg", "no")),
    note = TRUE,
    outfun = NULL, outargs = list(), outstyle = NULL
) {
    # adjust parameter
    names(reglist) %<>% ifthen(paste0("R", seq_along(reglist)))
    digits <- ifthen(as.integer(parse_c(esti$estimate)[2]), 3L)
    vari   <- adjvari(vari, reglist)
    star   <- adjstar(star, outfmt)
    lang   <- ifthen(outargs$lang,
                     substr(Sys.getenv("LANG", "en_US.utf8"), 1, 5))

    # gen data
    body   <- genbody(esti, reglist, vari, star, outfmt)
    stat   <- genstat(stat, reglist, digits, lang)
    header <- genheader(reglist, header)
    note   <- gennote(note, star, digits, lang)
    style  <- genstyle(outfmt, outargs, outstyle)

    # output
    out <- local({
        out.args <- list(body    = body,
                         stat    = stat,
                         header  = header,
                         star    = star,
                         caption = caption,
                         note    = note,
                         lang    = lang,
                         style   = style)
        if (is.null(outfun)) outfun <- paste0("out", outfmt)
        do.call(outfun, out.args)
    })

    # export
    out
}


