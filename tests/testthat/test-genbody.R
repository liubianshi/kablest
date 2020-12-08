context("generate body data.table from estimate result")

l.reg <- local({
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lm.D9 <- lm(weight ~ group)
    lm.D90 <- lm(weight ~ group - 1) # omitting intercept
    clotting <- data.frame(
        u = c(5,10,15,20,30,40,60,80,100),
        lot1 = c(118,58,42,35,27,25,21,19,18),
        lot2 = c(69,35,26,21,18,16,13,12,12)
    )
    glm.1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
    glm.2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
    l <- list(lm.D9, lm.D90, glm.1, glm.2)
    names(l) <- paste0("R", seq_along(l))
    l
})

test_that("adjust variable list", {
    expect_identical(adjvari(NULL, NULL)$name, character(0))
    expect_identical(adjvari(NULL, NULL)$label, character(0))

    result <- adjvari(NULL, l.reg)
    expect_length(result, 2L)
    expect_identical(result$name, result$label)
    vari = list(name = c("groupTrt", "log(u)"),
                label = list("log(u)" = "u_ln"))
    result2 <- adjvari(vari, l.reg)
    expect_equal(result2$label, c("groupTrt", "u_ln"))
    expect_error(adjvari(list(label = "N"), l.reg))
})

test_that("generate body table", {
    esti <- list(estimate = 3L, std.error = "(3)", singlerow = T)
    star = adjstar(list(0.01, "*"))
    vari <- list(c("groupTrt", "log(u)"), list("log(u)" = "log_u"))
    result <- genbody(esti, l.reg, vari, star, "text")
    expect_equal(result$term, c("groupTrt", "log_u"))
    expect_equal(result[1, R1], "-0.371 (0.311)")
    expect_equal(result[2, R3], "0.015* (0.000)")
    esti$singlerow <- FALSE
    result2 <- genbody(esti, l.reg, vari, star, "text")
    expect_equal(result2$term, c("groupTrt", "", "log_u", ""))
    expect_equal(result2[1:2, R1], c("-0.371", "(0.311)"))
})

test_that("test custom function", {
    .genesti <- function(reg, m = 1L) {
        reg_coef <- summary(reg)$coefficients
        coef_df <- as.data.frame(reg_coef, row.names = FALSE)
        coefnames <- c("estimate", "std.error", "statistic", "p.value")
        names(coef_df) <- coefnames
        coef_df$estimate  <- coef_df$estimate * m
        coef_df$std.error <- coef_df$std.error * m
        coef_df$term <- row.names(reg_coef)
        coef_df[c("term", coefnames)]
    }
    esti <- list(estimate = 3L, std.error = "(3)", singlerow = T,
                 fun = .genesti, fun.args = list(m = 10L))
    star = adjstar(list(0.01, "*"))
    vari <- list(c("groupTrt", "log(u)"), list("log(u)" = "log_u"))
    result <- genbody(esti, l.reg, vari, star, "text")
    expect_equal(result[1, R1], "-3.710 (3.114)")
    expect_equal(result[2, R3], "0.153* (0.004)")
})

