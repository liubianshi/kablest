context("get estimate result from regslit")

test_that("get estimate result from regslit", {
    expect_equal(parse_c(3), c("", "3", ""))
    expect_equal(parse_c("(2)"), c("(", "2", ")"))

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
        lot2 = c(69,35,26,21,18,16,13,12,12))
        glm.1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
        glm.2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
        list(lm.D9, lm.D90, glm.1, glm.2)
    })
    names(l.reg) <- paste0("R", seq_along(l.reg))
    vars <- c("groupTrt", "log(u)")

    expect_null(getesti("TEST", genestimate(l.reg), vars))
    expect_null(getesti("term", genestimate(l.reg), vars))

    result <- getesti("estimate", genestimate(l.reg), vars, fmt = 3)
    expect_equal(result$term, vars)
    expect_equal(names(result), c("term", names(l.reg)))
    expect_equal(as.character(result[1,]), c("groupTrt", "-0.371", "4.661", "", ""))

    result2 <- getesti("statistic", genestimate(l.reg), vars, fmt = "(2)")
    expect_equal(result2[[3]], c("(21.17)", ""))

    result3 <- getesti("p.value", genestimate(l.reg), vars, fmt = "[3]")
    expect_equal(result3[[2]], c("[0.249]", ""))

    com <- dfplus_row(result, result2, result3, common_col = "term")
    expect_equal(com$term, c("groupTrt", "", "", "log(u)", "", ""))
    expect_equal(com$R1, c("-0.371", "(-1.191)", "[0.249]", "", "", ""))

    com2 <- dfplus_element(result, result2, ignore_col = "term")
    expect_equal(com2$term, vars)
    expect_equal(com2$R1, c("-0.371 (-1.191)", " "))

    stardf <- getesti("p.value", genestimate(l.reg), vars) %>%
        purrr::map_dfc(genstar, star = list(cut = 0.01, symbol = "🌟"))
    result <- dfplus_element(result, stardf, "term", sep = "")
    com <- dfplus_row(result, result2, common_col = "term")
    expect_equal(com$R2, c("4.661🌟", "(21.17)", "", ""))
})


