context("Generate header list")

test_that("Generate header list", {
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
    names(l.reg) <- paste("R", seq_along(l.reg))
    header <- list(indep = TRUE, regname = FALSE, regno = TRUE, NULL)
    expect_true(is.list(l.reg))
    expect_equal(names(genheader(l.reg, header)), c("indep", "regno"))
    expect_equal(genheader(l.reg, header)$indep, c("weight", "weight", "lot1", "lot2"))
    expect_equal(genheader(l.reg, header)$regno, c("(1)", "(2)", "(3)", "(4)"))
    expect_equal(genheader(l.reg, list(t = "test"))$t, c("test", "test", "test", "test"))
    expect_equal(genheader(l.reg, list(regname = paste0("R", seq_along(l.reg)))),
                 list(regname = c("R1", "R2", "R3", "R4")))
    expect_equal(genheader(l.reg, list(regno = T, "x")),
                 list(regno = c("(1)", "(2)", "(3)", "(4)"), rep("x", 4)))
    expect_equal(genheader(l.reg, list(test = c(c1 = 2, c2 = 3))),
                 list(test = c("c1", "c1", "c2", "c2")))
})
