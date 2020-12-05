context("Generate header list")

test_that("Generate header list", {
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
