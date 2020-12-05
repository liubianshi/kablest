context("Generate Star form p-value")

test_that("genstar return a character vector", {
    p <- c(0.5, 0.1, 0.05, 0.01, 0.001)
    expect_equal(genstar(p), c("", "*", "**", "***", "***"))
    expect_equal(genstar(p, c(0.005, 0.01, 0.05)), c("", "", "*", "**", "***"))
    expect_equal(genstar(p, ,c("+", "*", "**")), c("", "+", "*", "**", "**"))
    expect_error(genstar(c(1.2)))
    expect_error(genstar(c(-0.2)))
})

