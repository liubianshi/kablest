context("Generate Star form p-value")

test_that("genstar return a character vector", {
    p <- c(0.5, 0.1, 0.05, 0.01, 0.001)
    s <- c("", "*", "*", "**", "***")
    starcut <- c(0.001, 0.01, 0.1)
    starsymbol <- c("*", "**", "***", "****")
    star <- list(cut = starcut, symbol = starsymbol)
    star_adj <- list(cut = c(0.1, 0.01, 0.001),
                     symbol = c("*", "**", "***"))
    star_adj_escape <- list(cut = c(0.1, 0.01, 0.001),
                     symbol = c("^\\*^", "^\\*\\*^", "^\\*\\*\\*^"))

    expect_error(adjstar(0.1, "a"))
    expect_equal(adjstar(star), star_adj)
    expect_equal(adjstar(star, "kable"), star_adj_escape)
    expect_type(genstar(p, adjstar(star)), "character")
    expect_equal(genstar(p, adjstar(star)), s)
    expect_error(genstar(c(1.2, star_adj)))
})

