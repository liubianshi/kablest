context("drop keep by name")

test_that("Test drop by name", {
    l <- list(x = 1, y = 2, z = 3)
    expect_equal(dropbyname(l), l)
    expect_equal(dropbyname(l, "t"), l)
    expect_equal(dropbyname(l, c("x", "y")), list(z = 3))
    expect_equal(keepbyname(l, c("x", "y")), list(x = 1, y = 2))

    v = c(x = 1, y = 2, 3)
    expect_equal(dropbyname(v, ""), c(x = 1, y = 2))
    expect_equal(keepbyname(v, c("x", "y")), c(x = 1, y = 2))
    expect_named(keepbyname(v, ""), "")
})


