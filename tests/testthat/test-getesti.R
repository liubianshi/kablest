context("get estimate result from regslit")

test_that("get estimate result from regslit", {
    vars <- c("groupTrt", "log(u)")
    expect_equal(parse_c(3), c("", "3", ""))
    expect_equal(parse_c("(2)"), c("(", "2", ")"))
    expect_equal(getesti("estimate", l.reg, vars, fmt = 3)[1,], 
                 c("groupTrt", "-0.371", "4.661", "", ""))
})
