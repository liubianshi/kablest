context("tabreg estimate result")

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
test_that("outtext", {
    result <- tabreg(l.reg[[1]], reglist = l.reg[2:4])
    expect_equal(result$term[13], "*R*^2^")
    expect_equal(result$R2[13], "0.982")
    expect_equal(result$R4[10], "0.024***")
})

test_that("markdown", {
    result <- tabreg(l.reg[[1]], reglist = l.reg[2:4],
                     outfmt = "markdown", caption = "test markdown")
    print(result)
    expect_s3_class(result, "knitr_kable")
    expect_true(grepl("^\\-+$", result[17]))
    expect_true(grepl("^ +weight +lot1", result[4]))
})

test_that("insertemptycolumn", {
    l <- list(c("a", "b", "b", "c", "c"), c("d", "d", "e", "e", "e"))
    name <- LETTERS[1:5]
    expect_equal(insertemptycolumn(l, name)[[1]],
                 list(c("a", "  ", "b", "b", "b", "  ", "c", "c"),
                      c("d", "d", "d", "  ", "e", "e", "e", "e"))
    )
})

test_that("outflextable", {
    result <- tabreg(l.reg, outfmt = "flextable",
                     header = list(c("A", "A", "B", "B"),
                                    name = c("dep", "no")))
    #print(str(result, max.level = 2))
    expect_equal(class(result$header), "complex_tabpart")
    expect_equal(dim(result$header$dataset), c(3, 9))
    expect_equal(class(result$footer), "complex_tabpart")
    expect_equal(dim(result$footer$dataset), c(1, 9))
    expect_equal(result$col_keys[c(2, 8)], c("..2", "..8"))
})

#parse_md("*R*aa^2^bb**2_cc_**") %>% mdlist2chunk() %>%
    #flextable::as_paragraph(list_values = .)
#str2paragraph(c("*R*^2^", "**B**~j~", "w"))
