context("get estimate stats from regslit")

test_that("get estimate stats from regslit", {
    expect_null(getstat(stat = NULL))
    expect_null(getstat(stat = list(name = ""), l.reg, 3L))
    expect_error(getstat(stat = list(name = "N", label = c("N1", "N2"))))

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
    vars <- c("groupTrt", "log(u)")
    expect_equal(getstat_byname("N", l.reg, 0L)[1], "20")
    expect_equal(getstat_byname("r2", l.reg, 3L),
                 c("0.073", "0.982", "", ""))

    stat <- list(name = c("test","N", "r2", "ar2"),
                 label = NULL, NULL,
                 "Year Effects" = rep("Y", 4), 
                 "Firm Effects" = rep("Y", 4))  
    terms <- c("Year Effects", "Firm Effects", "test",
               "N", "*R*^2^", "Adj *R*^2^")
    o_stat <- getstat(stat, l.reg, 3L)
    data.table::setcolorder(o_stat, "term")
    expect_equal(o_stat$term, terms)
    expect_equal(o_stat$R1[1:3],   c("Y", "Y", ""))
    expect_equal(o_stat$R1[4:6],   c("20", "0.073", "0.022"))

    stat <- list("Year Effects" = "Y")
    o_stat <- getstat(stat, l.reg, 3L)
    expect_equal(nrow(o_stat), 1L)
    expect_equal(o_stat$R4, "Y")
})




