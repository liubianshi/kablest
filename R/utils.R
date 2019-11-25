# Get the posistion of a subset of string vector.
fStringMatch <- function(x, sub_x, method = "exact") {
    if (!is.character(x))
        stop("The first augment must be character")
    if (method == "exact") {
        if (anyNA(match(sub_x, x)))
            stop(paste0(sub_x[is.na(match(sub_x, x))], collapse = " "),
                 ": not exists!")
        match(sub_x, x)    
    } else if (method == "regex") {
        purrr::map(sub_x, ~ stringr::str_which(x, .)) %>%
            purrr::flatten_int() %>% unique()
    } else {
        stop("Currently, only support `exact` and `regex` method")
    }
}

# zip character vector
fZipChar <- function(x) {
    if (length(x) == 1) {
        name <- x[1] 
        y <- 1
        names(y) <- name
        return(y)
    } 
    name <- x[c(TRUE, x[2:length(x)] != x[1:length(x)-1])]
    y <- rep(0, length(name))
    names(y) <- name
    j = 1
    for (i in seq_along(y)) {
        while (j <= length(x)) {
            if (x[j] == names(y)[i]) {
                y[i] <- y[i] + 1
            } else {
                break
            }
            j = j + 1
        }
    }
    y
}

# Get the dependent variable of a regression model
fGetDep <- function(objects) {
    str_match(deparse(objects$call),
          "^[^(]+\\(formula\\s*=\\s*(\\w+)\\s*~.*$")[1, 2]
}

 

