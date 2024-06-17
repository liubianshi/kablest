parser_lib <- list(
  fixest = list(
    indepvar = function(reg) names(reg$coefficients),
    depvar = function(reg) {
      depvar <- reg$fml[[2]] %>% as.character()
      return(depvar)
    },
    nobs    = function(reg) nobs(reg),
    r2      = function(reg) fixest::r2(reg)['r2'],
    ar2     = function(reg) fixest::r2(reg)['ar2'],
    pr2     = function(reg) fixest::r2(reg)['pr2'],
    coef_df = function(reg) {
      coef_df<- summary(reg)$coeftable %>% as.data.frame(stringsAsFactors = FALSE)
    }
  ),
  default = list(
    indepvar = function(reg) rownames(summary(reg)$coefficients),
    nobs = function(reg) ifthen(nobs(), length(reg$residuals)),
    depvar = function(reg) {
      model <- if (class(reg) == "fixest") reg$fml else reg$model
      stopifnot(!is.null(model))
      while (inherits(model, "call") || inherits(model, "formula")) {
        model <- model[[2]]
      }
      depvar <- if (inherits(model, "data.frame")) {
          names(reg$model)[1]
      } else if (inherits(model, "name")) {
          as.character(model)
      } else {
          stop("Can not get regression's independent variable")
      }
      return(depvar)
    },
    coef_df = function(reg) {
      summary(reg)$coefficients %>% as.data.frame(stringsAsFactors = FALSE)
    },
    r2 = function(reg) {
      ifthen(summary(reg)[["r.squared"]], NA)
    },
    ar2 = function(reg) {
      ifthen(summary(reg)[["adj.r.squared"]], NA)
    },
    stats = function(reg, stat) {
      ifthen(summary(reg)[[stat]], NA)
    }
  )
)

parser <- function(reg, e) {
  c <- class(reg)
  if (c %in% names(parser_lib) && e %in% names(parser_lib[[c]])) {
    return(do.call(parser_lib[[c]][[e]], list(reg = reg)))
  } 

  if (e %in% names(parser_lib$default)) {
    return(do.call(parser_lib[["default"]][[e]], list(reg = reg)))
  } 

  return(do.call(parser_lib[["default"]][["stats"]], list(reg = reg, stat = e)))

  
}
