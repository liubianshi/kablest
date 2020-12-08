#' Personal use fucntion
#'
#' @name kablest-package
#' @aliases kablest
#' @docType package
NULL

.onLoad <- function(libname, pkgname) {
    NULL
}

# Import -----------------------------------------------

#' @importFrom magrittr %>%
magrittr::`%>%`

#' @importFrom magrittr %<>%
magrittr::`%<>%`

#' @importFrom data.table :=
data.table::`:=`


#' @importFrom data.table setDT
data.table::setDT

#' @importFrom data.table data.table
data.table::data.table

#' @importFrom data.table as.data.table
data.table::as.data.table

#' @importFrom rlang !!
rlang::`!!`

#' @importFrom rlang !!!
rlang::`!!!`

