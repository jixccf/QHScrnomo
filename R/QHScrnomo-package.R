##' This is an artificial prostate cancer dataset used for illustrating the
##' usages of functions in \code{R} package and
##' \code{QHScrnomo}
##'
##' This is a simulated data set.
##'
##' @title Prostate cancer data set
##' @name prostate.dat
##' @docType data
##' @format A data frame with 2000 observations on the following 9 variables.
##'   \describe{ \item{list("UNIQID")}{patient ID}
##'   \item{list("TX")}{Treatment options of prostate cancera with levels 
##'   \code{EBRT}, 
##'   \code{PI}, \code{RP}}
##'    \item{list("PSA")}{Pre-treatment PSA levels}
##'   \item{list("BX_GLSN_CAT")}{Biopsy Gleason Score Sum. a factor with levels
##'    \code{1} for 2-6 \code{2} for 7 and \code{3} for 8-10}
##'    \item{list("CLIN_STG")}{Clinical stage with levels \code{T1}, \code{T2},
##'    \code{T3}}
##'    \item{list("AGE")}{Age at treatment date}
##'    \item{list("RACE_AA")}{patient ethnicity.a factor with levels \code{0}
##'    for other and \code{1} for African American}
##'    \item{list("TIME_EVENT")}{follow up time in months}
##'   \item{list("EVENT_DOD")}{followup status, 0 - censored, 1 - died of
##'    prostate cancer, 2 - died of other causes} }
##' @import rms
##' @keywords datasets
##' @examples
##'
##' data(prostate.dat)
##'
NULL


##' Data set cr.dat
##'
##' Another artificial dataset
##'
##' @title competing risks data
##' @name cr.dat
##' @docType data
##' @format A data frame with 1000 observations on the following 5 variables.
##'   \describe{ \item{list("ftime")}{follow up time}
##'   \item{list("fstatus")}{event status}
##'    \item{list("age")}{patient age}
##'   \item{list("gender")}{Gender}
##'    \item{list("smoker")}{is a smoker ?}
##' }
##' @keywords datasets
##' @examples
##'
##' data(cr.dat)
##' head(cr.dat)
NULL
