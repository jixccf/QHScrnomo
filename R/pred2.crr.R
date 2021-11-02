##' Predict Cumulative Incidence Rate
##'
##' Calculate the predicted cumulative incidence rate based on a saved
##' competing risks regression model.
##' The cumulative incidence is adjusted for other competing causes
##' rather than the event of interest.
##'
##' @title Predict Cumulative Incidence Rate
##' @param f.crr a saved competing risks regression model created by function
##'   \code{\link{crr.fit}}
##' @param lp a scalar being the sum of linear predictors for a single subject.
##' @param time expected time point, at which cumulative incidence rate will be
##'   assessed.
##' @return Return the predicted cumulative incidence rate.
##' @author Michael Kattan, Ph.D, Changhong Yu \cr Department of Quantitative
##'   Health Sciences\cr Cleveland Clinic\cr
##' @seealso \code{\link[cmprsk]{predict.crr}} \code{\link[QHScrnomo]{crr.fit}}
##' @keywords survival utilities
##' @examples
##'
##' data(prostate.dat)
##' library(Hmisc,TRUE)
##' library(rms,TRUE)
##' dd <- datadist(prostate.dat)
##' options( datadist = "dd")
##' f.cph <- cph(formula = Surv(TIME_EVENT, EVENT_DOD == 1 ) ~ rcs(AGE,3) +
##'     CLIN_STG + rcs(PSA, 3),
##'     data = prostate.dat, x = TRUE, y = TRUE, surv = TRUE)
##' f.crr <- crr.fit(fit = f.cph, cencode = 0, failcode = 1)
##'
##' # Estimate cumulative incidence rate by 6 year
##' QHScrnomo:::pred2.crr(prostate.crr, lp = 0.8, time = 60)
##'
`pred2.crr` <- function(f.crr, lp, time) {
    if (time > max(f.crr$uftime)) {
        stop("pick a smaller time!")
    }
    if (time < min(f.crr$uftime)) {
        stop("pick a greater time!")
    }
    lhat <- cumsum(exp(lp) * f.crr$bfitj)
    ci <- cbind(f.crr$uftime, 1. - exp(-lhat)) # cumulative incidence rate
    ci <- ci[ci[, 1.] <= time + 1e-10, ]
    ci <- ci[dim(ci)[1.], -1.]
    ci
}
