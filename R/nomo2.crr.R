##' Calculate Estimated Cumulative Incidence Rate
##' Calculate predicted cumulative incidence rate based on a competing risks
##' regression model.
##'
##' This function is usually used to transform regular failure probabilities to
##' competing risks adjusted probabilities, when a nomogram of competing risks
##' regression model is constructed started from a regular survival model. It
##' is not often called externally.
##'
##' @title Estimate Cumulative Incidence Rate
##' @param x a vector of sum of linear predictors for each subject.
##' @param f.crr a saved model fitted by function
##'   \code{\link[QHScrnomo]{crr.fit}}
##' @param time expected evaluation time
##' @return a vector with each element being the predicted cumulative incidence
##'   rate at the expected time.
##' @note internal function
##' @author Michael W. Kattan, Ph.D. and Changhong Yu.\cr Department of
##'   Quantitative Health Sciences, Cleveland Clinic
##' @seealso \code{\link[QHScrnomo]{pred2.crr}}
##'   \code{\link[QHScrnomo]{crr.fit}} \code{\link[cmprsk]{crr}}
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
##' # Now construct the crr object based on the above cph object.
##' # 0s are censored observations.
##' # 1s are deaths from prostate cancer and
##' # 2s are deaths from other causes (competing risks).
##' f.crr <- crr.fit(fit = f.cph, cencode = 0, failcode = 1)
##'
##' # Estimate cumulative incidence rate by 6 year
##' pred.lp <- predict.cmprsk(f.crr,lps = TRUE) # calculate X beta values
##' QHScrnomo:::nomo2.crr(pred.lp, prostate.crr, time = 60)
##'
`nomo2.crr` <- function(x, f.crr, time) {
    # center <- f.cph[["center"]]
    # assign("center", center)
    assign("f.crr", f.crr)
    assign("time", time)
    # sapply(x, FUN = function(x)pred2.crr(f.crr, x + center, time))
    vapply(
        x,
        FUN = function(x) {
            pred2.crr(f.crr, x, time)
        },
        0.5
    )
}
