##' Calculate concordance index
##'
##' to calculate the discrimination metric, concordance index for binary,
##' time-to event and competing risks outcomes
##'
##' @title Concordance index calculation
##' @param prob predicted risk of failure event, either probability or
##' risk score
##' @param fstatus failure(event) variable
##' @param ftime follow-up time variable for survival or competing risks
##' predictions
##' @param type type of regression models corresponding to different type
##' of outcomes.
##' 'logis' is the default value for binary outcome, 'surv' for ordinary
##' survival outcome
##' and 'crr' for competing risks outcome.
##' @param failcode coding for failure(event). 1 is the default value.
##' @param cencode coding for censoring. 0 is the defaul
##' @param tol error tolerance. the default value is 1e-20.
##' @return a vector of returned values.
##' \item{N}{the total number of observations in the input data}
##' \item{n}{the nonmissing number of observations that was used f
##' or calculation}
##' \item{usable}{the total number of usable pairs.}
##' \item{oncordant}{the number of concordant pairs}
##' \item{cindex}{the concordance index that equal to the number of
##' concordant pairs
##' divided by the total number of usable pairs.}
##' @author Changhong Yu, Michael Kattan, Brian Wells, Amy Nowacki.
##' @export
##' @examples
##' seed <- 5555
##'
##' # simulated data to test the function
##' n <- 20000
##' ftime <- rexp(n)
##' fstatus <- sample(0:2,n,replace=TRUE)
##' cov <- matrix(runif(3*n),nrow=n)
##' dimnames(cov)[[2]] <- c('x1','x2','x3')
##' dat <- data.frame(ftime,fstatus,cov)
##' require(Design)
##' print(z <- cph(Surv(ftime,fstatus == 1) ~ x1 + x2 + x3,data = dat))
##'
##' # summary(z)
##' z.p <- predict(z)
##'
##' #debug(cindex)
##' cindex(prob=z.p,fstatus=fstatus,type="logis")
##' cindex(prob=z.p,fstatus=fstatus,ftime=ftime,type="surv")
##' cindex(prob=z.p,fstatus=fstatus,ftime=ftime,type="crr")
##'
##' @keywords semiparametric regression
##' @useDynLib QHScrnomo cindexCrr cindexLog cindexSurv
##'

cindex <-
    function(
        prob, fstatus, ftime, type = "crr", failcode = 1, 
        cencode = 0, tol = 1e-20) {
        type <-
            match.arg(
                type, c("logistic", "survival", "crr"),
                several.ok = FALSE
            )
        if (
            all(regexpr(toupper(type), toupper(c(
                "logistic", "survival", "crr"
            ))) == -1)) {
            stop("type should be one of 'logistic','survival' or 'crr' !!!")
        }
        
        if (!is.na(pmatch("LOG", toupper(type)))) {
            if ((N <- length(prob)) != length(fstatus)) {
                stop(
                    "event variable has different length",
                    "from the predicted risk variable!"
                )
            }
            isna <- is.na(prob) + is.na(fstatus)
            n <- sum(isna == 0)
            prob <- prob[isna == 0]
            fstatus <- fstatus[isna == 0]
            fstatus <- ifelse(fstatus %in% failcode, 1, 0)
            
            out <-
                .C(
                    "cindexLog",
                    prob = as.double(prob),
                    fstatus = as.integer(fstatus),
                    n = as.integer(n),
                    npair = integer(2),
                    cindex = double(1),
                    PACKAGE = "QHScrnomo"
                )
        } else {
            if (!is.na(pmatch("SURV", toupper(type)))) {
                if (((N <- length(prob)) != length(fstatus)) |
                    (length(fstatus) != length(ftime))) {
                    stop(
                        "event variable has different length from",
                        "the predicted risk variable!"
                    )
                }
                isna <- is.na(prob) + is.na(fstatus) + is.na(ftime)
                n <- sum(isna == 0)
                prob <- prob[isna == 0]
                fstatus <- fstatus[isna == 0]
                ftime <- ftime[isna == 0]
                fstatus <- ifelse(fstatus %in% failcode, 1, 0)
                # sort the follow-up time in ascending order
                ftorder <- order(ftime)
                prob <- prob[ftorder]
                fstatus <- fstatus[ftorder]
                ftime <- ftime[ftorder]
                out <-
                    .C(
                        "cindexSurv",
                        prob = as.double(prob),
                        fstatus = as.integer(fstatus),
                        ftime = as.double(ftime),
                        n = as.integer(n),
                        npair = integer(2),
                        cindex = double(1),
                        PACKAGE = "QHScrnomo"
                    )[4:6]
            } else {
                if (((N <- length(prob)) != length(fstatus)) |
                    (length(fstatus) != length(ftime))) {
                    stop(
                        "event variable has different length from",
                        "the predicted risk variable!"
                    )
                }
                isna <- is.na(prob) + is.na(fstatus) + is.na(ftime)
                n <- sum(isna == 0)
                prob <- prob[isna == 0]
                fstatus <- fstatus[isna == 0]
                ftime <- ftime[isna == 0]
                fstatus <- ifelse(
                    fstatus %in% failcode, 1,
                    ifelse(fstatus %in% cencode, 0, 2)
                )
                # sort the follow-up time in ascending order
                ftorder <- order(ftime)
                prob <- prob[ftorder]
                fstatus <- fstatus[ftorder]
                ftime <- ftime[ftorder]
                out <-
                    .C(
                        "cindexCrr",
                        prob = as.double(prob),
                        fstatus = as.integer(fstatus),
                        ftime = as.double(ftime),
                        n = as.integer(n),
                        npair = integer(2),
                        cindex = double(1),
                        PACKAGE = "QHScrnomo"
                    )
            }
        }
        # browser()
        out <-
            c(
                N = N,
                n = n,
                usable = out$npair[1],
                concordant = out$npair[2],
                cindex = out$cindex
            )
        return(out)
    }
