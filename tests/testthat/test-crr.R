test_that("crr.fit works", {
  data(prostate.dat)
  dd <- datadist(prostate.dat)
  options(datadist = "dd")
  prostate.f <- cph(Surv(TIME_EVENT,EVENT_DOD == 1) ~ TX  + rcs(PSA,3), 
                    data = prostate.dat, x = TRUE, y= TRUE, surv=TRUE,time.inc = 144)
  expect_equal(attr(prostate.f, "class"), c("cph", "rms", "coxph"))
})
