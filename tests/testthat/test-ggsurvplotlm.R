test_that("error if not a survfit object", {
  expect_error(ggsurvplotlm(list()), "Input must be a survfit object.")
})

test_that("landmark plot of lung dataset", {
  fit = survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  result = ggsurvplotlm(fit, landmark_time=1, landmark_label="Landmark Time", data=survival::lung)
  expect_equal(class(result), c("ggsurvplot" ,"ggsurv"  ,   "list"))
})
