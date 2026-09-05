test_that("recreated lung dataset", {
  fit = survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  dataset = dataset_from_survfit(fit)
  expect_equal(nrow(dataset), 228)
  expect_equal(ncol(dataset), 3)
  expect_equal(names(dataset), c("time", "status", "strata"))
  expect_equal(max(dataset$time), max(survival::lung$time))
})


test_that("recreated aml dataset", {
  fit = survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)
  dataset = dataset_from_survfit(fit)
  expect_equal(nrow(dataset), 23)
  expect_equal(ncol(dataset), 3)
  expect_equal(names(dataset), c("time", "status", "strata"))
  expect_equal(max(dataset$time), max(survival::aml$time))
})

test_that("error if not a survfit object", {
  expect_error(dataset_from_survfit(list()), "Input must be a survfit object")
})
