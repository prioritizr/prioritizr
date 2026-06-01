test_that("interactive", {
  expect_true(
    rlang::with_interactive(do_run_example(), TRUE)
  )
})

test_that("pkgdown", {
  expect_true(
    withr::with_envvar(
      list(IN_PKGDOWN  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
  expect_true(
    withr::with_envvar(
      list(PKGDOWN_BUILD  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
})

test_that("Github Actions", {
  expect_false(
    withr::with_envvar(
      list(CI  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
  expect_false(
    withr::with_envvar(
      list(GITHUB_ACTIONS  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
  expect_false(
    withr::with_envvar(
      list(GITHUB_SHA  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
})

test_that("CRAN", {
  expect_false(
    withr::with_envvar(
      list("_R_CHECK_LICENSE_"  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
  expect_false(
    withr::with_envvar(
      list("_R_CHECK_TIMINGS_"  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
})

test_that("R-Universe", {
  expect_false(
    withr::with_envvar(
      list("MY_UNIVERSE"  = "true"),
      rlang::with_interactive(do_run_example(), FALSE)
    )
  )
})
