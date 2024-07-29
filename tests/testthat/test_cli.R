test_that("cli_pkg_theme", {
  expect_type(cli_pkg_theme(), "list")
})

test_that("cli_vtext", {
  expect_message(cli_vtext("asdf"), "asdf")
})

test_that("cli_warning", {
  expect_warning(cli_warning("asdf"), "asdf")
})

test_that("cli_deprecated", {
  w <- capture_warnings(cli_deprecated("asdf", "greg"))
  expect_length(w, 1)
  expect_match(w[[1]], "asdf")
  expect_match(w[[1]], "deprecated")
  expect_match(w[[1]], "greg")
})

test_that("cli_defunct", {
  expect_error(cli_defunct("asdf", "greg"), "asdf")
  expect_error(cli_defunct("asdf", "greg"), "defunct")
  expect_error(cli_defunct("asdf", "greg"), "greg")
})
