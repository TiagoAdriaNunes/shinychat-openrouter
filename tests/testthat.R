# Run from the project root: Rscript tests/testthat.R
box::use(
  testthat[test_dir],
)

test_dir("tests/testthat", stop_on_failure = TRUE)
