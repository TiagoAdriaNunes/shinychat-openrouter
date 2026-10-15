box::use(
  logger[log_threshold, OFF],
)

# testthat runs from tests/testthat; point box at the project root so `app/...` modules resolve
options(box.path = normalizePath(file.path("..", "..")))

# Keep test output readable
log_threshold(OFF)
