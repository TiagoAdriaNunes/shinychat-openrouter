# testthat runs from tests/testthat; point box at the project root so `app/...` modules resolve
options(box.path = normalizePath(file.path("..", "..")))

# Keep test output readable
logger::log_threshold(logger::OFF)
