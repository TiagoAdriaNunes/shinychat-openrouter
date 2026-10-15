# commons' run_r worker: a callr session started in this folder with a scrubbed environment and
# HOME set to its own commons-worker-* directory. Skip renv there: activating it takes ~4 s, over
# callr's fixed 3 s startup limit ("Could not start R session, timed out"), and callr passes the
# library paths anyway. Also drop what R read from this folder's .Renviron (the API key), which
# commons keeps out of the session on purpose.
if (grepl("commons-worker-", Sys.getenv("HOME"), fixed = TRUE)) {
  if (file.exists(".Renviron")) {
    Sys.unsetenv(sub("=.*", "", grep("=", readLines(".Renviron", warn = FALSE), value = TRUE)))
  }
} else {
  source("renv/activate.R")
}
