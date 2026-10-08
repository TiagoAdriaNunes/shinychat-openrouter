box::use(
  checkmate[test_string],
  cli[cli_abort],
  rlang[caller_env],
)

#' Abort early with an actionable message if the OpenRouter API key is missing.
#' @export
check_api_key <- function(env_var = "OPENROUTER_API_KEY", call = caller_env()) {
  if (!test_string(Sys.getenv(env_var), min.chars = 1)) {
    cli_abort(
      c(
        "Environment variable {.envvar {env_var}} is not set.",
        "i" = "Create a key at {.url https://openrouter.ai/keys}.",
        "i" = "Add {.code {env_var}=<your key>} to {.file ~/.Renviron} and restart R."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

#' Abort if the model id is not a single non-empty string.
#' @export
check_model <- function(model, call = caller_env()) {
  if (!test_string(model, min.chars = 1)) {
    cli_abort(
      c(
        "{.arg model} must be a single non-empty string, not {.obj_type_friendly {model}}.",
        "i" = "Set it in {.file app/logic/config.R}, e.g. {.val openrouter/free}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}
