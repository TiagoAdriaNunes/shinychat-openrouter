box::use(
  cli[cli_abort],
  rlang[caller_env, is_string],
)

#' Abort early with an actionable message if the OpenRouter API key is missing.
#' @export
check_api_key <- function(env_var = "OPENROUTER_API_KEY", call = caller_env()) {
  if (!nzchar(Sys.getenv(env_var))) {
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
  if (!is_string(model) || !nzchar(model)) {
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
