box::use(
  httr2[response, response_json],
  purrr[keep],
  stringr[str_ends, str_match],
)

# Minimal entries shaped like https://openrouter.ai/api/v1/models
fake_model <- function(
  id,
  name,
  prompt = "0",
  completion = "0",
  output = list("text"),
  intelligence = NULL,
  created = 0,
  tools = FALSE,
  reasoning = NULL
) {
  list(
    id = id,
    name = name,
    created = created,
    supported_parameters = if (tools) list("temperature", "tools") else list("temperature"),
    pricing = list(prompt = prompt, completion = completion),
    architecture = list(output_modalities = output),
    benchmarks = list(artificial_analysis = list(intelligence_index = intelligence)),
    reasoning = reasoning
  )
}

fake_catalogue <- list(
  data = list(
    fake_model("d/delta:free", "Delta (free)", created = 100),
    fake_model(
      "a/alpha:free",
      "Alpha (free)",
      intelligence = 10L,
      tools = TRUE,
      # Always reasons, so "none" isn't offered; no default effort given
      reasoning = list(mandatory = TRUE, supported_efforts = list("high", "none"))
    ),
    # The router is in excluded_models too (it can pick excluded models)
    fake_model("openrouter/free", "Free Models Router"),
    # Reasoning that can be turned on but has no effort levels
    fake_model("b/beta:free", "Beta (free)", intelligence = 30.5, reasoning = list(default_enabled = TRUE)),
    fake_model(
      "c/gamma:free",
      "Gamma (free)",
      created = 200,
      tools = TRUE,
      reasoning = list(mandatory = FALSE, supported_efforts = list("high", "medium", "low"), default_effort = "medium")
    ),
    fake_model("p/paid", "Paid", prompt = "0.000001", completion = "0.000002", intelligence = 99, tools = TRUE),
    fake_model("m/music", "Music (free)", output = list("text", "audio")),
    # Listed in excluded_models in config.yml
    fake_model("thinkingmachines/inkling:free", "Inkling (free)", intelligence = 50),
    # Listed in low_priority_models in config.yml
    fake_model("inclusionai/ling-3.1-flash", "Ling", intelligence = 90, tools = TRUE)
  )
)

# By intelligence (highest first), then unscored by newest, then low priority
fake_models <- c(
  "Beta (free)" = "b/beta:free",
  "Alpha (free)" = "a/alpha:free",
  "Gamma (free)" = "c/gamma:free",
  "Delta (free)" = "d/delta:free",
  "Ling" = "inclusionai/ling-3.1-flash"
)

# The fake_models that support tool calling, in the same order
fake_tool_models <- c(
  "Alpha (free)" = "a/alpha:free",
  "Gamma (free)" = "c/gamma:free",
  "Ling" = "inclusionai/ling-3.1-flash"
)

# reasoning_options() for fake_catalogue: the models with effort levels, in catalogue order
fake_efforts <- list(
  "a/alpha:free" = list(efforts = "high", default = "high"),
  "c/gamma:free" = list(efforts = c("high", "medium", "low"), default = "medium")
)

# Mock for httr2::local_mocked_responses(): the catalogue; for <models_url>/<id>/endpoints one
# provider with that model's `uptime` (named by model id, 100 when not given; NULL for no figure,
# NA for a failed lookup); and for the account's usable models (user_models_url) the catalogue
# without the ids in `unusable` (NA for a failed request)
fake_openrouter <- function(catalogue = fake_catalogue, uptime = list(), unusable = character()) {
  function(req) {
    if (str_ends(req$url, "/models/user")) {
      if (anyNA(unusable)) {
        return(response(status_code = 401))
      }
      return(response_json(body = list(data = keep(catalogue$data, \(m) !m$id %in% unusable))))
    }
    if (!str_ends(req$url, "/endpoints")) {
      return(response_json(body = catalogue))
    }
    id <- str_match(req$url, "/models/(.+)/endpoints$")[, 2]
    value <- if (id %in% names(uptime)) uptime[[id]] else 100
    if (isTRUE(is.na(value))) {
      return(response(status_code = 500))
    }
    response_json(body = list(data = list(endpoints = list(list(uptime_last_30m = value)))))
  }
}
