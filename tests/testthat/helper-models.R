# Minimal entries shaped like https://openrouter.ai/api/v1/models
fake_model <- function(id, name, prompt = "0", completion = "0", output = list("text"),
                       intelligence = NULL, created = 0, tools = FALSE) {
  list(
    id = id,
    name = name,
    created = created,
    supported_parameters = if (tools) list("temperature", "tools") else list("temperature"),
    pricing = list(prompt = prompt, completion = completion),
    architecture = list(output_modalities = output),
    benchmarks = list(artificial_analysis = list(intelligence_index = intelligence))
  )
}

fake_catalogue <- list(data = list(
  fake_model("d/delta:free", "Delta (free)", created = 100),
  fake_model("a/alpha:free", "Alpha (free)", intelligence = 10L, tools = TRUE),
  # The router is in excluded_models too (it can pick excluded models)
  fake_model("openrouter/free", "Free Models Router"),
  fake_model("b/beta:free", "Beta (free)", intelligence = 30.5),
  fake_model("c/gamma:free", "Gamma (free)", created = 200, tools = TRUE),
  fake_model("p/paid", "Paid", prompt = "0.000001", completion = "0.000002", intelligence = 99, tools = TRUE),
  fake_model("m/music", "Music (free)", output = list("text", "audio")),
  # Listed in excluded_models in config.yml
  fake_model("thinkingmachines/inkling:free", "Inkling (free)", intelligence = 50)
))

# By intelligence (highest first), then unscored by newest
fake_models <- c(
  "Beta (free)" = "b/beta:free",
  "Alpha (free)" = "a/alpha:free",
  "Gamma (free)" = "c/gamma:free",
  "Delta (free)" = "d/delta:free"
)

# The fake_models that support tool calling, in the same order
fake_tool_models <- c(
  "Alpha (free)" = "a/alpha:free",
  "Gamma (free)" = "c/gamma:free"
)
