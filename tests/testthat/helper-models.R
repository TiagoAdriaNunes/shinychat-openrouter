# Minimal entries shaped like https://openrouter.ai/api/v1/models
fake_model <- function(id, name, prompt = "0", completion = "0", output = list("text")) {
  list(
    id = id,
    name = name,
    pricing = list(prompt = prompt, completion = completion),
    architecture = list(output_modalities = output)
  )
}

fake_catalogue <- list(data = list(
  fake_model("b/beta:free", "Beta (free)"),
  fake_model("openrouter/free", "Free Models Router"),
  fake_model("a/alpha:free", "Alpha (free)"),
  fake_model("p/paid", "Paid", prompt = "0.000001", completion = "0.000002"),
  fake_model("m/music", "Music (free)", output = list("text", "audio"))
))

fake_models <- c(
  "Free Models Router" = "openrouter/free",
  "Alpha (free)" = "a/alpha:free",
  "Beta (free)" = "b/beta:free"
)
