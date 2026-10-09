# shinychat-openrouter

A small chatbot built with R Shiny, [shinychat](https://posit-dev.github.io/shinychat/r/) and [ellmer](https://ellmer.tidyverse.org/), talking to a free model on [OpenRouter](https://openrouter.ai/).

## Features

- Streaming chat UI with a stop button, greeting and light/dark mode (Bootstrap 5 via `bslib`).
- Model picker below the chat listing OpenRouter's [free models](https://openrouter.ai/collections/free-models), fetched live (cached for an hour) and ordered by Artificial Analysis intelligence index, most intelligent first. The chat starts with the top model; switching keeps the conversation. The `openrouter/free` router is used only if the list can't be fetched (it picks any free model at random, including non-chat ones).
- **World Bank data mode**: a switch in the footer turns the chat into a [commons](https://posit-dev.github.io/commons/r/) data agent that answers from World Development Indicators (GDP, growth, inflation, unemployment, population, trade, debt, Gini; every country and aggregate since 1990) downloaded with [`WDI`](https://vincentarelbundock.github.io/WDI/). The agent uses trusted measures (latest values, time series, rankings, compound growth) where it can, otherwise writes SQL or R, and marks each answer as trusted, cited or untrusted. Only free models that support tool calling are listed in this mode, and switching mode starts a new conversation. Answers are only shown if the model actually looked the data up: an answer from memory is dropped and the question asked again, and if the model still doesn't look anything up, the app says so and suggests another model instead of showing made-up figures.
- Modular code using [`box`](https://klmr.me/box/) for imports (no `library()` calls).
- Reproducible environment with `renv`.
- Logging with `logger` and clear startup errors with `cli`/`rlang`.
- Ready to deploy on [Fly.io](https://fly.io/) with Docker.

## Requirements

- R 4.6.1 (the version recorded in `renv.lock`)
- An OpenRouter API key: <https://openrouter.ai/keys>

## Setup

1. Restore the packages:

   ```r
   renv::restore()
   ```

2. Add your API key to a `.Renviron` file (in the project root or in `~`) and restart R:

   ```ini
   OPENROUTER_API_KEY=<your key>
   ```

   `.Renviron` is git-ignored. Never commit your key.

3. **Windows only**, to use World Bank data mode locally: add `R_CONFIG_ACTIVE=development` to the same `.Renviron`. commons runs the agent's R code in an OS sandbox that exists only on Linux and macOS; the `development` section of `config.yml` lets it fall back to best-effort guardrails, which are not a security boundary. Never set it on a deployed app.

## Run

```r
shiny::runApp()
```

The app checks the configuration at startup and stops with a clear message if `OPENROUTER_API_KEY` is missing or the model setting is invalid. It also loads the World Bank data: the first run downloads it (about 30 seconds) to `cache/wdi.rds`, which is reused for a week.

## Test

```sh
Rscript tests/testthat.R
```

Tests use `testthat` and mock every OpenRouter request with `httr2::local_mocked_responses()` and the World Bank data with a small fake panel, so they run offline and need no API key. GitHub Actions runs them on every push to `main` and on pull requests (`.github/workflows/tests.yml`).

## Configuration

All app settings live in [`config.yml`](config.yml), read with the [`config`](https://rstudio.github.io/config/) package:

| What | Section in `config.yml` |
| --- | --- |
| Title, greeting, input placeholder, disclaimer | `app` |
| System prompt | `chat` |
| Default model, model list URL, cache duration, request timeout, models hidden from the dropdown | `openrouter` |
| World Bank data mode: indicators, start year, data cache, agent instructions, messages | `world_bank` |
| Messages shown when a model is rate limited, unavailable or fails | `errors` |
| Colors and chat styling (any bslib theme variable) | `theme` |

To override values per environment, add a section (e.g. `production:`) with only the keys that differ and set `R_CONFIG_ACTIVE=production`.

Environment variables:

| Variable | Purpose |
| --- | --- |
| `OPENROUTER_API_KEY` | OpenRouter API key (required) |
| `LOG_LEVEL` | Log verbosity: `DEBUG`, `INFO`, `WARN`, `ERROR` (default `INFO`) |
| `R_CONFIG_ACTIVE` | Which `config.yml` section to use (default `default`) |

Message content is never logged.

## Project structure

```text
app.R                  Entry point: sets box.path, runs startup checks, starts the app
config.yml             All app settings (text, model, caching, theme)
app/
  main.R               Page UI and top-level server
  modules/chat.R       Chat module (page_chat + chat_openrouter + chat_server)
  logic/
    config.R           Reads config.yml and exports the settings
    models.R           Fetches and caches the free model list (optionally only tool-calling models)
    client.R           OpenRouter client that shows a friendly message when a model fails
    wdi.R              Downloads and caches the World Bank data (WDI package)
    agent.R            commons agent for World Bank data mode: data source, dictionary, lookup check
  measures/world-bank.R  Trusted calculations for the agent (commons @measure functions)
  context/world-bank.md  Notes on reading the indicators (commons context layer)
    theme.R            commons_theme() (page_chat_theme() + commons assets) from the config.yml theme section
    checks.R           Startup validation (cli::cli_abort)
tests/                 testthat suite (run with Rscript tests/testthat.R)
.github/workflows/     CI: runs the tests on every push and pull request
renv.lock              Locked package versions
Dockerfile             Container image for deployment
fly.toml               Fly.io configuration
```

## Deploy to Fly.io

Requires [`flyctl`](https://fly.io/docs/flyctl/install/) and a Fly.io account.

```sh
flyctl auth login
flyctl apps create <your-app-name>   # then set `app = "<your-app-name>"` in fly.toml
flyctl secrets set OPENROUTER_API_KEY=<your key>
flyctl deploy --ha=false
```

- `--ha=false` keeps a single machine, which Shiny needs because each session is tied to one machine.
- `fly.toml` uses a `shared-cpu-1x` machine with 1 GB of RAM in `gru` (São Paulo) that stops when idle and starts again on the next visit. Change `primary_region`, the `[[vm]]` block or `min_machines_running` to suit you.
- The first build takes a few minutes while the R packages are installed. The build also downloads the World Bank data into the image, so a cold machine doesn't wait for it.
- World Bank data mode needs the commons OS sandbox (seccomp plus Landlock or user namespaces), which a modern Linux kernel such as Fly.io's provides. If it isn't available, the mode shows "unavailable" and the log says why.

## Notes

- **Personal demo project.** Please don't share sensitive or personal information in the chat. The app shows the model in use and this warning in a footer under the input (`footer` in `app/modules/chat.R`, text in `app/logic/config.R`).
- **No authentication.** Anyone with the URL can use your OpenRouter key and its rate limits. Add authentication or keep the deployment private.
- **Free models.** They can be rate limited and may change or disappear. `stealth/*` models are run by an anonymous third party who may retain prompts and completions, so don't send sensitive data.
- **No history.** Conversations live in memory only and are lost when the machine stops or restarts.
