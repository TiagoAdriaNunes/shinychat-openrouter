# shinychat-openrouter

A small chatbot built with R Shiny, [shinychat](https://posit-dev.github.io/shinychat/r/) and [ellmer](https://ellmer.tidyverse.org/), talking to a free model on [OpenRouter](https://openrouter.ai/).

## Features

- Streaming chat UI with a stop button, greeting and light/dark mode (Bootstrap 5 via `bslib`).
- Free model out of the box: `stealth/space-bunny-alpha` (swap for `openrouter/free` or any other model in one line).
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

## Run

```r
shiny::runApp()
```

The app checks the configuration at startup and stops with a clear message if `OPENROUTER_API_KEY` is missing or the model setting is invalid.

## Configuration

| What | Where |
| --- | --- |
| Model, system prompt, greeting, disclaimer, title | `app/logic/config.R` |
| Colors and chat styling | `app/logic/theme.R` |
| Log verbosity | `LOG_LEVEL` environment variable (`DEBUG`, `INFO`, `WARN`, `ERROR`; default `INFO`) |

Message content is never logged.

## Project structure

```text
app.R                  Entry point: sets box.path, runs startup checks, starts the app
app/
  main.R               Page UI and top-level server
  modules/chat.R       Chat module (page_chat + chat_openrouter + chat_server)
  logic/
    config.R           Model, system prompt, greeting, title
    theme.R            page_chat_theme() (Bootstrap 5)
    checks.R           Startup validation (cli::cli_abort)
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
- The first build takes a few minutes while the R packages are installed.

## Notes

- **Personal demo project.** Please don't share sensitive or personal information in the chat. The app shows the model in use and this warning in a footer under the input (`footer` in `app/modules/chat.R`, text in `app/logic/config.R`).
- **No authentication.** Anyone with the URL can use your OpenRouter key and its rate limits. Add authentication or keep the deployment private.
- **Free models.** They can be rate limited and may change or disappear. `stealth/*` models are run by an anonymous third party who may retain prompts and completions, so don't send sensitive data.
- **No history.** Conversations live in memory only and are lost when the machine stops or restarts.
