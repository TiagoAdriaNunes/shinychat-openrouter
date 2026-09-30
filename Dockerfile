FROM rocker/r-ver:4.6.1

# System libraries needed by the R packages at build/run time
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Restore packages first so this layer is cached until renv.lock changes.
# Linux binaries come from Posit Package Manager via renv.
COPY .Rprofile renv.lock ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
RUN R -e "renv::restore(prompt = FALSE)"

COPY app.R ./
COPY app ./app

# OPENROUTER_API_KEY is provided at runtime (fly secrets set), never baked into the image
ENV LOG_LEVEL=INFO
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8080)"]
