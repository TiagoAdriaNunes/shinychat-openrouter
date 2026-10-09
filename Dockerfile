FROM rocker/r-ver:4.6.1

# System libraries needed by the R packages at build/run time (fonts and images: ragg, used by commons)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libwebp-dev \
    libuv1-dev \
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

COPY app.R config.yml ./
COPY app ./app

# Bake the World Bank data into the image (cache/wdi.rds) so a cold machine doesn't download it
# at startup; the app refreshes it once it is older than world_bank.cache_days in config.yml
RUN R -e "options(box.path = '/app'); box::use(app/logic/wdi[load_wdi]); invisible(load_wdi())"

# OPENROUTER_API_KEY is provided at runtime (fly secrets set), never baked into the image
ENV LOG_LEVEL=INFO
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8080)"]
