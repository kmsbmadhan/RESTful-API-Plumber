library(plumber)

serve_model <- plumb("claim_api.R")
serve_model$run(port = 8000)
