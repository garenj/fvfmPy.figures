setHook("rstudio.sessionInit", function(newSession) {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::navigateToFile("R/main.R")
  }
}, action = "append")
