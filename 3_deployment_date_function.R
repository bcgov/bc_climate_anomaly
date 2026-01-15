# Deployment date record  --------------------

deployment_history_file <- file("deployment_history.txt")

# record the time
deployment_date <- format(Sys.time(), '%d %B, %Y')
cat(paste0(deployment_date, "\n"),
    file = deployment_history_file,
    append = TRUE)
close(deployment_history_file)

#' #' Return the last recorded deployment date of the application.
#' load_deployment_date <-
#'   function(deployment_history_file = "deployment_history.txt") {
#'     deployment_history <- readLines(deployment_history_file)
#'
#'     # return the most recent line
#'     deployment_history[[length(deployment_history)]]
#'   }

