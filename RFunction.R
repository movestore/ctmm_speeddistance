library(ctmm)
library(purrr)
library(dplyr)

rFunction <- function(data) {
  # Ensure all telemetry columns are valid. When ctmm subsets the data it
  # renames invalid names, causing the `rbind()` inside `speed()` to fail
  # with "names do not match previous names"
  telemetry <- map(
    data[[2]],
    function(x) {
      names(x) <- make.names(names(x), unique = TRUE)
      x
    }
  )

  # Time since `since`, for the log, e.g. "3.4 secs" or "2.1 mins"
  elapsed <- function(since) format(round(Sys.time() - since, 1))
  start <- Sys.time()
  n <- length(telemetry)

  logger.info(paste0(
    "Estimating speed and distance for ", n, " animal(s). This can take a ",
    "while for many animals or long tracks."
  ))

  # Estimate the animals one after the other and log the progress of each
  s1 <- map(
    seq_len(n),
    function(i) {
      id <- names(data[[1]])[i]
      
      logger.info(paste0("Estimating speed for animal ", i, " of ", n, " (", id, ")"))
      animal_start <- Sys.time()
      
      est <- speed(data[[1]][[i]], telemetry[[i]], units = FALSE, trace = FALSE)
      
      logger.info(paste0(id, ": done after ", elapsed(animal_start)))
      est
    }
  )
  names(s1) <- names(data[[1]])
  logger.info(paste0("All animals estimated after ", elapsed(start)))

  # ctmm returns DOF = 0 when it can't estimate speed, usually because the
  # model has no velocity component. Those animals get NA in both files.
  no_estimate <- map_lgl(s1, ~ .x$DOF == 0)

  walk(
    names(s1)[no_estimate],
    function(id) {
      logger.warn(paste0(
        id,
        ": speed and distance could not be estimated. ",
        "See 'Errors and null handling' in the App documentation."
      ))
    }
  )

  xx <- map(s1, ~ .x$CI)
  xx <- do.call(rbind, xx)
  xx |> as_tibble() |>
    mutate(id = names(s1), parameter = "speed (meters/second)") |>
    mutate(across(c(low, est, high), ~ if_else(no_estimate, NA_real_, .x))) |>
    select(id, parameter, low, est, high) |>
    write.csv(appArtifactPath("speed.csv"), row.names = FALSE)

  duration <- map_dbl(data[[2]], ~ diff(range(.x$t)))

  xx <- map(s1, ~ .x$CI)
  xx <- do.call(rbind, xx)
  xx |> as_tibble() |>
    mutate(id = names(s1)) |>
    mutate(low = low * duration, est = est * duration, high = high * duration, parameter = "distance (meters)") |>
    mutate(across(c(low, est, high), ~ if_else(no_estimate, NA_real_, .x))) |>
    select(id, parameter, low, est, high) |>
    write.csv(appArtifactPath("distance.csv"), row.names = FALSE)

  return(data)
}
