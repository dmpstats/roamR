
#' Top-line function to run the IBM
#'
#' Prototyping Version

rmr_run <- function(ibm,
                    .parallel_plan = future::sequential()
                    ){

  step_period <- lubridate::period(ibm@model_config@time_step)

  ## Simulate agents, individually over time
  future::plan(.parallel_plan)

  furrr::future_map(
    agents,
    ~simulate_agent(),
    .options = furrr::furrr_options(seed = TRUE)
  )



  ## Collect and summarise results


  ## Output results

}
