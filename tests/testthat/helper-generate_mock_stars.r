# test helper function to generate mock stars objects for testing
# currently always generates regular grid, for one single attribute
generate_mock_stars <- function(n_xy = 5,
                                attr_name = 'z',
                                attr_units = NULL,
                                time_name = NULL,
                                time_type = c("posixt", "date", "month", "numeric"),
                                time_length = 10,
                                iter_name = NULL,
                                crs = 4326,
                                start_date = "2020-07-31"){

  time_type <- rlang::arg_match(time_type)

  n_iter <- 15
  n_vals <- n_xy*n_xy
  dim_sizes <- c(n_xy, n_xy)
  dim_names <- c("x", "y")

  if(!is.null(time_name)){
    if(!is.character(time_name))  stop("`time_name` must be a character.")
    n_vals <- n_vals * time_length
    dim_sizes <- c(dim_sizes, time_length)
    dim_names <- c(dim_names, time_name)
  }

  if(!is.null(iter_name)){
    if(!is.character(iter_name)) stop("`iter_name` must be a character.")
    n_vals <- n_vals * n_iter
    dim_sizes <- c(dim_sizes, n_iter)
    dim_names <- c(dim_names, iter_name)
  }


  x <- array(
    rlnorm(n_vals),
    dim = dim_sizes
  ) |>
    stars::st_as_stars() |>
    setNames(attr_name) |>
    stars::st_set_dimensions(names = dim_names) |>
    sf::st_set_crs(crs)

  # set attribute units
  x[[1]] <- units::set_units(x[[1]], attr_units, mode = "standard")

  if(!is.null(time_name)){
    t <- switch(
      time_type,
      posixt = seq(as.POSIXct(start_date, tz = "UTC") + lubridate::minutes(550), by = "day", length.out = time_length),
      month = month.abb[1:time_length],
      date = seq(as.Date(start_date), by = "week", length.out = time_length),
      numeric = 1:time_length
    )

    x <- stars::st_set_dimensions(x, which = time_name, values = t)
  }

  x
}
