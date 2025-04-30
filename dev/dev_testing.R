library(sf)
library(stars)


# ibm_cfg <- ModelConfig(
#   n_agents = 1000,
#   ref_sys = st_crs(4326),
#   aoc_bbx = c(0, 57, 2,58),
#   delta_x = 0.1,
#   delta_y = 0.1,
#   time_step = "1 day",
#   start_date = as.Date("2022-09-01"),
#   end_date = as.Date("2023-04-30")
# )

# ibm_cfg <- ModelConfig(
#   n_agents = 100L,
#   ref_sys = st_crs(27700),
#   aoc_bbx = c(0, 0, 10, 10),
#   delta_x = 0.1,
#   delta_y = 0.1,
#   time_step = "1 day",
#   start_date = as.Date("2022-09-01"),
#   end_date = as.Date("2023-04-30"),
#   start_sites =
# )
#
# ibm_cfg <- ModelConfig(
#   n_agents = 100L,
#   ref_sys = st_crs(27700),
#   aoc_bbx = c(0, 0, 10, 10),
#   delta_x = 0.1,
#   delta_y = 0.1,
#   time_step = "1 day",
#   start_date = as.Date("2022-09-01"),
#   end_date = as.Date("2023-04-30")
# )

drv_owfs











swim_cost <- function(sst){
  113-(2.75*sst)
}

swim_cost(2)


dive_cost <- function(){
  "Haha"
}

dive_cost()
class(dive_cost)


final_fn <- new_function(
  exprs(x = , y = NULL),
  expr({
    if(is.null(y)) y <- 0
    z <- x + y
    q <- x*2 - z
    t <- dplyr::as_tibble(mean(q))
    base_fn(t)
  })
)

final_fn(1, 2)





cost_fct_builder <- function(base_fct, drivers){

  if(!is.function(base_fct)) cli::cli_abort("Argument {.arg base_fct} must be a function.")

  #browser()

  base_fct_argnames <- rlang::fn_fmls_names(base_fct)
  base_fct_argnames <- base_fct_argnames %||% ""

  driver_ids <- lapply(drivers, \(x) x@id)

  # any(base_fct_argnames %in% driver_ids)
  # grepl("b|biom", base_fct_argnames)
  # grepl("[state|activity|behaviour]_[time|duration]", base_fct_argnames)


  # args of output functions is fixed. What varies is the body of the output
  # function based on characteristics of the input function
  fct_args <- exprs(drivers = NULL, agent_condition = NULL)

  if(any(base_fct_argnames %in% driver_ids)){

  }


  if("sst" %in% base_fct_argnames){
    new_function(
      args = fct_args,
      #exprs(x = , y = NULL),
      expr({
        if(is.null(y)) y <- 0
        z <- x + y
        q <- x*2 - z
        t <- dplyr::as_tibble(mean(q))
        base_fct(t)
      }),
      env = caller_env()
    )
  }else{
    #fct_body <- base_fct
    base_fct
  }

  #rlang::new_function(fct_args, fct_body)
}





final_fn <- cost_fct_builder(swim_cost, list(Driver())) ; final_fn
final_fn <- cost_fct_builder(dive_cost) ; final_fn

final_fn(12, 1)















setClass(
  "MockState",
  contains = "VarDist",
  slots = list(
    id = "character",
    cost_fn = "function"
  )
)

x_cost_fn_user <- function(sst){
  113-(2.75*sst)
}
new("MockState", cost_fn = state_x_cost)



rmr_initiate(
  config = ibm_config_rover, #ibm_cfg,
  species = rover,
  drivers = rover_drivers
)


list(
  Driver(id = "owfs", sf_obj = owf_foots, type = "impact"),
  Driver(id = "sst", stars_obj = sst_month, type = "habitat")
)


var <- "foo"
rlang::sym(var)
call("mean", rlang::sym(var), na.rm = TRUE)








?rlang::new_function()




tst <- new("MockCls", cost_fn = swimming_cost)

tst@cost_fn(24)


rlang::fn_fmls(tst@cost_fn)
rlang::fn_fmls_names(tst@cost_fn)

rover_drivers[rlang::fn_fmls_names(tst@cost_fn)]










test_fn <- rlang::new_function(
  rlang::fn_fmls_syms(tst@cost_fn),
  rlang::fn_body(tst@cost_fn)
)


rlang::fn_fmls_syms(tst@cost_fn)
rlang::fn_fmls_names(tst@cost_fn)





x <- rover_drivers[[driver_sst_idx]]@stars_obj
a <- Agent(species = rover, ibm_config_rover)


cost_fn <- function(sst_raster, loc, month){
  sst <- stars::st_extract(dplyr::filter(sst_raster, months == month), at = loc) |> as.numeric()
  tst@cost_fn(sst)
}


cost_fn(x, st_coordinates(location(a) + 1), "December")

sf::st_coordinates(location(a) + 1)




# if dependent on driver, and driver defined by raster, f(raster, where, when, iter)



fn_builder <- function(fn, drivers){
  if(!is.function(fn)) cli::cli_abort("Argument {.arg fn} must be a function.")

  fn_arg_nms <- rlang::fn_fmls_names(fn)
  fn_arg_syms <- rlang::fn_fmls_syms(fn)


  expr({
    dplyr::filter(sst_raster, months == month)
    x <- stars::st_extract(q)
    !!fn
  })


  browser()

  rlang::new_function(
    fn_arg_syms,
    rlang::expr({
      !!fn
    })
  )

  expr(!!fn)(2)

}






base_fn <- function(sst) 113-(2.75*sst)

fn_builder(base_fn, rover_drivers)






x <- expr(-1)
expr(f(!!x, y))

library(rlang)

new_function(pairlist2(x = 1, y = 3 * 6), quote(x * y))
new_function(exprs(x = 1, y = 3 * 6), quote(x * y))




drivers_id <- lapply(rover_drivers, \(x) x@id)
driver_sst_idx <- which(drivers_id == rlang::fn_fmls_names(tst@cost_fn))






location(a) |> st_coordinates()

a@condition@timestamp


x[, , , "December"]
st_extract(x, at = st_coordinates(location(a) + 1))  # TODO: how to handle returning NAs


class(units(x[[1]]))




y <- st_extract(x[, , , "December"], at = st_coordinates(location(a) + 1)) |> as.numeric()
#y <- units::set_units(y, units(x[[1]]), mode = "standard")





st_extract(x, at = st_sf(month = "December", geometry = st_sfc(location(a) + 1), crs = st_crs(x)), time_column = "months")



class(y$sst_mean_moy)



swimming_cost_a <- function(sst){
  sst <- units::set_units(sst, "Celsius")
  113-(2.75*units::drop_units(sst))
}

swimming_cost_b <- function(sst){
  113-(2.75*sst)
}

bench::mark(
  swimming_cost_a(10),
  swimming_cost_b(10),
  iterations = 5000
)



stars::st_set_dimensions()

swimming_cost(12)









energy_cost_factory <- function(f, drivers){

  if(!is.function(f)) cli::cli_abort("Argument {.arg f} must be a function.")

  arg_names <- rlang::fn_fmls_names(f)

}




x |> filter(months == "January") |> plot(axes = TRUE)
plot(location(a), add = TRUE, col = "red", pch = 19)

x |> filter(months == "January") |> plot(axes = TRUE, reset = FALSE)
plot(location(a), add = TRUE, col = "red", pch = 19)





tst@cost_fn(12)
lobstr::ast(tst@cost_fn)
is.function(tst@cost_fn)
is.call(rlang::expr(tst@cost_fn(1)))








