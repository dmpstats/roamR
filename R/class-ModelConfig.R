#' Class `<ModelConfig>`
#'
#' `<ModelConfig>` is an S4 class containing the user-defined configuration for
#' the Individual-based Model (IBM). It specifies key parameters such as the
#' number of agents, the spatio-temporal resolution of the model, the bounding
#' box (extent) of the area of calculation (AOC), the simulation start and end
#' date.
#'
#'
#' @slot movement_type Character string, specifying the movement model to simulate
#'   agent trajectories. Currently supported options:
#'    * `"di"`: Density-informed movement, where agent destinations stochastically
#'    generated based on population-level density maps.
#'    * `"crw"`: Correlated Random Walk model, where movement directions
#'    follow probability distributions that can be conditioned by habitat,
#'    environmental and/or physiological factors.
#' @slot n_agents Integer, the number of agents to track within the simulation.
#' @slot ref_sys Object of class <`crs`>, defining the Coordinate Reference System to
#'   be applied to the IBM. Must be specified via [sf::st_crs()].
#' @slot aoc_bbx Object of class <`bbox`>, specifying the area of calculation (AOC),
#' i.e. the spatial bounding box within which simulation occurs.
#' @slot delta_x,delta_y Numeric, specifying the cell (pixel) size in the x and
#'   y dimensions, respectively. These define the spatial resolution of the
#'   AOC. Required only when `movement_type = "crw"`; otherwise values
#'   are automatically derived from the resolution of provided
#'   density maps. Units are assumed to match those of `ref_sys`.
#' @slot delta_time Character string, defines the temporal resolution of the
#'   model. Valid options include "hours", "day", "week", "month" or "year", and can
#'   be preceded by a positive integer and a space, and followed by "s".
#' @slot start_date;end_date Date, respectively, defines the start and end
#'   dates for the simulation period.
#' @slot start_sites An `<sf>` object, defining the sites where agents start the
#'   simulation. Apart from the sites' geometry, this object must contain the
#'   following columns:
#'    * `id`: a unique identifier for each site.
#'    * `prop`: the proportion of `n_agents` allocated at each site. The values
#'     in this column must sum to 1.
#'
#'    If `start_sites` are not provided, agents start at random locations
#'    within the AOC.
#'
#' @slot end_sites An `<sf>` object, analogous to `start_sites`, but specifying
#'   the sites to which agents must return to at the end of the simulation. If
#'   `NULL` (the default), end locations are not forced upon agent. **Note:**
#'   This parameter is currently inactive and will be ignored.
#'
#'
#'
#' @seealso
#' [ModelConfig()] for creating `<ModelConfig>` objects and further
#' details on the specification of input values.
#'
#' @export

methods::setClass(
  Class = "ModelConfig",
  slots = list(
    movement_type = "character",
    n_agents = "integer",
    ref_sys = "crs",
    aoc_bbx = "bbox",
    delta_x = "numeric",
    delta_y = "numeric",
    delta_time = "character",
    #delta_time = "period",
    start_date = "Date",
    end_date = "Date",
    start_sites = "sf",
    end_sites = "sf"
  ),
  prototype = list(
    movement_type = NA_character_,
    n_agents = NA_integer_,
    ref_sys = sf::NA_crs_,
    aoc_bbx = sf::NA_bbox_,
    delta_x = NA_real_,
    delta_y = NA_real_,
    delta_time = NA_character_,
    #delta_time = lubridate::period(),
    start_date = as.Date(NA),
    end_date = as.Date(NA),
    start_sites = sf::st_sf(sf::st_sfc()),
    end_sites = sf::st_sf(sf::st_sfc())
  )
)



#' Create a `<ModelConfig>` object
#'
#' Helper function to define the model configuration of the IMB. It constructs
#' instances of <[`ModelConfig-class`]> objects.
#'
#' @param movement_type Character string, specifying the movement model to simulate
#'   agent trajectories. Currently supported options:
#'    * `"di"`: Density-informed movement, where agent destinations stochastically
#'    generated based on population-level density maps.
#'    * `"crw"`: Correlated Random Walk model, where movement directions
#'    follow probability distributions that can be conditioned by habitat,
#'    environmental and/or physiological factors.
#' @param n_agents Integer, the number of agents to track within the simulation.
#' @param ref_sys Object of class <`crs`>, defining the Coordinate Reference
#'   System to be applied to the IBM. Must be specified via [sf::st_crs()].
#' @param aoc_bbx Numeric vector or object of class <`bbox`>, specifying the
#'   area of calculation (AOC), i.e. the spatial bounding box within which simulation
#'   occurs. If numeric, expects a 4-length vector specifying `xmin`, `ymin`,
#'   `xmax` and `ymax` values.
#' @param delta_x,delta_y Numeric values, specifying the cell (pixel) size in the x and
#'   y dimensions, respectively. These define the spatial resolution of the
#'   AOC. Required only when `movement_type = "crw"`; otherwise values
#'   are automatically derived from the resolution of provided
#'   density maps. Units are assumed to match those of `ref_sys`.
#' @param delta_time Character string, defines the temporal resolution of the
#'   model. Valid options include "hours", "day", "week", "month" or
#'   "year", and can be preceded by a positive integer and a space, and followed
#'   by "s".
#' @param start_date;end_date Date, respectively, defines the start and end
#'   dates for the simulation period.
#' @param start_sites An `<sf>` object, defining the sites where agents start the
#'   simulation. Apart from the sites' geometry, this object must contain the
#'   following columns:
#'    * `id`: a unique identifier for each site.
#'    * `prop`: the proportion of `n_agents` allocated at each site. The values
#'     in this column must sum to 1.
#'
#'    If `NULL` (the default), agents start at random locations within the AOC.
#'
#' @param end_sites An `<sf>` object, analogous to `start_sites`, specifying the
#'   sites to which agents must return to at the end of the simulation. If
#'   `NULL` (the default), end locations are not forced upon agent. **Note:**
#'   This parameter is currently inactive and will be ignored.
#'
#'
#' @details
#'
#'
#' In the recommended workflow for building and running IBMs in roamR, the first
#' step is to define high-level model settings and controls by creating a
#' `<ModelConfig>` object. Key components of `<ModelConfig>` are outlined below.
#' For additional guidance, see `vignette("roamR-guide")`.
#'
#'
#' ## Movement Model
#'
#' At this stage, users only need to specify the movement methodology for the
#' simulation via `movement_type`. Detailed descriptions of each movement model
#' are available in `vignette("movement")`. Required input data underpinning the
#' chosen model should be provided through other components, such as `<Driver>`
#' and `<Species>` classes.
#'
#' ## Number of Agents
#'
#' `n_agents` should be large enough to capture uncertainty and the
#' distributional shape of outputs. However, computational costs must me
#' considered, as runtime and storage increase roughly linearly with the number
#' of agents. Note that simulated agents represent a sample of possible
#' realizations from the simulated population, so `n_agents` does not need to
#' match the actual population size.
#'
#'
#' ## Spatial Settings
#'
#' `ref_sys` sets the CRS underpinning the simulation. Spatial inputs
#' with differing CRS are automatically converted to `ref_sys` during model
#' initialization.
#'
#' `aoc_bbx` specifies the spatial bounding box that constrains agent movement;
#' agents cannot move beyond these limits. All spatial inputs are clipped to
#' this extent during model initialization, so it should encompass key spatial
#' features of the population (e.g., density maps, maximum range). Avoid
#' excessively large boundaries, as they increase storage requirements without
#' improving model realism.
#'
#'
#' Parameters `delta_x` and `delta_y` define the spatial resolution of the IMB:
#'
#' * They set the pixel size which, together with `aoc_bbx`, determines the
#' gridded spatial domain for the simulation.
#'
#' * Values sould balance spatial realism (i.e. movement responses to habitat)
#' with computational efficiency. Finer grids do not improve realism beyond the
#' resolution of the input data.
#'
#' * Under the default Density-informed movement model, these parameters are
#' automatically set to match the resolution of user-provided density maps.
#'
#'
#' ## Temporal Configuration
#'
#' Arguments `start_date` and `end_date` define the simulation period. Input
#' data must fully cover this interval, either through matching timestamps
#' or via aggregation over an appropriate temporal scale (e.g.,
#' monthly/annually means or totals) that spans the same period.
#'
#'
#' `delta_time` specifies the duration each simulation step. Agent locations and
#' physiological states are evaluated at the end of each step, informing energy
#' budget calculations and potential movement for the subsequent time-step. As
#' with the spatial configuration, chose a temporal resolution that balances
#' biological realism with computational efficiency: overly coarse steps may
#' miss important agent dynamics, while steps finer than the input data
#' resolution add cost without benefit.
#'
#'
#' ## `start_sites` and `end_sites`
#'
#' For site geometries other than points, agents' starting/end locations are
#' randomly drawn within the boundary of the geometry. For example, if a site is
#' defined by polygon(s), agents assigned to that site start/end at random
#' points within the polygon.
#'
#'
#'
#' @examples
#' library(sf)
#' library(ggplot2)
#'
#' # specify colonies
#' colonies <- st_sf(
#'   id = c("A", "B", "C"),
#'   prop = c(0.30, 0.30, 0.40),
#'   geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))),
#'   crs = 4326
#' )
#'
#' # initialize model configuration object
#' config <- ModelConfig(
#'   movement_type = "crw",
#'   n_agents = 1000,
#'   ref_sys = st_crs(4326),
#'   aoc_bbx = c(0, 0, 5, 5),
#'   delta_x = 0.25,
#'   delta_y = 0.25,
#'   delta_time = "1 day",
#'   start_date = as.Date("2022-09-01"),
#'   end_date = as.Date("2022-09-30"),
#'   start_sites = colonies,
#'   end_sites = colonies
#' )
#'
#' config
#'
#' # Accessors
#' aoc_bbx(config)
#' start_sites(config)
#' end_sites(config)
#'
#' # vizualizing the AOC's bounding box, the start and end sites
#' ggplot() +
#'   geom_sf(data = st_as_sfc(aoc_bbx(config)), col = "orange", fill = NA) +
#'   geom_sf(data = start_sites(config), size = 4, colour = "darkgreen") +
#'   geom_sf(data = end_sites(config), col = "red")
#'
#' @return An object of class <[ModelConfig-class]>
#'
#' @export
ModelConfig <- function(movement_type = c("di", "crw"),
                        n_agents = 100L,
                        ref_sys = sf::st_crs(4326),
                        aoc_bbx = c(0, 0, 10, 10),
                        delta_x = 0.25,
                        delta_y = 0.25,
                        delta_time = "1 day",
                        start_date = as.Date("2026-01-01"),
                        end_date = as.Date("2026-01-05"),
                        start_sites = NULL,
                        end_sites = NULL){

  # TODO:
  # (ii) unit-tests

  # Null input handling --------------------------------------------------------
  start_sites <- start_sites %||% sf::st_sf(sf::st_sfc())
  end_sites <- end_sites %||% sf::st_sf(sf::st_sfc())

  # Input validation -----------------------------------------------------------

  movement_type <- rlang::arg_match(movement_type)

  if(!inherits(ref_sys, "crs")){
    cli::cli_abort(c(
      "{.arg ref_sys} must be an object of class {.cls crs}, not {.cls {class(ref_sys)}}",
      "i" = "Use {.code sf::st_crs()} to specify a suitable CRS"
    ))
  }

  if(!is(aoc_bbx, "bbox")){
    if(!is.numeric(aoc_bbx)){
      cli::cli_abort("{.arg aoc_bbx} must must be a {.cls numeric} vector")
    }

    if(length(aoc_bbx) != 4){
      cli::cli_abort("{.arg aoc_bbx} must have length 4, not {length(aoc_bbx)}")
    }

    aoc_bbx <- structure(
      as.double(aoc_bbx),
      names = c("xmin", "ymin", "xmax", "ymax"),
      class = "bbox",
      crs = ref_sys)

  } else if(sf::st_crs(aoc_bbx) != ref_sys) {
    cli::cli_abort("{.arg aoc_bbx} and {.arg ref_sys} must refer to the same CRS")
  }

  if(is.numeric(n_agents)) n_agents <- as.integer(n_agents)


  # Slot input values management -----------------------

  # Under density-informed movement models, Spatial resolution is defined by the
  # density surface driver. So, overwrite delta_x and delta_y as NAs.
  if(movement_type == "di") delta_x <- delta_y <- NA_real_

  # Construct a new instance of <ModelConfig> -----
  methods::new(
    "ModelConfig",
    movement_type = movement_type,
    n_agents = n_agents,
    ref_sys = ref_sys,
    aoc_bbx = aoc_bbx,
    delta_x = delta_x,
    delta_y = delta_y,
    delta_time = delta_time,
    start_date = start_date,
    end_date = end_date,
    start_sites = start_sites,
    end_sites = end_sites
  )
}




# Validator -----------------------------------------------------
methods::setValidity("ModelConfig", function(object) {

  # TODO:

  err <- c()

  # Check if required slots are populated
  if (is.na(object@n_agents)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @n_agents: Provide the number of agents to simulate.")
    )
  }

  if (is.na(object@ref_sys)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @ref_sys: Missing value. Provide valid CRS for the AOC.")
    )
  }

  #browser()

  if (any(is.na(as.vector(object@aoc_bbx)))) {
    err <- c(
      err,
      cli::format_inline("\n- slot @aoc_bbx: Invalid input. Bounding box must be a vector of 4 non-NA values.")
    )
  }

  if(object@movement_type == "crw"){

    if (is.na(object@delta_x)) {
      err <- c(
        err,
        cli::format_inline(
          "\n- slot @delta_y: Missing value. Cell size in x dimension must be provided",
          " when {.code @movement_type = {.val crw}}."
        )
      )
    }

    if (is.na(object@delta_y)) {
      err <- c(
        err,
        cli::format_inline(
        "\n- slot @delta_y: Missing value. Cell size in y dimension must be provided",
        " when {.code @movement_type = {.val crw}}.")
      )
    }

  }

  if (is.na(object@delta_time)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @delta_time: Missing value. Provide the model's temporal resolution.")
    )
  }

  if (is.na(object@start_date)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @start_date: Missing value. Provide the model's start date.")
    )
  }

  if (is.na(object@end_date)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @end_date: Missing value. Provide the model's end date.")
    )
  }


  if (is.na(object@movement_type)) {
    err <- c(
      err,
      cli::format_inline("\n- slot @movement_type: Missing value. Provide the
                         movement model intended for the simulation.")
    )
  }

  # validate temporal resolution
  # check done via lubridate::period
  if(!is.na(object@delta_time)){
    if (is.na(lubridate::period(object@delta_time))) {
      err <- c(
        err,
        cli::format_inline(
          "\n- Slot @delta_time: Failed to parse {.val {object@delta_time}} as valid ",
          "time units. Use recognized units (e.g. {.val 1 day}, {.val 2 months})."
        )
      )
    }}

  # CRS consistency between aoc_bbx and ref_sys
  if(sf::st_crs(object@ref_sys) != sf::st_crs(object@aoc_bbx)){
    err <- c(
      err,
      cli::format_inline(
        "\n- Slots @ref_sys and @aoc_bbx must have indentical CRS."
      )
    )
  }


  # start/end sites
  err <- c(err, val_sites(object@start_sites, object@aoc_bbx))
  err <- c(err, val_sites(object@end_sites, object@aoc_bbx))


  # @movement_type
  valid_movement_types <- c("di", "crw")
  if (object@movement_type %notin% valid_movement_types) {
    err <- c(
      err,
      cli::format_inline(
        "\n- slot @movement_type: Invalid value {.val {object@movement_type}}. ",
        "Must be one of {.or {.val {valid_movement_types}}}."
      )
    )
  }

  if (length(err) > 0) {
    # need to collapse into single string for desired formatting
    do.call(paste, list(err, collapse = " "))
  } else{
    TRUE
  }
})


# Sites validation helper
val_sites <- function(sites, aoc_bbx){

  err <- list()
  n_sites <- nrow(sites)

  # only check for non-empty sf objects
  if(n_sites > 0){

    slot_name <- sub("object", "", rlang::caller_arg(sites))
    misscols <- setdiff(c("id", "prop"), names(sites))

    # validate attributes
    if(length(misscols) > 0){
      err <- c(err, cli::format_inline("\n- {slot_name} must contain {cli::qty(misscols)}column{?s} {.val {misscols}}"))
    }else if(sum(sites$prop) != 1){
      err <- c(err, cli::format_inline("\n- {slot_name}: values in column {.val prop} must add up to 1."))
    }

    # validate spatial requirements
    if(sf::st_crs(sites) != sf::st_crs(aoc_bbx)){
      err <- c(err, cli::format_inline(
        "\n- {slot_name} must have the same CRS as @aoc_bbx: ",
        "{.val {sf::st_crs(aoc_bbx)$input}}"
      ))
    } else{
      #browser()
      sites_in_aoc <- sf::st_within(sites, sf::st_as_sfc(aoc_bbx))
      n_sites_outside_aoc <- sum(lengths(sites_in_aoc) == 0)
      if(n_sites_outside_aoc > 0){
        err <- c(err, cli::format_inline(
          "\n- {slot_name}: {n_sites_outside_aoc} out of {n_sites}",
          "{cli::qty(n_sites_outside_aoc)} site{?s} {?is/are} ",
          "located outside the AOC area, as per @aoc_bbx."
        ))
      }
    }
  }

  err
}



# Methods  ---------------------------------------------------------------

## Accessors ------------------------------------

### @movement_type
#### getter
#' @export
setGeneric("movement_type", function(x) standardGeneric("movement_type"))
setMethod("movement_type", "ModelConfig", function(x) x@movement_type)

#### setter
#' @export
setGeneric("movement_type<-", function(x, value) standardGeneric("movement_type<-"))
setMethod("movement_type<-", "ModelConfig", function(x, value) {
  x@movement_type <- value
  validObject(x)
  x
})


### @start_sites
#### getter
#' @export
setGeneric("start_sites", function(x) standardGeneric("start_sites"))
setMethod("start_sites", "ModelConfig", function(x) x@start_sites)


### @end_sites
#### getter
#' @export
setGeneric("end_sites", function(x) standardGeneric("end_sites"))
setMethod("end_sites", "ModelConfig", function(x) x@end_sites)


### @aoc_bbx
#### getter
#' @export
setGeneric("aoc_bbx", function(x) standardGeneric("aoc_bbx"))
setMethod("aoc_bbx", "ModelConfig", function(x) x@aoc_bbx)



### @n_agent
#### getter
#' @export
setGeneric("n_agents", function(x) standardGeneric("n_agents"))
setMethod("n_agents", "ModelConfig", function(x) x@n_agents)

#### setter
#' @export
setGeneric("n_agents<-", function(x, value) standardGeneric("n_agents<-"))
setMethod("n_agents<-", "ModelConfig", function(x, value){
  x@n_agents <- value
  validObject(x)
  x
})





# visualize bbox,  start and end sites stored in `config`
# plot(st_as_sfc(config@aoc_bbx), axes = TRUE, col = NA, border = "blue", lwd = 1.5)
# plot(start_sites(config)["id"], pch = 19, col = "darkgreen", add = TRUE, cex = 1.5)
# plot(end_sites(config)["id"], pch = 19, col = "red", add = TRUE)


## Show ------------------------------------
# Only @start_site and @end_site are optionals, so they're the only ones getting
# a fancy red-coloured NAs. NA's in other slots should fail validation
setMethod("show", "ModelConfig", function(object) {

  align_width <- nchar("Temporal Resolution:") + 1
  bltcol <- "blue"

  cli::cat_line(cli::format_message("{.cls {is(object)[[1]]}} instance with attributes:"))

  #cli::cat_line(paste("CRS:", object@ref_sys$Name))

  #browser()

  crs_units_txt <- if(is.na(object@ref_sys)){
    ""
  } else {
    paste0("[", units::deparse_unit(object@ref_sys$ud_unit), "]") |>
      cli::col_grey()
  }

  # @movement_type
  mv_mod_txt <- paste0(
    format("Movement Model:", width = align_width),
    if(object@movement_type == "di"){
      "Density-informed"
    } else if(object@movement_type == "crw"){
      "Correlated Random Walk"
    } else{
      object@movement_type
    }
  )

  # @n_agents
  agents_txt <- paste0(
    format("No. Agents:", width = align_width),
    object@n_agents
  )


  # @ref_system
  rs <- object@ref_sys
  crs_label <- if (is.na(rs)) {
    "CRS:"
  } else if (rs$IsGeographic) {
    "Geodetic CRS:"
  } else {
    "Projected CRS:"
  }

  crs_txt <- paste0(format(crs_label, width = align_width), rs$Name)


  # @delta_x and @delta_y
  sp_res_txt <- if(object@movement_type == "crw") {
    paste0(
      format("Spatial resolution:", width = align_width),
      object@delta_x, " x ", object@delta_y, " ", crs_units_txt
    )
  } else {
    NULL
  }

  # @aoc_bbx
  bbox_txt <- paste0(
    format("Bounding box:", width = align_width),
    paste(
      sapply(1:4, \(i) paste(names(object@aoc_bbx)[i], object@aoc_bbx[i], sep = ": ")),
      collapse = "  "
    ),
    " ", crs_units_txt
  )

  # @start_date and @end_date
  sim_duration <- difftime(as.Date(object@end_date), as.Date(object@start_date), units = "days")
  sim_duration_text <- paste0("(", as.numeric(sim_duration), " days)") |>
    cli::col_grey()

  sim_period_txt <- paste0(
    format("Simulation period:", width = align_width),
    object@start_date,
    " -- ",
    object@end_date,
    " ",
    sim_duration_text
  )

  # @delta_time
  temp_res_txt <- paste0(
    format("Temporal resolution:", width = align_width),
    object@delta_time
  )

  #browser()

  # @start_sites and @end_sites
  start_site_txt <- textify_site(object@start_sites, "Start", align_width)
  end_site_txt <- textify_site(object@end_sites, "End", align_width)

  # print
  cli::cat_bullet(c(
    mv_mod_txt, agents_txt, sim_period_txt, temp_res_txt, bbox_txt, sp_res_txt,
    crs_txt, start_site_txt, end_site_txt),
    bullet_col = bltcol
  )

})




textify_site <- function(x, word_start, align_width, n = 5){

  if(nrow(x) == 0){
    paste0(
      format(paste0(word_start, " site:"), width = align_width),
      cli::col_red("NA")
    )
  } else{

    if(nrow(x) > n){
      y <- x[1:n, drop = FALSE]
      feat_subset_txt <- paste0("   First ", n," sites:\n")
    } else{
      y <- x
      feat_subset_txt <-  ""
    }

    paste0(
      format(paste0(word_start, " ", ifelse(nrow(x) == 1, "site", "sites"), ":"), width = align_width),
      gsub(" collection", "", capture.output(print(x))[[1]]),
      cli::col_grey(" [", sf::st_crs(x)$Name, "]\n"),
      feat_subset_txt,
      paste(
        paste0(
          #"\t",
          #paste(rep(" ", align_width + 5), collapse = ""),
          "   ",
          capture.output(data.frame(y))),
        collapse = "\n"
      )
    )
  }
}


# cli::cat_line(cli::format_message("A {.cls move2} with `track_id_column` {.val 3} and `time_column` {.val 'rrs'}"))
# cli::cat_line("A {.cls move2} with `track_id_column` {.val 3}", col = "blue") #cli::cli_text("{names(turtwick_ibm_cfg@aoc_bbx)}")


