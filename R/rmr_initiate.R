#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' incorporating species, habitat, structures, and configuration settings.
#'
#' @param species an object of class `<Species>`, comprising the species-level
#'   characteristics of the simulated agents (see [Species()])
#' @param habitat an object of class `<Habitat>`, detailing habitat-level
#'   features of the Area of Calculation (AOC) where the simulation takes place
#'   (see [Habitat()])
#' @param structures a `list` consisting exclusively of objects of class
#'   `<Structure>` [see Structure()], listing man-made physical structures within the
#'   AOC that may influence the agents' movement
#' @param config an object of class `<ModelConfig>`, specifying the primary
#'   configuration settings for the IBM (see [ModelConfig()])
#'
#' @export
#'
rmr_initiate <- function(species, habitat, structures, config){

  ## input validation
  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(habitat, "Habitat", class_fn = "roamR::Habitat")
  check_class(structures, "Structure", inlist = TRUE, class_fn = "roamR::Struture")
  check_class(config, "ModelConfig", class_fn = "roamR::ModelConfig")


  ## initialize Species



  ## initialize Agents

  IBM(
    agents = list(),
    species = species,
    habitat = habitat,
    structures = list(),
    config = config
  )

}
