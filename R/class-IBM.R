#' `<IBM>`
#'
#' `<IBM>` is an S4 class specifying one instance of an Individual-Based Model.
#' It encapsulates all elements required to run the simulation
#'
#' TODO: Flesh out description
#'
#' @include class-Species.R class-Environment.R class-Structure.R class-ModelConfig.R
#'
#' @export
methods::setClass(
  Class = "IBM",
  slots = list(
    agents = "list",
    species = "Species",
    environment = "Environment",
    structures = "list",
    config = "ModelConfig"
  ),
  prototype = list(
    agents = list(),
    species = new("Species"),
    environment = Environment(),
    structures = list(
      owf = Structure()
    ),
    config = ModelConfig()
  )
)




#' Create a `<IBM>` object
#'
#' Helper function to construct instances of <[`IBM-class`]> objects
#'
#' @export
IBM <- function(agents = list(),
                species = new("Species"),
                environment = Environment(),
                structures = list(owf = Structure(type = "OWF")),
                config = ModelConfig()){


  methods::new(
    "IBM",
    agents = agents,
    species = species,
    environment = environment,
    structures = structures,
    config = config
  )

}
