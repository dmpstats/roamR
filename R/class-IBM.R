#' `<IBM>`
#'
#' `<IBM>` is an S4 class specifying one instance of an Individual-Based Model.
#' It encapsulates all elements required to run the simulation
#'
#' TODO: Flesh out description
#'
#' @include class-Species.R class-Habitat.R class-Structure.R class-ModelConfig.R
#'
#' @export
methods::setClass(
  Class = "IBM",
  slots = list(
    agents = "list",
    species = "Species",
    habitat = "Habitat",
    structures = "list",
    config = "ModelConfig"
  ),
  prototype = list(
    agents = list(),
    species = new("Species"),
    habitat = Habitat(),
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
                habitat = Habitat(),
                structures = list(owf = Structure(type = "OWF")),
                config = ModelConfig()){


  methods::new(
    "IBM",
    agents = agents,
    species = species,
    habitat = habitat,
    structures = structures,
    config = config
  )

}
