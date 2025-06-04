#' `<IBM>`
#'
#' `<IBM>` is an S4 class specifying one instance of an Individual-Based Model.
#' It encapsulates all elements required to run the simulation
#'
#' TODO: Flesh out description
#'
#' @include class-Species.R  class-ModelConfig.R
#'
#' @export
methods::setClass(
  Class = "IBM",
  slots = list(
    agents = "list",
    drivers = "list",
    species = "Species",
    model_config = "ModelConfig"
  ),
  prototype = list(
    agents = list(),
    drivers = list(),
    species = Species(),
    model_config = ModelConfig()
  )
)




#' Create a `<IBM>` object
#'
#' Helper function to construct instances of <[`IBM-class`]> objects
#'
#' @export
IBM <- function(agents = list(),
                 species = new("Species"),
                 drivers = list(),
                 model_config = ModelConfig()){


  methods::new(
    "IBM",
    agents = agents,
    drivers = drivers,
    species = species,
    model_config = model_config
  )

}



# Methods-----------------------------------------------------------------

## Accessors ------------------------------------
### @model_config@ref_sys
#### getter
setGeneric("ref_sys", function(x) standardGeneric("ref_sys"))
setMethod("ref_sys", "IBM", function(x) x@model_config@ref_sys)

