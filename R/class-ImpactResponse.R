#' Class `<ImpactResponse>`
#'
#' `<ImpactResponse>` is an S4 class defining species-level, per-agent responses
#' to man-made structures or other non-environmental impact factors. Designed as
#' a sub-class of <[`Species-class`]>, it currently interacts with objects of
#' class <[`Structure-class`]> to influence simulated agent properties such as
#' movement and behaviour.
#'
#' @slot impact_id character string, a unique identifier of the structure or
#'   impact effect. Must match the identifier of an existing impact, e.g. slot
#'   `id` of a defined [Structure-class] object.
#' @slot displace_prob an object of class `<Vardist>`, specifying the
#'   probability of agent displacement associated with `impact_id`, indicating the
#'   likelihood of an agent being redistributed from/to the location of the
#'   impact.
#' @slot displace_fn function (provisional), defining the relationship between
#'   agent displacement and proximity to the impact source (e.g., the intensity
#'   of repulsion movements as a function of distance from the edge of a
#'   structure's footprint).
#' @slot disturb_prob an object of class `<Vardist>`, defining the probability
#'   of agent disturbance associated with `impact_id`, representing the likelihood of
#'   an agent experiencing disturbance due to the impact.
#' @slot disturb_behav a character string, identifying the specific behaviour affected
#'   by the disturbance
#' @slot disturb_extent an object of class `<Vardist>`, quantifying the magnitude
#'   of the disturbance on the specified `disturb_behav`
#'
#'
#' @seealso Helper functions [VarDist()] and [ImpactResponse()] for constructing
#'   objects of the respective classes
#'
#' @export

methods::setClass(
  Class = "ImpactResponse",
  slots = list(
    impact_id = "character",
    displace_prob = "VarDist",
    displace_fn = "function",
    disturb_prob = "VarDist",
    disturb_behav = "character",
    disturb_extent = "VarDist"
  )
)




#' Create a `<ImpactResponse>` object
#'
#' Helper function to construct instances of <[`ImpactResponse-class`]> objects,
#' describing pecies-level responses to man-made structures or (eventually!)
#' other non-environmental factors. It's intended to be a sub-class of
#' [Species-class], and currently interacts with [Structure-class] objects to
#' influence simulated aspects of the traced agents such as movement and
#' behaviour.
#'
#' @param impact_id character string, a unique identifier of the structure or
#'   impact effect. Must match the identifier of an existing impact, e.g. slot
#'   `id` of a defined [Structure-class] object.
#' @param displace_prob an object of class `<Vardist>`, specifying the
#'   probability of agent displacement associated with `impact_id`, indicating the
#'   likelihood of an agent being redistributed from/to the location of the
#'   impact.
#' @param displace_fn function (provisional), defining the relationship between
#'   agent displacement and proximity to the impact source (e.g., the intensity of
#'   avoidance movements as a function of distance from the impact centroid).
#' @param disturb_prob an object of class `<Vardist>`, defining the probability
#'   of agent disturbance associated with `impact_id`, representing the likelihood of
#'   an agent experiencing disturbance due to the impact.
#' @param disturb_behav a character string, identifying the specific behaviour affected
#'   by the disturbance
#' @param disturb_extent an object of class `<Vardist>`, quantifying the magnitude
#'   of the disturbance on the specified `disturb_behav`
#'
#'
#' @export

ImpactResponse <- function(impact_id = NA_character_,
                           displace_prob = VarDist(),
                           displace_fn = function(x) x,
                           disturb_prob = VarDist(),
                           disturb_behav = c("flying", "swimming", "diving",
                                             "foraging", "water_resting",
                                             "travelling","nest_resting",
                                             "other"),
                           disturb_extent = VarDist()
                           ){

  # input validation
  disturb_behav <- rlang::arg_match(disturb_behav)
  check_class(displace_prob, "VarDist")
  check_class(displace_fn, "function")
  check_class(disturb_prob, "VarDist")
  check_class(disturb_extent, "VarDist")


  # construct a new instance of <ImpactResponss>
  new(
    "ImpactResponse",
    impact_id = impact_id,
    displace_prob = displace_prob,
    displace_fn = displace_fn,
    disturb_prob = disturb_prob,
    disturb_behav = disturb_behav,
    disturb_extent = disturb_extent
  )


}
