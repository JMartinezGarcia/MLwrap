#' Example Data Set
#'
#' This dataset contains simulated data of a psychometric trial.
#'
#' @format A data frame with 1000 rows and 10 columns:
#' \describe{
#'   \item{psych_well}{Psychological Wellbeing Indicator. Continous with [0,100]}
#'   \item{psych_well_bin}{Psychological Wellbeing Binary Indicator. Factor with {"Low", "High"}}
#'   \item{psych_well_pol}{Psychological Wellbeing Polytomic Indicator. Factor with {"Low", "Somewhat", "Quite a bit", "Very Much"}}
#'   \item{gender}{Patient Gender. Factor {"Female", "Male"}}
#'   \item{age}{Patient Age. Continous [18, 85]}
#'   \item{socioec_status}{Socioeconomial Status Indicator. Factor {"Low", "Medium", "High"}}
#'   \item{emot_intel}{Emotional Intelligence Indicator. Continous [24, 120]}
#'   \item{resilience}{Resilience Indicator}{Continous [4, 20]}
#'   \item{depression}{Depression Indicator}{Continous [0, 63]}
#'   \item{life_sat}{Life Satisfaction Indicator}{Continous [5, 35]}
#' }
"sim_data"
