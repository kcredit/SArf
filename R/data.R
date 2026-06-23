#' Dublin Small Area Health Rating Index Data
#'
#' A spatial dataset of 895 Small Areas (SAs) in Dublin, Ireland, containing
#' a Health Rating Index (HRI) and associated socioeconomic and environmental
#' predictors. This dataset was used in Credit et al. (2026) to demonstrate
#' the SArf package applied to urban health inequalities.
#'
#' @format An sf object with 2,407 rows and 8 columns (including geometry).
#' All numeric variables are z-score standardised (mean 0, SD 1):
#' \describe{
#'   \item{HRI_gaus_p}{Health Rating Index (Gaussian-weighted PCA composite),
#'     z-scored. Higher values indicate better environmental health conditions.}
#'   \item{In22_ED}{Pobal HP Deprivation Index 2022, z-scored. Higher values
#'     indicate greater deprivation.}
#'   \item{NoAuto_p}{Proportion of households without access to a car, z-scored.}
#'   \item{POPD}{Population density (persons per square metre), z-scored.}
#'   \item{log_dist}{Log distance to the nearest primary or secondary road, z-scored.}
#'   \item{ov60}{Proportion of the population aged 60 or over, z-scored.}
#'   \item{nonIrish}{Proportion of residents who are non-Irish citizens, z-scored.}
#'   \item{geometry}{sf geometry column (polygons, EPSG:2157 Irish Transverse Mercator).}
#' }
#'
#' @details
#' The data are derived from the 2022 Irish Census Small Area boundaries and
#' associated Census data (Central Statistics Office, Ireland), combined with
#' environmental health indicators compiled for the Health Rating Index project.
#'
#' The full analysis code and raw data are available at:
#' \url{https://github.com/kcredit/health-rating-index}
#'
#' @source
#' Credit, K., Kaur, D., & Eccles, E. (2026). Analysing urban inequalities in
#' environment and health at the neighbourhood scale in Dublin through a new
#' open-access Health Rating Index. \emph{Wellbeing, Space and Society}, 10,
#' 100356. \doi{10.1016/j.wss.2026.100356}
#'
#' Central Statistics Office Ireland (2022). Census 2022 Small Area Statistics.
#' \url{https://www.cso.ie}
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "model_data.shp", package = "SArf")
#' data <- sf::st_read(data_path)
#' summary(data$HRI_gaus_p)
#' }
NULL
