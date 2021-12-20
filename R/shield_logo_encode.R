#' Convert a png into base64 string
#'
#' This function will create a local png shield
#'
#' @param logo The left hand side of the shield
#' @export

shield_logo_encode <- function(
  logo
) {
  # check output is png
  if (!endsWith(x = logo,suffix = ".png")) stop("logo must be a png file")
  base64enc::base64encode(logo)
}
