#' Build a png shield
#'
#' This function will create a local svg and png shield
#'
#' @param stub The left hand side of the shield
#' @param label The right hand side of the shield
#' @param logo Either the path to a png logo (ideal is 14x14 pixels) or a
#' pre-encoded base64 image (keep it small as this passed as a call!).
#' @param color What color you want the badge to be.
#' @param filename Where to output the file - can be a path, but must end
#' in png.
#' @param method Which service should be used for rendering. Right now it's
#' just shields.io
#' @param verbose Whether to print messages or not
#' @export

shield_build <- function(
  stub,
  label,
  logo,
  color = "blue",
  filename = "generated_badge",
  method = "shields.io",
  verbose = FALSE
) {

  # guess if base64 or png
  if (endsWith(x = logo,suffix = ".png")) {
    if (verbose) message("png filepath detected, doing base64 encoding")
    encoded_logo <- shield_logo_encode(logo)
  } else {
    if (verbose) message("No .png detected, assumption pre-encoded")
    encoded_logo <- logo
  }

  # Generate URL
  if (method == "shields.io") {
    # PNG
    png_url <- glue::glue(
      "https://raster.shields.io/badge/{stub}-{label}-{color}.svg?logo=data:image/png;base64,{encoded_logo}"
    )
    download.file(
      url = png_url,
      destfile = glue::glue("{filename}.png"),
      quiet = !verbose
    )
    if (verbose) message("File saved to ",glue::glue("{filename}.png"))
    # SVG
    svg_url <- glue::glue(
      "https://img.shields.io/badge/{stub}-{label}-{color}.svg?logo=data:image/png;base64,{encoded_logo}"
    )
    download.file(
      url = svg_url,
      destfile = glue::glue("{filename}.svg"),
      quiet = !verbose
    )
    if (verbose) message("File saved to ",glue::glue("{filename}.svg"))
  } else {
    stop("Only shields.io available currently")
  }
}




