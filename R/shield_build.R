#' Build a png shield
#'
#' This function will create a local png shield
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

shield_build <- function(
  stub,
  label,
  logo,
  color = "blue",
  filename = "generated_badge.png",
  method = "shields.io",
  verbose = FALSE
) {
  # check output is png
  if (!endsWith(x = filename,suffix = ".png")) stop("filename must be a png file")

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
    png_url <- glue::glue(
      "https://raster.shields.io/badge/{stub}-{label}-{color}.svg?logo=data:image/png;base64,{encoded_logo}"
    )
    download.file(
      url = png_url,
      destfile = filename,
      quiet = verbose
    )
    if (verbose) message("File saved to ",filename)

  } else {
    stop("Only shields.io set currently")
  }
}




