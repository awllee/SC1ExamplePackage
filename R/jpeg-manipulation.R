#' Convert a colour image to grayscale
#'
#' @param img colour image as a 3D array
#'
#' @return matrix corresponding to grayscale image
#' @export
#'
#' @examples
#' grayscale(jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg")))
grayscale <- function(img) {
  dims <- dim(img)
  stopifnot(length(dims) == 3 && dims[3] == 3)
  mtx1 <- img[,,1]
  mtx2 <- img[,,2]
  mtx3 <- img[,,3]
  mtx <- (mtx1 + mtx2 + mtx3)/3
  return(mtx)
}

#' Load a JPEG
#'
#' @param filename JPEG file name
#'
#' @return 3D array corresponding to image
#' @export
#'
#' @examples
#' load.jpeg(system.file("img", "Rlogo.jpg", package="jpeg"))
load.jpeg <- function(filename) {
  img <- jpeg::readJPEG(filename)
  return(img)
}

#' Plot image
#'
#' @param img image to plot
#'
#' @export
#'
#' @examples
#' view.image(load.jpeg(system.file("img", "Rlogo.jpg", package="jpeg")))
#' view.image(grayscale(load.jpeg(system.file("img", "Rlogo.jpg", package="jpeg"))))
view.image <- function(img) {
  rst <- grDevices::as.raster(img)
  graphics::plot(rst)
}

#' Fix image
#'
#' Thresholds values above by 1 and below by 0.
#'
#' @param mtx numeric matrix
#'
#' @return thresholded matrix
#' @export
#'
#' @examples
#' fix.image(matrix(seq(-0.5,1.4,0.1), 5, 4))
fix.image <- function(mtx) {
  return(pmin(pmax(mtx,0),1))
}
