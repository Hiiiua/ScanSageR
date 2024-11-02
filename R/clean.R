#' Get binarized cimg
#'
#' Convert the x3p and its mask to a cimg
#'
#' @importFrom terra as.raster
#' @importFrom x3ptools as x3p_read x3p_add_mask x3p_sample
#' @importFrom png readPNG
#' @importFrom imager imfill
#'
boundary_cimg <- function (x3p_path, mask_path, downsample_m, color="W"){
  land <- x3p_read(x3p_path)
  mask <- png::readPNG(mask_path)
  overlay <- x3p_add_mask(land, mask = terra::as.raster(mask))
  down0 <- overlay %>% x3p_sample(m=20)

  if (color=="W"){
    imfill(x=down0$header.info$sizeX, y=down0$header.info$sizeY, z=1, val=1)
    boundaryW[is.na(down0$surface.matrix)]=0
    return(boundaryW)
  }
  else{
    boundaryB = imfill(x=down$header.info$sizeX, y=down$header.info$sizeY, z=1, val=0)
    boundaryB[is.na(down$surface.matrix)]=1
    return(boundaryB)
  }
}
