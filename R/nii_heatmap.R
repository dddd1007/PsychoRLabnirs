#' Map the nii file as standard heatmap by ggplot2
#'
#' \code{nii_heatmap} can map nii file as standard heatmap by using ggplot2. So
#' you will get a legend to tell you the meaning of colour in this heatmap.
#'
#' @param nii_image Must be a single nii image data. If your nii data is a list
#'   please use \code{\link{mean_group_nii}} to get a single image data.
#'
#' @return An picture plot by ggplot2.
#'
#' @export
#' @author Xiaokai Xia (夏骁凯) \email{xia@@xiaokai.me}

nii_heatmap <- function(nii_image){
  image_vector <- as.numeric(nii_image)
  dim_num <- dim(nii_image)
  y <- sort(rep(1: dim_num[1], dim_num[1]),decreasing = F)
  x <- rep(1:dim_num[2], dim_num[2])
  image_table <- data.frame(image = image_vector, x = x, y = y)
  library(ggplot2)
  ggplot(data = image_table, aes(x = x, y = y, fill = image)) + geom_tile() + scale_fill_distiller(palette = "Spectral") + theme_bw() + coord_equal(ratio=0.7)
}

nii_convert_table <- function(nii_image){
  image_vector <- as.numeric(nii_image)
  dim_num <- dim(nii_image)
  y <- sort(rep(1: dim_num[1], dim_num[1]),decreasing = F)
  x <- rep(1:dim_num[2], dim_num[2])
  image_table <- data.frame(image = image_vector, x = x, y = y)
  return(image_table)
}
