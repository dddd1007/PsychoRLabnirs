#' Generate the layout map for Labnir
#'
#' Generate a layout map which you will need it in your paper
#'
#' @param ncol Is the column number in your layout
#' @param nrow Is the row number in your layout
#' @param save_filename Is the filename for your layout picture
#'
#' @export
#' @author Xiaokai Xia (夏骁凯)
#' \email{xia@@xiaokai.me}

layout_map <- function(ncol = NA, nrow = NA, save_filename = "layout.png"){
  if (!require(tidyverse)){
    install.packages(tidyverse)
  }

  map_col <- 2*ncol - 1
  map_row <- 2*nrow - 1
  map_data <- data.frame(matrix(0, ncol = 4, nrow = map_col * map_row))
  names(map_data) <- c("X", "Y", "Num", "color")

  map_data$X <- rep(1:map_col, map_row)
  map_data$Y <- sort(rep(map_row:1, map_col), decreasing = T)

  num <- 1 : (((map_col * map_row) - 1 )/ 2)
  for(i in 1 : max(num)){
    map_data$Num[2*i] <- num[i]
  }
  map_data$Num[map_data$Num == 0] <- NA
  map_data$color[map_data$color == 0] <- NA

  color_map <- map_data %>%
    filter(Y %% 2 == 1, X %% 2 == 1) %>%
    select(X, Y, color)
  if(nrow(color_map) %% 2 == 0){
    color_map$color <- c("red", "blue")
    }else{
    color_map$color[-nrow(color_map)] <- c("red", "blue")
    color_map$color[nrow(color_map)] <- "red"
  }

  p <- left_join(map_data[-4], color_map, by = c("X","Y")) %>%
    ggplot(aes(x = X, y = Y)) + geom_tile(aes(fill = color)) + scale_fill_identity(c("red" = "red", "blue" = "blue")) + coord_equal(ratio = 1) + geom_text(aes(label = Num)) + theme_bw()
  ggsave(filename = save_filename, plot = p, width = 12, dpi = 300)
}
