




fnirs_heatmap <- function(map_vector, row = NULL, col = NULL) {
  map_row <- 2*row - 1
  map_col <- 2*col - 1
  dim_num <- map_row * map_col
  #map_vector <- read.csv(choose.file())
  map_table <- data.frame(matrix(0, nrow = dim_num, ncol = 4))
  names(map_table) <- c("X", "Y", "beta", "Channel")
  map_table$X <- rep(1:map_col, map_row)
  map_table$Y <- sort(rep(1 : map_row, map_col))
  ch <- 1:length(map_vector)

  for(i in 1:length(map_vector)){
    map_table[2*i,3] <- map_vector[i]
    map_table[2*i,4] <- ch[i]
  }
  map_table$beta[map_table$beta == 0] <- NA
  map_table$Channel[map_table$Channel == 0] <- NA

  for(i in 1 : nrow(map_table)){
    if(is.na(map_table$beta[i])){
      na_x <- map_table$X[i]
      na_y <- map_table$Y[i]

      num_1 <- filter(map_table, X == (na_x - 1), Y == (na_y))$beta
      num_2 <- filter(map_table, X == (na_x + 1), Y == (na_y))$beta
      num_3 <- filter(map_table, X == (na_x), Y == (na_y - 1))$beta
      num_4 <- filter(map_table, X == (na_x), Y == (na_y + 1))$beta

      mean_vector <- c(num_1, num_2, num_3, num_4)

      map_table$beta[i] <- mean(mean_vector)
    }
  }

  library(ggplot2)
  ggplot(map_table, aes(x = X, y = Y, fill = beta)) + scale_fill_distiller(palette = "Spectral") + geom_tile(show.legend = T) + geom_text(aes(label = Channel))+ coord_equal(ratio=1)
}
