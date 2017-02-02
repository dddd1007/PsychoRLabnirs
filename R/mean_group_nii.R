# 计算多个个体nii文件的均值矩阵
mean_group_nii <- function(nii_list) {
  dim_single <- dim(nii_list[[1]])
  file_num <- length(nii_list)
  dim_array <- c(dim_single, file_num)
  total_array <- array(dim = dim_array)
  for(i in 1:length(nii_list)){
    total_array[,,i] <- matrix(as.numeric(nii_list[[i]]), nrow = dim_single[1], ncol = dim_single[2])
  }

  mean_matrix <- apply(total_array, MARGIN = 1:2, mean)

  return(mean_matrix)
}
