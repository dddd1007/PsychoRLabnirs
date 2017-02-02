#' Caculate the mean of beta of many cond files and return all beta in your cond
#' files.
#'
#' \code{beta_mean} return a list which is mean of beta in some cond files that
#' in one experiment condition. It also return all beta value in choosen files.
#'
#' @param cond_name Must be a charactor that you need to analysis.
#'
#' @return A list contain the mean of beta and all beta value.
#'
#' @export
#' @author Xiaokai Xia (夏骁凯)
#' \email{xia@@xiaokai.me}

beta_mean <- function(cond_name = NA) {
  choosepath <- tcltk::tk_choose.dir()
  filepath <- dir(include.dirs = T, recursive = T)
  filepath <- filepath[stringr::str_detect(filepath, pattern = cond_name)]
  filepath <- paste0(choosepath, filepath)
  col_num <- length(extract_fNIRS_beta(filepath[1]))
  beta_table <- data.frame(matrix(0, nrow = length(filepath), ncol = col_num))
  names(beta_table) <- paste0("Ch", 1:col_num)

   for(i in filepath){
    beta_table[1:length(filepath), ] <- extract_fNIRS_beta(i)
  }

  beta_mean_result <- apply(beta_table, MARGIN = 2, mean)
  beta_list <- list(beta_mean = beta_mean_result, beta_total = beta_table)
}
