t_test_beta <- function(beta1, beta2) {
  beta1_total <- beta1$beta_total
  beta2_total <- beta2$beta_total
  ch_num <- ncol(beta1_total)
  ch_name <- names(beta1_total)

  result <- data.frame(matrix(0, nrow = ch_num, ncol = 3))
  names(result) <- c("Ch", "p-value", "sig.")
  for(i in 1:ch_num){
    t_test_result <- t.test(beta1_total[,i], beta2_total[,i])
    result[i,1] <- ch_name[i]
    result[i,2] <- t_test_result$p.value
    if(t_test_result$p.value < 0.05){
      result[i,3] <- "***"
    }
    else{
      result[i,3] <- "No"
    }

  }
  return(result)
}
