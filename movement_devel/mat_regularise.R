#' regulartization of fractional power of matrix
#'

regularize_row <- function(r) {

  i <- 0

  while((sum(r) != 1|any(r < 0 )) & i <= length(r)){
      adj <- (sum(r)-1)/length(r)
      r_star <- r - adj
      r[r!=0] <- r_star[r!=0]
      r <- ifelse(r < 0, 0, r)
      i <- i + 1
      print(i)
    }

  r
}


temp <- diag(test$d)^(1/4)


apply(temp, 1, regularize_row)
