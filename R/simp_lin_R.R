simp_lin_R <- function(x,y){
  if (length(x) != length(y)){
    stop("x and y are not of the same length")
  }

  ### run simp_lin_cpp
  output <- simp_lin_cpp(x,y)
}
