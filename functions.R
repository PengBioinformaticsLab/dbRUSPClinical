#R script of functions related to the shiny application


#Function to make a list out of given information
makeList <- function(x){
  rlt <- list()
  for(i in 1:length(x)){
    rlt[[i]] <- i
  }
  names(rlt) <- x
  rlt
}

#Method to oscillate between lm and gam methods for correlation
mymethod <- function(formula, data, weights, ...) {
  if(nrow(data) < 5) return(lm(y ~ x, data = data, ...))
  else  return(mgcv::gam(y ~ s(x, bs = "cs", k = 3), data = data, ...))
}