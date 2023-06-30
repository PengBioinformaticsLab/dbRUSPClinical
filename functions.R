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

