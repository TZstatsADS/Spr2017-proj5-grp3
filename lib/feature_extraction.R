
feature <- function(B = background, df){
  index <- colnames(B) %in% df$Code
  output <- B[,index] 
  return (output)
}



# setwd("~/Desktop/GR5243/Spr2017-proj5-grp3")
# background <- read.csv("data/FFChallenge/background.csv")
# dad <- read.csv("data/ff_dad_cb9.csv")

# test <- feature(background, dad)

