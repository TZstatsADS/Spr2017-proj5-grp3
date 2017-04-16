# Input: two data frames. One is background.csv, the other
#        is the one you want to extract corresponding features
#        from the background.csv
# Output: a data frame.
feature <- function(B = background, df){
  index <- colnames(B) %in% df$Code
  output <- B[,index] 
  return (output)
}



# setwd("~/Desktop/GR5243/Spr2017-proj5-grp3")
# background <- read.csv("data/FFChallenge/background.csv")
# dad <- read.csv("data/ff_dad_cb9.csv")


###########Sample#################
# test <- feature(background, dad)

