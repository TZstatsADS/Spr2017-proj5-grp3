#extract codebook csv file from txt file

fileName <- "../data/ff_teacher_cb9.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)
L = list()
for (i in 1:length(linn)){
  if((linn[i]=="--------------------------------------------------------------------------------------------------------------") & (linn[i+1]!=""))
  {
    L[i] = linn[i+1]
  }
  
  else
  {
    # read until hit the next dashed line
  }
}
close(conn)

i=1
while(i<length(L))
{
  if(length(L[[i]])==0)
  {
    L=L[-i]
    i=1
  }
  i=i+1
}

L = as.data.frame(as.matrix(unlist(L)))
write.csv(L, "../data/asdf.csv")

# open asdf.csv in excel [facepalm]
# delete header so A1 is the cell containing the code and description and a lot of white space
# type left(A1, 10) in cell H1, right(A1, 100) in cell I1, fill to the bottom of the data
# copy all cells containing left/right
# paste on A1 by value
# type trim(A1) in H1 and trim(A2) in I1
# copy all cells containing trim
# paste on A1 by value
# save as an appropriate name

name = c("child", "teacher", "mom", "dad")

for (i in length(name))
{
  dir = paste("../data/ff_", name[i], "_cb9.csv", sep="")
  x = read.csv(dir, col.names = c("Code", "Description"))
  write.csv(x, dir)
  
}
