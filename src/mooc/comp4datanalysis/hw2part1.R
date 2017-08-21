outcome <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
ncol(outcome)
names(outcome)
hist(as.numeric(outcome[,11]), xlab="xaxis", ylab="yaxis", main="nnn")