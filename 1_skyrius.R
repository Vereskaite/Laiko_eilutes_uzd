library(fBasics)
library(markdown)
############ 1 uzdavinys #######
data_1.1 <- read.table("d-ibm3dx7008.txt",header=T) 
ibm <- data_1.1[,2]
sibm <- ibm*100
basicStats(sibm)
