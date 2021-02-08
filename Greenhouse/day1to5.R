source("importdata.R")


tab <- as.data.frame("species","treatment","average length", "average fresh weight", "average")
for(i in unique(factor(data.final$species))) { 
for(j in 1:4) { 
a1 <- data.final[ which(data.final$species ==i & data.final$treatment == j),]

tab <-as.data.frame(tab,c(i,j,mean(a1$dry.weight),mean(a1$final.length),mean(a1$fresh.weight)))


}}