source("importdata.R")


triticum <- as.data.frame(head(data.final,24))
triticum

lolium <- as.data.frame(tail(data.final,24))
lolium


lmMod <- lm(dry.weight ~ treatment, data=triticum)

plot(lmMod)
