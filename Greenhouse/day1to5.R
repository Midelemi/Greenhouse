source("importdata.R")
library("ggplot2")
library("dplyr")
library("ggpubr")
library(car)
library(agricolae)
data.final
TA2 <- aggregate(list(data.final$dry.weight, data.final$final.length, data.final$fresh.weight,data.final$root.shoot, data.final$nr.leaves, data.final$pr.growth, data.final$aerial.length, data.final$root.length), 
                 by = list(data.final$treatment, data.final$species),
                 FUN = function(x) c(mean = mean(x))
                 )
TA2 <- as.data.frame(as.matrix(TA2))
names(TA2) <- c("treatment", "species", "dry.weight","final.length","fresh.weight","root.shoot","nr.leaves","pr.growth", "aerial.length","root.length")

barplot(height=as.numeric(TA2$dry.weight), names=TA2$treatment)



### Triticum
dt <- subset(data.final, data.final$species == "triticum")
## 1. Dry weight


res.aov <- aov(lm(dry.weight ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(dry.weight ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.1166
shapiro.test(x = aov_residuals )
#p-value = 0.03 > 0.05 we can NOT assume normality
summary(res.aov) #Is useless here we can not assume normality

kruskal.test(dry.weight ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.


anova <- lm(dry.weight ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - dry weight", ylab = "Dry weight", xlab = "Type of spray", sub = "two groups of effects: a and b")


##2.Fresh weight

res.aov <- aov(lm(fresh.weight ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(fresh.weight ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.2145
shapiro.test(x = aov_residuals )
#p-value = 0.24 > 0.05 we can assume normality
summary(res.aov) 
#With a P value < 0.05 we prove there there are NOT significant difference

pairwise.t.test(dt$fresh.weight, dt$treatment ,
                p.adjust.method = "BH")

#3.Length


res.aov <- aov(lm(final.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(final.length ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.56
shapiro.test(x = aov_residuals )
#p-value = 0.4311 > 0.05 we can assume normality
summary(res.aov) 
#With a P value < 0.05 we prove there there are  significant difference
pairwise.t.test(dt$final.length, dt$treatment ,
                p.adjust.method = "BH")

##4.Root shoot
dt <- subset(data.final, data.final$species == "triticum")
res.aov <- aov(lm(root.shoot ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )




leveneTest(root.shoot ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.41
shapiro.test(x = aov_residuals )
#p-value = 0.1232 > 0.05 we can  assume normality
summary(res.aov) #NO SIGNIFICANT Differences
kruskal.test(root.shoot ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.


anova <- lm(root.shoot ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - root shoot", ylab = "%", xlab = "Type of spray", sub = "two groups of effects: a and b")





###4.number of leaves

dt <- subset(data.final, data.final$species == "triticum")



res.aov <- aov(lm(nr.leaves ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(nr.leaves ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.62
shapiro.test(x = aov_residuals )
#p-value = 0.082 > 0.05 we canassume normality
summary(res.aov) 

pairwise.t.test(dt$nr.leaves, dt$treatment ,
                p.adjust.method = "BH")

kruskal.test(dry.weight ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.


anova <- lm(dry.weight ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - dry weight", ylab = "Dry weight", xlab = "Type of spray", sub = "two groups of effects: a and b")

##root length

dt <- subset(data.final, data.final$species == "triticum")


res.aov <- aov(lm(root.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(root.length ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.1166
shapiro.test(x = aov_residuals )
#p-value = 0.18 > 0.05 we can NOT assume normality
summary(res.aov) #Is useless here we can not assume normality

kruskal.test(root.length ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

anova <- lm(root.length ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - root length", ylab = "Dry weight", xlab = "Type of spray")

##aerial.length

dt <- subset(data.final, data.final$species == "triticum")


res.aov <- aov(lm(aerial.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(aerial.length ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.032


kruskal.test(aerial.length ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

anova <- lm(aerial.length ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - aerial.length", ylab = "Dry weight", xlab = "Type of spray")





###Lollium
dt <- subset(data.final, data.final$species == "lolium")
##1.Dry weight

res.aov <- aov(lm(dry.weight ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(dry.weight ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.5332
shapiro.test(x = aov_residuals )
#p-value = 0.02255 > 0.05 we can NOT assume normality
#summary(res.aov) 


anova <- lm(dry.weight ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Lollium - dry weight", ylab = "Dry weight", xlab = "Type of spray", sub = "two groups of effects: a and b")

##2.fresh.weight

dt <- subset(data.final, data.final$species == "lolium")
res.aov <- aov(lm(fresh.weight ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(fresh.weight ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.2145
shapiro.test(x = aov_residuals )
#p-value = 0.2455 > 0.05 we can NOT assume normality
summary(res.aov) 
#With a P value=0.6 > 0.05 we prove there there are NOT significant difference

anova <- lm(fresh.weight ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Lollium - dry weight", ylab = "Dry weight", xlab = "Type of spray", sub = "two groups of effects: a and b")


##3.final.length
res.aov <- aov(lm(final.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(final.length  ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.5591
shapiro.test(x = aov_residuals )
#p-value = 0.4311 > 0.05 we can NOT assume normality
summary(res.aov) 
#P value = 0.27, no significant differences



##4.Root shoot
dt <- subset(data.final, data.final$species == "lolium")
res.aov <- aov(lm(root.shoot ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )




leveneTest(root.shoot ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.17
shapiro.test(x = aov_residuals )
#p-value = 0.7 > 0.05 we can  assume normality
summary(res.aov) #Significant difference

pairwise.t.test(dt$root.shoot, dt$treatment ,
                p.adjust.method = "BH")
#a a a b


##nr.leaves
dt <- subset(data.final, data.final$species == "lolium")

res.aov <- aov(lm(nr.leaves ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )




leveneTest(nr.leaves ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.74
shapiro.test(x = aov_residuals )
#p-value = 0.6333 > 0.05 we can  assume normality
summary(res.aov) #Significant difference

pairwise.t.test(dt$nr.leaves, dt$treatment ,
                p.adjust.method = "BH")

kruskal.test(nr.leaves ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

anova <- lm(nr.leaves ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - root length", ylab = "Dry weight", xlab = "Type of spray")


##root length

dt <- subset(data.final, data.final$species == "lolium")


res.aov <- aov(lm(root.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(root.length ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.1166
shapiro.test(x = aov_residuals )
#p-value = 0.18 > 0.05 we can NOT assume normality
summary(res.aov) #Is useless here we can not assume normality

kruskal.test(root.length ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

anova <- lm(root.length ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - root length", ylab = "Dry weight", xlab = "Type of spray")

##aerial.length

dt <- subset(data.final, data.final$species == "lolium")


res.aov <- aov(lm(aerial.length ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(aerial.length ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.032


kruskal.test(aerial.length ~ treatment, data = dt)
#With a P Value smaller than 0.05
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

anova <- lm(aerial.length ~ treatment, data = dt)
agri <- HSD.test(anova, 'treatment', alpha = 0.05, group=TRUE, main = "HSD Test")
plot(agri, main = "Triticum - aerial.length", ylab = "Dry weight", xlab = "Type of spray")



#Final Length

my_bar <- barplot(as.numeric(TA2$final.length) , border=F , names.arg=TA2$species , 
                  las=1, space=c(0.25,0.25,0.25,0.25,2.5,0.25,0.25,0.25),
                  col=c("green" , "red" ,"blue4" ,  "blue2"), 
                  xlab = "Species", 
                   ylim=c(0,120) , 
                  main="Final lenght" )
#abline(v=c(4.5) , col="grey")
#text(my_bar + 60 , paste(" a") ,cex=1 )
par(mar=c(5.1, 4.1, 4.1, 11), xpd=TRUE)

legend(x="topright", legend = c("Control","Water stress","200mM", "500mM" ) , 
       col = c("green" , "red" ,"blue4" ,  "blue2") , 
       xpd=TRUE,inset=c(-1.4,-0.2),
       bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE)

#Dry weight



my_bar <- barplot(as.numeric(TA2$dry.weight) , border=F , names.arg=TA2$species , 
                  las=1, space=c(0.25,0.25,0.25,0.25,2.5,0.25,0.25,0.25),
                  col=c("green" , "red" ,"blue4" ,  "blue2"), 
                  xlab = "Species", 
                  ylim=c(0,4.5) , 
                  main="Dry weight" )
#abline(v=c(4.5) , col="grey")
#text(my_bar + 60 , paste(" a") ,cex=1 )
par(mar=c(5.1, 4.1, 4.1, 11), xpd=TRUE)

legend(x="topright", legend = c("Control","Water stress","200mM", "500mM" ) , 
       col = c("green" , "red" ,"blue4" ,  "blue2") , 
       xpd=TRUE,inset=c(-1.4,-0.2),
       bty = "n", pch=20 , pt.cex = 2, cex = 1, horiz = FALSE)


#Fresh weight

my_bar <- barplot(as.numeric(TA2$fresh.weight) , border=F , names.arg=TA2$species , 
                  las=1, space=c(0.25,0.25,0.25,0.25,2.5,0.25,0.25,0.25),
                  col=c("green" , "red" ,"blue4" ,  "blue2"), 
                  xlab = "Species", 
                  ylab = "Weight (g)",
                  ylim=c(0,6) , 
                  main="Fresh weight" )
#abline(v=c(4.5) , col="grey")
#text(my_bar + 60 , paste(" a") ,cex=1 )
par(mar=c(5.1, 4.1, 4.1, 11), xpd=TRUE)

legend(x="topright", legend = c("Control","Water stress","200mM", "500mM" ) , 
       col = c("green" , "red" ,"blue4" ,  "blue2") , 
       xpd=TRUE,inset=c(-1.4,-0.2),
       bty = "n", pch=20 , pt.cex = 2, cex = 1, horiz = FALSE)

##nr.leaves

my_bar <- barplot(as.numeric(TA2$nr.leaves) , border=F , names.arg=TA2$species , 
                  las=1, space=c(0.25,0.25,0.25,0.25,2.5,0.25,0.25,0.25),
                  col=c("green" , "red" ,"blue4" ,  "blue2"), 
                  xlab = "Species", 
                  ylab = "number of leaves",
                  ylim=c(0,12) , 
                  main="leaves (number)" )
#abline(v=c(4.5) , col="grey")
#text(my_bar + 60 , paste(" a") ,cex=1 )
par(mar=c(5.1, 4.1, 4.1, 11), xpd=TRUE)

legend(x="topright", legend = c("Control","Water stress","200mM", "500mM" ) , 
       col = c("green" , "red" ,"blue4" ,  "blue2") , 
       xpd=TRUE,inset=c(-1.4,-0.2),
       bty = "n", pch=20 , pt.cex = 2, cex = 1, horiz = FALSE)

#tab <- data.frame(matrix(ncol = 5, nrow = 20))

#colnames(tab) <- c("species","treatment","average length", "average fresh weight", "average")

#for(j in 1:4) { 
#a1 <- data.final[ which(data.final$species =="triticum" & data.final$treatment == j),]
#tab <- data.frame(tab, "triticum",j,mean(a1$dry.weight),mean(a1$final.length),mean(a1$fresh.weight))}

library("writexl")
write_xlsx(TA2,"c:\\Users\\serba\\Desktop\\Table.xlsx")
