source("importdata.R")
library("ggplot2")
library("dplyr")
library("ggpubr")
library(car)
library(agricolae)

TA2 <- aggregate(list(data.final$dry.weight, data.final$final.length, data.final$fresh.weight), 
                 by = list(data.final$treatment, data.final$species),
                 FUN = function(x) c(mean = mean(x))
                 )
TA2 <- as.data.frame(as.matrix(TA2))
names(TA2) <- c("treatment", "species", "dry.weight","final.length","fresh.weight")

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

res.aov <- aov(lm(fresh.weight ~ treatment, dt))
aov_residuals <- residuals(object = res.aov )


leveneTest(fresh.weight ~ as.character(treatment), data = dt)
#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.
#P Value 0.2145
shapiro.test(x = aov_residuals )
#p-value = 0.2455 > 0.05 we can NOT assume normality
summary(res.aov) 
#With a P value=0.6 > 0.05 we prove there there are NOT significant difference

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


my_bar <- barplot(as.numeric(TA2$final.length) , border=F , names.arg=TA2$species , 
                  las=3, space=c(0,0,0,0,2.5),
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                
                   ylim=c(0,100) , 
                  main="" )
abline(v=c(4.5) , col="grey")
text(my_bar , paste( ) ,cex=1) 
legend("bottom", legend = c("Control","Water stress","200mM", "500mM" ) , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))


#tab <- data.frame(matrix(ncol = 5, nrow = 20))

#colnames(tab) <- c("species","treatment","average length", "average fresh weight", "average")

#for(j in 1:4) { 
#a1 <- data.final[ which(data.final$species =="triticum" & data.final$treatment == j),]
#tab <- data.frame(tab, "triticum",j,mean(a1$dry.weight),mean(a1$final.length),mean(a1$fresh.weight))}

