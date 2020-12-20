setwd("~/Dropbox/Data Science Semester 2/Reg Analysis/Labs")
install.packages("readxl")
install.packages(VIM)
library("readxl")
library(VIM)

#Section A EDA
boxplot(wine$quality, col = 'pink',outcol= 'red', main = 'Distribution of Quality Scores',ylab = 'Score')
hist(wine$quality, col = 'pink', main = 'Distribution of Quality Scores', xlab = 'Quality Score',ylab ='Count')


boxplot(wine$sulphur, col = 'yellow',outcol= 'red', main = 'Distribution of Sulphur Content', ylab ='Sulphur Concentration')
hist(wine$sulphur, col = 'yellow', main = 'Distribution of Sulphur Content', xlab = 'Sulphur Concentration',ylab ='Count')
plot(wine$quality~wine$sulphur)
abline(modelq)
pairs(wine[,c(1,2)], pch = 21,bg=c("green3"))
df = as.data.frame(wine)
modelq = lm(df$quality~df$sulphur,data = df)

predict(modelq, new, interval = 'prediction', se.fit = T)
predict(modelq, new, interval = 'confidence', se.fit = T)

#out pops the lwr & upr confidence intervals for height given the age you input, you can scale it up by adding to the new data vector:

new = data.frame(Sulphur = c(100,190)) # defining your explanatory variable here is vital, otherwise you won't have the column name Age and predict/confidence predict() will just use it's training data
# really cool, makes a sequence of values between your max & min
Age_grid = seq(min(df$quality), max(df$quality),by=8.6/31)

new = data.frame(Sulphur = c(10.5,19.5)) # defining your explanatory variable here is vital, otherwise you won't have the column name Age and predict/confidence predict() will just use it's training data
# really cool, makes a sequence of values between your max & min
Age_grid = seq(min(df$sulphur), max(df$sulphur),by=.1)

dist_ci= predict(modelq, newdata = data.frame(Sulphur= Age_grid), interval = "confidence", level = 0.95)

dist_pi= predict(modelq, newdata = data.frame(Sulphur = Age_grid), interval = "prediction", level = 0.95)



ylim = c(min(dist_pi), max(dist_pi)
abline(modelq)

ag = Age_grid
dc = dist_ci
dp = dist_pi

lines(ag, dc[,"lwr"], col = "blue", lwd = 1, lty = 2)

lines(ag, dc[,"upr"], col = "blue", lwd = 1, lty = 2)

plot(df$sulphur~df$quality, xlab = "Sulphur Content", ylab = "Quality Score", pch = 20, cex = .75)
dist_pi= predict(modelq, newdata = data.frame(Sulphur = Age_grid), interval = "prediction", level = 0.95)
lines(ag, dp[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(ag, dp[,"upr"], col = "red", lwd = 1, lty = 3)

dist_ci= predict(modelq, newdata = data.frame(Sulphur= Age_grid), interval = "confidence", level = 0.95)


plot(plant_model)







wine = read.csv("wine.csv")
aggr(wine)
head(wine)
aggr(cholesterol)

viscosity %>% group_by(Catalyst) %>% 
  summarise(mean_viscosity = mean(Viscosity), var_viscosity = var(Viscosity))
viscosity %>% group_by(pH) %>% 
  summarise (mean_viscosity = mean(Viscosity), var_viscosity = var(Viscosity))

# interaction plot, cool but a little unclear due to labels being obscured
interaction.plot(viscosity$pH, viscosity$Catalyst, viscosity$Viscosity)

fligner.test(Viscosity~interaction(pH,Catalyst),data= viscosity)
#both methods of instantiating the two way ANOVA
model1 = aov(Viscosity~Catalyst*pH, data = viscosity)
