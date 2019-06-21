#Tyler Aman
#psych 401 R data
#working Directory
setwd("~/Desktop/Umich 2017/Psych 401/Emily")
install.packages('psy')
library('psy')
#downlodaing data
filepath = list.files()[9]
data = read.csv(filepath)
attach(data)
data[,c(23,26:27,35:36)]
#means and SDs for warmth, SDO and competency questions
mean(Warm_Wom_1, na.rm = T);sd(Warm_Wom_1, na.rm = T)
mean(Warm_Pol_1, na.rm = T);sd(Warm_Pol_1, na.rm = T)
mean(SDO_M, na.rm = T);sd(SDO_M, na.rm = T);
mean(competency_mean, na.rm = T);sd(competency_mean, na.rm = T);cronbach(data[,23:37]) 
mean(feminine_mean, na.rm = T);sd(feminine_mean, na.rm = T);cronbach(data[,c(24,28:30,37)])
mean(masculine_mean, na.rm = T);sd(masculine_mean, na.rm = T);cronbach(data[,c(23,26:27,35:36)])
mean(Neutral_mean, na.rm = T);sd(Neutral_mean, na.rm = T);cronbach(data[,c(25,31:34)])
#overall model

model_overall = lm(competency_mean ~ SDO_M)
summary(model_overall)
plot(model_overall)
plot(SDO_M, competency_mean, main = "SDO's effect on overall policy competency", 
     xlab = "Mean SDO" , ylab = "Overall Percieved Competency", col = "firebrick")
abline(model_overall, col = "darkorange")

#model for feminine policies
model_Fcompency = lm(feminine_mean ~ SDO_M)
summary(model_Fcompency)
plot(model_Fcompency)
plot(SDO_M, feminine_mean, main = "SDO's effect on feminine policy competency", 
     xlab = "Mean SDO" , ylab = "Feminine Policy Competency", col = "forestgreen")
abline(model_overall, col = "darkblue")

#model for masculine policies
model_Mcompency = lm(masculine_mean ~ SDO_M)
sumary(model_Mcompency)
plot(model_Mcompency)
plot(SDO_M, masculine_mean, main = "SDO's effect on masculine policy competency", 
     xlab = "Mean SDO" , ylab = "Masculine Policy Competency", col = "hotpink")
abline(model_overall, col = "chocolate")

#model for neutral policies
model_Ncompency = lm(Neutral_mean ~ SDO_M)
summary(model_Ncompency)
plot(model_Ncompency)
plot(SDO_M, Neutral_mean, main = "SDO's effect on neutral policy competency", 
     xlab = "Mean SDO" , ylab = "Neutral Policy Competency", col = "steelblue")
abline(model_overall, col = "darkgreen")

