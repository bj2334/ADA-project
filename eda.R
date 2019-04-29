#### read in data ####
heart <- read.csv('Framingham Heart Data.csv')
#### format columns (type I censoring) ####
heart$d <- heart$AgeAtDeath - heart$AgeAtStart
heart$c <- ifelse(heart$Status == 'Alive', 0, 1)
tc <- max(heart$d, na.rm = T)
heart$x <- ifelse(is.na(heart$d), tc, heart$d)
#### plots ####
library(survival)
km <- survfit(Surv(heart$x, heart$c) ~ 1, type = 'kaplan-meier')
## survival plot
plot(km$time,km$surv, type="s",xlab="Time",ylab="Survival")
skm <- summary(km)
skm[4]
km.df <- summary(km)
head(heart)
