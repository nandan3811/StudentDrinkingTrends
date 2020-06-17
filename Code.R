#Graph 1
ggplot(data=PortugeseDrinking, aes(x=Dalc)) +geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x = "Weekday Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Number of students",title = "Weekday Drinking Categories")

#Graph 2 
ggplot(data=MathDrinking, aes(x=Dalc)) +geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x = "Weekday Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Number of students",title = "Weekday Drinking Categories")

#Means
PortugeseMean <- mean(PortugeseDrinking$Dalc) #1.50
MathMean <- mean(MathDrinking$Dalc) #1.48

#Means for weekends
PortugeseMeanWeekend <- mean(PortugeseDrinking$Walc) #2.28
MathMeanWeekend <- mean(MathDrinking$Walc) #2.29

#Graph 3
ggplot(data=PortugeseDrinking, aes(x=Walc)) +geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x = "Weekend Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Number of students",title = "Weekend Drinking Categories")

#Graph 4
ggplot(data=MathDrinking, aes(x=Walc)) +geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x = "Weekend Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Number of students",title = "Weekend Drinking Categories")

#Hypothesis Testing for weekdays vs weekends
sd.weekday <- sd(PortugeseDrinking$Dalc) 
sd.weekend <- sd(PortugeseDrinking$Walc)
num.weekday <- length(PortugeseDrinking$Dalc)
num.weekend <- length(PortugeseDrinking$Walc)
z.score<-(PortugeseMeanWeekend-PortugeseMean)/sqrt((sd.weekend^2/num.weekend)+(sd.weekday^2/num.weekday))
p <- 1 -pnorm(z.score)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score, col='red') 
  #Z = 12.524
  #p = 0

#Graph 5
ggplot(PortugeseDrinking, aes(x=factor(PortugeseDrinking$Walc), y=PortugeseDrinking$absences)) + stat_summary(fun.y="mean", geom="bar") + geom_label(stat = 'summary', fun.y=mean, aes(label = round(..y.., 2)), nudge_x = 0.1, hjust = 0) +labs(x = "Weekend Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Average School Absences",title = "Weekend Drinking Consumption vs Average School Absences")

#Graph 6
ggplot(MathDrinking, aes(x=factor(MathDrinking$Walc), y=MathDrinking$absences)) + stat_summary(fun.y="mean", geom="bar") + geom_label(stat = 'summary', fun.y=mean, aes(label = round(..y.., 2)), nudge_x = 0.1, hjust = 0) +labs(x = "Weekend Drinking Consumption (1 meaning very low 5 meaning very high)", y = "Average School Absences",title = "Weekend Drinking Consumption vs Average School Absences")

#Means for Absences
PortugeseAbsenceMean <- mean(PortugeseDrinking$absences) #3.66
MathAbsenceMean <- mean(MathDrinking$absences) #5.71

#Hypothesis Testing for Absences for Math students vs Portugese Students
sd.Portugese <- sd(PortugeseDrinking$absences)
sd.Math <- sd(MathDrinking$absences)
num.Portugese <- length(PortugeseDrinking$absences)
num.Math <- length(MathDrinking$absences)
z.score<-(MathAbsenceMean-PortugeseAbsenceMean)/sqrt((sd.Portugese^2/num.Portugese)+(sd.Math^2/num.Math))
p <- 1 -pnorm(z.score)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score, col='red') 
  #Z = 4.637
  #p = .00000178 = .000178%

#Hypothesis Testing for Portugese Students who are low level drinkers vs high level drinkers for absences
mean.low <- mean(PortugeseDrinking[PortugeseDrinking$Walc <= 3,]$absences)
mean.high <- mean(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$absences)
sd.low <- sd(PortugeseDrinking[PortugeseDrinking$Walc <= 3,]$absences)
sd.high <- sd(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$absences)
num.high <- length(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$absences)
z.score<-(mean.high-mean.low)/sqrt((sd.low^2/num.low)+(sd.high^2/num.high))
p <- 1 -pnorm(z.score)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score, col='red') 
  #Z = 3.185
  #p = .000725

#Hypothesis Testing for Portugese Students who are low level drinkers vs high level drinkers for grades
mean.lowgrades <- mean(PortugeseDrinking[PortugeseDrinking$Walc <= 3,]$G3) #low drinking level
mean.highgrades <- mean(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$G3) #high drinking level
sd.lowgrades <- sd(PortugeseDrinking[PortugeseDrinking$Walc <= 3,]$G3)
sd.highgrades <- sd(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$G3)
num.lowgrades <- length(PortugeseDrinking[PortugeseDrinking$Walc <= 3,]$G3)
num.highgrades <- length(PortugeseDrinking[PortugeseDrinking$Walc > 3,]$G3)
z.score<-(mean.lowgrades-mean.highgrades)/sqrt((sd.lowgrades^2/num.lowgrades)+(sd.highgrades^2/num.highgrades))
p <- 1 -pnorm(z.score)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score, col='red') 
  #Z: 4.278
  #p: .0000095 = .00095%


#Patterns

#Students who do not study that much are more likely to be heavy drinkers:
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3),])/nrow(PortugeseDrinking) #20.34%
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$studytime ==1),])/nrow(PortugeseDrinking[PortugeseDrinking$studytime == 1,]) #32.55%
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$studytime ==1),]) #69

#Students who go out at a high rate with friends are more likely to be heavy drinkers:
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3),])/nrow(PortugeseDrinking) #20.34%
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$goout >3),])/nrow(PortugeseDrinking[PortugeseDrinking$goout > 3,]) #37.85
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$goout >3),]) #95

#Students who Fail at least 1 class and grew up in an urban area are more likely to be heavy drinkers:
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3),])/nrow(PortugeseDrinking) #20.34%
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$failure >= 1)&(PortugeseDrinking$address == "U"),])/nrow(PortugeseDrinking[(PortugeseDrinking$failure >= 1)&(PortugeseDrinking$address == "U"),]) #33.33%
nrow(PortugeseDrinking[(PortugeseDrinking$Walc > 3)&(PortugeseDrinking$failure >= 1)&(PortugeseDrinking$address == "U"),]) #20



