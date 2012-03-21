library(foreign)
library(arm)
library(ggplot2)
library(car)
library(effects)
#setwd("~/Feedback study")
## Read data provided by Rachel
dta <- read.spss("Nurse feedback ward time level data.sav",to.data.frame=TRUE)


### Remove redundant levels from factors
trust <- (dta[,"Trustnum"])
dta[,"Trustnum"] <- trust[, drop=TRUE] 
ward <- dta[,"Wardname"]
dta[,"Wardname"] <- ward[, drop=TRUE]
group <- dta[,"Group"]
dta[,"Group"] <- group[, drop=TRUE]
xtabs(~Trustnum, data=dta)
xtabs(~Wardname, data=dta)
xtabs(~Group, dta)
lvs <- c("Control","BasicFeedback","FeedbackPlus")
levels(dta[,"Group"]) <- lvs

## Remove single case from Lewisham 
ix <- dta$Trustnum == "The Lewisham Hospital NHS Trust"
dta <- dta[!ix,]

## Add time variable
ix2 <- order(dta[,1],dta[,2])
dta <- dta[ix2,]
months <- read.csv("Time.csv")
dta2 <- data.frame(dta,months=months[,"months"])

#dart <- dta2$Trustnum == "Dartford and Gravesham NHS Trust"
#m1 <- lm(MeanNursingScore ~ Trustnum  + Group * months, data=dta2)
#display(m1)
#lht(m1,"GroupBasicFeedback:months = GroupFeedbackPlus:months")


###################
## Regression reported in paper
###############################
mm1 <- lmer(MeanNursingScore ~ Trustnum  + Group*months + (1|Wardname), data=dta2)
display(mm1)

#lht(mm1,"GroupBasicFeedback:months = GroupFeedbackPlus:months")
#
#wave <- factor(dta$time)
#m2 <- lm(MeanNursingScore ~ Trustnum + Group * wave, data=dta)
#display(m2)

nd <- data.frame(months=1:6,Group="FeedbackPlus",Trustnum="Dartford and Gravesham NHS Trust")
p1 <- predict(m1,nd)
plot(1:6,p1)

##########################
## Effects plot used in paper
###########################
e1 <- allEffects(mm1,xlevels=list(months=c(0,3,6,9,12,15,18)))
e1[2]


plot(e1)

pp <- ggplot(dta2, aes(x=months,y=MeanNursingScore, group=Wardname)) + geom_line() + facet_wrap( ~ Group)
pp


lm(MeanNursingScore ~  Trustnum + months , data=dta2,subset=dta2$Group=="Control")
lm(MeanNursingScore ~  Trustnum + months , data=dta2,subset=dta2$Group=="BasicFeedback")
lm(MeanNursingScore ~  Trustnum + months , data=dta2,subset=dta2$Group=="FeedbackPlus")

# Dartford only
dart <- dta2$Trustnum == "Dartford and Gravesham NHS Trust"
m1dart <- lm(MeanNursingScore ~ Group * months, data=dta2, subset=dart)
display(m1dart)
lht(m1dart,"GroupBasicFeedback:months = GroupFeedbackPlus:months")
mm1dart <- lmer(MeanNursingScore ~ Group*months + (1|Wardname), data=dta2, subset=dart)
summary(mm1dart)
lht(mm1dart,"GroupBasicFeedback:months = GroupFeedbackPlus:months")

e1 <- allEffects(mm1dart,xlevels=list(months=c(0,3,6,9,12,15,18)))
e1[2]

plot(e1,xlab="Months",ylab="Mean Nursing Score",main="Group x months effect plot",alternating=FALSE)

pp <- ggplot(dta2, aes(x=months,y=MeanNursingScore, group=Wardname)) + geom_line(aes(colour=Trustnum)) + facet_wrap( ~ Group,nrow=2)
pp

lm()

