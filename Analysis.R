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

mmb <- update(mm1, .~. - Group:months)

mmb1 <- update(mmb, .~. - Group)
display(mmb1)
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
###########
#### Baseline, no Group
b.b1 <- fixef(mmb1)
Months <- gl(7,1,21,labels=seq(0,18,by=3))
Months <- as.numeric(Months)
MeanNurseScore <- b.b1[1] + b.b1[3]*Months
b1.plot.dta <- data.frame(MeanNurseScore,Months)
ggplot(b1.plot.dta, aes(x=Months,y=MeanNurseScore)) + geom_line() + theme_bw() +
  ylab("Mean Nursing Score")

### Baseline
mth <- seq(0,18,by=3)
b.base <- fixef(mmb)
Control <- b.base[1] + b.base[5]*mth
BasicFB <- Control + b.base[3] 
FBPlus <- Control + b.base[4]
Months <- gl(7,1,21,labels=seq(0,18,by=3))
Months <- as.numeric(Months)
Group <- gl(3,7,labels=c("Control","Basic Feedback","Feedback Plus"))

base.plot.dta <- data.frame(Months,MeanNurseScore=c(Control,BasicFB,FBPlus),Group)

ggplot(base.plot.dta,aes(x=Months,y=MeanNurseScore,colour=Group)) + geom_line()+
  ylab("Mean Nursing Score") + theme_bw()

e1 <- allEffects(mm1,xlevels=list(months=c(0,3,6,9,12,15,18)))
e1[2]

e2 <- effect('Group:months',mm1,xlevels=list(months=c(0,3,6,9,12,15,18)))

plot(e2)

Months <- gl(7,3,labels=seq(0,18,by=3))
Months <- as.numeric(Months)
MeanNurseScore <- e2$fit
Group <- gl(3,1,21,labels=c("Control","Basic Feedback","Feedback Plus"))
eff.plot.dta <- data.frame(Months,MeanNurseScore,Group)
ggplot(eff.plot.dta,aes(x=Months,y=MeanNurseScore,colour=Group)) + geom_line()+
  ylab("Mean Nursing Score") + theme_bw()

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

