dim(ToothGrowth)
str(ToothGrowth)
ToothGrowth
table(ToothGrowth$supp,ToothGrowth$dose)
summary(ToothGrowth)

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


tg<- ToothGrowth%>%group_by(supp,dose)
levels(tg$supp)<-c("Orange Juice","Ascorbic Acid")
tg
g1<-ggplot(tg,aes(x=len,fill=factor(dose)))
g1+geom_density(alpha=0.2)+theme_bw()+labs(title = "Guinea pig tooth length by dossage",x="Tooth Length")

#although the graph vaguely resembles normal distribution ,we will assume that there is normal distribution of tooth length

tg_supp_dose<-summarise(tg,mean_len = mean(len))
tg_supp_dose<-as.data.frame(tg_supp_dose)
levels(tg_supp_dose$supp)<-c("Orange Juice","Ascorbic Acid")
tg_supp_dose
#basic summary of the data
g<-ggplot(tg,aes(x=len,color=factor(dose),fill = factor(dose)))
g<-g+geom_density(alpha=0.4)+facet_grid(dose~supp) + theme_bw() + geom_vline(data = tg_supp_dose, aes(xintercept= mean_len)) 
g<-g+geom_text(data = tg_supp_dose,aes(x=mean_len,label = mean_len),y = 0.1, angle = 90, vjust = -0.2,color="black",size=3.5)
g+labs(title = "Guinea pig tooth length by dossage for each type of supplement",x ="Tooth Length")

#Hypo 1:
#orange juice and ascorbic deliever the same growth rate of tooth 
hypo1<-t.test(len~supp,data = tg)
hypo1$conf.int
hypo1$p.value

#the confidence interval includes 0 so we cant deny that they can offer same growth rate.
#but the p value is larger than 0.05 so we fail to reject the null hypo ,hence we accept null hypo

#Hypo2:
#for dosage 0.5 the tooth growth rate is same 
hypo2<-t.test(len~supp,data = subset(tg,dose==0.5))
hypo2$conf.int  
hypo2$p.value
#since the confidence interval doesnt include 0 and the p valuse is also less than 0.05 so we reject null hypo2
#alt hypo that 0.5 mg dosage of orange juice give more growth than 0.5 mg of ascorbic acid is accepted

#Hypo3:
#for dosage 1 the tooth growth rate is same 
hypo3<-t.test(len~supp,data = subset(tg,dose==1))
hypo3$conf.int  
hypo3$p.value
#since the confidence interval doesnt include 0 and the p valuse is also less than 0.05 so we reject null hypo3
#alt hypo that 1.0 mg dosage of orange juice give more growth than 1.0mg of ascorbic acid is accepted

#Hypo4:
#for dosage 2mg the tooth growth rate is same 
hypo4<-t.test(len~supp,data = subset(tg,dose==2))
hypo4$conf.int  
hypo4$p.value
#since the confidence interval  include 0 and the p valuse is also well beyond  0.05 so we fail to reject null hypo




