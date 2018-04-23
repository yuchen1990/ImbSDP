#
# A Comprehensive Investigation of the Role of Imbalanced Learning for Software Defect Prediction}
# author{Qinbao Song, Yuchen Guo and Martin Shepperd}
# Email: wispcat@hotmail.com

#
#step[0] generate file "check_difference_17Fbe2017.csv"
#
Edata <- read.table("rawdata_20180418.csv",header=T,sep=",")
Edata$ImbGroup <- ifelse(Edata$ImbLevel>3.94,"M&H IR","Low IR")
Edata$ImbGroup <- factor(Edata$ImbGroup, levels = c("Low IR", "M&H IR"))
summary(Edata)
tempdata<-NULL
for (bi in c('C4.5','RF','IBk','Ripper','LR','NB','SVM')){
  for (mi in c('CK','NET','PROC','CK+NET','CK+PROC','NET+PROC','CK+NET+PROC')){
    
    control <- subset(Edata, Metrics==mi & Classifier==bi & ImbLearnerType=='none')
    print(mi)
    for (imb in c('Bag','Bst','US','OS','UOS','SMOTE','COS','EM1v1',
                  'UBag','OBag','UOBag','SBag','UBst','OBst','UOBst','SBst')){
      treatment <- subset(Edata, Metrics==mi & Classifier==bi & ImbLearnerType==imb)
      #print(imb)
      treatment$dset<-control$Dataset
      treatment$bMCC<-control$MCC
      tempdata<-rbind(tempdata, treatment)
    }}}
sum(tempdata$dset!=tempdata$Dataset) #check match
#data3<-subset(tempdata, tempdata$dset!=tempdata$Dataset)
#summary(data3)
#xdata<-subset(tempdata,select = -c(DefectRate, Gmean, AUC, Bal, F1, Recall, SLoe))
tempdata$dMCC<-tempdata$MCC-tempdata$bMCC
summary(tempdata)
write.csv(tempdata,"check_difference_18APR2018.csv", row.names=FALSE)

#
#step[1] Distribution of IR
#
Edata <- read.table("SDP__106_public_datasets.csv",header=T,sep=",")
library(ggplot2)
data1<-subset(Edata, Edata$Chosen=="Yes")
m1<-median(log(data1$IR,2))
m2<-median(Edata$IR)
ggplot(Edata, aes(x=log(IR,2))) + geom_histogram(aes(fill=Chosen),bins=40) + geom_vline(xintercept = log(m2,2),color="purple",linetype = 2)
print(m2)
print(min(Edata$IR))
print(max(Edata$IR))
library(fBasics)
basicStats(Edata$IR)


#
#step[2] MCC vs log(IR,2)
#
Edata <- read.table("rawdata_18APR2018.csv",header=T,sep=",")
library(ggplot2)
m2<-3.94
Edata$IR<-Edata$ImbLevel
data1<-subset(Edata, ImbLearner=="N")
summary(data1)
p1<-ggplot(data1, aes(log(IR,2), MCC))+geom_point()+scale_shape(solid = FALSE)+geom_smooth(method="loess")
p1+geom_vline(xintercept = log(m2,2),color="purple",linetype = 2)+annotate("text", label = "Low IR     <-------|------->     Medium+ IR", x = 2.15, y = 0.7, size = 4, colour = "purple")
library(WRS2)
pbcor(data1$MCC, log(data1$IR,2))

#for Reviewer 3
data1<-subset(Edata, ImbLearner=="N" & ImbLevel<10)

p2<-ggplot(Edata, aes(log(IR,2), MCC)) #here
p2+geom_point(aes(colour=ImbLearner))+scale_shape(solid = FALSE)+
  geom_smooth(method="loess",aes(colour = ImbLearner))+ geom_vline(xintercept = log(m2,2),color="purple",linetype = 2)

#
#step[3] basic stat
#
Edata <- read.table("check_difference_18APR2018.csv",header=T,sep=",")

colors=c(rep("dimgray",13),rep("white",27))
hist(Edata$dMCC,breaks=40, xlab = "Difference on MCC",main="Histogram of Difference",col=colors)
#abline(v=0,col="Blue")
print(sum(Edata$dMCC<0)/length(Edata$dMCC)) #negative effect percentage
library(fBasics)
basicStats(Edata$dMCC)
library(chemometrics)
sd_trim(Edata$dMCC,trim=0.2) #trimmed sd
source("Rallfun-v33.txt")
res<-trimpb(Edata$dMCC,tr=.2,alpha=.05,nboot=2000,pop=1) #trimmed mean and CI
print(res)

#
#step[4] boxplot
#

Edata <- read.table("check_difference_18APR2018.csv",header=T,sep=",")
m2=3.94
Edata$ImbGroup <- ifelse(Edata$ImbLevel>m2, "Medium+ IR" ,"Low IR")
Edata$ImbGroup <- factor(Edata$ImbGroup, levels = c("Low IR", "Medium+ IR"))
#Edata$ImbGroup <- ifelse(Edata$ImbLevel>3.94, ifelse(Edata$ImbLevel>10, "High IR", "Moderate IR") ,"Low IR")
#Edata$ImbGroup <- factor(Edata$ImbGroup, levels = c("Low IR", "Moderate IR", "High IR"))
summary(Edata)
library(ggplot2)
ggplot(Edata, aes(ImbGroup, dMCC))+geom_boxplot(aes(colour=ImbGroup),notch=TRUE)

#here we plot all the boxplots ;-)
Edata$Classifier <- factor(Edata$Classifier, levels = c("SVM","C4.5","LR","Ripper","IBk","RF","NB"))
ggplot(Edata, aes(Classifier, dMCC))+geom_boxplot(aes(colour=ImbGroup),notch=TRUE)

Edata$Metrics <- factor(Edata$Metrics, levels = c("CK","NET","PROC","CK+NET","CK+PROC","NET+PROC","CK+NET+PROC"))
ggplot(Edata, aes(Metrics, dMCC))+geom_boxplot(aes(colour=ImbGroup),notch=TRUE)+
  labs(x='Input Metrics')+theme(axis.text.x=element_text(angle=10))

Edata$ImbLearnerType <- factor(Edata$ImbLearnerType, levels = c('UBag','UOBag','EM1v1','SBst','SBag','UBst','OBag','SMOTE','COS',
                                                                'OS','UOS','UOBst','US','OBst','Bst','Bag'))
ggplot(Edata, aes(ImbLearnerType, dMCC))+geom_boxplot(aes(colour=ImbGroup),notch=TRUE,show.legend=FALSE)+labs(x='Imbalanced Learning Method')

#
#step[5] trimmed means of dMCC
#
source("Rallfun-v33.txt")
library(xtable)
Edata <- read.table("check_difference_18APR2018.csv",header=T,sep=",")
library(psych)
library(orddom)

m2=3.94
Edata$ImbGroup <- ifelse(Edata$ImbLevel>m2, "Medium+ IR" ,"Low IR")
Edata$ImbGroup <- factor(Edata$ImbGroup, levels = c("Low IR", "Medium+ IR"))
summary(Edata)

temptab <- NULL  
for (gi in c("Low IR", "Medium+ IR")){
  print(gi)
  data1<- subset(Edata, Edata$ImbGroup==gi)
  res<-trimpb(data1$dMCC,tr=.2,alpha=.05,nboot=2000,pop=1)
  str1<-paste(round(res$estimate, digits = 3)," (",round(res$ci[1], digits = 3),
              ", ",round(res$ci[2], digits = 3), ")" , sep = "")
  res<-orddom(data1$bMCC,data1$MCC,paired=TRUE)
  str2<-paste(round(as.numeric(res[11]), digits = 3)," (",round(as.numeric(res[13]), digits = 3),
              ", ",round(as.numeric(res[14]), digits = 3), ")" , sep = "")
  temptab<-rbind(temptab, c(gi, str1, str2))
}
print(temptab)
tempLaTeXtab1 <- xtable(temptab,caption="-")
print(tempLaTeXtab1)

#classifier
temptab <- NULL  
for (gi in c("Low IR", "Medium+ IR")){
  for (bi in c("SVM","C4.5","LR","Ripper","IBk","RF","NB")){
    print(bi)
    data1<- subset(Edata, Edata$Classifier==bi & Edata$ImbGroup==gi)
    res<-trimpb(data1$dMCC,tr=.2,alpha=.05,nboot=2000,pop=1)
    str1<-paste(round(res$estimate, digits = 3)," (",round(res$ci[1], digits = 3),
                ", ",round(res$ci[2], digits = 3), ")" , sep = "")
    res<-orddom(data1$bMCC,data1$MCC,paired=TRUE)
    str2<-paste(round(as.numeric(res[11]), digits = 3)," (",round(as.numeric(res[13]), digits = 3),
                                        ", ",round(as.numeric(res[14]), digits = 3), ")" , sep = "")
    temptab<-rbind(temptab, c(bi, str1, str2))
  }
}
print(temptab)
tempLaTeXtab1 <- xtable(temptab,caption="-")
print(tempLaTeXtab1)

#metrics
temptab <- NULL  
for (gi in c("Low IR", "Medium+ IR")){
  for (mi in c('CK','NET','PROC','CK+NET','CK+PROC','NET+PROC','CK+NET+PROC')){
    print(mi)
    data1<- subset(Edata, Edata$Metrics==mi & Edata$ImbGroup==gi)
    res<-trimpb(data1$dMCC,tr=.2,alpha=.05,nboot=2000,pop=1)
    str1<-paste(round(res$estimate, digits = 3)," (",round(res$ci[1], digits = 3),
                ", ",round(res$ci[2], digits = 3), ")" , sep = "")
    res<-orddom(data1$bMCC,data1$MCC,paired=TRUE)
    str2<-paste(round(as.numeric(res[11]), digits = 3)," (",round(as.numeric(res[13]), digits = 3),
                ", ",round(as.numeric(res[14]), digits = 3), ")" , sep = "")
    temptab<-rbind(temptab, c(mi, str1, str2))
  }
}
print(temptab)
tempLaTeXtab1 <- xtable(temptab,caption="-")
print(tempLaTeXtab1)

#c('UBag','UOBag','EM1v1','SBag','OBag','UBst','SMOTE','COS','OS','UOS','UOBst','US','SBst','OBst','Bst','Bag')

#imbLearner
temptab <- NULL  
for (gi in c("Low IR", "Medium+ IR")){
  for (imb in c('UBag','UOBag','EM1v1','SBag','OBag','UBst','SMOTE','COS',
                'OS','UOS','UOBst','US','SBst','OBst','Bst','Bag')){
    print(imb)
    data1<- subset(Edata, Edata$ImbLearnerType==imb & Edata$ImbGroup==gi)
    res<-trimpb(data1$dMCC,tr=.2,alpha=.05,nboot=2000,pop=1)
    str1<-paste(round(res$estimate, digits = 3)," (",round(res$ci[1], digits = 3),
                ", ",round(res$ci[2], digits = 3), ")" , sep = "")
    res<-orddom(data1$bMCC,data1$MCC,paired=TRUE)
    str2<-paste(round(as.numeric(res[11]), digits = 3)," (",round(as.numeric(res[13]), digits = 3),
                ", ",round(as.numeric(res[14]), digits = 3), ")" , sep = "")
    temptab<-rbind(temptab, c(imb, str1, str2))
  }
}
print(temptab)
tempLaTeXtab1 <- xtable(temptab,caption="-")
print(tempLaTeXtab1)

#
#step[6] big table
#
Edata <- read.table("check_difference_18APR2018.csv",header=T,sep=",")
# Win/Draw/Loss counts
WDLtable <- NULL
dv2      <- NULL

for (bi in c("SVM","C4.5","LR","Ripper","IBk","RF","NB")){
  for (mi in c('CK','NET','PROC','CK+NET','CK+PROC','NET+PROC','CK+NET+PROC')){
    dv  <- cbind(bi, mi)
    for (imb in c('UBag','UOBag','EM1v1','SBst','SBag','UBst','OBag','SMOTE',
                  'COS','OS','UOS','UOBst','US','OBst','Bst','Bag')){
      data1 <- subset(Edata, Metrics==mi & Classifier==bi & ImbLearnerType==imb)
      Wcount <- sum(data1$MCC>data1$bMCC)
      Dcount <- sum(data1$MCC==data1$bMCC)
      Lcount <- sum(data1$MCC<data1$bMCC)
      dv<-cbind(dv, paste(Wcount, Dcount, Lcount, sep="/"))
      res<-wilcox.test(data1$MCC,data1$bMCC,paired=TRUE,alternative="greater",exact=FALSE)
      dv2<-rbind(dv2,res[3])
    }
    WDLtable<-rbind(WDLtable,dv)
    #pvtable<-rbind(pvtable,dv2)
  }
}

library(mutoss) #BY p-value correction
res<-BY(dv2,0.05)
summary(res$rejected)
length(with(res,which(rejected==TRUE)))
pvtable  <- NULL
for (ri in 0:48){
  pvtable <- rbind(pvtable, c("Bi","Mi",res$rejected[(ri*16+1):(ri*16+16)]))
}
summary(pvtable)
summary(WDLtable)
WDLtable <- ifelse(pvtable=="FALSE", paste('-cellcolor{gray!25}',WDLtable,sep=""), WDLtable)
colnames(WDLtable) <- c('Classifier','Metrics',
                        'UBag','UOBag','EM1v1','SBst','SBag','UBst','OBag','SMOTE',
                        'COS','OS','UOS','UOBst','US','OBst','Bst','Bag')
#write.table(WDLtable, "wldTalbe_BY_re1.csv",sep=",")
library(xtable)
tempLaTeXtab1 <- xtable(WDLtable,caption="-")
print(tempLaTeXtab1, include.rownames=FALSE)


#END