#
# Merge all raw data to one file~
#
#
# A Comprehensive Investigation of the Role of Imbalanced Learning for Software Defect Prediction}
# author{Qinbao Song, Yuchen Guo and Martin Shepperd}
# Email: wispcat@hotmail.com

softMetrics <- c("CK", "NET", "PROC", "CK_NET", "CK_PROC", "NET_PROC", "CK_NET_PROC")
datasets <- data.frame(
    Dataset=c("ant-1.3", "ant-1.4", "ant-1.5", "ant-1.6",
             "camel-1.0", "camel-1.2", "camel-1.4", "camel-1.6",
             "ivy-2.0", "jedit-3.2", "jedit-4.0", "jedit-4.1", 
             "jedit-4.2", "jedit-4.3", "log4j-1.0", "poi-2.0", 
             "synapse-1.0", "synapse-1.1", "synapse-1.2","velocity-1.6",
             "xerces-1.2", "xerces-1.3", "Eclipse_JDT_Core", "Eclipse_PDE_UI",
             "Equinox_Framework", "Lucene", "Mylyn"),
    ModuleNum = c(125,  178,  293,  351,  339,  608,  872,  965,  352,  
                  272,  306,  312,  367,  492,  135,  314,  157,  222,
                  256,  229,  440,  453,  997, 1497,  324,  691, 1862),
    DefectNum = c(20,  40,  32,  92,  13, 216, 145, 188,  40,  
                  90,  75,  79,  48,  11,  34,  37,  16,  60,
                  86,  78,  71,  69,  206, 209, 129,  64, 245)
    )

datasets$DefectRate <- datasets$DefectNum/datasets$ModuleNum
datasets$ImbLevel <- (datasets$ModuleNum-datasets$DefectNum)/datasets$DefectNum

print(datasets$ImbLevel)
  
m_data <- NULL
cnt <- 0
for (mtri in softMetrics){
  cnt <- cnt + 1
  path <- paste(cnt, mtri, sep = "_")
  for (di in 1:length(datasets$Dataset)){
    file <- paste(datasets[di,]$Dataset, "--", mtri, ".csv", sep="")
    print(file)
    data1<-read.csv(paste(path, "output", file,sep="/"))
    tmp1<- (data1$TP+data1$FP)*(data1$TP+data1$FN)*(data1$TN+data1$FP)*(data1$TN+data1$FN)
    data1$MCC <- ifelse(tmp1>0, (data1$TP*data1$TN-data1$FP*data1$FN)/sqrt(tmp1), 0)
    data2<- aggregate(cbind(AUC, MCC)~Dataset+Metrics+Learner+ImbMethod, data=data1, mean)
    data2$ModuleNum <- rep(datasets[di,]$ModuleNum, length(data2$Dataset))
    data2$DefectNum <- rep(datasets[di,]$DefectNum, length(data2$Dataset))
    data2$DefectRate <- rep(datasets[di,]$DefectRate, length(data2$Dataset))
    data2$ImbLevel <- rep(datasets[di,]$ImbLevel, length(data2$Dataset))
    m_data <- rbind(m_data, data2)
  }
}
summary(m_data)
write.csv(m_data, file = "rawdata_all.csv")
