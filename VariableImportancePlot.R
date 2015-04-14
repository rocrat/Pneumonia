#Make importance plot from pilot coefs trained on Lasso and Grouped Lasso
cfs<-read.csv("C:/Projects/Pneumonia/Pilot_coefs.csv")
#convert to long form
lasso<-cfs[,1:2]
names(lasso)[2]<-"Coefficient"
lasso$Type<-"LASSO"
glasso<-cfs[c(1,3)]
names(glasso)[2]<-"Coefficient"
glasso$Type<-"Grouped LASSO"
csflong<-rbind(lasso,glasso)
csflong$imp<-ifelse(csflong$Coefficient==0,"No","Yes")

names(csflong)[1]<-"Variable"
library(ggplot2)

ggplot(data=csflong[csflong$Variable!="(Intercept)",],aes(x=Variable,y=Coefficient))+geom_point(size=3, aes(color=imp))+geom_hline(yintercept=0) +ylab("Linear Coefficient")+scale_color_manual("Non-zero\nCoefficient", values=c("red","blue"))+facet_wrap(~Type, ncol=1)+ theme(axis.text.x=element_text(angle=-50, hjust=0))
