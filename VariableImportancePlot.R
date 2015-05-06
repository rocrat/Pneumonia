#Make importance plot from pilot coefs trained on Lasso and Grouped Lasso
cfs<-read.csv("C:/Projects/Pneumonia/Pilot_coefs_w_names.csv")
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

#map axis text to colors can only figure out how to do this manually at this point
myColors<-c(rep("black",9),rep("blue",10), rep("black",1), rep("blue",3), "black", "blue", "black", "blue", rep("black",2), rep("blue",4), rep("black",2), "blue")
cfs<-cfs[order(cfs$X),]
myColors<-ifelse(abs(cfs$Grouped.Coef)>.1,"blue","black")

ggplot(data=csflong[csflong$Variable!="(Intercept)" & csflong$Type=="Grouped LASSO",],aes(x=Variable,y=Coefficient))+geom_point(size=3, aes(color=imp))+geom_hline(yintercept=0) +ylab("Penalized Linear Coefficient")+scale_color_manual("Non-zero\nCoefficient", values=c("red","blue"))+ theme(axis.text.x=element_text(angle=-50, hjust=0, color=myColors[-1], size=6))
