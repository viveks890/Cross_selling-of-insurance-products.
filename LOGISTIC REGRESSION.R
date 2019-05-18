library(readr)
library(RColorBrewer)
library(caret)
library(car)
library(ROCR)
library(ResourceSelection)
set.seed(123)
z1<-read.csv("D:\\DATA ANALYSIS\\PROJECT\\UNITED INDIA INSURANCE\\EXPLORATION AND MODELLING\\NEWDATA.csv")
names(z1)
z2<-subset(z1,select = -X)
#View(z2)
names(z2)
z2$Converted<-ifelse(z2$Converted==3,1,0)
z3<-subset(z2,select = -c(New_Product_Type,Current_Product_Type,Current_Product,Education,Income,Family_Members,Gender))
############## DATA PARTITION ###########################
index<-createDataPartition(z3$Converted,p=0.75,list=F)
train<-z3[index,]
test<-z3[-index,]
############# MODELLING ##############
m1<-glm(Converted~.,data = train,family = "binomial")
m1
summary(m1)
vif(m1)
bck<-step(m1,direction = "backward")
bck
summary(bck)
vif(bck)
pre<-predict(bck,newdata = test,type = "response")
est<-ifelse(pre>0.5,1,0)
#View(est)
res<-data.frame(org=test$Converted,est=est)
View(res)
########################## CONFUSION MATRIX ################################
x<-table(res$org,res$est)
x
sum(diag(x))/sum(x)*100
######################### ROC CURVE ####################################
p1<-prediction(pre,test$Converted)
p2<-performance(p1,"tpr","fpr")
plot(p2,col="red")
abline(0,1,col="blue")


auc<-performance(p1,measure = "auc")
auc@y.values
