library(glmnet)
library(caret)
library(ROCR)
dfModelInputData[dfModelInputData>9999990]<-NA
dfModelInputData<-dfModelInputData[,-which((names(dfModelInputData)%in%c("flagSplit","AMTL_1S3PAWWWWGC","PBC1_PHRNUMR_ACCMP","AMTL_1S7PAWWWWWS","NUML_1SPDA034WGC","AMTL_1MRPAWW4WWS","NUML_1SPDA034WGC","AMTL_1SPRWWW4WWS","AMTL_1SPRAWW1WGC","AMTL_1SBAAWW1WGC","AMTL_1SAPAWW1WGC","NUML_1SPDAWW1WGC","AMTT_1SSRPA064","NUML_1SPDWWW1WGC","AMTC_1SMUA244","AMTL_1MRPAWW4WWS","AMTL_1SPRWWW4WWS","AMTL_1SPRAWW1WGC","AMTL_1SAPA031WGC","AMTL_1M7PAWWWWWS","AMTL_1SCMAWW1WGC","AMTC_1MCLH242","AMTL_1S4PAWWWWWS","NUML_1SPDW124WGW","NUML_1SPDW064WWW","AMTT_1SSRPA034","AMTL_1MBAAWW1WGC","PBC1_PHRNUMR_ACCMP","AMTL_1S3PAWWWWGC","AMTL_1S7PAWWWWWS","PBC1_PHRRTO_ACCMPFEMPNOW","AMTL_1S3PAWWWWWS")))]
#第五行代码的意思是我将我之前用300个300个变量连续做lasso实验，说是有零方差的变量，就直接先全部删除了
CM<-colMeans(Data1,na.rm=TRUE)
index<-which(is.na(Data1),arr.ind=TRUE)
Data1[index]<-CM[index[,2]]
sum(is.na(Data1))
Data1$target<-factor(ifelse(Data1$target==0,"Zero","One"))
index <- createDataPartition(Data1[,"target"], p=0.7, list=F)
trainset1<- Data1[index, ]
testset1 <-Data1[-index,]
 # 建模
mod=glm(trainset1$target~.,data=trainset1,family="binomial")
logisticmod<-mod
summary(logisticmod)
mod=step(logisticmod)#backwardlogisticmod
#nothing<-glm(trainset$target~1,family="binomial")
#mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="forward")#forwardlogisticmod
#mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="both")#bothwayselectedlogisticmod 
ctrl<-trainControl(method="LGOCV",summaryFunction=twoClassSummary,classProbs = TRUE, savePredictions = TRUE)
mod=glmnet(x=as.matrix(trainset1[,-which(names(trainset1)%in%c("target"))]),y=trainset1$target,alpha=1,lambda=10^seq(3,-2,length=100),family="binomial")
plot(mod)
glmnGrid<-expand.grid(.alpha=c(1),.lambda=10^seq(3,-2,length=100))
glmnTuned<-train(x=trainset1[,-which(names(trainset1)%in%c("target"))],y=trainset1$target,method="glmnet",tuneGrid=glmnGrid,preProc=c("center","scale"),metric="ROC",trControl=ctrl)
glmnTuned
mod=glmnet(x=as.matrix(trainset1[,-which(names(trainset1)%in%c("target"))]),y=trainset1$target,alpha=1,lambda=0.0225702,family="binomial")                                                     
predict(mod,type="coefficients",s=0.0225702)#令alpha=1就是lasso.mod，令alpha=0就是ridge.mod，令alpha=seq(0,1,length=10)就是Penalized.mod
 # 预测
ZeroProb<-predict(mod,newdata=testset1,type="response")# 如果是logistic,前向后向就用这个
ZeroProb<-predict(mod1,as.matrix(testset1[,-which(names(testset1)%in%c("target"))]),type="response")#如果是lasso,ridge就用这个
OneProb<-1-ZeroProb
testset1$target<-as.numeric(ifelse(testset1$target=="Zero",0,1))
plot(performance(prediction(OneProb,testset1$target),"tpr","fpr"))
auc(testset1$target,OneProb)
testset1$target<-factor(ifelse(testset1$target==0,"Zero","One"))
OneProb<-factor(ifelse(OneProb>0.5,"One","Zero"))
confusionMatrix(data=OneProb,reference=testset1$target,positive="One")
