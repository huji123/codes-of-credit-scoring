#数据预处理
dfModelInputData[dfModelInputData>9999990]<-NA
install.packages("Hmisc")
library(Hmisc)
dfModelInputData$flagSplit
dfModelInputData$flagSplit<-as.numeric(dfModelInputData$flagSplit)#方便之后处理缺失值如果不转化为数值型该列会全部变为缺失值
sum(is.na(dfModelInputData$flagSplit))#看转化后是否存在缺失值，若存在则不能进行上一步
dfModelInputData$flagSplit#对比转化之前的，看训练集和验证集分别转化为了什么数值
fillfuction<-function(x){impute(x,mean)}
dfModelInputData<-lapply(dfModelInputData,fillfuction)
View(dfModelInputData)
dfModelInputData$target<-factor(ifelse(dfModelInputData$target==0,"Zero","One"))
trainset<-subset(dfModelInputData,dfModelInputData$flagSplit==2,select=-dfModelInputData$flagSplit)
testset<-subset(dfModelInputData,df==1,select=-dfModelInputData$flagOOT)
dim(trainset)
dim(testset)

#建模
library(pROC)
library(glmnet)
library(caret)
mod=glm(trainset$target~.,data=trainset,family="binomial")#logistic model
logisticmod<-mod
mod=step(logisticmod)#backwardlogisticmod
nothing<-glm(trainset$target~1,family="binomial")
mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="forward")#forwardlogisticmod
mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="both")#bothwayselectedlogisticmod 
ctrl<-trainControl(method="LGOCV",summaryFunction=twoClassSummary,classProbs = TRUE, savePredictions = TRUE)
mod=glmnet(x=as.matrix(trainset[,1:14]),y=trainset$target,alpha=1,lambda=10^seq(10,-2,length=100),family="binomial")
plot(mod)
glmnGrid<-expand.grid(.alpha=c(1),.lambda=10^seq(10,-2,length=100))
glmnTuned<-train(x=trainset[,1:14],y=trainset$target,method="glmnet",tuneGrid=glmnGrid,preProc=c("center","scale"),metric="ROC",trControl=ctrl)
glmnTuned
mod=glmnet(x=as.matrix(trainset[,1:14]),y=trainset$target,alpha=1,lambda=0.01,family="binomial")                                                     
predict(mod,type="coefficients",s=0.01)[1:15,]#令alpha=1就是lasso.mod，令alpha=0就是ridge.mod，令alpha=seq(0,1,length=10)就是Penalized.mod

#预测
ZeroProb<-predict(mod,newdata=testset,type="response")

#评估
roc1<-roc(response=testset$target,predictor=ZeroProb,levels=rev(levels(testset$target)))
auc1<-auc(roc1)
auc1
logisticsauc1<-auc1
backwardlogisticauc1<-auc1
forwardlogisticauc1<-auc1
bothwayselectedlogisticauc1<-auc1
lassoauc1<-auc1
ridgeauc1<-auc1
Penalizedauc1<-auc1
ZeroProb<-factor(ifelse(ZeroProb>0.5,"Zero","One"))
confusionMatrix(data=ZeroProb,reference=testset$target,positive="Zero")
logisticconfusionMatrix1=confusionMatrix
backwardconfusionMatrix1=confusionMatrix
forwardconfusionMatrix1=confusionMatrix
bothwayselectedconfusionMatrix1=confusionMatrix
lassoconfusionMatrix1=confusionMatrix
ridgeconfusionMatrix1=confusionMatrix
PenalizedconfusionMatrix1=confusionMatrix

#检验
X<-(logisticsauc1,backwardlogisticauc1,forwardlogisticauc1,bothwayselectedlogisticauc1,lassoauc1,ridgeauc1,Penalizedauc1)
chisq.test(X)
