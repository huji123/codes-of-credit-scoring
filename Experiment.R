dfModelInputData[dfModelInputData>9999990]<-NA#将大于9999990的值转化为NA
CM<-colMeans(dfModelInputData,na.rm=TRUE)
index<-which(is.na(dfModelInputData),arr.ind=TRUE)
dfModelInputData[index]<-CM[index[,2]]#用均值插补每列
sum(is.na(dfModelInputData))#然后发现还有缺失值，缺失值个数刚好是样本个数，
#之前试过用caret包里边的prePocess()函数预处理原数据集，警告有个变量无法参与计算，然后我就发现这个变量全部都是缺失值
dfModelInputData<-dfModelInputData[,-which((names(dfModelInputData)%in%c("NUML_1SPDA034WGC")))]#把全是缺失值的那个变量删除了
sum(is.na(dfModelInputData))#果然就没有缺失值了
dfModelInputData$target<-factor(ifelse(dfModelInputData$target==0,"Zero","One"))
trainset<-subset(dfModelInputData,dfModelInputData$flagSplit=="2",select=-dfModelInputData$flagSplit)#因为之前将字符型“train"变成了数值”2“
testset<-subset(dfModelInputData,dfModelInputData$flagSplit=="1",select=-dfModelInputData$flagSplit)

#建模
mod=glm(trainset$target~.,data=trainset,family="binomial")#logistic model
logisticmod<-mod
summary(logisticmod)
#mod=step(logisticmod)#backwardlogisticmod
#nothing<-glm(trainset$target~1,family="binomial")
#mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="forward")#forwardlogisticmod
#mod=step(nothing,scope=list(lower=formula(nothing),uper=formula(logisticmod),direction="both")#bothwayselectedlogisticmod 
#ctrl<-trainControl(method="LGOCV",summaryFunction=twoClassSummary,classProbs = TRUE, savePredictions = TRUE)
#mod=glmnet(x=as.matrix(trainset[,1:14]),y=trainset$target,alpha=1,lambda=10^seq(10,-2,length=100),family="binomial")
#plot(mod)
#glmnGrid<-expand.grid(.alpha=c(1),.lambda=10^seq(10,-2,length=100))
#glmnTuned<-train(x=trainset[,1:14],y=trainset$target,method="glmnet",tuneGrid=glmnGrid,preProc=c("center","scale"),metric="ROC",trControl=ctrl)
#glmnTuned
#mod=glmnet(x=as.matrix(trainset[,1:14]),y=trainset$target,alpha=1,lambda=0.01,family="binomial")                                                     
#predict(mod,type="coefficients",s=0.01)[1:15,]#令alpha=1就是lasso.mod，令alpha=0就是ridge.mod，令alpha=seq(0,1,length=10)就是Penalized.mod

#预测
OneProb<-predict(mod,newdata=testset,type="response")

#评估
roc1<-roc(response=testset$target,predictor=OneProb,levels=rev(levels(testset$target)))
auc1<-auc(roc1)
auc1
logisticsauc1<-auc1
backwardlogisticauc1<-auc1
forwardlogisticauc1<-auc1
bothwayselectedlogisticauc1<-auc1
lassoauc1<-auc1
ridgeauc1<-auc1
Penalizedauc1<-auc1
OneProb<-factor(ifelse(ZeroProb>0.5,"One","Zero"))
confusionMatrix(data=OneProb,reference=testset$target,positive="One")
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
