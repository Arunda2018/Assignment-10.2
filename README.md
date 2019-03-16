# Assignment-10.2
set.seed(1)
> n=500
> library(clusterGeneration)
> library(mnormt)
> S=genPositiveDefMat("eigen",dim=15)
> S=genPositiveDefMat("unifcorrmat",dim=15)
> X=rmnorm(n,varcov=S$Sigma)
> library(corrplot)
> corrplot(cor(X), order = "hclust")

P=exp(Score)/(1+exp(Score))
> Y=rbinom(n,size=1,prob=P)
> df=data.frame(Y,X)
> allX=paste("X",1:ncol(X),sep="")
> names(df)=c("Y",allX)
The variable importance plot is obtained by growing some trees,

> require(randomForest)
> fit=randomForest(factor(Y)~., data=df)
Then we can use simple functions

> (VI_F=importance(fit))
    MeanDecreaseGini
X1          31.14309
X2          31.78810
X3          20.95285
X4          13.52398
X5          13.54137
X6          10.53621
X7          10.96553
X8          15.79248
X9          14.19013
X10         10.02330
X11         11.46241
X12         11.36008
X13         10.82368
X14         10.17462
X15         10.45530
which can also be obtained using

> library(caret)
> varImp(fit)
     Overall
X1  31.14309
X2  31.78810
X3  20.95285
X4  13.52398
X5  13.54137
X6  10.53621
X7  10.96553
X8  15.79248
X9  14.19013
X10 10.02330
X11 11.46241
X12 11.36008
X13 10.82368
X14 10.17462
X15 10.45530
But the popular plot that we see in all reports is usually

> varImpPlot(fit,type=2)
tmp=rownames(fit$splits)
> allVars=colnames(attributes(fit$terms)$factors)  
> rownames(fit$splits)=1:nrow(fit$splits)
> splits=data.frame(fit$splits)
> splits$var=tmp
> splits$type=""
> frame=as.data.frame(fit$frame)
> index=0
> for(i in 1:nrow(frame)){
+   if(frame$var[i] != ""){
+   index=index + 1
+   splits$type[index]="primary"
+   if(frame$ncompete[i] > 0){
+   for(j in 1:frame$ncompete[i]){
+   index=index + 1
+   splits$type[index]="competing"}}
+   if(frame$nsurrogate[i] > 0){
+   for(j in 1:frame$nsurrogate[i]){
+      index=index + 1
+      splits$type[index]="surrogate"}}}}
> splits$var=factor(as.character(splits$var))
> splits=subset(splits, type != "surrogate")
> out=aggregate(splits$improve,
+     list(Variable = splits$var),
+     sum, na.rm = TRUE)
> allVars=colnames(attributes(fit$terms)$factors)
>  if(!all(allVars %in% out$Variable)){
+   missingVars=allVars[!(allVars %in% out$Variable)]
+   zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
+   out=rbind(out, zeros)}
> out2=data.frame(Overall = out$x)
> rownames(out2)=out$Variable
> out2
      Overall
X1  51.024692
X10  4.328443
X11 19.087255
X12 10.399549
X13 15.248933
X15  9.989834
X2  68.758329
X3  41.986055
X4  15.211913
X5  18.247668
X7  18.857998
X8  43.318540
X9  30.299429
X6   0.000000
X14  0.000000
 VI_T=out2
> barplot(unlist(VI_T/sum(VI_T)),names.arg=1:15)
barplot(t(VI_F/sum(VI_F)))
