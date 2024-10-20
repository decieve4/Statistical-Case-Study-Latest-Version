source("SEM1/Project1/stylometryfunctions.R")
source("SEM1/Project1/Preprocessing.R")
demo <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")
x<-NULL
a<-apply(X_pca[1:3, ], 2, sum)
b<-apply(X_pca[4:8, ], 2, sum)
c<-X_pca[9,]
d<-apply(X_pca[10:15, ], 2, sum)
e<-apply(X_pca[16:17, ], 2, sum)
f<-apply(X_pca[18:19, ], 2, sum)
g<-apply(X_pca[20:22, ], 2, sum)
h<-apply(X_pca[23:26, ], 2, sum)
i<-X_pca[27,]
j<-apply(X_pca[28:33, ], 2, sum)
k<-apply(X_pca[34:38, ], 2, sum)
l<-X_pca[39,]
x<-rbind(a,b,c,d,e,f,g,h,i,j,k,l)
for (i in 1:nrow(x)){
  x[i,]<-x[i,]/sum(x[i,])
}
for(j in i:ncol(x)){
  x[,j]<-(x[,j]-mean(x[,j]))/sd(x[,j])
}
d<-dist(x)
print(d)
pts<-cmdscale(d)
plot(pts,type='n')
title(main="Multidimensional Scaling Plot For All Authors")
text(pts[,1],pts[,2],label=demo$authornames,cex=0.8)

