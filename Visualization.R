demo <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")
x<-NULL
demo1<-demo[-9]
for (i in 1:length(demo$features)){
  x<-rbind(x,apply(demo$features[[i]],2,sum))
}
nrow(x)
length(demo$features)
ncol(x)
for (i in 1:nrow(x)){
  x[i,]<-x[i,]/sum(x[i,])
}
for(j in i:ncol(x)){
  x[,j]<-(x[,j]-mean(x[,j]))/sd(x[,j])
}
d<-dist(x)
pts<-cmdscale(d)
plot(pts,type='n')
text(pts[,1],pts[,2],label=demo$authornames,cex=0.8)
