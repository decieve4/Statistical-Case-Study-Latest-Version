demo <- loadCorpus("./SEM1/Project1/Data/frankenstein/FunctionWords/", featureset = "frequentwords70")

##ALL
x<-NULL
for (i in 1:length(demo$features)){
  x<-rbind(x,apply(demo$features[[i]],2,sum))
}
for (i in 1:nrow(x)){
  x[i,]<-x[i,]/sum(x[i,])
}
for(j in i:ncol(x)){
  x[,j]<-(x[,j]-mean(x[,j]))/sd(x[,j])
}
d<-dist(x)
pts<-cmdscale(d)
plot(pts,type='n')
title(main="Multidimensional Scaling Plot For All Authors")
text(pts[,1],pts[,2],label=demo$authornames,cex=0.8)

##MARY AND FRANKENSTEIN
y<-demo$features[[4]]
y<-rbind(y,demo$features[[9]])
bookname<-c("Falkner","LastMan","Lodore","Mathilda","Valperga","Warbeck","Frankenstein")
for (i in 1:nrow(y)){
  y[i,]<-y[i,]/sum(y[i,])
}
for(j in i:ncol(y)){
  y[,j]<-(y[,j]-mean(y[,j]))/sd(y[,j])
}
d<-dist(y)
pts<-cmdscale(d)
plot(pts,type="n")
title(main="Multidimensional Scaling Plot For MaryShelly and Frankenstein")
text(pts[,1],pts[,2],label=bookname,cex=0.8)


##Find BOOK Names
booknames <- demo$booknames
booknames <- lapply(booknames, function(x) gsub(".txt", "", x))
booknames <- lapply(seq_along(booknames), function(i) {
  if (i == 7) {
    booknames[[i]]
  } else {
    gsub("^[^_]*_", "", booknames[[i]])
  }
})
print(booknames)
demo2<-demo$features[[9]]
for (i in 1:length(demo$features)){
  y<-demo$features[[i]]
  y<-rbind(y,demo2)
  for (i in 1:nrow(y)){
    y[i,]<-y[i,]/sum(y[i,])
  }
  for(j in i:ncol(y)){
    y[,j]<-(y[,j]-mean(y[,j]))/sd(y[,j])
  }
  d<-dist(y)
  pts<-cmdscale(d)
  plot(pts,type="n")
  title(main="Multidimensional Scaling Plot Forand Frankenstein")
  text(pts[,1],pts[,2],label=bookname[[i]],cex=0.8)
  
}