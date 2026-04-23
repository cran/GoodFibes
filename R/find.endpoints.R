
find.endpoints<-function(images,mode="origin.insertion",show.plot=FALSE,method="kmeans"){
  slices<-vector(length=length(images),mode="list")
  for(i in 1:length(images)){
    slice <- which(!load.image(images[i]) == 0, arr.ind = T)[,1:2]
    if(dim(slice)[1]==0){slice<-matrix(NA,ncol=2,nrow=1)}
    slice<-cbind(slice,rep(i,nrow(slice)))
    slices[[i]]<-slice
  }
  slices<-do.call(rbind,slices)
  slices<-slices[-which(is.na(slices[,1])),]
  slices2<-slices
  
  if(mode=="origin.insertion"){
    if(method=="kmeans"){
    centers<-kmeans(slices,2)}
    if(method=="hclust"){
      slices<-slices[sample(1:nrow(slices),5000,replace=FALSE),]
      dist.mat<-dist(slices)
      cluster<-cutree(hclust(dist.mat,method="ward.D2"),k=2)
      centers<-list(cluster=cluster)
    }
    if(show.plot){
      plot3d(slices[,1],slices[,2],slices[,3],aspect=FALSE,col=centers$cluster)
    }
    
    end1<-apply(slices[which(centers$cluster==1),],2,mean)
    end2<-apply(slices[which(centers$cluster==2),],2,mean)
    endpoints<-rbind(end1,end2)
  } else if(mode=="tendon") {
    colnames(slices)<-c("x","y","z")
    slices<-as.data.frame(slices)
    fit<-lm(cbind(x,y) ~ z,data=slices)
    along.line<-cbind(predict(fit, newdata = list(z = slices$z)),slices$z)
    
    end1<-along.line[which(along.line[,3]==min(along.line[,3]))[1],]
    end2<-along.line[which(along.line[,3]==max(along.line[,3]))[1],]
    endpoints<-rbind(end1,end2)
  }
  colnames(endpoints)<-c("x","y","z")
  return(list(endpoints=endpoints, mask=slices2))
}
