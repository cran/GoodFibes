fiber.angle <-
function(fib.list, axis, centered=TRUE){
  fangle<-rep(NA,length(fib.list))
  other.axis<-setdiff(1:3,axis)
  
  if(axis==1){
  adjx<-min(sapply(fib.list,function(x){min(x$fiber.points[,1])}))
  nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
  adjy<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,2])}))/nfib
  adjz<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,3])}))/nfib
  }
  
  if(axis==2){
    adjy<-min(sapply(fib.list,function(x){min(x$fiber.points[,2])}))
    nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
    adjx<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,1])}))/nfib
    adjz<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,3])}))/nfib
  }
  
  if(axis==3){
    adjz<-min(sapply(fib.list,function(x){min(x$fiber.points[,3])}))
    nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
    adjx<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,1])}))/nfib
    adjy<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,2])}))/nfib
  }
  
for(i in 1:length(fib.list)){
  fiber.dat<-fib.list[[i]]$fiber.points
  if(centered){fiber.dat<-fiber.dat-matrix(c(adjx,adjy,adjz),nrow=nrow(fiber.dat),ncol=ncol(fiber.dat),byrow=TRUE)}
  new.df<-data.frame(x=fiber.dat[,other.axis[1]],y=fiber.dat[,other.axis[2]],z=fiber.dat[,axis])
  fit <- lm(z ~ y + x,data = new.df)
  new.fibes<-cbind(new.df$x,new.df$y,predict(fit))
  zero.point<-c(0,0,fit$coefficients[1]) 
  
  dists<-as.matrix(dist(rbind(zero.point,new.df)))[,1]
  new.end<-new.df[order(dists, decreasing = TRUE)[1]-1,]
  hyp<-sqrt(sum((new.end-zero.point)^2))
  adj<-abs(new.end[3]-zero.point[3])
  fangle[i]<-acos(adj/hyp)*180/pi
  
  
}
  fangle<-unlist(fangle)
  return(fangle)
}
