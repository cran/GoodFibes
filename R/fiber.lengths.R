fiber.lengths <-
function(fib.list, res = NULL, df = 2, length.out=500){
  nfibs<-length(fib.list)
  fib.length<-rep(NA, length=nfibs)
  for(j in 1:nfibs){
    if(nfibs==1){
      fib.points<-fib.list$fiber.points
    } else {  
      fib.points<-fib.list[[j]]$fiber.points
    }
    starts<-min(fib.points[,3])
    stops<-max(fib.points[,3])
    
    tryCatch({
      fb.df<-data.frame(x = fib.points[,1],y=fib.points[,2],z=fib.points[,3])
      newdata<-seq(starts,stops,length.out=length.out)
      fit1 <- tryCatch(lm(cbind(x,y)~splines::ns(z,df=df), data = fb.df))
      fib.smoothed <- cbind(predict(fit1, newdata = list(z = newdata)),newdata)
      
      fib.segs<-vector(length=(nrow(fib.smoothed)-1))
      for(i in 1:(nrow(fib.smoothed)-1)){
        fib.segs[i]<-sqrt(sum((fib.smoothed[i,]-fib.smoothed[i+1,])^2))
      }
      
      fib.length[j]<-sum(fib.segs)},error = function(msg){fib.length[j]<-NA})
    
  }
  
  
  if(!is.null(res)){
    fib.length<-fib.length*res
  }
  
  return(fib.length)
  
}
