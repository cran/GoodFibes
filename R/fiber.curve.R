fiber.curve <-
function(fib.list, df, check=TRUE, length.out=500){
  fl<-fiber.lengths(fib.list,res=1,df=df,length.out=length.out)
  sl.dist<-rep(NA,length(fib.list))
  for(i in 1:length(fib.list)){
    fib.points<-fib.list[[i]]$fiber.points
    starts <- min(fib.points[, 3])
    stops <- max(fib.points[, 3])
    fb.df <- data.frame(x = fib.points[, 1], y = fib.points[,2], z = fib.points[, 3])
    newdata <- seq(starts, stops, length.out = length.out)
    if(df==1){ fit1 <- tryCatch(lm(cbind(x,y)~z, data = fb.df))
    } else {
      fit1 <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df))}
    fib.smoothed <- cbind(predict(fit1, newdata = list(z = newdata)), newdata)
    endr<-nrow(fib.smoothed)
    sl.dist[i]<-sqrt(sum((fib.smoothed[1,]-fib.smoothed[endr,])^2))
    }
  
  
  curvature<-fl/sl.dist
  
  if(check){
    uplim<-boxplot.stats(curvature)$stats[5]
    iffy<-which(curvature>uplim)
    return(list(curvature=curvature,problem.fibers=iffy))
  } else {
    return(list(curvature=curvature))
  }
  
}
