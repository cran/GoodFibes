fiber.curve <-
function(fib.list, df, check=TRUE){
  fl<-fiber.lengths(fib.list,res=1,df=df)
  sl.dist<-rep(NA,length(fib.list))
  for(i in 1:length(fib.list)){
    fib.points<-fib.list[[i]]$fiber.points
    endr<-nrow(fib.points)
    sl.dist[i]<-sqrt(sum((fib.points[1,]-fib.points[endr,])^2))
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
