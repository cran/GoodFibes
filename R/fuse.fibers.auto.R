fuse.fibers.auto <-
function(fiber.list,min.vox,min.improvement=0.25,df=2,length.out=50,max.iter=10, verbose=FALSE){
  new.fiber.list<-fiber.list
  iteration<-1
  repeat{
    if(verbose){cat("iteration #",iteration,"\n",sep=" ")}
    fiber.step<-fuse.fibers(new.fiber.list,min.vox,min.improvement,df,length.out)
    if(length(fiber.step)==1){break()}
    new.fiber.list<-fiber.step$merged.fibers
    iteration<-iteration+1
    if(iteration==max.iter & !verbose){cat("max iterations reached", "\n",sep=" ")
      break()}
  }
  return(new.fiber.list)
  
}
