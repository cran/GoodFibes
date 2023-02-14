muscle.plot.stl <-
function(fiber.list, res = 1, df = 2, radius = 1, cols=NULL, 
                          save.plot=FALSE, file.name="muscle.fibers.stl", mirror.axis=FALSE){
  open3d()
  if(is.null(cols)){
    cols=rep("blue", length(fiber.list))
  }
  
  for(j in 1:length(fiber.list)){
    fiber.dat<-fiber.list[[j]]$fiber.points
    fiber.dat<-fiber.dat*res
    if(mirror.axis){
      fiber.dat[,2]<-fiber.dat[,2]*-1
    }
    fit <- lm(fiber.dat[,1:2] ~ splines::ns(fiber.dat[,3], df = df))
    lines3d(cbind(predict(fit), fiber.dat[,3]),col=cols[j], alpha=0.8)
  }
  
  
  if(save.plot){
    writeSTL(file.name)
  }
  
}
