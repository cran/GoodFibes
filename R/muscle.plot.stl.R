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
    fb.df <- data.frame(x = fiber.dat[, 1], y = fiber.dat[,2], z = fiber.dat[, 3])
    if(df==1){ fit <- tryCatch(lm(cbind(x,y)~z, data = fb.df))
    } else {
      fit <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df))}
    lines3d(cbind(predict(fit, newdata = list(z = fiber.dat[,3])), fiber.dat[,3]),col=cols[j], alpha=0.8)
  }
  
  
  if(save.plot){
    writeSTL(file.name)
  }
  
}
