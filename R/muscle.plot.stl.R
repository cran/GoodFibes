muscle.plot.stl <-
function(fiber.list, res = 1, df = 2, radius = 1, cols=NULL, 
                          save.plot=FALSE, file.name="muscle.fibers", mirror.axis=FALSE,type="stl", ...){
  open3d()
  if(is.null(cols)){
    cols=rep("blue", length(fiber.list))
  }
  
  if(type=="ply"){  warning("PLY can be extremely slow, especially if saving to file!", immediate. = TRUE)
  }
  if(type=="ply" & res==1){res<-0.02}
  
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
  
    if(type=="stl"){
      lines3d(cbind(predict(fit, newdata = list(z = fiber.dat[,3])), fiber.dat[,3]),col=cols[j], alpha=0.8)
    }
    if(type=="ply"){
      vertices<-cbind(predict(fit, newdata = list(z = fiber.dat[,3])), fiber.dat[,3])
      cly<- cylinder3d(vertices,color=cols[j],radius=radius*res/2,sides=8,closed = -2, ...)
      shade3d(cly) 
      wire3d(cly, col=cols[j])
      
    }
      }
    
  
  
  if(save.plot){
    if(type=="stl"){
    writeSTL(paste0(file.name,".stl"))
    }
    if(type=="ply"){
      writePLY(paste0(file.name,".ply"),withColors=TRUE)
    }
  }
  
}
