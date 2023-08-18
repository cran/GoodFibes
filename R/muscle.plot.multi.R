muscle.plot.multi <-
function(fiber.list,images,df=2, outline=30, cols=NULL, mirror.axis=FALSE){
  if(is.null(cols)){cols=rep("blue",length(fiber.list))}
  sliceseq<-round(seq(1,length(images),length.out=outline))
  boundaries<-vector(length = length(images),mode="list")
  for(i in sliceseq){
    slice<-which(!load.image(images[i])==0, arr.ind = T)[,1:2]  
    if(!is.matrix(slice)){next()}
    if(dim(slice)[1]==0){next()}
    boundaries[[which(sliceseq==i)]]<-concaveman(slice)
  }
  open3d()
  
  for(k in 1:length(sliceseq)){
    if(is.null(boundaries[[k]])){next()}
    bound<-cbind(boundaries[[k]],rep(sliceseq[k],nrow(boundaries[[k]])))
    if(mirror.axis){
      points3d(bound[,1],bound[,2]*-1,bound[,3],alpha=0.02)
      
    } else {
      points3d(bound[,1],bound[,2],bound[,3],alpha=0.02)
      
    }
  }
  
  
  for(j in 1:length(fiber.list)){
    fiber.dat<-fiber.list[[j]]$fiber.points
    if(mirror.axis){
    fiber.dat[,2]<-fiber.dat[,2]*-1}
    fb.df <- data.frame(x = fiber.dat[, 1], y = fiber.dat[,2], z = fiber.dat[, 3])
    if(df==1){ fit <- tryCatch(lm(cbind(x,y)~z, data = fb.df))
    } else {
      fit <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df))}
    lines3d(cbind(predict(fit, newdata = list(z = fiber.dat[,3])), fiber.dat[,3]),col=cols[j],alpha=0.4)
    }
}
