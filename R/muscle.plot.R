muscle.plot <-
function(fiber.dat,images,df=4,mirror.axis=FALSE,outline=50, size=2){
  if(mirror.axis){fiber.dat[,2]<-fiber.dat[,2]*-1}
  open3d()
  
  sliceseq<-round(seq(1,length(images),length.out=outline))
  boundaries<-vector(length = length(images),mode="list")
  for(i in sliceseq){
    slice<-which(!load.image(images[i])==0, arr.ind = T)[,1:2]  
    if(!is.matrix(slice)){next()}
    if(dim(slice)[1]==0){next()}
    boundaries[[which(sliceseq==i)]]<-concaveman(slice)
  }

   for(k in 1:length(sliceseq)){
    if(is.null(boundaries[[k]])){next()}
    bound<-cbind(boundaries[[k]],rep(sliceseq[k],nrow(boundaries[[k]])))
    
    if(mirror.axis){
      points3d(bound[,1],bound[,2]*-1,bound[,3],alpha=0.02)
      } else {
      points3d(bound[,1],bound[,2],bound[,3],alpha=0.02)
      }
   }
  points3d(fiber.dat,col="red",size=size)
  fit <- lm(fiber.dat[,1:2] ~ splines::ns(fiber.dat[,3], df = df))
  lines3d(cbind(predict(fit), fiber.dat[,3]))
}
