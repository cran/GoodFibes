hemisphere.points <-
function(radius, show.plot=FALSE, backstep=0){
  theta <- seq(0,2*pi,0.02)
  u <- seq(0,1,length.out=(radius+1)*4)
  if(!backstep==0){u<-c(-(u[((backstep)*4):2]),u)}
  
  coords<-array(dim=c(length(theta),3,length(u)))
  for(i in 1:length(u)){
    x <- sqrt(1-u[i]^2)*cos(theta)
    y <- sqrt(1-u[i]^2)*sin(theta)
    coords[,1,i]<-x
    coords[,2,i]<-y
    coords[,3,i]<-rep(u[i],length(theta))
  }
  coords<-radius*coords
  
  rcoords<-round(coords,0)
  rcoords<-cbind(as.numeric(rcoords[,1,]),as.numeric(rcoords[,2,]),as.numeric(rcoords[,3,]))
  ucoords<-unique(rcoords)
  
  if(show.plot == TRUE){
    for(i in 1:nrow(ucoords)){
      points3d(ucoords[i,1],ucoords[i,2],ucoords[i,3],col="red")
    }
  }
  return(ucoords)
}
