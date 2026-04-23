fiber.angle <-
  function(fib.list, axis=3, reference="axis",endpoints=NULL,end.to.end=FALSE){
    fangle<-rep(NA,length(fib.list))
    if(reference=="line.of.action"){axis=3}
    if(reference=="line.of.action" & is.null(endpoints)){stop("need endpoints when calculating from line of action")}
    other.axis<-setdiff(1:3,axis)
    
    if(reference=="line.of.action"){
      ######## finding rotations for fibers using endpoints
      lower<-which(endpoints[,3]==min(endpoints[,3]))
      centering<-matrix(endpoints[lower,],nrow=2,ncol=3,byrow=TRUE)
      centered<-endpoints-centering
      reference.point<-which(!apply(centered,1,sum)==0)
      v1<-sqrt(sum(centered[reference.point,c(1,3)]^2))
      
      thetay<-acos(centered[reference.point,3]/v1)
      roty<-matrix(c(cos(thetay),0,sin(thetay), 0,1,0, -sin(thetay),0,cos(thetay)),ncol=3,nrow=3,byrow=TRUE)
      rotated<-centered%*%roty
      if(rotated[reference.point,1]>0.1){
        thetay<- -thetay
        roty<-matrix(c(cos(thetay),0,sin(thetay), 0,1,0, -sin(thetay),0,cos(thetay)),ncol=3,nrow=3,byrow=TRUE)
        rotated<-centered%*%roty
      }
      
      v2<-sqrt(sum(rotated[reference.point,c(2,3)]^2))
      thetax<- acos(rotated[reference.point,3]/v2)
      rotx<-matrix(c(1,0,0, 0,cos(thetax),-sin(thetax), 0,sin(thetax),cos(thetax)),ncol=3,nrow=3,byrow=TRUE)
      rotated2<-rotated%*%rotx
      if(rotated2[reference.point,2]>0.1){
        thetax<- -thetax
        rotx<-matrix(c(1,0,0, 0,cos(thetax),-sin(thetax), 0,sin(thetax),cos(thetax)),ncol=3,nrow=3,byrow=TRUE)
        rotated2<-rotated%*%rotx
      }
      ######### rotating the fiber to the reference line
      for(i in 1:length(fib.list)){
        fib.list[[i]]$fiber.points <- (fib.list[[i]]$fiber.points-matrix(centering[1,],ncol=3,nrow=nrow(fib.list[[i]]$fiber.points),byrow=TRUE)) %*% roty %*% rotx
      }
    }
    
    
    
    for(i in 1:length(fib.list)){
      fiber.dat<-fib.list[[i]]$fiber.points
      
      if(end.to.end){
        fiber.dat<-fiber.dat[c(1,nrow(fiber.dat)),]
      }
      
      
      
      if(reference=="axis" | reference=="line.of.action"){
        fangle[i]<-angle(abs(prcomp(fiber.dat)$rotation[,1]),c(0,0,1))
      }
      
      if(reference=="plane.xz"){
        evec<-prcomp(fiber.dat)$rotation[c(1,3),1]
        if(evec[2]<0){evec<-evec*-1}
        fangle[i]<-90-angle(evec,c(1,0))
        
      }  
      if(reference=="plane.yz"){
        evec<-prcomp(fiber.dat)$rotation[c(2,3),1]
        if(evec[2]<0){evec<-evec*-1}
        fangle[i]<-90-angle(evec,c(1,0))
        
      }  
      if(reference=="plane.xy"){
        evec<-prcomp(fiber.dat)$rotation[c(1,2),1]
        if(evec[2]<0){evec<-evec*-1}
        fangle[i]<-90-angle(evec,c(1,0))
        
      }  
    }
    fangle<-unlist(fangle)
    return(fangle)
  }





