fiber.angle <-
  function(fib.list, axis=3, centered=TRUE,reference="axis"){
    fangle<-rep(NA,length(fib.list))
    other.axis<-setdiff(1:3,axis)
    
    if(axis==1){
      adjx<-min(sapply(fib.list,function(x){min(x$fiber.points[,1])}))
      nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
      adjy<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,2])}))/nfib
      adjz<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,3])}))/nfib
    }
    
    if(axis==2){
      adjy<-min(sapply(fib.list,function(x){min(x$fiber.points[,2])}))
      nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
      adjx<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,1])}))/nfib
      adjz<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,3])}))/nfib
    }
    
    if(axis==3){
      adjz<-min(sapply(fib.list,function(x){min(x$fiber.points[,3])}))
      nfib<-sum(sapply(fib.list,function(x){nrow(x$fiber.points)}))
      adjx<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,1])}))/nfib
      adjy<-sum(sapply(fib.list,function(x){sum(x$fiber.points[,2])}))/nfib
    }
    
    for(i in 1:length(fib.list)){
      fiber.dat<-fib.list[[i]]$fiber.points
      if(centered){fiber.dat<-fiber.dat-matrix(c(adjx,adjy,adjz),nrow=nrow(fiber.dat),ncol=ncol(fiber.dat),byrow=TRUE)}
      new.df<-data.frame(x=fiber.dat[,other.axis[1]],y=fiber.dat[,other.axis[2]],z=fiber.dat[,axis])
      fit <- lm(z ~ x + y,data = new.df)
      new.fibes<-cbind(new.df$x,new.df$y,predict(fit))
      
      end<-dim(new.fibes)[1]
      
      if(reference=="axis"){
        adj<-sqrt((new.fibes[1,1]-new.fibes[end,1])^2+(new.fibes[1,2]-new.fibes[end,2])^2)
        opp<-abs(new.fibes[1,3]-new.fibes[end,3])
        fangle[i]<-90-atan(opp/adj)*180/pi
      }
      
      if(reference=="plane.xz"){
        adj<-new.fibes[1,1]-new.fibes[end,1]
        opp<-new.fibes[1,3]-new.fibes[end,3]
        new.angle<-atan(opp/adj)*180/pi
        if(new.angle<0){new.angle<-new.angle+180}
        fangle[i]<-90-new.angle
        
        
      }  
      if(reference=="plane.yz"){
        adj<-new.fibes[1,2]-new.fibes[end,2]
        opp<-new.fibes[1,3]-new.fibes[end,3]
        new.angle<-atan(opp/adj)*180/pi
        if(new.angle<0){new.angle<-new.angle+180}
        fangle[i]<-90-new.angle
        
      }  
      
    }
    fangle<-unlist(fangle)
    return(fangle)
  }





