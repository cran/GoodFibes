fuse.fibers <-
function(fiber.list,min.vox,min.improvement=0.25,df=2,length.out=100){
  fl<-fiber.lengths(fiber.list,res=1,df=df)
  combine.mat<-c(NA,NA)
  fused.list<-vector(mode="list")
  
  for(i in 1:length(fiber.list)){
    for(j in 1:length(fiber.list)){
      if(i==j){next()}
      if(i %in% combine.mat){next()}
      if(j %in% combine.mat){next()}
      
      fb.df1<-data.frame(x = fiber.list[[i]]$fiber.points[,1],y=fiber.list[[i]]$fiber.points[,2],z=fiber.list[[i]]$fiber.points[,3])
      newdata<-seq(min(fb.df1$z),max(fb.df1$z),length.out=length.out)
      if(df==1){ fit1 <- tryCatch(lm(cbind(x,y)~z, data = fb.df1))
      } else {
        fit1 <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df1))}
      pred1 <- cbind(predict(fit1, newdata = list(z = newdata)))
      
      fb.df2<-data.frame(x = fiber.list[[j]]$fiber.points[,1],y=fiber.list[[j]]$fiber.points[,2],z=fiber.list[[j]]$fiber.points[,3])
      newdata<-seq(min(fb.df2$z),max(fb.df2$z),length.out=length.out)
      if(df==1){ fit2 <- tryCatch(lm(cbind(x,y)~z, data = fb.df2))
      } else {
        fit2 <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df2))}
      pred2 <- cbind(predict(fit2, newdata = list(z = newdata)))
      
      
      two.fibs.list<-rbind(pred1,pred2)
      dist.mat<-as.matrix(dist(two.fibs.list))
      
      min.dists<-apply(dist.mat[1:length.out,(length.out+1):(length.out*2)],1,min)
      is.close<-sum(ifelse(min.dists<min.vox,1,0))
      if(is.close==0){next()}
      
      combined.fiber<-rbind(fiber.list[[i]]$fiber.points,fiber.list[[j]]$fiber.points)
      order.ref<-order(combined.fiber[,3])
      combined.fiber<-combined.fiber[order.ref,]
      combined.list<-list(fiber.points=combined.fiber)
      comb.fl<-fiber.lengths(combined.list)
      improvement<-(comb.fl/max(fl[c(i,j)]))-1
      
      if(improvement < min.improvement){next()}
      
      new.df<-data.frame(x=combined.fiber[,1],y=combined.fiber[,2],z=combined.fiber[,3])
      if(df==1){ fit <- tryCatch(lm(cbind(x,y)~z, data = new.df))
      } else {
        fit <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = new.df))}
      fib.smoothed<-cbind(predict(fit, newdata=list(z=combined.fiber[,3])), combined.fiber[,3])
      
      resid1<-sqrt(apply(((predict(fit, newdata=list(z=fiber.list[[i]]$fiber.points[,3])))-
                            fiber.list[[i]]$fiber.points[,1:2])^2,1,sum))
      resid2<-sqrt(apply(((predict(fit, newdata=list(z=fiber.list[[j]]$fiber.points[,3])))-
                            fiber.list[[j]]$fiber.points[,1:2])^2,1,sum))
      resid<-c(resid1,resid2)
      
      resid.original<-sqrt(apply(rbind(fit1$residuals,fit2$residuals)^2,1,sum))
      
      if(mean(resid)<min.vox){
        combine.mat<-rbind(combine.mat,c(i,j))
        fiber.points<-combined.fiber
        fused.list[[dim(combine.mat)[1]]]<-fiber.points
      } else {next()}
      
    } 
  }
  
  if(is.null(dim(combine.mat))){message("there are no fibers to fuse\n")
    fused<-0
    return(fused)
  } else {
    
    
    combine.mat<-combine.mat[2:nrow(combine.mat),]
    fused.list<-fused.list[2:length(fused.list)]
    if(!is.matrix(combine.mat)){
      combine.mat<-matrix(combine.mat,nrow=1,ncol=2)
    }
    
    colnames(combine.mat)<-c("fiber1","fiber2")
    new.fiber.list<-fiber.list
    for(i in 1:length(fused.list)){
      fiber.points<-as.matrix(fused.list[[i]])
      new.fiber.list[[combine.mat[i,1]]]$fiber.points<-fiber.points
    }
    new.fiber.list<-new.fiber.list[-combine.mat[,2]]
    return(list(fibers.to.merge=combine.mat,merged.fibers=new.fiber.list))
    
  }
}
