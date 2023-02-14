quality.check <-
function(fib.list,images,res,min.length=NULL,length.out=200, df=2){
  grey.list<-vector(length=length(fib.list),mode="list")
  
  for(i in 1:length(fib.list)){
      
    fib.points<-fib.list[[i]]$fiber.points
    starts<-min(fib.points[,3])
    stops<-max(fib.points[,3])
    fb.df<-data.frame(x = fib.points[,1],y=fib.points[,2],z=fib.points[,3])
    newdata<-seq(starts,stops,length.out=length.out)
    fit1 <- lm(cbind(x,y)~splines::ns(z,df=df), data = fb.df)
   fiber <- round(cbind(predict(fit1, newdata = list(z = newdata)),newdata),0)
    fiber<-unique(fiber)
    grey.check<-matrix(ncol=2,nrow=nrow(fiber))
    plot.index<-unique(fiber[,3])
    ncheck<-length(unique(fiber[,3]))
    
    
    
    for(k in 1:ncheck){
      tempimg<-as.matrix(load.image(images[plot.index[k]]))
      steps<-which(fiber[,3] %in% plot.index[k])
      grey.check[steps,2]<-fiber[steps[1],3]
      
      
      for(m in 1:length(steps)){
      
      x<-fiber[steps[m],1]
      y<-fiber[steps[m],2]
      if(x<=0|y<=0){next()}
      if(x>dim(tempimg)[1] | y > dim(tempimg)[2]){next()}
      grey.check[steps[m],1]<-tempimg[x,y]
      } 
    }
    grey.list[[i]]<-na.omit(grey.check)
  }
    
   
  fl<-fiber.lengths(fib.list,res=res,df=df)
  greysd<-sapply(grey.list,function(x){sd(x[,1])})/fl
  qual<-scale(greysd)
  
  p<-barplot(sort(qual,decreasing = TRUE))
  abline(h=as.numeric(quantile(qual)[4]),lty=3)
  iffy<-which(qual>(quantile(qual)[4] + IQR(qual)*1.5))
  text(p[1:length(iffy)],sort(qual[iffy],decreasing=TRUE),iffy,pos=1)
  
  if(!is.null(min.length)){
    iffy<-c(iffy,which(fl<=min.length))
  }
  
  
  return(list(quality=greysd,problem.fibers=iffy, grey.values=grey.list))
}
