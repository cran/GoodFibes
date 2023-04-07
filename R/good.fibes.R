good.fibes <- function(images, zero.image, radius, threshold=NULL, cutoff, scaler=1, blackcut=0.95, seeds=1, show.plot=TRUE,start.seed=NULL,allowed.black=0,bound.buffer=0,backstep=0,verbose=TRUE){
  depth<-radius+1
  if(is.null(threshold)){threshold<-cutoff}
  if(radius > 11){stop("radius cannot be more than 11")}
  if(threshold < cutoff){stop("threshold must be equal to or greater than cutoff")}
  
  res.list<-vector(length=seeds,mode="list")
  
  #### starting seed point
  abovethres<-as.matrix(which(load.image(images[zero.image])>threshold,arr.ind = T))[,1:2]
  
  if(verbose){cat("choosing starting points","\n",sep=" ")}
  
  thres.dist<-dist(abovethres) 
  thresclust<-hclust(thres.dist, method="average")  
  thres.cut<-cutree(thresclust,k=seeds)
  
  loc<-matrix(ncol=2,nrow=seeds)
  for(tc in 1:seeds){
    grouptc<-which(thres.cut==tc)
    if(!is.null(start.seed)){set.seed(start.seed)}
    loc[tc,]<-abovethres[sample(grouptc,1),]
  }
  
  
  
  for(zz in 1:seeds){
    tryCatch({
      if(seeds > 1){
        startx<-loc[zz,1]
        starty<-loc[zz,2]
      } else {
        startx<-loc[1]
        starty<-loc[2]
      }
      
      ######
      
      if(show.plot){
        plot(load.image(images[zero.image]),main = "foward walk")
        points(startx,starty,col="red",pch=16,cex=1)
      }
      
      ##### creating image list
      imseq<-zero.image:(zero.image+radius)
      imglist<-vector(length=depth,mode="list")
      for(i in imseq){
        imglist[[which(imseq==i)]]<-load.image(file=images[i])
      }
      for(j in 1:depth){
        imglist[[j]][which(imglist[[j]]<cutoff)]<-0
      }
      
      #### creating the first data matrix
      tracker<-1
      fib.track<-matrix(ncol=3,nrow=length(images))
      ## record starting point
      fib.track[1,]<-c(startx,starty,zero.image)
      
      
      
      ############ launch ##############
      
      
      
      
      
      
      ########################################################
      ################## Forward #############################
      ########################################################
      if(verbose){cat("beginning forward walk from zero image for fiber",zz,"\n",sep=" ")
      }
      ### first measurements
      
      ucoords<-hemisphere.points(radius)
      lpl<-pointsGenerator(startx = startx, starty = starty, ucoords=ucoords, radius=radius,backstep=backstep)
      
      white.mat<-get.whites(imglist,lpl,depth)
      white.sd<-apply(white.mat,1,sd)
      white.sd<-white.sd/max(white.sd)
      
      ### no initial trajectory, all paths equally probable
      dist.check<-rep(1,length=nrow(white.mat))
      
      white.optim<-white.sd * dist.check^scaler
      white.optim<-white.optim/max(white.optim)
      cross.check<-apply(ifelse(white.mat[,2:radius]==0,1,0),1,sum)
      cross.check<-ifelse(cross.check<=allowed.black,0,1000)
      end.check<-ifelse(white.mat[,depth]==0,1000,0)
      white.optim<-white.optim + cross.check + end.check
      
      #### get dist check for next step before moving on
      
      
      
      ####### first step
      best<-which(white.optim==min(white.optim))
      if(length(best)>1){sample(best,1)}
      
      
      new.dat<-lpl[[best]][nrow(lpl[[best]]),]
      traj.ref<-lpl[[best]][nrow(lpl[[best]]),]-c(startx,starty,0)
      new.startx<-new.dat[1]
      new.starty<-new.dat[2]
      new.zero<-new.dat[3]
      new.zero.image<-zero.image+new.zero
      
      
      
      if(show.plot){
        plot(load.image(images[new.zero.image]),main="forward walk")
        points(new.startx,new.starty,col="red",pch=16,cex=1)
      }
      
      
      
      tracker<-tracker+1
      fib.track[tracker,]<-c(new.startx,new.starty,new.zero.image)
      
      
      
      ######## repeater for steps 3 and onwards
      
      for(z in 1:length(images)){
        
        ucoords<-hemisphere.points(radius,backstep=backstep)
        lpl<-pointsGenerator(startx = new.startx, starty = new.starty, ucoords = ucoords, radius = radius,backstep=backstep)
        
        
        imseq<-(new.zero.image-backstep):(new.zero.image+radius)
        for(i in imseq){
          imglist[[which(imseq==i)]]<-load.image(file=images[i])
        }
        for(j in 1:length(imglist)){
          imglist[[j]][which(imglist[[j]]<cutoff)]<-0
        }
        
        white.mat<-get.whites(imglist,lpl,depth,backstep=backstep)
        
        str.dist<-vector(length=nrow(white.mat))
        for(q in 1:length(str.dist)){
          other.coords<- lpl[[q]][nrow(lpl[[q]]),]-c(new.startx,new.starty,0)
          str.dist[q]<-sqrt(sum((traj.ref-other.coords)^2))
        }
        
        dist.check<-str.dist/max(str.dist)+1
        white.sd<-apply(white.mat,1,sd)
        white.sd<-white.sd/max(white.sd)
        
        white.optim<-white.sd * dist.check^scaler
        white.optim<-white.optim/max(white.optim)
        
        cross.check<-apply(ifelse(white.mat[,2:radius]==0,1,0),1,sum)
        cross.check<-ifelse(cross.check<=allowed.black,0,1000)
        end.check<-ifelse(white.mat[,depth]==0,1000,0)
        white.optim<-white.optim + cross.check + end.check
        
        best<-which(white.optim==min(white.optim))
        if(length(best)>1){sample(best,1)}
        
        rep.check<-row.match(c(0,0,new.zero.image)+lpl[[best]][nrow(lpl[[best]]),], as.data.frame(fib.track), nomatch=0)
        if(rep.check>0){best<-order(white.optim)[2]}
        
        
        
        if(sum(ifelse(white.optim>=1000,0,1))==0){break()}
        
        
        ##check if the end points of the possible lines are black (outside muscle)
        ends.black<-which(white.mat[,depth]==0)
        ends.black<-length(ends.black)/nrow(white.mat)
        
        if(ends.black>blackcut){break()} 
        
        new.dat<-lpl[[best]][nrow(lpl[[best]]),]
        traj.ref<-lpl[[best]][nrow(lpl[[best]]),]-c(new.startx,new.starty,0)
        new.startx<-new.dat[1]
        new.starty<-new.dat[2]
        new.zero<-new.dat[3]
        new.zero.image<-new.zero.image+new.zero
        
        
        
        if(new.zero.image>(length(images)-depth)){break()}
        
        
        if(boundary.check(images, new.zero.image, new.startx, new.starty, bound.buffer, cutoff)>0){break()} 
        
        
        
        tracker<-tracker+1
        fib.track[tracker,]<-c(new.startx,new.starty,new.zero.image)
        
        
        
        if(show.plot){
          plot(load.image(images[new.zero.image]),main="forward walk")
          points(new.startx,new.starty,col="red",pch=16,cex=1)
        }
      }
      
      
      
      
      
      
      ##########################################
      ################# backward ###############
      ##########################################
      if(verbose){cat("beginning backward walk from zero image for fiber",zz,"\n",sep=" ")
      }
      ###reset
      ucoords<-hemisphere.points(radius)
      lpl<-pointsGenerator(startx = startx, starty = starty, ucoords = ucoords, radius = radius,backstep=backstep)
      
      
      
      imseq<-zero.image:(zero.image-radius)
      imglist<-vector(length=depth,mode="list")
      for(i in imseq){
        imglist[[which(imseq==i)]]<-load.image(file=images[i])
      }
      for(j in 1:depth){
        imglist[[j]][which(imglist[[j]]<cutoff)]<-0
      }
      
      
      white.mat<-get.whites(imglist,lpl,depth,backstep=0)
      white.sd<-apply(white.mat,1,sd)
      white.sd<-white.sd/max(white.sd)
      
      
      
      dist.check<-rep(1,length=nrow(white.mat))
      
      
      
      tracker<-1
      fib.trackback<-matrix(ncol=3,nrow=length(images))
      ## record starting point
      fib.trackback[1,]<-c(startx,starty,zero.image)
      
      ############ launch ##############
      
      
      
      
      white.optim<-white.sd * dist.check^scaler 
      white.optim<-white.optim/max(white.optim)
      
      cross.check<-apply(ifelse(white.mat[,2:radius]==0,1,0),1,sum)
      cross.check<-ifelse(cross.check<=allowed.black,0,1000)
      end.check<-ifelse(white.mat[,depth]==0,1000,0)
      white.optim<-white.optim + cross.check + end.check
      
      
      
      ####### first step
      best<-which(white.optim==min(white.optim))
      if(length(best)>1){best<-sample(best,1)}
      
      
      
      rep.check<-row.match(lpl[[best]][nrow(lpl[[best]]),], as.data.frame(fib.trackback), nomatch=0)
      if(rep.check>0){best<-order(white.optim)[2]}
      
      
      
      new.dat<-lpl[[best]][nrow(lpl[[best]]),]
      traj.ref<-lpl[[best]][nrow(lpl[[best]]),]-c(startx,starty,0)
      new.startx<-new.dat[1]
      new.starty<-new.dat[2]
      new.zero<-new.dat[3]
      new.zero.image<-zero.image-new.zero
      
      if(show.plot){
        plot(load.image(images[new.zero.image]),main="backward walk")
        points(new.startx,new.starty,col="red",pch=16,cex=1)
      }
      
      
      
      tracker<-tracker+1
      fib.trackback[tracker,]<-c(new.startx,new.starty,new.zero.image)
      
      ##### repeater for steps 3 and onward
      
      for(z in 1:length(images)){
        
        
        
        ucoords<-hemisphere.points(radius,backstep=backstep)
        lpl<-pointsGenerator(startx = new.startx, starty = new.starty, ucoords = ucoords, radius = radius,backstep=backstep)
        
        
        ### new image list
        imseq<-(new.zero.image+backstep):(new.zero.image-radius)
        for(i in imseq){
          imglist[[which(imseq==i)]]<-load.image(file=images[i])
        }
        for(j in 1:depth){
          imglist[[j]][which(imglist[[j]]<cutoff)]<-0
        }
        
        
        
        white.mat<-get.whites(imglist,lpl,depth,backstep=backstep)
        
        str.dist<-vector(length=nrow(white.mat))
        for(q in 1:length(str.dist)){
          other.coords<- lpl[[q]][nrow(lpl[[q]]),]-c(new.startx,new.starty,0)
          str.dist[q]<-sqrt(sum((traj.ref-other.coords)^2))
        }
        
        
        dist.check<-str.dist/max(str.dist)+1
        
        
        
        white.sd<-apply(white.mat,1,sd)
        white.sd<-white.sd/max(white.sd)
        
        white.optim<-white.sd * dist.check^scaler
        white.optim<-white.optim/max(white.optim)
        
        cross.check<-apply(ifelse(white.mat[,2:radius]==0,1,0),1,sum)
        cross.check<-ifelse(cross.check<=allowed.black,0,1000)
        end.check<-ifelse(white.mat[,depth]==0,1000,0)
        white.optim<-white.optim + cross.check + end.check
        
        if(sum(ifelse(white.optim>=1000,0,1))==0){break()}
        
        
        ##check if the end points of the possible lines are black (outside muscle)
        ends.black<-which(white.mat[,depth]==0)
        ends.black<-length(ends.black)/nrow(white.mat)
        
        if(ends.black>blackcut){break()} 
        
        best<-which(white.optim==min(white.optim))
        if(length(best)>1){best<-sample(best,1)}
        
        rep.check<-row.match(c(0,0,new.zero.image)+lpl[[best]][nrow(lpl[[best]]),]*(c(1,1,-1)), as.data.frame(fib.trackback), nomatch=0)
        if(rep.check>0){best<-order(white.optim)[2]}
        
        
        new.dat<-lpl[[best]][nrow(lpl[[best]]),]
        traj.ref<-lpl[[best]][nrow(lpl[[best]]),]-c(new.startx,new.starty,0)
        new.startx<-new.dat[1]
        new.starty<-new.dat[2]
        new.zero<-new.dat[3]
        new.zero.image<-new.zero.image-new.zero
        if(new.zero.image<depth){break()}
        
        imseq<-new.zero.image:(new.zero.image-radius)
        for(i in imseq){
          imglist[[which(imseq==i)]]<-load.image(file=images[i])
        }
        for(j in 1:depth){
          imglist[[j]][which(imglist[[j]]<cutoff)]<-0
        }
        
        tracker<-tracker+1
        fib.trackback[tracker,]<-c(new.startx,new.starty,new.zero.image)
        
        if(show.plot){
          plot(load.image(images[new.zero.image]),main="backward walk")
          points(new.startx,new.starty,col="red",pch=16,cex=1)
        }
        
        if(boundary.check(images, new.zero.image, new.startx, new.starty, bound.buffer, cutoff)>0){
          break()
        } 
      }
      
      
      fib.track<-fib.track[!is.na(fib.track[,1]),]
      fib.trackback<-fib.trackback[!is.na(fib.trackback[,1]),]
      
      ntb<-nrow(fib.trackback)
      
      fib.final<-rbind(fib.trackback[ntb:1,], fib.track[2:nrow(fib.track),])
      
      res.list[[zz]]<-list(fiber.points=fib.final)
      
    }, error = function(msg){res.list[[zz]]<-list(fiber.points=NA)})
    
    
  }
  
  if(seeds == 1){
    res.list<-res.list[[1]]
  }
  drop.check<-sapply(res.list,function(x){length(x$fiber.points)}) 
  keeps<-which(drop.check>1)
  if(length(keeps)<seeds & verbose){cat(seeds-length(keeps),"fiber did not run properly, dropped\n",sep=" ")}
  res.list<-res.list[keeps]
  
  return(res.list)
  
}
