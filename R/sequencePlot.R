sequencePlot <-
function(fib.track,images,threshold=0.1,sleep.time=0.5){
  
  for(i in 1:nrow(fib.track)){
    im1<-load.image(images[fib.track[i,3]])
    im1[which(im1<threshold)]<-0
    plot(im1)
    points(fib.track[i,1],fib.track[i,2],col="red",pch=16,cex=0.75)
    Sys.sleep(sleep.time)
  }
  
}
