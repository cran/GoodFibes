equalize.stack <-
function(images, n, save.images=FALSE){
 
  slice<-load.image(images[[n]])
  
  eqf <- ecdf(slice)
  eqslice<-as.cimg(eqf(slice),dim=dim(slice))
  eqslice<-eqslice-min(eqslice)
  eqslice<-eqslice/max(eqslice)
  plot(eqslice)
  
  if(save.images){
  for(i in 1:length(images)){
    slice<-load.image(images[[i]])
    greys<-slice[which(slice>0)]
    
    if(length(greys)==0){save.image(slice,file=paste("eq_",images[i],".png",sep=""))
    } else {
      
      eqf <- ecdf(slice)
      eqslice<-as.cimg(eqf(slice),dim=dim(slice))
      eqslice<-eqslice-min(eqslice)
      eqslice<-eqslice/max(eqslice)
      save.image(eqslice, file=paste("eq_",images[i],".png",sep=""))
    }
  }
  }
 
}
