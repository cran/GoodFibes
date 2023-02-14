boundary.check <-
function(images, new.zero.image, new.startx, new.starty, bound.buffer, cutoff){
  slice<-which(!load.image(images[new.zero.image])==0, arr.ind = T)[,1:2]
  slice[which(slice<cutoff)]<-0
  boundaries<-concaveman(slice)
  distances<-apply(boundaries,1,function(x){sqrt(sum((x-c(new.startx,new.starty))^2))})
  res<-sum(ifelse(distances<bound.buffer,1,0))
  return(res)
}
