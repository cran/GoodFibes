thresholdPlot <-
function(images, n, threshold){
  slice<-load.image(images[[n]])
  slice[which(slice<threshold)]<-0
  plot(slice, rescale=FALSE)
  return(slice)
}
