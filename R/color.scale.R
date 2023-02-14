color.scale <-
function(fl,color1,color2){
  
  colfunc <- colorRampPalette(c(color1,color2))
  cols<-colfunc(length(fl)+1)
  
  scale.fl<-(fl-min(fl))/(max(fl)-min(fl))*length(fl)+1
  final.cols<-cols[scale.fl]
  
}
