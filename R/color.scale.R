color.scale <-
function(fl,cols,min.fl=NULL,max.fl=NULL){
  if((cols=="viridis")[1]){cols<-c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF","#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF")}
  if((cols=="turbo")[1]){cols<-c("#30123BFF", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF", "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF","#CB2A04FF", "#7A0403FF")}
  if((cols=="inferno")[1]){cols<-c("#000004FF", "#1B0C42FF", "#4B0C6BFF", "#781C6DFF", "#A52C60FF", "#CF4446FF", "#ED6925FF", "#FB9A06FF","#F7D03CFF", "#FCFFA4FF")}
  if((cols=="rainbow")[1]){cols<-c("#FF0000", "#FF8000", "#FFFF00", "#80FF00", "#00FF00", "#00FF80", "#00FFFF", "#0080FF", "#0000FF",
                                   "#8000FF")}
  
  
  colfunc <- colorRampPalette(cols)
  cols<-colfunc(200)
  if(is.null(min.fl)){fl<-c(fl,min(fl))} else {fl<-c(fl,min.fl)}
  if(is.null(max.fl)){fl<-c(fl,max(fl))} else {fl<-c(fl,max.fl)}
  
  scale.fl<-(fl-min(fl))/(max(fl)-min(fl))*199+1
  final.cols<-cols[scale.fl[1:(length(scale.fl)-2)]]
  
}

