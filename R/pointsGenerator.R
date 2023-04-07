pointsGenerator <-
function(startx, starty, ucoords, radius=radius, backstep){
  
  start.points<-cbind(rep(startx,radius+1),rep(starty,radius+1),rep(0,radius+1))
  if(backstep==0){
  ucoords<-ucoords[-which(ucoords[,3]==0),]}
  line.point.list<-vector(length = nrow(ucoords),mode="list")
  
  n<-nrow(ucoords)
  
  for(i in 1:n){
    
    x<-round(seq(0,ucoords[i,1],length.out=(radius+1)),0)
    y<-round(seq(0,ucoords[i,2],length.out=(radius+1)),0)
    z<-round(seq(0,ucoords[i,3],length.out=(radius+1)),0)
    line.points<-cbind(x,y,z)
    line.point.list[[i]]<-line.points+start.points
  }
  
  return(line.point.list) 
}
