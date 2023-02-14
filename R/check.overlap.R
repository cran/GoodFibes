check.overlap <-
function(fiber.list, min.vox, df = 2){
fl<-fiber.lengths(fiber.list, df = df)
close.mat<-c(NA,NA,NA,NA)
for(i in 1:length(fiber.list)){
for(j in 1:length(fiber.list)){
if(i==j){next()}
two.fibs.list<-rbind(fiber.list[[i]]$fiber.points,fiber.list[[j]]$fiber.points)
fib1row<-nrow(fiber.list[[i]]$fiber.points)
fib2row<-nrow(fiber.list[[j]]$fiber.points)
dist.mat<-as.matrix(dist(two.fibs.list))
min.dists<-apply(dist.mat[1:fib1row,(fib1row+1):(fib1row+fib2row)],1,min)
if(mean(min.dists)<=min.vox){
  close.mat<-rbind(close.mat,c(i,j,mean(min.dists),max(dist.mat[1:fib1row,(fib1row+1):(fib1row+fib2row)])))
} else {next()}
}
}

if(!is.null(dim(close.mat))){
close.mat<-close.mat[2:nrow(close.mat),]
close.mat<-close.mat[order(close.mat[,3]),]
}


if(is.null(dim(close.mat))){message("there are no overlapping fibers\n")
  drops<-0
  return(drops)
} else {
  drops<-c(NA,NA)
    for(i in 1:nrow(close.mat)){
      if(i>1){
        if(close.mat[i,1] %in% drops[,2] | close.mat[i,2] %in% drops[,2]){next()}}
      check<-fl[close.mat[i,1]]-fl[close.mat[i,2]]
      drops<-rbind(drops, 
                   c(ifelse(check>=0,close.mat[i,1],close.mat[i,2]),
                     ifelse(check>=0,close.mat[i,2],close.mat[i,1])))
    }
  
colnames(drops)<-c("keep","discard")


drops<-drops[2:nrow(drops),]
return(list(drop.fibers=sort(unique(drops[,2])),overlapping.fibers=drops, fibers.removed=fiber.list[-unique(drops[,2])]))
}
}
