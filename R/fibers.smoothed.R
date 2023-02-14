fibers.smoothed <-
function(fib.list, df){
fiber.smoothed<-vector(mode="list",length=length(fib.list))
  
for(j in 1:length(fib.list)){
  fiber.dat<-fib.list[[j]]$fiber.points
  fit <- lm(fiber.dat[,1:2] ~ splines::ns(fiber.dat[,3], df = df))
  fib.smooth<-cbind(predict(fit), fiber.dat[,3])
  fiber.smoothed[[j]]<-list(fiber.points<-fib.list[[j]], fiber.smoothed<-fib.smooth)
  }
return(fiber.smoothed)
}
