fibers.smoothed <-
function(fib.list, df){
fiber.smoothed<-vector(mode="list",length=length(fib.list))
  
for(j in 1:length(fib.list)){
  fiber.dat<-fib.list[[j]]$fiber.points
  fb.df <- data.frame(x = fiber.dat[, 1], y = fiber.dat[,2], z = fiber.dat[, 3])
  if(df==1){ fit <- tryCatch(lm(cbind(x,y)~z, data = fb.df))
  } else {
  fit <- tryCatch(lm(cbind(x,y)~nsp(z,df=df), data = fb.df))}
  fib.smooth<-cbind(predict(fit), fiber.dat[,3])
  fiber.smoothed[[j]]<-list(fiber.points<-fib.list[[j]], fiber.smoothed<-fib.smooth)
  }
return(fiber.smoothed)
}
