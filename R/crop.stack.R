crop.stack <-
function(images, bounds=NULL, save.images=FALSE){
  
  
  if(!is.null(bounds)){
    xmin<-bounds[1]
    xmax<-bounds[2]
    ymin<-bounds[3]
    ymax<-bounds[4]
  } else {
  
  boundaries<-vector(length = length(images),mode="list")
  for(i in 1:length(boundaries)){
    slice<-which(!load.image(images[i])==0, arr.ind = T)[,1:2]
    if(!is.matrix(slice)){next()}
    if(dim(slice)[1]==0){next()}
    boundaries[[i]]<-concaveman(slice)
  }
  
  xmins<-rep(NA,length(boundaries))
  xmaxs<-xmins
  ymins<-xmins
  ymaxs<-xmins
  
  for(i in 1:length(boundaries)){
    
    
    if(is.null(boundaries[[i]])){next()}
    
  xmins[i]<-min(boundaries[[i]][,1])
  xmaxs[i]<-max(boundaries[[i]][,1])
  ymins[i]<-min(boundaries[[i]][,2])
  ymaxs[i]<-max(boundaries[[i]][,2])
  
}
  xmin<-min(na.omit(xmins))-15
  xmax<-max(na.omit(xmaxs))+15
  ymin<-min(na.omit(ymins))-15
  ymax<-max(na.omit(ymaxs))+15
  
  if(xmin<0){xmin<-0}
  if(ymin<0){ymin<-0}
  }

  message("cropping images")
  for(i in 1:length(images)){
    temp<-load.image(file=images[[i]])
    
    ###### following code taken from imager::imsub
    ###### associated functions from imager
        subs<-function (im, cl, consts, envir = parent.frame()){
      if (missing(consts)) {
        consts <- list(width = width(im), height = height(im), 
                       depth = depth(im), spectrum = spectrum(im))
      }
      vl <- intersect(all.names(cl), c("x", "y", "z", "cc"))
      if (length(vl) > 1) {
        stop("Use only one of x,y,z,cc at a time")
      }
      else {
        vname <- vl
        mval <- list(x = width(im), y = height(im), z = depth(im), 
                     cc = spectrum(im))
        maxval <- mval[[vl]]
        consts[[vname]] <- 1:maxval
        inds <- eval(cl, list2env(consts, envir))
        if (vname == "x") {
          (as.array(im)[inds, , , , drop = FALSE]) %>% cimg
        }
        else if (vname == "y") {
          (as.array(im)[, inds, , , drop = FALSE]) %>% cimg
        }
        else if (vname == "z") {
          (as.array(im)[, , inds, , drop = FALSE]) %>% cimg
        }
        else {
          (as.array(im)[, , , inds, drop = FALSE]) %>% cimg
        }
      }
    }
    
    mutate_plyr<-function (.data, ...) 
    {
      stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
      cols <- as.list(substitute(list(...))[-1])
      cols <- cols[names(cols) != ""]
      for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
      }
      .data
    }
    
    
    
    
    im<-temp
    l <- as.list(substitute(list(x %inr% c(xmin,xmax),y %inr% c(ymin,ymax)))[-1])
    consts <- list(width = width(im), height = height(im), depth = depth(im), spectrum = spectrum(im))
    consts <- mutate_plyr(consts, cx = width/2, cy = height/2, 
                                   cz = depth/2)
    env <- new.env(parent = parent.frame())
    newim<-Reduce(function(a, b) subs(a, b, consts, envir = env), l, 
           init = im)
    ####### end code from imager::imsub
    
    if(save.images){save.image(newim,file=paste("cropped_", images[i],".png",sep=""))}
  }
  return(list(xmin =  xmin, xmax = xmax, ymin = ymin, ymax = ymax))
}
