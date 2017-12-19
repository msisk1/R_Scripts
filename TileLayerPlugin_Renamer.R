rm(list=ls(all=TRUE)) # clear memory
packages<- c("tools", "foreign","png","jpeg") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first


# setwd("C:/Temp/Tiles") 
setwd("/home/matthew/Documents/HUE") 
stub <- "ArcStreet"


all.jpeg.tiles <- list.files(path = stub,pattern="\\.jpeg$")
out.folder <- paste(stub,"_Processed",sep="")
#pattern = FOLDER-FILE-ZOOM

for (each.jpeg.tile in all.jpeg.tiles){
  each.all <- file_path_sans_ext(each.jpeg.tile)
  print(each.all)
  parts <- strsplit(each.all, "-")[[1]]
  zoomy <- parts[3]
  foldery <- parts[1]
  filey <- parts[2]
  
  if (!file.exists(out.folder)){
    dir.create(out.folder)
  }
  if (!file.exists(paste(out.folder,zoomy,sep="/"))){
    dir.create(paste(out.folder,zoomy,sep="/"))
  }
  if (!file.exists(paste(out.folder,zoomy,foldery,sep="/"))){
    dir.create(paste(out.folder,zoomy,foldery,sep="/"))
  }
  
  each.path <- paste(out.folder,"/",zoomy,"/",foldery,"/",filey,".png",sep = "")
  each.jpg <- readJPEG(paste(stub,"/",each.jpeg.tile,sep=""))
  writePNG(each.jpg,each.path)
}
