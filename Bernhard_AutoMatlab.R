# This code converts some matlab files for injest into Notre Dame's Institutional Repositiory'

#Setup


rm(list=ls(all=TRUE)) # clear memory


#install.packages("reshape")
packages<- c("R.matlab", "ggplot2", "reshape", "tools") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Bernhard_AutoMatlab\\AutoMobileVariabilityData")



create.text.file<-function(first.name, second.name, num.records){
        ### All files have PIK (= pickup) or ROD (=Rodeo)
        ### Some files have AB (= airborne) or SB (= structure-borne)
        ### Some files have REF (= reference vehicle remeasured after 9 other vehicle to test repeatability and variation occurring on the reference vehicle). 
        
        if (first.name == "FAB"){
                substrate.type <- "Air"
        }else if(first.name == "FSB"){
                substrate.type <- "Structure"
        }else{
                print("ERROR!!! in First")       
        }#end  if (first.name == "fab"){
        
        if (grepl("PIK", second.name)){
                car.type <- "Pickup"
        }else if (grepl("ROD", second.name)) {
                car.type <- "Rodeo"
        }else{
                print("ERROR!!! in Second")
        }
        if (grepl("REF", second.name)){
                reference <- TRUE
                ref.text <- "Reference"
        }else{
                reference <- FALSE
                ref.text <-""
        }
        
        
        brk <-""
        #l1 <-  paste("Magnitude of the frequency response function for the 57 Pickups: Structure-borne.",sep="")
        l1 <-  paste("Magnitude of the frequency response function for ",car.type,"s (",num.records,"): ",substrate.type,"-borne ",ref.text,"",sep="")
        
        l2 <-  paste("All data stored in the Matlab file are then duplicated for preservation in flat CSV files",sep="")
        l3 <-  paste("1). Matlab file: ",second.name,".MAT containing both tables ",sep="")
        l4 <-  paste("2). CSV File: ",second.name,".CSV: Only containing the independent variables of the magnitude of frequency response function (dB, the independent variables) ",sep="")
        l5 <-  paste("3). CSV File: ",first.name,".CSV: The frequencies for Structure-borne (Hz; the dependent Variable)",sep="")
        if(reference){
                l6 <- "This is a reference vehicle remeasured after 9 other vehicle to test repeatability and variation occurring on the reference vehicle"
        }else{
                l6 <- ""
        }
        l7 <-  paste(second.name," - Matlab",sep="")
        l8 <-  paste(second.name," - CSV: Magnitude of Frequency Response Function (Independent Variables)",sep="")
        l9 <- paste(first.name," - CSV: Frequency Structure-borne (Dependent Variable)",sep="")
        returned <- c(l1,brk,l2,l3,l4,l5,l6,brk,l7,l8,l9)
        returned
}#end create.text.file



files <- list.files(path=getwd(), pattern="*.MAT", full.names=T, recursive=FALSE)


lapply(files, function(x) {
        mat.file <- readMat(x)
        one.path <- file_path_sans_ext(x)
        if (!file.exists(one.path)){dir.create(one.path)} #Creates A new folder to hold all of them
        
        first.name <- toupper(names(mat.file)[1])
        first.list <- mat.file[1]
        first.data <- data.frame(mat.file[1])
        first.file <- paste(one.path,"\\",first.name,".csv", sep="")
        write.table( first.data, first.file, sep=",",  col.names=FALSE, row.names = FALSE)
        first.trans <- data.frame(t(first.data))
        
        second.name <- toupper(names(mat.file)[2])
        second.list <- mat.file[2]
        second.data <- data.frame(mat.file[2])
        second.file <- paste(one.path,"\\",second.name,".csv", sep="")
        write.table( second.data, second.file, sep=",",  col.names=FALSE, row.names = FALSE)
        second.trans <- data.frame(t(second.data))
        num.records <- NROW(second.data)
        file.copy(x, paste(one.path, "\\", basename(x),".mat", sep=""))
        text.all <- create.text.file(first.name,second.name,num.records)
        fileConn<-file(paste(one.path, "\\", basename(x),"_notes.txt", sep=""))
        writeLines(text.all, fileConn)
        close(fileConn)
        
})



#Plots
all.together <- cbind(first.trans, second.trans)   #Merging the datasets
names(all.together)[names(all.together) == 't.first.data.'] <- first.name #Renaming
all.melted <- melt(all.together, id = first.name)    #Creating a melted dataset for plotting
myplot <- function(df, x_string, y_string, group_string) {
  ggplot(df, aes_string(x = x_string, y = y_string, group = group_string, color = group_string)) + geom_line()
}    #Function for plotting
myplot(all.melted, first.name, "value","variable")

