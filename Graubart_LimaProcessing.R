#Setup
rm(list=ls(all=TRUE)) # clear memory

packages<- c("plyr","foreign") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:\\GISWork_2\\Graubart_Lima\\Lima") 


input.table <- read.csv("1613_census_table.csv",stringsAsFactors=F)


#Processing the shapetable to make a tie between blocks and ids
shp.table <- read.dbf("Export_Output_2.dbf",as.is=T)
shp.table <- shp.table[complete.cases(shp.table),]
temp <- strsplit(as.character(shp.table$Indices),";",fixed=TRUE)
n <- sapply(temp, length)
shp.table2 <- shp.table[rep(seq_len(nrow(shp.table)),times=n),]
shp.table2$block.no <- unlist(temp)
remove(shp.table)
#pert <- as.numeric(strsplit(paste(shp.table$Indices, collapse = ";"), ";")[[1]])



#totals
totals <-as.data.frame(table(input.table$block.no))
names(totals)[names(totals) == 'Var1']    <- 'block.no'
names(totals)[names(totals) == 'Freq']    <- 'total'

shp.table2<-merge(shp.table2, totals, by="block.no",all.x = T)

#gender
input.table$female.[input.table$female.  == " f"] <- "f"
gender <- as.data.frame.matrix(xtabs(~block.no+female., data=input.table))
gender$block.no <- as.numeric(rownames(gender))
gender$V1 <- NULL


#marital Status
input.table$marital.stat[input.table$marital.stat  == "c "] <- "c"
input.table$marital.stat[input.table$marital.stat  == "C"] <- "c"
input.table$marital.stat[input.table$marital.stat  == " "] <- ""
input.table$marital.stat[input.table$marital.stat  == "divorciada"] <- "d"
mar <- as.data.frame.matrix(xtabs(~block.no+marital.stat, data=input.table))
mar$block.no <- as.numeric(rownames(mar))
mar$V1 <- NULL



both <-merge(mar,gender,by="block.no")

with.blockID <- merge(shp.table2, both, by="block.no", all.x = T)
with.blockID$block.no <- NULL
with.blockID$Id <- NULL
with.blockID$Indices <- NULL
agged <-aggregate(. ~ Block_ID, data=with.blockID, FUN=sum)
write.csv(agged,"r_out_table.csv")
