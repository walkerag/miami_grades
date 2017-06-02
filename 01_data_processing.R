#####################################
#STAGE 1 PROCESSING
#####################################

#Import txt files created using pdftotext
#Keep desired rows, format columns, then save

path<-'/Users/walkerag/Documents/miami_grades/raw_txt_files/'

rm(list=ls())
gc()

library(stringr)
library(plyr)

#File read-in
#This was a very manual process due to idiosyncracies of particular PDFs, hence no loop

year<-1999
year<-2000
year<-2001
year<-2002
year<-2003
year<-2004
year<-2005
year<-2006
year<-2007
year<-2008
year<-2009
year<-2010
year<-2011
year<-2012
year<-2013
year<-2014
year<-2015
year<-2016
semester<-10
semester<-20
semester<-15
semester<-30

if(semester==10){
  file <- read.table(file=paste0(path,year+1,"10 - Fall ",year,".txt"), 
                     sep='\t', quote=NULL, comment='', header=FALSE,stringsAsFactors = FALSE)
}

if(semester==15){
  file <- read.table(file=paste0(path,year,"15 - Winter ",year,".txt"), 
                     sep='\t', quote=NULL, comment='', header=FALSE,stringsAsFactors = FALSE)
}

if(semester==20){
  file <- read.table(file=paste0(path,year,"20 - Spring ",year,".txt"), 
                     sep='\t', quote=NULL, comment='', header=FALSE,stringsAsFactors = FALSE)
}

if(semester==30){
  file <- read.table(file=paste0(path,year,"30 - Summer ",year,".txt"), 
                     sep='\t', quote=NULL, comment='', header=FALSE,stringsAsFactors = FALSE)
}

#Remove multiple spaces
test<-gsub("\\s+", " ", test)

#Split out into cols by space
test<-strsplit(as.character(test),' ',fixed=TRUE)
x<-lengths(test)
max(x)
min(x)

#Rbind
test2<-rbind.fill(lapply(test,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
print('Done')
rm(test)

#First 24 cols
test2<-test2[,1:24]

#Remove unnecessary fields in first column
test2<-test2[!(test2$V1 %in% c('Grade','Run:','Program:','Office','Oxford','Miami','UG','GR','A+','%')),]
#View(test2)
test2<-subset(test2,select=-c(V1))

#Remove unnecessary fields in second column
test2<-test2[!(test2$V2 %in% c('Grade','Run:','Program:','Office','Oxford','Miami','UG','GR','A+','%')),]
#View(test2)

test2<-test2[!is.na(test2$V4),]
test2<-test2[test2$V2 %in% c('Course') | !is.na(test2$V5),]
test2<-test2[test2$V2 %in% c('Course') | !is.na(test2$V6),]
test2<-test2[test2$V2 %in% c('Course') | !is.na(test2$V7),]
#test2<-test2[test2$V2 %in% c('Course') | !is.na(test2$V8),]
test2<-test2[!(test2$V2 %in% c('PHS')) | !(test2$V3 %in% c('Phy')),]
test2<-test2[!(test2$V2 %in% c('Family')) | !(test2$V3 %in% c('Studies')),]
test2<-test2[!(test2$V2 %in% c('Phy')) | !(test2$V3 %in% c('Edu,')),]
test2<-test2[!(test2$V2 %in% c('ARC')) | !(test2$V3 %in% c('Architecture')),]
test2<-test2[!(test2$V2 %in% c('FSW')) | !(test2$V3 %in% c('Family')),]
test2<-test2[!(test2$V2 %in% c('GRK')) | !(test2$V3 %in% c('Greek')),]
test2<-test2[!(test2$V2 %in% c('LAT')) | !(test2$V3 %in% c('Latin')),]
test2<-test2[!(test2$V2 %in% c('PPS')) | !(test2$V3 %in% c('Paper')),]
test2<-test2[!(test2$V2 %in% c('SPA')) | !(test2$V3 %in% c('Speech')),]
test2<-test2[!(test2$V2 %in% c('LAT')) | !(test2$V3 %in% c('Latin')),]
test2<-test2[!(test2$V2 %in% c('CSA')) | !(test2$V3 %in% c('Computer')),]
test2<-test2[!(test2$V2 %in% c('PSE')) | !(test2$V3 %in% c('Paper')),]
test2<-test2[!(test2$V2 %in% c('ECE')) | !(test2$V3 %in% c('Electrical')),]
test2<-test2[!(test2$V2 %in% c('EAS')) | !(test2$V3 %in% c('Engineering')),]
test2<-test2[!(test2$V2 %in% c('MME')) | !(test2$V3 %in% c('Manufact')),]
test2<-test2[!(test2$V2 %in% c('PCE')) | !(test2$V3 %in% c('Paper')),]
test2<-test2[!(test2$V2 %in% c('EHS')) | !(test2$V3 %in% c('Education,')),]
test2<-test2[!(test2$V2 %in% c('CSE')) | !(test2$V3 %in% c('Comp')),]
test2<-test2[!(test2$V3 %in% c('American')),]
test2<-test2[!(test2$V3 %in% c('Chemical')),]
test2<-test2[!(test2$V3 %in% c('Culture')),]
test2<-test2[!(test2$V3 %in% c('School')),]
test2<-test2[!(test2$V3 %in% c('Chem,')),]
test2<-test2[!(test2$V3 %in% c('Civic')),]
test2<-test2[!(test2$V3 %in% c('Col')),]
test2<-test2[!(test2$V3 %in% c('College')),]
test2<-test2[!(test2$V3 %in% c('Mechan')),]
test2<-test2[!(test2$V3 %in% c('Of')),]
test2<-test2[!(test2$V3 %in% c('Paper')),]

#Get course info and grade counts
counts<-test2[!is.na(test2$V23),]
class<-test2[is.na(test2$V23),]

sort(unique(class$V3))

#If equal merge together and rename fields
if(dim(class)[1]==dim(counts)[1]){
  
  print('Match')
  comb<-cbind(class,counts)
  
  #Keep needed cols and rename
  comb<-comb[,c(1:14,24:45)]
  names(comb)<-c("Dept","Course","Section","Last","First","Middle",
                 "C1","C2","C3","C4","C5","C6","C7","C8",
                 "A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F","W","WP","WF","I","X","Y","P","S","GPA")
  rm(class)
  rm(counts)
  rm(test2)
  rm(file)
  
  saveRDS(comb,paste0(path,"miami_",year,"_",semester))
  View(tail(comb))

}else(print('Mismatch'))




