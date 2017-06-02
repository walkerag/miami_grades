#####################################
#STAGE 2 PROCESSING: MORE PROCESSING!
#####################################

#Import partially formatted txt files, then do some more formatting
#Main issue is spillover of variables due to unequal field lengths: e.g. some professor names ended up in the course name
#Added a few variables, such as flagging Luxembourg and Honors classes
#Adjusted the department codes to account for changes over the years

rm(list=ls())
gc()

library(stringr)
library(plyr)
library(tidyverse)
library(extrafont)
library(scales)
library(reshape)
library(ggrepel)
library(stringr)
library(readability)
library(syllable)
library(betareg)
library(ggplot2)

path<-'/Users/walkerag/Documents/miami_grades/raw_txt_files/'
out_path<-'/Users/walkerag/Documents/miami_grades/formatted_data/'

########################################
#DATA READ-IN
########################################

years<-seq(from=1999,to=2016,by=1)
semesters<-c(10,15,20,30)
dat_all<-NULL
for(i in 1:length(years)){
  
  for(j in 1:length(semesters)){
    
    print(i)
    print(j)
    
    dat<-NULL
    
    try(
      dat<-readRDS(paste0(path,"miami_",years[i],"_",semesters[j]))
      ,TRUE
    )
    
    if(length(dat)>0){
      
      print('Success')
      dat$year<-years[i]
      dat$semester<-semesters[j]
      dat_all<-rbind(dat_all,dat)
      
    }
    
  }
  
}

#Check years and semester
summ <- dat_all %>% group_by(year,semester) %>% summarise(total=n())
#View(summ)

#Format years and semesters so ordering is correct

dat_all$semester_name<-'NA'
dat_all[dat_all$semester==10,"semester_name"]<-"Fall"
dat_all[dat_all$semester==15,"semester_name"]<-"Winter"
dat_all[dat_all$semester==20,"semester_name"]<-"Spring"
dat_all[dat_all$semester==30,"semester_name"]<-"Summer"
table(dat_all$semester_name)

dat_all$semester_num<-0
dat_all[dat_all$semester==10,"semester_num"]<-4
dat_all[dat_all$semester==15,"semester_num"]<-1
dat_all[dat_all$semester==20,"semester_num"]<-2
dat_all[dat_all$semester==30,"semester_num"]<-3
table(dat_all$semester_num)

#Concat year and semester number
dat_all$semester_year_num<-as.numeric(paste0(dat_all$year,dat_all$semester_num))

#Check years and semester
summ <- dat_all %>% group_by(semester_year_num) %>% summarise(total=n())
#View(summ)
#Now sort in correct order

#Remove course totals
dat_all<-dat_all[dat_all$Dept!="Course",]

#########################################
#FORMAT PROFESSOR NAMES AND COURSE TITLES
#########################################

#If professor's last name is one character, move names over
rows<-nchar(dat_all$Last)==1
sum(rows)
dat_all[rows,"Section"]<-paste0(dat_all[rows,"Section"]," ",dat_all[rows,"Last"])
dat_all[rows,"Last"]<-dat_all[rows,"First"]
dat_all[rows,"First"]<-dat_all[rows,"Middle"]
dat_all[rows,"Middle"]<-""
head(dat_all[rows,])

#If C1 looks like a middle name, move over
rows<-nchar(dat_all$C1)==2 & grepl("[.]",dat_all$C1) & dat_all$Middle==""
sum(rows)
dat_all[rows,"Middle"]<-dat_all[rows,"C1"]
dat_all[rows,"C1"]<-""
head(dat_all[rows,])

#If C1 looks like a middle name, and all names slots are filled, delete
rows<-nchar(dat_all$C1)==2 & grepl("[.]",dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-""
head(dat_all[rows,])

#If last name is >2 and middle is >2, and first does not look like a middle, middle is probably part of course title
unique(dat_all[nchar(dat_all$First)==2 & nchar(dat_all$Middle)>2 & grepl("[.]",dat_all$First),"Middle"])

#Fix some rare cases
rows<-dat_all$Middle=="Understanding"
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""
head(dat_all[rows,])

#Fix double-barrelled surname issue
rows<-dat_all$Middle %in% c("Gwendoline","Jacqueline")
sum(rows)
dat_all[rows,"Last"]<-paste0(dat_all[rows,"Last"],"-",dat_all[rows,"First"])
dat_all[rows,"First"]<-dat_all[rows,"Middle"]
dat_all[rows,"Middle"]<-dat_all[rows,"C1"]
dat_all[rows,"C1"]<-""
head(dat_all[rows,])

#Should all be courses
sort(unique(dat_all[nchar(dat_all$Middle)>2 & !grepl("[.]",dat_all$First) & dat_all$C1!="","Middle"]),decreasing=TRUE)
rows<-nchar(dat_all$Middle)>2 & !grepl("[.]",dat_all$First) & dat_all$C1!="" & !(dat_all$Middle %in% c('Sophie'
                                                                                                       ,'Sidury'
                                                                                                       ,'Sandhya'
                                                                                                       ,'Rudy'
                                                                                                       ,'Raisa'
                                                                                                       ,'Mostafa'
                                                                                                       ,'Louise'
                                                                                                       ,'Lauren'
                                                                                                       ,'Keith'
                                                                                                       ,'Karen'
                                                                                                       ,'Kathleen'
                                                                                                       ,'Katia'
                                                                                                       ,'Judith'
                                                                                                       ,'Jean'
                                                                                                       ,'Jane'
                                                                                                       ,'Jennifer'
                                                                                                       ,'Guy'
                                                                                                       ,'Eva'
                                                                                                       ,'Cristina'
                                                                                                       ,'Alice'
                                                                                                       ,'Stephen'
                                                                                                       ,'Michael'))
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""
head(dat_all[rows,])

#Find and adjust single character middle names
unique(dat_all[nchar(dat_all$Middle)==1,"Middle"])
dat_all[dat_all$Middle=="-" & !is.na(dat_all$C1),]

rows<-dat_all$Middle=="A" & !is.na(dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""

rows<-dat_all$Middle=="N" & !is.na(dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""

rows<-dat_all$Middle=="E" & !is.na(dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""

rows<-dat_all$Middle=="I" & !is.na(dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""

rows<-dat_all$Middle=="-" & !is.na(dat_all$C1)
dat_all<-dat_all[!rows,]

rows<-nchar(dat_all$Middle)==2 & !grepl("Vi",dat_all$Middle) & !grepl("[.]",dat_all$Middle) & !is.na(dat_all$C1)
sum(rows)
dat_all[rows,"C1"]<-paste0(dat_all[rows,"Middle"]," ",dat_all[rows,"C1"])
dat_all[rows,"Middle"]<-""
head(dat_all[rows,])

#########################################
#ADDITIONAL VARIABLES AND FORMATTING
#########################################

#Paste course title together
dat_all$course_name<-apply(dat_all[,c("C1","C2","C3","C4","C5","C6","C7","C8")], 1, function(x) paste(na.omit(x),collapse=" "))
dat_all<-subset(dat_all,select=-c(C1,C2,C3,C4,C5,C6,C7,C8))
dat_all$course_name<-gsub(" N N","",dat_all$course_name)
dat_all$course_name<-gsub(" Y N","",dat_all$course_name)
dat_all$course_name<-gsub(" Y Y","",dat_all$course_name)
dat_all$course_name<-gsub(" N Y","",dat_all$course_name)

#Take course number with no letters
dat_all$Course_digits<-gsub("[[:alpha:]]+","",dat_all$Course)
dat_all$Course_letters<-gsub("[[:digit:]]+","",dat_all$Course)
tail(dat_all)

table(dat_all$Course_letters)

#Flag Luxembourg classes
dat_all$lux_flag<-ifelse(dat_all$Course_letters=="L" & grepl("lux",tolower(dat_all$course_name)),1,0)
dat_all$lux_flag<-ifelse(dat_all$Course_letters=="L" & grepl("L",dat_all$Section),1,dat_all$lux_flag)
dat_all$lux_flag<-ifelse(dat_all$Course_letters=="L" & grepl("french",tolower(dat_all$course_name)),1,dat_all$lux_flag)
dat_all$lux_flag<-ifelse(dat_all$Course_letters=="L" & grepl("european",tolower(dat_all$course_name)),1,dat_all$lux_flag)
dat_all$lux_flag<-ifelse(dat_all$Course_letters=="L" & grepl("international",tolower(dat_all$course_name)),1,dat_all$lux_flag)
dat_all$lux_flag<-ifelse(dat_all$Dept=="LUX",1,dat_all$lux_flag)
sum(dat_all$lux_flag)

#Flag labs
dat_all$lab_flag<-ifelse(dat_all$Course_letters=="L" & grepl("lab",tolower(dat_all$course_name)),1,0)
sum(dat_all$lab_flag)

#Flag honors courses
dat_all$hon_flag<-ifelse(dat_all$Course_letters=="H",1,0)
dat_all$hon_flag<-ifelse(dat_all$Dept=="HON",1,dat_all$hon_flag)
dat_all$hon_flag<-ifelse(grepl("honor",tolower(dat_all$course_name)),1,dat_all$hon_flag)
sum(dat_all$hon_flag)

#Update departments
#Attempted to standardize department codes that have changed over the years
#Looked up classes to figure out where they'd migrated once a major code stopped being used

#Look for majors that switched codes
maj_summ<-dat_all %>% group_by(Dept) %>% summarize(
  first=min(year)
  ,last=max(year)
)

dat_all$Dept_new<-dat_all$Dept
#AER to PHY
dat_all[dat_all$Dept_new=="AER","Dept_new"]<-"PHY"
#BOT to BIO
dat_all[dat_all$Dept_new=="BOT","Dept_new"]<-"BIO"
#CFA to CCA
dat_all[dat_all$Dept_new=="CFA","Dept_new"]<-"CCA"
#COM to STC
dat_all[dat_all$Dept_new=="COM","Dept_new"]<-"STC"
#CPE to CPB
dat_all[dat_all$Dept_new=="CPE","Dept_new"]<-"CPB"
#CSA to CSE
dat_all[dat_all$Dept_new=="CSA","Dept_new"]<-"CSE"
#DSC to ISA
dat_all[dat_all$Dept_new=="DSC","Dept_new"]<-"ISA"
#EAS to CEC
dat_all[dat_all$Dept_new=="EAS","Dept_new"]<-"CEC"
#EGR to MME
dat_all[dat_all$Dept_new=="EGR","Dept_new"]<-"MME"
#ENV to IES
dat_all[dat_all$Dept_new=="ENV","Dept_new"]<-"IES"
#GRK to LAT
#dat_all[dat_all$Dept_new=="GRK","Dept_new"]<-"LAT"
#HBW to LAT
#dat_all[dat_all$Dept_new=="HBW","Dept_new"]<-"LAT"
#MIS to ISA
dat_all[dat_all$Dept_new=="MIS","Dept_new"]<-"ISA"
#PCE to CPB
dat_all[dat_all$Dept_new=="PCE","Dept_new"]<-"CPB"
#PSE to CPB
dat_all[dat_all$Dept_new=="PSE","Dept_new"]<-"CPB"
#PHS to KNH
dat_all[dat_all$Dept_new=="PHS","Dept_new"]<-"KNH"
#PPS to CPB
dat_all[dat_all$Dept_new=="PPS","Dept_new"]<-"CPB"
#SAN to CSE
dat_all[dat_all$Dept_new=="SAN","Dept_new"]<-"CSE"
#SCA to CCA
dat_all[dat_all$Dept_new=="SCA","Dept_new"]<-"CCA"
#WCP to WST
dat_all[dat_all$Dept_new=="WCP","Dept_new"]<-"WST"
#WMS to WGS
dat_all[dat_all$Dept_new=="WMS","Dept_new"]<-"WGS"
#ZOO to BIO
dat_all[dat_all$Dept_new=="ZOO","Dept_new"]<-"BIO"

maj_summ_new<-dat_all %>% group_by(Dept_new) %>% summarize(
  first=min(year)
  ,last=max(year)
)

#Give class a common name based on most frequent use
dat_all<-dat_all %>% group_by(Dept_new,Course_digits) %>% mutate(most_common_name=names(which.max(table(course_name))))

#Number of sections
dat_all<-dat_all %>% group_by(year,semester,Dept_new,Course) %>% mutate(total_sections=n())
summary(dat_all$total_sections)

#Label academic year
dat_all$academic_year<-ifelse(dat_all$semester_name %in% c('Fall'),dat_all$year+1,dat_all$year)
unique(dat_all$academic_year)

#Flag if first academic year course offered (excluding 2000)
dat_all<-dat_all %>% group_by(Dept_new,Course) %>% mutate(first_year=min(academic_year))
dat_all$first_year_flag<-ifelse(dat_all$academic_year==dat_all$first_year & dat_all$academic_year>2000,1,0)
summary(dat_all$first_year_flag)

#Format variable names
names(dat_all)<-c("Dept","Course","Section","Last","First","Middle","A_p","A","A_m","B_p",
                  "B","B_m","C_p","C","C_m","D_p","D","D_m","F","W","WP","WF","I","X","Y","P","S","GPA","year",
                  "semester","semester_name","semester_num","semester_year_num","course_name","Course_digits","Course_letters"
                  ,"lux_flag","lab_flag","hon_flag","Dept_new","most_common_name",
                  "total_sections","academic_year","first_year","first_year_flag")

#Convert to numeric
dat_all[,c("A_p","A","A_m","B_p","B","B_m","C_p","C","C_m","D_p","D","D_m","F","W","WP","WF","I","X","Y","P","S","GPA")] <- 
  sapply(dat_all[,c("A_p","A","A_m","B_p","B","B_m","C_p","C","C_m","D_p","D","D_m","F","W","WP","WF","I","X","Y","P","S","GPA")], as.numeric)

#Total graded students (non withdrawals)
dat_all$graded_total<-apply(dat_all[,c("A_p","A","A_m","B_p","B","B_m","C_p","C","C_m","D_p","D","D_m","F")],1,sum)
#Total students included in GPA calculation (includes withdrawals with an F)
dat_all$GPA_total<-dat_all$graded_total+dat_all$WF
#Total students (includes all withdrawals)
dat_all$total_students<-dat_all$GPA_total+dat_all$W+dat_all$WP

#Withdrawal rate
dat_all$withdrawal_rate<-(dat_all$W+dat_all$WF+dat_all$WP)/(dat_all$W+dat_all$WF+dat_all$WP+dat_all$graded_total)
summary(dat_all$withdrawal_rate)

#Department name
dat_all$Dept_name<-'Unknown'
dat_all[dat_all$Dept_new=="AAA","Dept_name"]<-"Asian/Asian American Studies"
dat_all[dat_all$Dept_new=="ACC","Dept_name"]<-"Accountancy"
dat_all[dat_all$Dept_new=="ACE","Dept_name"]<-"American Culture & English"
dat_all[dat_all$Dept_new=="AES","Dept_name"]<-"Aerospace Studies"
dat_all[dat_all$Dept_new=="AMS","Dept_name"]<-"American Studies"
dat_all[dat_all$Dept_new=="ARB","Dept_name"]<-"Arabic"
dat_all[dat_all$Dept_new=="ARC","Dept_name"]<-"Architecture & Interior Design"
dat_all[dat_all$Dept_new=="ART","Dept_name"]<-"Art"
dat_all[dat_all$Dept_new=="ATH","Dept_name"]<-"Anthropology"
dat_all[dat_all$Dept_new=="BIO","Dept_name"]<-"Biology"
dat_all[dat_all$Dept_new=="BLS","Dept_name"]<-"Business Legal Studies"
dat_all[dat_all$Dept_new=="BUS","Dept_name"]<-"Business Analysis"
dat_all[dat_all$Dept_new=="BWS","Dept_name"]<-"Black World Studies"
dat_all[dat_all$Dept_new=="CAS","Dept_name"]<-"College Of Arts And Science"
dat_all[dat_all$Dept_new=="CCA","Dept_name"]<-"College Of Creative Arts"
dat_all[dat_all$Dept_new=="CEC","Dept_name"]<-"College Of Engineering & Computing"
dat_all[dat_all$Dept_new=="CHI","Dept_name"]<-"Chinese"
dat_all[dat_all$Dept_new=="CHM","Dept_name"]<-"Chemistry & Biochemistry"
dat_all[dat_all$Dept_new=="CLS","Dept_name"]<-"Classics"
dat_all[dat_all$Dept_new=="CMS","Dept_name"]<-"Comparative Media Studies"
dat_all[dat_all$Dept_new=="CPB","Dept_name"]<-"Chemical, Paper & Biomed Engineering"
dat_all[dat_all$Dept_new=="CSE","Dept_name"]<-"Computer Science & Software Engineering"
dat_all[dat_all$Dept_new=="DST","Dept_name"]<-"Disability Studies"
dat_all[dat_all$Dept_new=="ECE","Dept_name"]<-"Electrical & Computer Engineer"
dat_all[dat_all$Dept_new=="ECO","Dept_name"]<-"Economics"
dat_all[dat_all$Dept_new=="EDL","Dept_name"]<-"Educational Leadership"
dat_all[dat_all$Dept_new=="EDP","Dept_name"]<-"Educational Psychology"
dat_all[dat_all$Dept_new=="EDT","Dept_name"]<-"Teacher Education"
dat_all[dat_all$Dept_new=="EGM","Dept_name"]<-"Engineering Management"
dat_all[dat_all$Dept_new=="EHS","Dept_name"]<-"Education, Health And Society"
dat_all[dat_all$Dept_new=="ENG","Dept_name"]<-"English"
dat_all[dat_all$Dept_new=="ESP","Dept_name"]<-"Entrepreneurship"
dat_all[dat_all$Dept_new=="FAS","Dept_name"]<-"Fashion Design"
dat_all[dat_all$Dept_new=="FIN","Dept_name"]<-"Finance"
dat_all[dat_all$Dept_new=="FRE","Dept_name"]<-"French"
dat_all[dat_all$Dept_new=="FST","Dept_name"]<-"Film Studies"
dat_all[dat_all$Dept_new=="FSW","Dept_name"]<-"Family Studies And Social Work"
dat_all[dat_all$Dept_new=="GEO","Dept_name"]<-"Geography"
dat_all[dat_all$Dept_new=="GER","Dept_name"]<-"German"
dat_all[dat_all$Dept_new=="GHS","Dept_name"]<-"Global Health Studies"
dat_all[dat_all$Dept_new=="GLG","Dept_name"]<-"Geology"
dat_all[dat_all$Dept_new=="GSC","Dept_name"]<-"Graduate School Community"
dat_all[dat_all$Dept_new=="GTY","Dept_name"]<-"Gerontology"
dat_all[dat_all$Dept_new=="HIN","Dept_name"]<-"Hindi"
dat_all[dat_all$Dept_new=="HON","Dept_name"]<-"Honors"
dat_all[dat_all$Dept_new=="HST","Dept_name"]<-"History"
dat_all[dat_all$Dept_new=="IDS","Dept_name"]<-"Interdisciplinary"
dat_all[dat_all$Dept_new=="IES","Dept_name"]<-"Environmental Sciences"
dat_all[dat_all$Dept_new=="IMS","Dept_name"]<-"Interactive Media Studies"
dat_all[dat_all$Dept_new=="ISA","Dept_name"]<-"Information Systems & Analytics"
dat_all[dat_all$Dept_new=="ITL","Dept_name"]<-"Italian"
dat_all[dat_all$Dept_new=="ITS","Dept_name"]<-"International Studies"
dat_all[dat_all$Dept_new=="JPN","Dept_name"]<-"Japanese"
dat_all[dat_all$Dept_new=="JRN","Dept_name"]<-"Journalism"
dat_all[dat_all$Dept_new=="KNH","Dept_name"]<-"Kinesiology And Health"
dat_all[dat_all$Dept_new=="KOR","Dept_name"]<-"Korean"
dat_all[dat_all$Dept_new=="LAS","Dept_name"]<-"Latin American Studies"
dat_all[dat_all$Dept_new=="LAT","Dept_name"]<-"Latin Language & Literature"
dat_all[dat_all$Dept_new=="MAC","Dept_name"]<-"Media And Culture"
dat_all[dat_all$Dept_new=="MBI","Dept_name"]<-"Microbiology"
dat_all[dat_all$Dept_new=="MGT","Dept_name"]<-"Management"
dat_all[dat_all$Dept_new=="MKT","Dept_name"]<-"Marketing"
dat_all[dat_all$Dept_new=="MME","Dept_name"]<-"Mechanical & Manufacturing Engineering"
dat_all[dat_all$Dept_new=="MTH","Dept_name"]<-"Mathematics"
dat_all[dat_all$Dept_new=="MUS","Dept_name"]<-"Music"
dat_all[dat_all$Dept_new=="NSC","Dept_name"]<-"Naval Science"
dat_all[dat_all$Dept_new=="PHL","Dept_name"]<-"Philosophy"
dat_all[dat_all$Dept_new=="PHY","Dept_name"]<-"Physics"
dat_all[dat_all$Dept_new=="PLW","Dept_name"]<-"Pre-Law Studies"
dat_all[dat_all$Dept_new=="PMD","Dept_name"]<-"Premedical Studies"
dat_all[dat_all$Dept_new=="POL","Dept_name"]<-"Political Science"
dat_all[dat_all$Dept_new=="POR","Dept_name"]<-"Portuguese"
dat_all[dat_all$Dept_new=="PSY","Dept_name"]<-"Psychology"
dat_all[dat_all$Dept_new=="REL","Dept_name"]<-"Comparative Religion"
dat_all[dat_all$Dept_new=="RUS","Dept_name"]<-"Russian"
dat_all[dat_all$Dept_new=="SJS","Dept_name"]<-"Social Justice Studies"
dat_all[dat_all$Dept_new=="SOC","Dept_name"]<-"Sociology"
dat_all[dat_all$Dept_new=="SPA","Dept_name"]<-"Speech Pathology & Audiology"
dat_all[dat_all$Dept_new=="SPN","Dept_name"]<-"Spanish"
dat_all[dat_all$Dept_new=="STA","Dept_name"]<-"Statistics"
dat_all[dat_all$Dept_new=="STC","Dept_name"]<-"Strategic Communication"
dat_all[dat_all$Dept_new=="THE","Dept_name"]<-"Theatre"
dat_all[dat_all$Dept_new=="UNV","Dept_name"]<-"University"
dat_all[dat_all$Dept_new=="WGS","Dept_name"]<-"Women, Gender & Sexuality Studies"
dat_all[dat_all$Dept_new=="WST","Dept_name"]<-"Western Program"
dat_all[dat_all$Dept_new=="BTE","Dept_name"]<-"Business Technology"
dat_all[dat_all$Dept_new=="NSG","Dept_name"]<-"Nursing"
dat_all[dat_all$Dept_new=="ENT","Dept_name"]<-"Engineering Technology"
dat_all[dat_all$Dept_new=="HBW","Dept_name"]<-"Hebrew"
dat_all[dat_all$Dept_new=="CIT","Dept_name"]<-"Comp Information Technology"
dat_all[dat_all$Dept_new=="GRK","Dept_name"]<-"Greek"
dat_all[dat_all$Dept_new=="CJS","Dept_name"]<-"Criminal Justice Studies"
dat_all[dat_all$Dept_new=="BIS","Dept_name"]<-"Integrative Studies"
dat_all[dat_all$Dept_new=="LUX","Dept_name"]<-"Luxembourg"
dat_all[dat_all$Dept_new=="CRD","Dept_name"]<-"Civic And Regional Development"
dat_all[dat_all$Dept_new=="LST","Dept_name"]<-"Liberal Studies"


#SAVE DATA
write.csv(dat_all, file = paste0(out_path,"miami_gpa_data.csv"),row.names=FALSE)

