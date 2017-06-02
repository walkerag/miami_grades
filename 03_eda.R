########################################
#EDA: MAKE PRETTY CHARTS, TABLEAU INPUT
########################################

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

path<-'/Users/walkerag/Documents/miami_grades/formatted_data/'

dat_all<-read.csv(file = paste0(path,"miami_gpa_data.csv"),stringsAsFactors = FALSE)

#######################################
#INITIAL PLOTS
#######################################

#Only keep classes where at least 3 students were graded
dat_all<-dat_all[dat_all$graded_total>=3,]

#Check GPA number is replicable
dat_all$GPA_est<-((dat_all$A_p*(4)) + 
                    (dat_all$A*(4)) + (dat_all$A_m*(3.7)) + (dat_all$B_p*(3.3)) + (dat_all$B*(3)) + (dat_all$B_m*(2.7)) + 
                    (dat_all$C_p*(2.3)) + (dat_all$C*(2)) + (dat_all$C_m*(1.7)) + (dat_all$D_p*(1.3)) + (dat_all$D*(1)) + (dat_all$D_m*(0.7)))/(dat_all$GPA_total)
dat_all$GPA_est<-round(dat_all$GPA_est,2)

dat_all$GPA_diff<-abs(dat_all$GPA-dat_all$GPA_est)

summary(dat_all$GPA_diff)
#Pretty close!

#USE DEPT NEW
dat_all<-subset(dat_all,select=-c(Dept))

#Fix up some class names
dat_all[dat_all$Dept_new=="MUS" & dat_all$Course_digits=="100","most_common_name"]<-"March. Band, Orchestra/Ensembles/Colleg. Chorale"
dat_all[dat_all$Dept_new=="EDP" & dat_all$Course_digits=="201"
        ,"most_common_name"]<-"Human Development & Learning"
dat_all[dat_all$Dept_new=="STC" & dat_all$Course_digits=="135"
        ,"most_common_name"]<-"Intro to Public Expression & Critical Inquiry"
dat_all[dat_all$Dept_new=="MGT" & dat_all$Course_digits=="291"
        ,"most_common_name"]<-"Organizational Behavior & Theory"
dat_all[dat_all$Dept_new=="ISA" & dat_all$Course_digits=="235"
        ,"most_common_name"]<-"Info. Systems: Concepts/Tech./Applications"
dat_all[dat_all$Dept_new=="MGT" & dat_all$Course_digits=="302"
        ,"most_common_name"]<-"Intro to Operations & Supply Chain Mgt."
dat_all[dat_all$Dept_new=="ART" & dat_all$Course_digits=="188"
        ,"most_common_name"]<-"History of Western Art: Renaissance-Modern"

#Summarize classes
class_summ<-dat_all %>% group_by(Dept_new,Course_digits,most_common_name) %>% summarize(
  classes=n()
  ,students=sum(graded_total)
  ,est_gpa=sum(GPA_total*GPA)/sum(GPA_total)
)

#Top 30 classes by students and GPA
top_30<-class_summ[order(class_summ$students,decreasing=TRUE),][1:30,]
top_30$label<-paste0(top_30$Dept_new," ",top_30$Course_digits,": ",top_30$most_common_name)
top_30<-top_30[order(top_30$est_gpa,decreasing=TRUE),]
ggplot(data=top_30, aes(x=reorder(label,est_gpa), y=est_gpa, fill=students)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  coord_flip() +
  ggtitle("Top 30 Most Popular Miami Classes, Sorted by GPA") +
  ylab("GPA") +
  xlab("Course") +
  scale_fill_continuous(low="lightskyblue1", high="dodgerblue",limits=c(5000,70000),name="Students") + 
  scale_y_continuous(limits=c(0,4),breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4.0)) +
  theme(text = element_text(size = 22,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.title.align=0
  ) 
rm(top_30)
rm(class_summ)

#################################
#GPA distribution by course level
#################################

#Class level
dat_all$class_level<-substr(dat_all$Course,1,1)
table(dat_all$class_level)

#View(dat_all[dat_all$class_level=="F",])
#F is first year seminar
#Make a first year seminar flag and strip F
rows<-dat_all$class_level=="F"
sum(rows)
dat_all$first_year_seminar_flag<-ifelse(rows,1,0)
sum(dat_all$first_year_seminar_flag)
dat_all[rows,"class_level"]<-"1"
rm(rows)

#Use second digit for Honors prefix classes
rows<-dat_all$class_level=="H"
dat_all$class_level<-ifelse(rows,substr(dat_all$Course,2,2),dat_all$class_level)
rm(rows)
table(dat_all$class_level)

#View(dat_all[dat_all$class_level=="0",])

#Combine upper level into "graduate" bucket
dat_all$class_level_format<-paste0(dat_all$class_level,"00")
dat_all$class_level_format<-ifelse(dat_all$class_level %in% c("5","6","7"),"Graduate (500-700)",dat_all$class_level_format)
table(dat_all$class_level_format)

#Grade summary by course level
class_level_summ<-dat_all %>% group_by(class_level_format) %>% summarize(
  A_p=sum(A_p)
  ,A=sum(A)
  ,A_m=sum(A_m)
  ,B_p=sum(B_p)
  ,B=sum(B)
  ,B_m=sum(B_m)
  ,C_p=sum(C_p)
  ,C=sum(C)
  ,C_m=sum(C_m)
  ,D_p=sum(D_p)
  ,D=sum(D)
  ,D_m=sum(D_m)
  ,F=sum(F)
)

#Remove 00 level
class_level_summ<-class_level_summ[!(class_level_summ$class_level_format %in% c("000")),]
class_level_summ

#Get % totals
class_level_summ[,-1]<-class_level_summ[,-1]/apply(class_level_summ[,-1],1,FUN=sum)
class_level_summ

# melt the data frame for plotting
data.m <- melt(data.frame(class_level_summ), id.vars='class_level_format',varying=list(2:14),v.names="conc",direction="long")
rm(class_level_summ)

# plot everything
data_labels<-c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F")
ggplot(data.m, aes(class_level_format, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity",color="black") +
  ggtitle("Grade Distribution by Course Level") +
  ylab("Percentage of Grades") +
  xlab("Course Level") +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.7),breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)) +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) + 
  scale_fill_discrete(name="Grades",
                      labels=data_labels)
rm(data.m)
rm(data_labels)

#####################################
#GPA by Department (Tableau input)
#####################################

#All
major_summ<-dat_all[dat_all$academic_year>=2014,] %>% group_by(Dept_new,Dept_name) %>% summarize(
  grade_type='All'
  ,total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)

#Graduate
grad_major_summ<-dat_all[dat_all$academic_year>=2014 & dat_all$class_level %in% c("5","6","7"),] %>% group_by(Dept_new,Dept_name) %>% summarize(
  grade_type='Graduate (500 and above)'
  ,total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)

#High level undergrad
hu_major_summ<-dat_all[dat_all$academic_year>=2014 & dat_all$class_level %in% c("3","4"),] %>% group_by(Dept_new,Dept_name) %>% summarize(
  grade_type='300 and 400 Level'
  ,total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)

#low level undergrad
lu_major_summ<-dat_all[dat_all$academic_year>=2014 & dat_all$class_level %in% c("1","2"),] %>% group_by(Dept_new,Dept_name) %>% summarize(
  grade_type='100 and 200 Level'
  ,total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)

comb<-rbind(major_summ,grad_major_summ)
comb<-rbind(comb,hu_major_summ)
comb<-rbind(comb,lu_major_summ)
rm(major_summ)
rm(grad_major_summ)
rm(lu_major_summ)
rm(hu_major_summ)

#Add a ranking based on number of grades
comb<-comb %>% group_by(grade_type) %>% mutate(grade_rank=rank(desc(total_grades)))
#View(comb)

#Formatted major field
comb$major_comb<-paste0(comb$Dept_new,": ",comb$Dept_name)

#Save to txt
write.table(comb, paste0(path,"/dept_summary.txt"), sep="\t",row.names = FALSE)
rm(comb)

#############################
#GPA by Course Level, Year
#############################

annual_gpa<-dat_all %>% group_by(academic_year,class_level_format) %>% summarize(
  total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)
annual_gpa$`Course Level`<-annual_gpa$class_level_format
ggplot(annual_gpa[annual_gpa$class_level_format!="000",]
       , aes(x=academic_year,y=weighted_gpa,col=`Course Level`)) +   
  geom_line(lwd=3) +
  ggtitle("GPA by Course Level, Year") +
  ylab("GPA") +
  xlab("Year") +
  scale_y_continuous(limits=c(2.8,4),breaks=c(2.8,3.0,3.2,3.4,3.6,3.8,4.0)) +
  scale_x_continuous(limits=c(2000,2016),breaks=c(2000,2002,2004,2006,2008,2010,2012,2014,2016)) +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.key.size = unit(3,"line")
  ) + 
  guides(colour = guide_legend(override.aes = list(lwd=4)))

rm(annual_gpa)

###############################
#GPA by Course Level, Semester
###############################

sem_gpa<-dat_all %>% group_by(semester_name,class_level_format) %>% summarize(
  total_grades=sum(graded_total)
  ,total_classes=n()
  ,weighted_gpa=sum(GPA*GPA_total)/sum(GPA_total)
)

sem_gpa$semester_name<-factor(sem_gpa$semester_name,level=c("Fall","Winter","Spring","Summer"))
sem_gpa$`Course Level`<-sem_gpa$class_level_format
ggplot(sem_gpa[sem_gpa$class_level_format!="000",]
       , aes(x=semester_name
             ,y=weighted_gpa
             ,col=`Course Level`
             ,group = `Course Level`
       )) +   
  geom_line(lwd=3) +
  ggtitle("GPA by Course Level, Semester") +
  ylab("GPA") +
  xlab("Semester") +
  scale_y_continuous(limits=c(2.8,4),breaks=c(2.8,3.0,3.2,3.4,3.6,3.8,4.0)) +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.key.size = unit(3,"line")
  ) + 
  guides(colour = guide_legend(override.aes = list(lwd=4)))
rm(sem_gpa)