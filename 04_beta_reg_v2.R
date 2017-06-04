########################################
#BETA REGRESSION
########################################

rm(list=ls())
gc()
options(scipen=999)

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
mod_path<-'/Users/walkerag/Documents/miami_grades/model_output/'

dat_all<-read.csv(file = paste0(path,"miami_gpa_data.csv"),stringsAsFactors = FALSE)

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

#Combine upper level into "graduate" bucket
dat_all$class_level_format<-paste0(dat_all$class_level,"00")
dat_all$class_level_format<-ifelse(dat_all$class_level %in% c("5","6","7"),"Graduate (500-700)",dat_all$class_level_format)
table(dat_all$class_level_format)

#Flag if prof's first year teaching
dat_all<-dat_all %>% group_by(Last,First,Middle) %>% mutate(prof_first_year=min(academic_year))
summary(dat_all$prof_first_year)
dat_all$prof_first_year_flag<-ifelse(dat_all$prof_first_year>2000 & dat_all$academic_year==dat_all$prof_first_year,1,0)
summary(dat_all$prof_first_year_flag)

#No zero level
dat_all<-dat_all[dat_all$class_level!="0",]

#Remove pass/fail
dat_all<-dat_all[dat_all$P==0,]

#At least one person got a non-F grade
dat_all<-dat_all[(dat_all$graded_total-dat_all$F)>0,]

#####################################
#MODEL START
#####################################

#Training data
obs<-dim(dat_all)[1]
set.seed(1234)
train_size<-30000
train<-sample(1:obs,size=train_size,replace=FALSE)
train_dat<-dat_all[train,]
test_dat<-dat_all[-train,]

#Make class level numeric
train_dat$class_level_num<-as.numeric(train_dat$class_level)
test_dat$class_level_num<-as.numeric(test_dat$class_level)

#Scale GPA new version
n<-dim(train_dat)[1]
train_dat$GPA_scale<-((train_dat$GPA*(n-1)+0.5)/n)/4
test_dat$GPA_scale<-((test_dat$GPA*(n-1)+0.5)/n)/4
summary(train_dat$GPA_scale)
summary(test_dat$GPA_scale)

#Flag classes with multiple sections
train_dat$multiple_sections<-ifelse(train_dat$total_sections>1,1,0)
test_dat$multiple_sections<-ifelse(test_dat$total_sections>1,1,0)

#Replace low frequency departments
summary(train_dat$GPA_total)
summary(train_dat$graded_total)
x<-table(dat_all$Dept_new)/length(dat_all$Dept_new)
0.001*train_size
x[x<0.001]
train_dat[train_dat$Dept_new %in% c(names(x)[x<0.001]),"Dept_new"]<-"Other"
test_dat[test_dat$Dept_new %in% c(names(x)[x<0.001]),"Dept_new"]<-"Other"

#Start year at one
train_dat$academic_year<-train_dat$academic_year-1999
test_dat$academic_year<-test_dat$academic_year-1999
sort(unique(train_dat$academic_year))

#Some transforms
train_dat$academic_year_sq<-train_dat$academic_year*train_dat$academic_year
train_dat$GPA_total_sq<-train_dat$GPA_total*train_dat$GPA_total
test_dat$academic_year_sq<-test_dat$academic_year*test_dat$academic_year
test_dat$GPA_total_sq<-test_dat$GPA_total*test_dat$GPA_total

train_dat$graded_total_log<-log(train_dat$graded_total)
train_dat$GPA_total_log<-log(train_dat$GPA_total)
test_dat$graded_total_log<-log(test_dat$graded_total)
test_dat$GPA_total_log<-log(test_dat$GPA_total)

#Save training and test data
saveRDS(test_dat,file=paste0(mod_path,"test_dat.rds"))
saveRDS(train_dat,file=paste0(mod_path,"train_dat.rds"))

#Choose fixed effects using BIC first, then add variance terms
#Using BIC as looking to have parsimonious model

gy.1 <- betareg(GPA_scale ~ 
                  academic_year
                + academic_year_sq
                + GPA_total
                + GPA_total_log
                + class_level_num
                + Dept_new 
                #+ Dept_new*class_level_num
                + Dept_new*academic_year
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                ,data = train_dat)
print(summary(gy.1))

gy.2 <- betareg(GPA_scale ~ 
                  academic_year
                + academic_year_sq
                + GPA_total
                + GPA_total_log
                + class_level_num
                + Dept_new 
                + Dept_new*class_level_num
                #+ Dept_new*academic_year
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                ,data = train_dat)
print(summary(gy.2))

BIC(gy.1)
BIC(gy.2)
#2 wins

gy.3 <- betareg(GPA_scale ~ 
                  academic_year
                + academic_year_sq
                + GPA_total
                + GPA_total_log
                + class_level_num
                + Dept_new 
                #+ Dept_new*class_level_num
                #+ Dept_new*academic_year
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                ,data = train_dat)
print(summary(gy.3))

BIC(gy.2)
BIC(gy.3)
#3 wins, narrowly

gy.4 <- betareg(GPA_scale ~ 
                  academic_year
                #+ academic_year_sq
                + GPA_total
                + GPA_total_log
                + class_level_num
                + Dept_new 
                #+ Dept_new*class_level_num
                #+ Dept_new*academic_year
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                ,data = train_dat)
summary(gy.4)

BIC(gy.3)
BIC(gy.4)
#Keep 4 for interpretation

plot(gy.4)
print('Done')

#Residual checks
gy_res.4 <- cbind(
  residuals(gy.4, type = "pearson"),
  residuals(gy.4, type = "deviance"),
  residuals(gy.4, type = "response"),
  residuals(gy.4, type = "weighted"),
  residuals(gy.4, type = "sweighted"),
  residuals(gy.4, type = "sweighted2")
)
colnames(gy_res.4) <- c("pearson", "deviance", "response",
                        "weighted", "sweighted", "sweighted2")
gy_res.4<-data.frame(gy_res.4)
summary(gy_res.4$sweighted2)

#Check pearson residuals
plot(x=gy.4$fitted.values,y=gy_res.4$pearson)

#Look at outliers
View(train_dat[abs(gy_res.4$pearson)>4,])

#ADD VARIANCE TERMS

gy.5 <- betareg(GPA_scale ~ 
                  academic_year
                #+ academic_year_sq
                + GPA_total
                + GPA_total_log
                + class_level_num
                + Dept_new 
                #+ Dept_new*class_level_num
                #+ Dept_new*academic_year
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                | Dept_new
                ,data = train_dat)
print(summary(gy.5))

BIC(gy.4)
BIC(gy.5)

gy.6 <- betareg(GPA_scale ~ 
                  academic_year
                + GPA_total
                + class_level_num
                + lux_flag
                + lab_flag
                + hon_flag
                + semester_name
                + multiple_sections
                + first_year_flag
                + prof_first_year_flag
                + total_sections
                + Dept_new 
                | Dept_new + GPA_total
                ,data = train_dat)
print(summary(gy.6))

BIC(gy.6)

saveRDS(gy.6,file=paste0(mod_path,"beta_mod.rds"))

BIC(gy.4)
BIC(gy.5)
BIC(gy.6)

#Residual checks
gy_res.6 <- cbind(
  residuals(gy.6, type = "pearson"),
  residuals(gy.6, type = "deviance"),
  residuals(gy.6, type = "response"),
  residuals(gy.6, type = "weighted"),
  residuals(gy.6, type = "sweighted"),
  residuals(gy.6, type = "sweighted2")
)
colnames(gy_res.6) <- c("pearson", "deviance", "response",
                        "weighted", "sweighted", "sweighted2")
gy_res.6<-data.frame(gy_res.6)
summary(gy_res.6$sweighted2)

saveRDS(gy_res.6,file=paste0(mod_path,"resid_df.rds"))

#Check pearson residuals
gy_res.6$fitted<-gy.6$fitted.values
ggplot(data=gy_res.6,aes(x=fitted*4,y=pearson)) + 
  geom_point(col="forestgreen") +
  scale_x_continuous(limits=c(0,4),breaks=c(0,0.5,1,1.5,2,2.5,3.,3.5,4)) +
  scale_y_continuous(limits=c(-4,4),breaks=c(-4,-2,0,2,4)) +
  ggtitle("Training Data Fitted Values vs Pearson Residuals") +
  ylab("Pearson Residual") +
  xlab("Fitted Value") +
  theme(text = element_text(size = 25,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

#Look at outliers
View(train_dat[abs(gy_res.4$pearson)>4,])

###################################
#TRAIN/TEST PREDICTION EVAL
###################################

#Check prediction function works as expected
train_dat$pred <- predict(gy.6,newdata=train_dat)
sum(train_dat$pred-gy.6$fitted.values)

#Training residuals scatter plot
ggplot(data=train_dat,aes(x=pred*4,y=GPA)) + 
  geom_point(col="forestgreen") + geom_abline(slope=1,lwd=2) +
  scale_x_continuous(limits=c(0,4),breaks=c(0,1,2,3,4)) +
  scale_y_continuous(limits=c(0,4),breaks=c(0,1,2,3,4)) +
  ggtitle("Training Data Predicted vs Actual") +
  ylab("Actual GPA") +
  xlab("Predicted GPA") +
  theme(text = element_text(size = 25,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

#Train Residuals
resid<-train_dat$GPA-(train_dat$pred*4)
t<-length(resid)
sum(abs(resid)<1)/t
sum(abs(resid)<0.5)/t
sum(abs(resid)<0.25)/t
sum(abs(resid)<0.1)/t

#Plot training residuals
resid<-data.frame(resid)
ggplot(data=resid,aes(x=resid)) + geom_density(col="forestgreen",lwd=2) +
  scale_x_continuous(limits=c(-2,2),breaks=c(-2,-1,0,1,2)) +
  ggtitle("Training Data Residuals Density Plot") +
  ylab("Density") +
  xlab("Training Residuals") +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

#Test residuals
#Splitting up increased over predict speed considerably
test_dat$pred<-5
test_dat[1:10000,"pred"] <- predict(gy.6,newdata=test_dat[1:10000,])
test_dat[10000:20000,"pred"] <- predict(gy.6,newdata=test_dat[10000:20000,])
test_dat[20000:30000,"pred"] <- predict(gy.6,newdata=test_dat[20000:30000,])
test_dat[30000:40000,"pred"] <- predict(gy.6,newdata=test_dat[30000:40000,])
test_dat[40000:50000,"pred"] <- predict(gy.6,newdata=test_dat[40000:50000,])
test_dat[50000:60000,"pred"] <- predict(gy.6,newdata=test_dat[50000:60000,])
test_dat[60000:70000,"pred"] <- predict(gy.6,newdata=test_dat[60000:70000,])
test_dat[70000:80000,"pred"] <- predict(gy.6,newdata=test_dat[70000:80000,])
test_dat[80000:dim(test_dat)[1],"pred"] <- predict(gy.6,newdata=test_dat[80000:dim(test_dat)[1],])
summary(test_dat$pred)

#Resave train and test with predictions
saveRDS(train_dat,file=paste0(mod_path,"train_dat.rds"))
saveRDS(test_dat,file=paste0(mod_path,"test_dat.rds"))

#Test Residuals
resid<-test_dat$GPA-(test_dat$pred*4)
t<-length(resid)
sum(abs(resid)<1)/t
sum(abs(resid)<0.5)/t
sum(abs(resid)<0.25)/t
sum(abs(resid)<0.1)/t

#Test baseline
resid_base<-test_dat$GPA-mean(test_dat$GPA)
sum(abs(resid_base)<1)/t
sum(abs(resid_base)<0.5)/t
sum(abs(resid_base)<0.25)/t
sum(abs(resid_base)<0.1)/t

#Test residuals scatter plot
ggplot(data=test_dat,aes(x=pred*4,y=GPA)) + 
  geom_point(col="maroon") + geom_abline(slope=1,lwd=2) +
  scale_x_continuous(limits=c(0,4),breaks=c(0,1,2,3,4)) +
  scale_y_continuous(limits=c(0,4),breaks=c(0,1,2,3,4)) +
  ggtitle("Test Data Predicted vs Actual") +
  ylab("Actual GPA") +
  xlab("Predicted GPA") +
  theme(text = element_text(size = 25,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

#Plot test residuals
resid<-data.frame(resid)
ggplot(data=resid,aes(x=resid)) + geom_density(col="maroon",lwd=2) +
  scale_x_continuous(limits=c(-2,2),breaks=c(-2,-1,0,1,2)) +
  ggtitle("Test Data Residuals Density Plot") +
  ylab("Density") +
  xlab("Test Residuals") +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

#exp(xB)=prediction

#####################
#SIMULATED CHANGES
#####################


gy.6<-readRDS(file=paste0(mod_path,"beta_mod.rds"))
train_dat<-readRDS(file=paste0(mod_path,"train_dat.rds"))
test_dat<-readRDS(file=paste0(mod_path,"test_dat.rds"))

#Look at test data only

####################
#HONORS FLAG
####################

#Make a copy
test_set<-test_dat
#Flip flag
test_set$hon_flag<-abs(test_set$hon_flag-1)
#Get new predictions
test_set$pred_flip<-5
test_set[1:10000,"pred_flip"] <- predict(gy.6,newdata=test_set[1:10000,])
test_set[10000:20000,"pred_flip"] <- predict(gy.6,newdata=test_set[10000:20000,])
test_set[20000:30000,"pred_flip"] <- predict(gy.6,newdata=test_set[20000:30000,])
test_set[30000:40000,"pred_flip"] <- predict(gy.6,newdata=test_set[30000:40000,])
test_set[40000:50000,"pred_flip"] <- predict(gy.6,newdata=test_set[40000:50000,])
test_set[50000:60000,"pred_flip"] <- predict(gy.6,newdata=test_set[50000:60000,])
test_set[60000:70000,"pred_flip"] <- predict(gy.6,newdata=test_set[60000:70000,])
test_set[70000:80000,"pred_flip"] <- predict(gy.6,newdata=test_set[70000:80000,])
test_set[80000:dim(test_set)[1],"pred_flip"] <- predict(gy.6,newdata=test_set[80000:dim(test_set)[1],])
summary(test_set$pred_flip)

#Keep non-honors original
test_set<-test_set[test_set$hon_flag==1,]

#Get difference, multiply by 4 to get in GPA scale
test_set$change<-(test_set$pred_flip-test_set$pred)*4
summary(test_set$change)

#Plot change in predictions
ggplot(data=test_set,aes(x=change)) + geom_density(col="dodgerblue",lwd=2) +
  scale_x_continuous(limits=c(-1,1),breaks=c(-1,0,1)) +
  ggtitle("Honors Class Estimated GPA Effect") +
  ylab("Density") +
  xlab("Change in Predicted GPA: Non-Honors to Honors") +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

####################
#NEW PROFESSOR
####################

#Make a copy
test_set<-test_dat
#Flip flag
test_set$prof_first_year_flag<-abs(test_set$prof_first_year_flag-1)
#Get new predictions
test_set$pred_flip<-5
test_set[1:10000,"pred_flip"] <- predict(gy.6,newdata=test_set[1:10000,])
test_set[10000:20000,"pred_flip"] <- predict(gy.6,newdata=test_set[10000:20000,])
test_set[20000:30000,"pred_flip"] <- predict(gy.6,newdata=test_set[20000:30000,])
test_set[30000:40000,"pred_flip"] <- predict(gy.6,newdata=test_set[30000:40000,])
test_set[40000:50000,"pred_flip"] <- predict(gy.6,newdata=test_set[40000:50000,])
test_set[50000:60000,"pred_flip"] <- predict(gy.6,newdata=test_set[50000:60000,])
test_set[60000:70000,"pred_flip"] <- predict(gy.6,newdata=test_set[60000:70000,])
test_set[70000:80000,"pred_flip"] <- predict(gy.6,newdata=test_set[70000:80000,])
test_set[80000:dim(test_set)[1],"pred_flip"] <- predict(gy.6,newdata=test_set[80000:dim(test_set)[1],])
summary(test_set$pred_flip)

#Keep non first year originally
test_set<-test_set[test_set$prof_first_year_flag==1,]

#Get difference, multiply by 4 to get in GPA scale
test_set$change<-(test_set$pred_flip-test_set$pred)*4
summary(test_set$change)

#Plot change in predictions
ggplot(data=test_set,aes(x=change)) + geom_density(col="dodgerblue",lwd=2) +
  scale_x_continuous(limits=c(-1,1),breaks=c(-1,0,1)) +
  ggtitle("First Year Professor Estimated GPA Effect") +
  ylab("Density") +
  xlab("Change in Predicted GPA: Non-First to First") +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 

htmlTable(summary(gy.6))

####################
#TEN YEAR JUMP
####################

#Make a copy
test_set<-test_dat
#Add ten years
test_set$academic_year<-abs(test_set$academic_year+10)
#Get new predictions
test_set$pred_flip<-5
test_set[1:10000,"pred_flip"] <- predict(gy.6,newdata=test_set[1:10000,])
test_set[10000:20000,"pred_flip"] <- predict(gy.6,newdata=test_set[10000:20000,])
test_set[20000:30000,"pred_flip"] <- predict(gy.6,newdata=test_set[20000:30000,])
test_set[30000:40000,"pred_flip"] <- predict(gy.6,newdata=test_set[30000:40000,])
test_set[40000:50000,"pred_flip"] <- predict(gy.6,newdata=test_set[40000:50000,])
test_set[50000:60000,"pred_flip"] <- predict(gy.6,newdata=test_set[50000:60000,])
test_set[60000:70000,"pred_flip"] <- predict(gy.6,newdata=test_set[60000:70000,])
test_set[70000:80000,"pred_flip"] <- predict(gy.6,newdata=test_set[70000:80000,])
test_set[80000:dim(test_set)[1],"pred_flip"] <- predict(gy.6,newdata=test_set[80000:dim(test_set)[1],])
summary(test_set$pred_flip)

#Keep classes from first 6 years
test_set<-test_set[test_set$academic_year<=16,]

#Get difference, multiply by 4 to get in GPA scale
test_set$change<-(test_set$pred_flip-test_set$pred)*4
summary(test_set$change)

ggplot(data=test_set,aes(x=change)) + geom_density(col="dodgerblue",lwd=2) +
  scale_x_continuous(limits=c(-1,1),breaks=c(-1,0,1)) +
  ggtitle("Ten Year Estimated GPA Effect") +
  ylab("Density") +
  xlab("Change in Predicted GPA: Ten Years On") +
  theme(text = element_text(size = 26,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) 
#Mean 0.018

summary(test_set$change)
