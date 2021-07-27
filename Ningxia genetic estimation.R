# >>>>>>>>>>>>>>Genetic estimation for population from Ningxia<<<<<<<<<<<<<<<<<<<<<
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!READ ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!You must change the input and output directories!!!!!!!!
# !!!!Save your raw data in a safe space or have another copy!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------data formatting-----------------------------
# Load packages -----------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)
# Input data files and rename(calve, insemination, birth, pregnant detection and pedigree)--------------------------------------------------------
# Please remember to change these directories before performing a new estimation. 
chandu_all <- as.data.frame(fread('D:\\chenzw\\数据库\\宁夏\\格式化数据（一直更新）\\202101\\calve_all.csv',encoding='UTF-8'))
chusheng_all <- as.data.frame(fread('D:\\chenzw\\数据库\\宁夏\\格式化数据（一直更新）\\202101\\birth_all.csv',encoding='UTF-8'))
shujing_all <- as.data.frame(fread('D:\\chenzw\\数据库\\宁夏\\格式化数据（一直更新）\\202101\\ins_all.csv',encoding='UTF-8'))
renjian_all <- as.data.frame(fread('D:\\chenzw\\数据库\\宁夏\\格式化数据（一直更新）\\202101\\preg_all.csv',encoding='UTF-8'))
ped_all <- as.data.frame(data.table::fread('D:\\chenzw\\数据库\\宁夏\\系谱\\202107\\pedigree_v2_format.csv'))
corid <- read.csv('D:\\chenzw\\数据库\\北京\\系谱\\公牛对照表\\2021\\公牛对照表-lwq-202012.csv')
farmcode <- read.csv('D:\\chenzw\\遗传评估/farmcode.csv')

names(chandu_all) <- c('FID','parity','cddate','SB','CE','bw','ct','HERD','CALF')
names(shujing_all) <- c('FID','parity','sjdate','MID','ST','SYN','HERD','SC')
names(chusheng_all) <- c('FID','SIRE','DAM','csdate','HERD')
names(renjian_all) <- c('FID','parity','rjdate','result','HERD','type')
names(corid) <- c('sire','MID')

chandu_all$renum <-1:length(chandu_all$FID)
shujing_all$renum <-1:length(shujing_all$FID)
chusheng_all$renum <-1:length(chusheng_all$FID)
# Set output directory and create a new fold for DMU----------------------------------------------------
out_dir <- 'D:\\chenzw\\遗传评估\\宁夏遗传评估\\202108\\'
dir.create(paste(out_dir,'DMU',sep= ''))
# 【all data】Herd-year (HY) and Year-month (YM) effects ----------------------------------------
# HYI and YMI represent effects of insemination; HYC and YMC represent effects of calving; HYB and YMB represent effects of birth.
# Because some farms have letters in their original codes, we need to recode them in order to cater for the standard format of DMU.
chandu_all$cddate <- as.Date(chandu_all$cddate)
chusheng_all$csdate <- as.Date(chusheng_all$csdate)
shujing_all$sjdate <- as.Date(shujing_all$sjdate)
farmcode <- farmcode[,c('farmcode','recode')]

# Merge original codes and recoded codes.
chandu_all <- merge(chandu_all,farmcode,by.x='HERD',by.y='farmcode',all.x = T)
shujing_all <- merge(shujing_all,farmcode,by.x='HERD',by.y='farmcode',all.x = T)
chusheng_all <- merge(chusheng_all,farmcode,by.x='HERD',by.y='farmcode',all.x = T)

# HY = paste(farm code, year, sep = ''); YM = paste(year, month, sep = '')
# e.g. HY = 1012010; YM = 201012
shujing_all$YMI <- paste(year(shujing_all$sjdate),month(shujing_all$sjdate),sep='')
shujing_all$HYI <- paste(shujing_all$recode,as.numeric(year(shujing_all$sjdate)),sep='')
chandu_all$YMC <- paste(year(chandu_all$cddate),month(chandu_all$cddate),sep='')
chandu_all$HYC <- paste(chandu_all$recode,as.numeric(year(chandu_all$cddate)),sep='')
chusheng_all$YMB <- paste(year(chusheng_all$csdate),month(chusheng_all$csdate),sep='')
chusheng_all$HYB <- paste(chusheng_all$recode,as.numeric(year(chusheng_all$csdate)),sep='')

# Delete the redundant columns and dataframes.
shujing_all <- subset(shujing_all, select = -c(HERD,recode))
chusheng_all <- subset(chusheng_all, select = -c(HERD,recode))
chandu_all <- subset(chandu_all, select = -c(HERD,recode))
rm(farmcode)

# Keep insemination >= 2012; calving >= 2013; birth >= 2007
shujing_all<-shujing_all[which(year(shujing_all$sjdate)>=2012),]
chandu_all<-chandu_all[which(year(chandu_all$cddate)>=2013),]
chusheng_all<-chusheng_all[which(year(chusheng_all$csdate)>=2007),]
# 【insemination data】AI technician (ST) effects and semen type (SC) effects -----------------------------------------------------------
# Extract technicians from raw data and recode them.
shujing_all$ST <- gsub('(\\.)0$', '', shujing_all$ST) # Rule some irregular records. (e.g. '64025117.0' to '64025117')
st <- data.frame(table(shujing_all$ST))
st <- st[nchar(as.character(st$Var1)) != 0,] # Delete blank rows.
st$stnum <- 1:length(st$Var1) # Number them.
st <- subset(st,select = -Freq)
shujing_all <- merge(shujing_all,st,by.x='ST',by.y='Var1',all.x=T)
shujing_all <- subset(shujing_all,select = -ST)
shujing_all$stnum <- paste(64,str_pad(shujing_all$stnum,3,'left',0),sep = '')
shujing_all[shujing_all$stnum == '64NA','stnum'] <- 999999 # Regard missing records as the same level.
rm(st)

# Conventional semen = 1; sexed semen = 2; missing = 9. 
shujing_all[shujing_all$SC == 9,'SC'] <- 999999

# Delete duplicated records.
shujing_all$cp <- paste(shujing_all$parity,shujing_all$FID,sep='')
shujing_all%<>%
  group_by(cp,sjdate)%>%
  filter(row_number()==1)%>%
  ungroup()

# 【calving data】Calving ease (CE), stillbirth (SB), parity (P), calve sex (SEX)--------
chandu_all <- subset(chandu_all, select = -c(ct,bw))
chandu_all[is.na(chandu_all$SB),'SB'] <- -99
chandu_all$CE <- as.numeric(chandu_all$CE)
chandu_all[is.na(chandu_all$CE),'CE'] <- -99
chandu_all <- within(chandu_all,{ # 3 ==> 2, 4 & 5 ==> 3
  CE[!CE %in% c(1,2,3,4,5)] <- -99
  CE[CE == 3] <- 2
  CE[CE == 4| CE == 5] <- 3
})

# Calve sex: female = 1; male = 2; twins = 3.
chandu_all[is.na(chandu_all$CALF),'CALF'] <- 999999

# Parity <= 12
chandu_all <- chandu_all[-which(chandu_all$parity > 12),]

# Delete duplicated records and keep the highest level of calving ease in each parity.
chandu_all$cp <- paste(chandu_all$parity,chandu_all$FID,sep = '')
chandu_all%<>%
  group_by(cp,YMC)%>%
  filter(CE %in% max(CE))%>%
  ungroup()
chandu_all <- chandu_all[!duplicated(chandu_all),]
chandu_all%<>%
  group_by(cp)%>%
  filter(row_number() == 1)%>%
  ungroup()

# 【birth data】Remove duplicated records -----------------------------------------------------------------
chusheng_all<-chusheng_all[!duplicated(chusheng_all$FID),]


# 1. Determine the pregnant date ----------------------------------------------
# Pregnant = 1; not pregnant = 2
# First step: In each parity, the last service date (20-90 d before the last detect date of which the detect result was +) is pregnant date
# Second step: For those records without detect results, regard the last service date with calving record in corresponding parity as pregnant date
renjian_all[which(grepl('\\+',renjian_all$result)==T|grepl('P',renjian_all$result)==T),'preg'] <- 1
renjian_all[is.na(renjian_all$preg),'preg'] <- 2
renjian_all <- renjian_all[-which(renjian_all$result=='E'),]
renjian_all$cp <- paste(renjian_all$parity,renjian_all$FID,sep='')
renjian_all$rjdate <- as.Date(renjian_all$rjdate)
renjian_all <- subset(renjian_all, select = -c(result,HERD))
# First step
a <- renjian_all[which(renjian_all$preg==1),]
a%<>%
  group_by(cp)%>%
  filter(rjdate %in% max(rjdate))%>% # last detect date with pregnant result
  ungroup()
a <- subset(a,select = -cp)
sjrj <- merge(shujing_all,a,by=c('FID','parity'),all= F)
sjrj$dif <- as.numeric(sjrj$rjdate-sjrj$sjdate)
sjrj <- sjrj[which(sjrj$dif %in% seq(20,90)|sjrj$dif==0),]
sjrj%<>%
  group_by(cp,rjdate)%>%
  filter(dif %in% min(dif))%>%
  ungroup()
sjrj%<>%
  group_by(cp,sjdate)%>%
  filter(dif %in% min(dif))%>%
  ungroup()
sjrj <- sjrj[!duplicated(sjrj$cp,sjrj$sjdate),]
sjrj <- sjrj[c('renum','preg')]
shujing_pr1 <- merge(shujing_all,sjrj,by='renum',all.y=T) # pregnant
shujing_pr2 <- merge(shujing_all,sjrj,by='renum',all.x=T)
shujing_pr2[is.na(shujing_pr2$preg),'preg'] <- 2
shujing_pr2 %<>%
  group_by(cp)%>%
  filter(preg %in% min(preg))%>% # If there were repeated records with different results, choose the pregnant one.
  ungroup()
shujing_pr2 <- shujing_pr2[shujing_pr2$preg == 2,]
rm(a)
# Second step
cd_pr<-chandu_all
cd_pr$cp <- paste(cd_pr$parity-1,cd_pr$FID,sep='')
cd_pr <- cd_pr[c('FID','cp','cddate')]
shujing_pr2 <- merge(shujing_pr2,cd_pr,by='cp',all.x=T)
shujing_pr2_2 <- shujing_pr2[is.na(shujing_pr2$cddate),]
shujing_pr2%<>%
  group_by(cp)%>%
  filter(sjdate %in% max(sjdate))%>%
  ungroup()
shujing_pr2_1 <- shujing_pr2[!is.na(shujing_pr2$cddate),]
shujing_pr2_1$preg <- 1
rm(cd_pr)
# Merge all
aa <- shujing_pr2_2[,c('renum','preg')]
shujing_pr2 <- shujing_pr2_1
shujing_pr2 <- subset(shujing_pr2,select = -c(FID.y,cddate))
shujing_pr2$FID <- shujing_pr2$FID.x
shujing_pr2 <- shujing_pr2[,colnames(shujing_pr1)]
shujing_pr <- rbind(shujing_pr1,shujing_pr2)%>%
  group_by(cp)%>%
  filter(preg %in% min(preg))%>%
  ungroup()
aa1 <- merge(shujing_all,aa,by='renum',all.x=T)
aa1 <- aa1[is.na(aa1$preg),]
aa1 <- subset(aa1,select = -preg)
bb <- shujing_pr[c('renum','preg')]
aa1 <- merge(aa1,bb,by = 'renum',all.x = T)
aa1[is.na(aa1$preg),'preg'] <- 2
shujing_pr <- aa1
rm(shujing_pr1,shujing_pr2,sjrj,shujing_pr2_2,shujing_pr2_1,aa1,bb,aa)


# 2. Age at first service (AFS) ---------------------------------------------------------------------
# 270d <= AFS <= 900d
shujing_first <- shujing_all[which(shujing_all$parity == '0'),]
shujing_first%<>%
  group_by(FID)%>%
  filter(sjdate %in% min(sjdate))%>%
  ungroup()
chusheng_AFS <- chusheng_all[,c('FID','csdate','HYB','YMB')]
AFS_all <- merge(shujing_first,chusheng_AFS,by='FID',all.x=T)
AFS_all$AFS <- AFS_all$sjdate-AFS_all$csdate
AFS_controled <- AFS_all[which(AFS_all$AFS %in% seq(270,900)),]
AFS_controled%<>%
  group_by(FID)%>%
  filter(renum %in% min(renum))%>%
  ungroup()
AFS_controled$AFS <- as.numeric(AFS_controled$AFS)
rm(AFS_all,shujing_first)

# 3. Age at first calving (AFC) ----------------------------------------------
# 500d <= AFC <= 1100d
chandu_first <- chandu_all[which(chandu_all$parity == '1'),]
chandu_first%<>%
  group_by(FID)%>%
  filter(cddate %in% min(cddate))%>%
  filter(renum %in% min(renum))%>%
  ungroup()
chusheng_AFS <- chusheng_all[,c('FID','csdate','YMB','HYB')]
AFC_all <- merge(chandu_first,chusheng_AFS,by='FID',all.x=T)
AFC_all$AFC <- as.numeric(AFC_all$cddate-AFC_all$csdate)
AFC_controled<-AFC_all[which(AFC_all$AFC %in% seq(500,1100)),]
aa <- AFS_controled[c('stnum','FID','SC')]
AFC_controled <- merge(AFC_controled,aa,by = 'FID',all.x = T)
AFC_controled[is.na(AFC_controled$stnum),'stnum'] <- 999999
AFC_controled[is.na(AFC_controled$SC),'SC'] <- 999999
rm(chusheng_AFS,AFC_all,aa)

# 4. Interval from calving to first service (ICF_C)---------------------------------
# 20d <= ICF <= 230d
shujing_deleted <- shujing_all
shujing_deleted$cowparity <- paste(shujing_deleted$parity+1,shujing_deleted$FID,sep='')
shujing_allfirst<-shujing_deleted%>%
  group_by(cowparity)%>%
  filter(sjdate %in% min(sjdate))%>%
  ungroup()
shujing_allfirst%<>%
  group_by(cowparity)%>%
  filter(row_number()==1)%>%
  ungroup()
shujing_allfirst_ICF <- shujing_allfirst[,c('FID','parity','sjdate','stnum')]
ICF_all <- merge(chandu_all,shujing_allfirst_ICF,by=c('FID','parity'),all.x=T)
ICF_all$ICF <- as.numeric(ICF_all$sjdate-ICF_all$cddate)
ICF_controled <- ICF_all%>%
  filter(ICF %in% seq(20,230))
rm(shujing_allfirst_ICF,ICF_all)

# 5. Stillbirth (SB) and calving ease (CE) ---------------------------------------------------------
# SB: stillbirth = 1, normal = 0; CE: normal = 1, slight = 2, hard = 3.
# In order to include service sire effects, MID is needed to be merged. 
# First step: Filter the last record of service that lead to pregnant (260d-302d) in each parity and merge corresponding MID.
# Second step: If not, choose the latest service record 260d-302d before calving record.
# First step
a <- shujing_pr[shujing_pr$preg == 1,c('cp','sjdate','MID')]
b <- chandu_all %>%
  mutate(cp = paste(parity-1,FID,sep = ''))
c <- shujing_all[,c('cp','sjdate','MID')]
MID_all_1 <- merge(b,a,by = 'cp',all = F)
MID_all_1$dif <- as.numeric(MID_all_1$cddate - MID_all_1$sjdate)
MID_all_1_1 <- MID_all_1[MID_all_1$dif %in% seq(260,302),]
MID_all_1_2 <- MID_all_1[!MID_all_1$dif %in% seq(260,302),]
MID_all_1_1 <- subset(MID_all_1_1,select = -dif)
MID_all_1_2 <- subset(MID_all_1_2,select = -dif)

# Second step
MID_all_2 <- merge(b,a,by = 'cp',all.x = T)
MID_all_2 <- MID_all_2[is.na(MID_all_2$sjdate),]
MID_all_2 <- rbind(MID_all_2,MID_all_1_2)
MID_all_2 <- subset(MID_all_2,select = -c(sjdate,MID))
MID_to_sj <- merge(MID_all_2,c,by = 'cp',all.x = T) %>%
  group_by(cp)%>%
  mutate(dif = cddate-sjdate) %>%
  filter(dif %in% min(dif)) %>%
  ungroup()
MID_to_sj[!MID_to_sj$dif %in% seq(260,302),'MID'] <- NA
MID_to_sj <- MID_to_sj[colnames(MID_all_1_1)]
MID_all <- rbind(MID_to_sj,MID_all_1_1)

# Merge MID in pedigree.
MID_all <- merge(corid,MID_all,by.x = 'sire',by.y = 'MID',all.y = T)
MID_all[is.na(MID_all$MID),'MID'] <- 999999

# SB and CE 
SB_all <- MID_all[MID_all$SB > 0,c('HYC','YMC','parity','CALF','MID','FID','SB')]
CE_all <- MID_all[MID_all$CE > 0,c('HYC','YMC','parity','CALF','MID','FID','CE')]

rm(MID_all,MID_all_1,MID_all_1_1,MID_all_1_2,MID_all_2,MID_to_sj,a,b,c)
# 6. Interval from first service to last service (IFL) -----------------------
# IFL = 0d OR 19d <= IFL <= 355d
pr <- shujing_pr[which(shujing_pr$preg==1),]
pr$cp <- paste(pr$parity+1,pr$FID,sep='')
pr <- pr[,c('cp','sjdate')]
IFL_all <- merge(shujing_allfirst,pr,by.x='cowparity',by.y='cp',all=F)
IFL_all$IFL <- as.numeric(IFL_all$sjdate.y-IFL_all$sjdate.x)
IFL_all <- IFL_all[which(IFL_all$IFL %in% seq(0,355)),]
IFL_all <- IFL_all[-which(IFL_all$IFL %in% seq(1,18)),]
IFL_all <- IFL_all[c('HYI','YMI','parity','stnum','SC','FID','IFL')]
rm(pr)

# 7. Age at first service group effect (MF) ---------------------------------------
MF <- AFS_controled[,c('FID','AFS')]
MF[which(MF$AFS %in% seq(270,439)),'MF']<-1
MF[which(MF$AFS %in% seq(440,469)),'MF']<-2
MF[which(MF$AFS %in% seq(470,499)),'MF']<-3
MF[which(MF$AFS %in% seq(500,529)),'MF']<-4
MF[which(MF$AFS %in% seq(530,900)),'MF']<-5
MF<-MF[,c('FID','MF')]

# 8. Merge MF with ICF, CE, SB, IFL; merge MID and FID with pedigree code, then output -----------------------
ped_simple <- ped_all[c('id','code')]
for (i in c('CE_all','SB_all','IFL_all','ICF_controled')){
  a <- get(i)
  aa <- merge(a,MF,by = 'FID',all.x = T)
  aa[is.na(aa$MF),'MF'] <- 999999
  aa$FID <- paste('HOCHN',aa$FID,sep = '')
  aa <- merge(aa,ped_simple,by.x = 'FID',by.y = 'id',all = F)
  if (i == 'CE_all' | i == 'SB_all'){
    aa <- merge(aa,ped_simple,by.x = 'MID',by.y = 'id',all.x = T)
    aa[is.na(aa$code.y),'code.y'] <- 999999
    aa <- subset(aa,select = -c(FID,MID))
    names(aa)[(ncol(aa)-1):ncol(aa)] <- c('FID','MID')
  } else{
    aa <- subset(aa,select = -FID)
    names(aa)[ncol(aa)] <- 'FID'
  }
  assign(i,aa)
  rm(a,aa)
}
for (i in c('AFS_controled','AFC_controled')){
  a <- get(i)
  aa <- a
  aa$FID <- paste('HOCHN',aa$FID,sep = '')
  aa <- merge(aa,ped_simple,by.x = 'FID',by.y = 'id',all = F)
  aa <- subset(aa,select = -FID)
  names(aa)[ncol(aa)] <- 'FID'
  assign(i,aa)
  rm(a,aa)
}
AFS <- AFS_controled[c('HYB','YMB','stnum','FID','AFS')]
AFC <- AFC_controled[c('HYB','YMB','stnum','SC','FID','AFC')]
CE_H <- CE_all[CE_all$parity == 1,c('HYC','YMC','MF','CALF','FID','MID','CE')]
CE_C <- CE_all[CE_all$parity > 1,c('HYC','YMC','MF','CALF','parity','FID','MID','CE')]
SB_H <- SB_all[SB_all$parity == 1,c('HYC','YMC','MF','CALF','FID','MID','SB')]
SB_C <- SB_all[SB_all$parity > 1,c('HYC','YMC','MF','CALF','parity','FID','MID','SB')]
IFL_H <- IFL_all[IFL_all$parity == 0,c('HYI','YMI','stnum','SC','MF','FID','IFL')]
IFL_C <- IFL_all[IFL_all$parity > 0,c('HYI','YMI','stnum','SC','MF','parity','FID','IFL')]
ICF <- ICF_controled[c('HYC','YMC','stnum','MF','parity','FID','ICF')]
# Out put
for (i in c('AFS','AFC','IFL_H','IFL_C','ICF','CE_H','CE_C','SB_H','SB_C')){
  a <- get(i)
  print(sum(is.na(a))) # check if there's any NA
  write.table(a,paste(out_dir,'DMU\\',i,'.txt',sep = ''),quote = F,row.names = F,col.names = F)
}
# 9. Run DMUAI ------------------------------------------------------------
# !ATTENTION! Please put the dir files and pedigree for DMU into the same directory as output files before running!
setwd(paste(out_dir,'DMU\\',sep =''))
for (i in c('AFS','AFC','IFL_H','IFL_C','ICF','CE_H','CE_C','SB_H','SB_C')){
  system(paste('run_dmuai ',i,sep = ''))
}
# ----------------------calculate genetic parameters-------------------------
# 【calving traits】CE and SB--------------------------------------
# 1. Parameters
name <- c('CE_H','SB_H','CE_C','SB_C')
a <- as.data.frame(matrix(NA,ncol = 9,nrow = 4))  
a[,1] <- name
names(a)<-c('trait','fh2','fh2se','fre','frese','mh2','mh2se','mre','mrese')
for (i in name){
  df<-read.table(paste(i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA)
  if (i=='CE_H'|i=='SB_H'){
    var_a_f<-df[2,2]
    cov_f_m<-df[3,2]
    var_a_m<-df[4,2]
    var_pe_m<-df[5,2]
    var_e<-df[6,2]
    var_re_m<-var_a_m+var_pe_m
    var_p<-var_a_f+var_a_m+var_pe_m+var_e+2*cov_f_m
    var_var_a_f<-df[7,4]
    var_var_a_m<-df[12,4]
    var_var_pe_m<-df[16,4]
    var_var_e<-df[21,4]
    cov_af_af<-var_var_a_f
    cov_am_am<-var_var_a_m
    cov_pem_pem<-var_var_pe_m
    cov_e_e<-var_var_e
    cov_af_am<-df[10,4]
    cov_af_pem<-df[13,4]
    cov_af_e<-df[17,4]
    cov_am_pem<-df[15,4]
    cov_am_e<-df[19,4]
    cov_pem_e<-df[20,4]
    cov_af_rem<-cov_af_am+cov_af_pem
    cov_am_rem<-cov_am_am+cov_am_pem
    cov_pem_rem<-cov_pem_pem+cov_am_pem
    cov_rem_rem<-cov_am_rem+cov_pem_rem
    cov_rem_e<-cov_am_e+cov_pem_e
    var_var_rem<-cov_rem_rem
    cov_af_p<-cov_af_af+cov_af_rem+cov_af_e
    cov_am_p<-cov_af_am+cov_am_rem+cov_am_e
    cov_pem_p<-cov_af_pem+cov_pem_rem+cov_pem_e
    cov_rem_p<-cov_af_rem+cov_rem_rem+cov_rem_e
    cov_e_p<-cov_af_e+cov_rem_e+cov_e_e
    cov_p_p<-cov_af_p+cov_rem_p+cov_e_p
    var_var_p<-cov_p_p
    fh<-var_a_f/var_p
    mh<-var_a_m/var_p
    mre<-var_re_m/var_p
    fhse<-sqrt(((var_a_f/var_p)^2)*(var_var_a_f/(var_a_f^2)+var_var_p/(var_p^2)-2*cov_af_p/(var_a_f*var_p)))
    mhse<-sqrt(((var_a_m/var_p)^2)*(var_var_a_m/(var_a_m^2)+var_var_p/(var_p^2)-2*cov_am_p/(var_a_m*var_p)))
    mrese<-sqrt(((var_re_m/var_p)^2)*(var_var_rem/(var_re_m^2)+var_var_p/(var_p^2)-2*cov_rem_p/(var_re_m*var_p)))
    b<-c(round(fh,4),round(fhse,4),NA,NA,round(mh,4),round(mhse,4),round(mre,4),round(mrese,4))
    a[which(a$trait==i),2:9]<-b
  } else {
    var_a_f<-df[2,2]
    var_a_m<-df[4,2]
    cov_f_m<-df[3,2]
    var_pe_f<-df[5,2]
    var_pe_m<-df[6,2]
    var_e<-df[7,2]
    var_re_m<-var_a_m+var_pe_m
    var_re_f<-var_a_f+var_pe_f
    var_p<-var_a_f+var_a_m+var_pe_m+var_pe_f+var_e
    var_var_a_f<-df[8,4]
    var_var_a_m<-df[13,4]
    var_var_pe_f<-df[17,4]
    var_var_pe_m<-df[22,4]
    var_var_e<-df[28,4]
    cov_af_af<-var_var_a_f
    cov_am_am<-var_var_a_m
    cov_pem_pem<-var_var_pe_m
    cov_pef_pef<-var_var_pe_f
    cov_e_e<-var_var_e
    cov_af_am<-df[11,4]
    cov_af_pem<-df[18,4]
    cov_af_pef<-df[14,4]
    cov_af_e<-df[23,4]
    cov_am_pem<-df[20,4]
    cov_am_pef<-df[16,4]
    cov_am_e<-df[25,4]
    cov_pem_e<-df[27,4]
    cov_pem_pef<-df[21,4]
    cov_pef_e<-df[26,4]
    cov_af_rem<-cov_af_am+cov_af_pem
    cov_am_rem<-cov_am_am+cov_am_pem
    cov_pem_rem<-cov_pem_pem+cov_am_pem
    cov_pef_rem<-cov_am_pef+cov_pem_pef
    cov_rem_rem<-cov_am_rem+cov_pem_rem
    cov_rem_e<-cov_am_e+cov_pem_e
    cov_af_ref<-cov_af_af+cov_af_pef
    cov_am_ref<-cov_af_am+cov_am_pef
    cov_pem_ref<-cov_pem_pef+cov_am_pef
    cov_pef_ref<-cov_af_pef+cov_pef_pef
    cov_ref_ref<-cov_af_ref+cov_pef_ref
    cov_ref_e<-cov_af_e+cov_pef_e
    cov_ref_rem<-cov_pef_rem+cov_af_rem
    var_var_rem<-cov_rem_rem
    var_var_ref<-cov_ref_ref
    cov_af_p<-cov_af_rem+cov_af_ref+cov_af_e
    cov_am_p<-cov_am_ref+cov_am_rem+cov_am_e
    cov_pem_p<-cov_pem_ref+cov_pem_rem+cov_pem_e
    cov_rem_p<-cov_rem_rem+cov_ref_rem+cov_rem_e
    cov_pef_p<-cov_pef_ref+cov_pef_rem+cov_pef_e
    cov_ref_p<-cov_ref_ref+cov_ref_rem+cov_ref_e
    cov_e_p<-cov_ref_e+cov_rem_e+cov_e_e
    cov_p_p<-cov_ref_p+cov_rem_p+cov_e_p
    var_var_p<-cov_p_p
    fh<-var_a_f/var_p
    mh<-var_a_m/var_p
    mre<-var_re_m/var_p
    fre<-var_re_f/var_p
    fhse<-sqrt(((var_a_f/var_p)^2)*(var_var_a_f/(var_a_f^2)+var_var_p/(var_p^2)-2*cov_af_p/(var_a_f*var_p)))
    mhse<-sqrt(((var_a_m/var_p)^2)*(var_var_a_m/(var_a_m^2)+var_var_p/(var_p^2)-2*cov_am_p/(var_a_m*var_p)))
    mrese<-sqrt(((var_re_m/var_p)^2)*(var_var_rem/(var_re_m^2)+var_var_p/(var_p^2)-2*cov_rem_p/(var_re_m*var_p)))
    frese<-sqrt(((var_re_f/var_p)^2)*(var_var_ref/(var_re_f^2)+var_var_p/(var_p^2)-2*cov_ref_p/(var_re_f*var_p)))
    b<-c(round(fh,4),round(fhse,4),round(fre,4),round(frese,4),round(mh,4),round(mhse,4),round(mre,4),round(mrese,4))
    a[which(a$trait==i),2:9]<-b
  }
}
write.csv(a,paste(out_dir,'calving parameters.csv',sep = ''),row.names=F)
# 2. EBVs
name <- c('CE_H','CE_C','SB_H','SB_C')
a <- data.frame(name)
a$af <- NA
a$am <- NA

ped_all$birthdate <- as.Date(ped_all$birthdate,format='%Y-%m-%d')
ped_all <- ped_all[complete.cases(ped_all$birthdate),]
ped_all[which(substr(ped_all$id,6,6)=='M'),'aa'] <- 1
ped_all[is.na(ped_all$aa),'aa']<-2

# create directories
dir.create(paste(out_dir,'育种值',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系\\筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系\\未筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系\\筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系\\未筛选',sep = ''))

# output unfiltered EBVs
for (i in name){
  df <- read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA)
  all <- read.table(paste(out_dir,'DMU\\',i,'.SOL',sep=''))
  a[which(a$name==i),2] <- df[2,2]
  a[which(a$name==i),3] <- df[4,2]
  # dam effect EBV
  mx<-all[which(all[,1]==4&all[,3]==1),]
  mx$REL<-1-((mx$V9)^2)/a[which(a$name==i),2]
  mx<-mx[which(mx$REL>0&mx$REL!=1),]
  mx$AC<-sqrt(mx$REL)
  b<-merge(mx,ped_all,by.x='V5',by.y='code',all.x=T)
  b<-b[,-c(2:5,13:14)]
  c<-b[which(b$aa==1),]
  c<-c[,-12]
  assign(paste('mx_',i,'_male',sep=''),c)
  c<-c[,c(1,8,4:7,11)]
  names(c)<-c('系谱编号','ID','育种值','育种值标准误','育种值可靠性','育种值准确性','出生日期')
  write.csv(c,paste(out_dir,'育种值\\产犊性状\\母系\\未筛选\\',i,'_公牛育种值.csv',sep=''),row.names=F)
  d<-b[which(b$aa==2),]
  d<-d[,-12]
  assign(paste('mx_',i,'_female',sep=''),d)
  d<-d[,c(1,8,4:7,11)]
  names(d)<-c('系谱编号','ID','育种值','育种值标准误','育种值可靠性','育种值准确性','出生日期')
  write.csv(d,paste(out_dir,'育种值\\产犊性状\\母系\\未筛选\\',i,'_母牛育种值.csv',sep=''),row.names=F)
  # service sire effect EBV
  fx<-all[which(all[,1]==4&all[,3]==2),]
  fx$REL<-1-((fx$V9)^2)/a[which(a$name==i),3]
  fx<-fx[which(fx$REL>0&fx$REL!=1),]
  fx$AC<-sqrt(fx$REL)
  b<-merge(fx,ped_all,by.x='V5',by.y='code',all.x=T)
  b<-b[,-c(2:5,13:14)]
  c<-b[which(b$aa==1),]
  c<-c[,-12]
  assign(paste('fx_',i,'_male',sep=''),c)
  c<-c[,c(1,8,4:7,11)]
  names(c)<-c('系谱编号','ID','育种值','育种值标准误','育种值可靠性','育种值准确性','出生日期')
  write.csv(c,paste(out_dir,'育种值\\产犊性状\\父系\\未筛选\\',i,'_公牛育种值.csv',sep=''),row.names=F)
  d<-b[which(b$aa==2),]
  d<-d[,-12]
  assign(paste('fx_',i,'_female',sep=''),d)
  d<-d[,c(1,8,4:7,11)]
  names(d)<-c('系谱编号','ID','育种值','育种值标准误','育种值可靠性','育种值准确性','出生日期')
  write.csv(d,paste(out_dir,'育种值\\产犊性状\\父系\\未筛选\\',i,'_母牛育种值.csv',sep=''),row.names=F)
}

# filter bulls born after 2009 and have more than five daughters
name<-c('CE_H','SB_H','CE_C','SB_C')
for (i in name){
  a<-get(paste('mx_',i,'_male',sep=''))
  b<-a[which(lubridate::year(a$birthdate)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  assign(paste('mx_',i,'_male',sep=''),c)
  rm(a,c)
}
pedfreq <- data.frame(table(ped_all$sirecode))
pedfreq<-pedfreq[-1,]

for (i in name){
  f<-read.table(paste(out_dir,'DMU\\',i,'.txt',sep=''))
  f$herd<-as.numeric(substr(f$V1,1,nchar(f$V1)-4))
  a<-merge(f,ped_all,by.x=paste('V',ncol(f)-3,sep=''),by.y='code',all.x=T)
  a<-a[!duplicated(a[,1]),]
  b<-data.frame(table(a$sirecode))
  h<-a[,c(ncol(a)-3,ncol(f))]
  h<-h[!duplicated(h),]
  h2<-data.frame(table(h$sirecode))
  c<-get(paste('mx_',i,'_male',sep=''))
  e<-merge(b,c,by.x='Var1',by.y='code',all.x=T)
  e<-merge(e,pedfreq,by='Var1',all.x=T)
  e<-e[complete.cases(e$EBV),]
  rm(b,c)
  e<-merge(e,h2,by='Var1',all.x=T)
  e<-e[,c(1,7,9,10,2,11,3,4,5,6)]
  names(e)<-c('系谱编号','公牛号','出生日期','总女儿数','有表型的女儿数','群体数','估计育种值EBV','标准误','可靠性','准确性')
  e$出生年份<-lubridate::year(e$出生日期)
  write.csv(e,paste(out_dir,'\\育种值\\产犊性状\\母系\\筛选\\',i,'.csv',sep=''),row.names = F)
  rm(f,e,a)
} 

for (i in name){
  a<-get(paste('fx_',i,'_male',sep=''))
  b<-a[which(lubridate::year(a$birthdate)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  c<-c[,c(1,6,2:5,8)]
  colnames(c)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','出生日期')
  write.csv(c,paste(out_dir,'育种值\\产犊性状\\父系\\筛选\\',i,'.csv',sep=''),row.names = F)
  rm(a,c)
}


# 【other traits】AFS, AFC, IFL_H, IFL_C and ICF---------------------------
# 1. Parameters
tnames<-c('AFC','AFS','ICF','IFL_C','IFL_H')
for (i in tnames){
  assign(i,read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA))
  a<-get(i)
  if (a[1,1] == 2){
    var_k<-a[2,2]
    var_e<-a[3,2]
    var_p<-var_k+var_e
    var_var_k<-a[4,4]
    var_var_e<-a[6,4]
    cov_var_ke<-a[5,4]
    var_var_p<-var_var_k+var_var_e+2*cov_var_ke
    cov_var_kp<-var_var_k+cov_var_ke
    heritability<- var_k/var_p
    SE<-sqrt(((var_k/var_p)^2)*(var_var_k/(var_k^2)+var_var_p/(var_p^2)-2*(cov_var_kp/(var_k*var_p))))
    assign(paste(i,'_result',sep=''),data.frame(t(c(heritability,SE,NA,NA))))
    print(paste(i,round(var_k,4),sep=' '))
    rm(var_k,var_e,var_p,var_var_e,var_var_k,var_var_p,cov_var_ke,cov_var_kp,heritability,SE)
  } else {
    if (a[1,1] == 3){
      var_k<-a[2,2]
      var_pe<-a[3,2]
      var_e<-a[4,2]
      var_re<-var_k+var_pe
      var_p<-var_e+var_re
      var_var_k<-a[5,4]
      var_var_pe<-a[7,4]
      var_var_e<-a[10,4]
      cov_var_kpe<-a[6,4]
      cov_var_ke<-a[8,4]
      cov_var_pee<-a[9,4]
      var_var_re<-var_var_k+var_var_pe+2*cov_var_kpe
      cov_var_ree<-cov_var_ke+cov_var_pee
      var_var_p<-var_var_k+var_var_pe+var_var_e+2*cov_var_kpe+2*cov_var_ke+2*cov_var_pee
      cov_var_kp<-var_var_k+cov_var_kpe+cov_var_ke
      cov_var_rep<-var_var_re+cov_var_ree
      heritability <- var_k/var_p
      print(paste(i,round(var_k,4),sep=' '))
      SE_heritability <- sqrt(((var_k/var_p)^2)*(var_var_k/(var_k^2)+var_var_p/(var_p^2)-2*(cov_var_kp/(var_k*var_p))))
      repeatability<- var_re/var_p
      SE_repeatability<-sqrt(((var_re/var_p)^2)*(var_var_re/(var_re^2)+var_var_p/(var_p^2)-2*(cov_var_rep/(var_re*var_p))))
      assign(paste(i,'_result',sep=''),data.frame(t(c(heritability,SE_heritability,repeatability,SE_repeatability))))
    }
  }
}

total<-as.data.frame(matrix(NA,ncol = 4,nrow = 5))  
for(i in 1:5){
  total[i,]<-get(paste(tnames[i],'_result',sep=''))[1,]
}
total<-cbind(total,as.data.frame(tnames))
names(total)<-c('h2','h2_SE','re','re_SE','trait')
total<-total[,c(5,1:4)]
total$H2<-paste(round(total$h2,3),'±',round(total$h2_SE,3),sep='')
total$RE<-paste(round(total$re,3),'±',round(total$re_SE,3),sep='')
write.csv(total,paste(out_dir,'other parameters.csv',sep = ''),row.names = F)

# 2. EBVs
tnames<-c('AFC','AFS','ICF','IFL_C','IFL_H')
d<-data.frame(tnames)
d$A<-NA
for (i in tnames){
  assign(i,read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA))
  a<-get(i)
  d[which(d$tnames==i),2]<-a[2,2]
}
for (i in tnames){
  assign(i,read.table(paste(out_dir,'DMU\\',i,'.SOL',sep='')))
  a<-get(i)
  a<-a[which(a[,1]==4&a[,3]==1),]
  a$REL<-1-((a$V9)^2)/d[which(d$tnames==i),2]
  a<-a[which(a$REL>0),]
  a$AC<-sqrt(a$REL)
  b<-merge(a,ped_all,by.x='V5',by.y='code',all.x=T)
  b<-b[,-c(2:5,13:14)]
  c<-b[which(b$aa==1),]
  c<-c[complete.cases(c$birthdate),]
  c<-c[,-12]
  assign(paste(i,'_EBV_ped_m',sep=''),c)
  c<-b[which(b$aa==2),]
  c<-c[complete.cases(c$birthdate),]
  c<-c[,-11]
  assign(paste(i,'_EBV_ped_f',sep=''),c)
  a<-merge(a,ped_all,by.x='V5',by.y='code',all.x=T)
  a<-a[complete.cases(a$birthdate),-c(2:7,12:14,16,17,18)]
  assign(paste(i,'_EBV',sep=''),a)
  rm(a,b,c)
}

# calculate reliabilities
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_f',sep=''))
  print(paste(i,',',round(mean(a$V8)-sd(a$V8),3),'~',round(mean(a$V8)+sd(a$V8),3),',',round(mean(a$REL),4),sep=''))
  rm(a)
}
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_m',sep=''))
  print(paste(i,',',round(mean(a$V8)-sd(a$V8),3),'~',round(mean(a$V8)+sd(a$V8),3),',',round(mean(a$REL),4),sep=''))
  rm(a)
}

# create directories
dir.create(paste(out_dir,'育种值\\其他性状',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\母牛',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛\\未筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛\\筛选',sep = ''))

ped_f<-ped_all[,c('code','birthdate')]
# female EBVs
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_f',sep=''))
  a<-a[,c(1,4:8)]
  a<-merge(a,ped_f,by.x = 'V5',by.y='code',all.x=T)
  a<-a[,c(1,6,2:5,7)]
  colnames(a)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','生日')
  write.csv(a,paste(out_dir,'育种值\\其他性状\\母牛\\',i,'.csv',sep=''),row.names=F)
}
# male EBVs (unfiltered)
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_m',sep=''))
  a<-a[,c(1,8,4:7,11)]
  colnames(a)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','生日')
  write.csv(a,paste(out_dir,'育种值\\其他性状\\公牛\\未筛选\\',i,'.csv',sep=''),row.names=F)
}
# male EBVs (born after 2009 and have more than five daughters)
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_m',sep=''))
  b<-a[which(lubridate::year(a$birthdate)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  assign(paste(i,'_EBV_ped_m',sep=''),c)
  rm(a,c)
}

for (i in tnames){
  if (i!='ICF'){
    f<-read.table(paste(out_dir,'DMU\\',i,'.txt',sep=''))
    f$herd<-as.numeric(substr(f$V1,1,nchar(f$V1)-4))
    a<-merge(f,ped_all,by.x=paste('V',ncol(f)-2,sep=''),by.y='code',all.x=T)
    a<-a[!duplicated(a[,1]),]
    b<-data.frame(table(a$sirecode))
    b<-b[-1,]
    h<-a[,c(ncol(a)-3,ncol(f))]
    h<-h[!duplicated(h),]
    h2<-data.frame(table(h$sirecode))
    h2<-h2[-1,]
    c<-get(paste(i,'_EBV_ped_m',sep=''))
    e<-merge(b,c,by.x='Var1',by.y='code',all.x=T)
    e<-merge(e,pedfreq,by='Var1',all.x=T)
    e<-e[complete.cases(e$EBV),]
    rm(b,c)
    e<-merge(e,h2,by='Var1',all.x=T)
    e<-e[,c(1,7,9,10,2,11,3,4,5,6)]
    names(e)<-c('系谱编号','公牛号','出生日期','总女儿数','有表型的女儿数','群体数','估计育种值EBV','标准误','可靠性','准确性')
    e$出生年份<-lubridate::year(e$出生日期)
    write.csv(e,paste(out_dir,'育种值\\其他性状\\公牛\\筛选\\',i,'.csv',sep=''),row.names = F)
    rm(f,e,a)
  } else{
    f<-read.table(paste(out_dir,'DMU\\',i,'.txt',sep=''))
    f$herd<-as.numeric(substr(f$V2,1,nchar(f$V1)-4))
    a<-merge(f,ped_all,by.x=paste('V',ncol(f)-2,sep=''),by.y='code',all.x=T)
    a<-a[!duplicated(a[,1]),]
    b<-data.frame(table(a$sirecode))
    b<-b[-1,]
    h<-a[,c(ncol(a)-3,ncol(f))]
    h<-h[!duplicated(h),]
    h2<-data.frame(table(h$sirecode))
    h2<-h2[-1,]
    c<-get(paste(i,'_EBV_ped_m',sep=''))
    e<-merge(b,c,by.x='Var1',by.y='code',all.x=T)
    e<-merge(e,pedfreq,by='Var1',all.x=T)
    e<-e[complete.cases(e$EBV),]
    rm(b,c)
    e<-merge(e,h2,by='Var1',all.x=T)
    e<-e[,c(1,7,9,10,2,11,3,4,5,6)]
    names(e)<-c('系谱编号','公牛号','出生日期','总女儿数','有表型的女儿数','群体数','估计育种值EBV','标准误','可靠性','准确性')
    e$出生年份<-lubridate::year(e$出生日期)
    write.csv(e,paste(out_dir,'育种值\\其他性状\\公牛\\筛选\\',i,'.csv',sep=''),row.names = F)
    rm(f,e,a)
  }
} 

# other results -----------------------------------------------------------
# 【statistical summary of phenotypes】
tnames<-c('AFC','AFS','CE_C','CE_H','SB_C','SB_H','ICF','IFL_C','IFL_H')
d<-data.frame(tnames)
d$A<-NA
d$num<-NA
for (i in 1:9){
  f<-read.table(paste(out_dir,'DMU\\',tnames[i],'.txt',sep=''))
  d[i,2]<-paste(round(mean(f[,ncol(f)]),4),'±',round(sd(f[,ncol(f)]),4))
  d[i,3]<-nrow(f)
}
names(d) <- c('性状','均值±标准差','数据量')
write.csv(d,paste(out_dir,'描述性统计.csv',sep = ''),row.names = F)
