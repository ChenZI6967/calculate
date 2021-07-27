# >>>>>>>>>>>>>>Genetic estimation for population from Beijing<<<<<<<<<<<<<<<<<<<<<
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!READ ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!You must change the input and output directories!!!!!!!!
# !!!!Save your raw data in a safe space or have another copy!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------data formatting-----------------------------
# Models: AFS=HYB + YMB + ST + A + E
#         AFC= HYB + YMB + ST + SC + A + E
#         ICF = HYC + YMC + ST + MF + P + PE + A + E
#         IFL_H = HYI + YMI + ST + SC + MF + A + E
#         IFL_C = HYI + YMI + ST + SC + MF + P + PE + A + E
#         CE_H = HYC + YMC + MF + Sex + A + E
#         CE_C = HYC + YMC + MF + Sex + P + A + E
#         SB_H = HYC + YMC + MF + Sex + A + E
#         SB_C = HYC + YMC + MF + Sex + P + A + E
# Load packages -----------------------------------------------------------
pkg <- c('data.table','dplyr','lubridate','tidyverse','stringr','magrittr','ggplot2','grid','gtable')
for (i in pkg){
  if (!requireNamespace(i,quietly = T)) install.packages(i)
}
lapply(pkg, library, character.only = T)
# Merge new data ----------------------------------------------------------
# Original data comes from dairy farms and needs to be merged into our formatted data.
new_dir <- 'D:\\chenzw\\数据库\\北京\\2108\\繁殖数据20210101-20210630\\' # Please change the directory of new original data.
old_dir <- 'D:\\chenzw\\数据库\\北京\\格式化数据（一直更新）\\2012\\' # Please change  the directory of old formatted data.
update_dir <- 'D:\\chenzw\\数据库\\北京\\格式化数据（一直更新）\\2108\\' # Please change the directory of output updated data.

new_cd <- as.data.frame(fread(paste(new_dir,'产犊.csv',sep = ''),encoding = 'UTF-8'))
new_sj <- as.data.frame(fread(paste(new_dir,'配种.csv',sep = ''),encoding = 'UTF-8'))
new_cs <- as.data.frame(fread(paste(new_dir,'出生.csv',sep = ''),encoding = 'UTF-8'))
new_rj <- as.data.frame(fread(paste(new_dir,'妊检.csv',sep = ''),encoding = 'UTF-8'))
names(new_cd) <- c('牛场编号','牛场名称','牛号','胎次','产犊日期','与配公牛','产犊难易','犊牛性别','存活情况','犊牛出生重')
names(new_cs) <- c('牛场编号','牛场名称','牛号','出生日期','母亲号','出生重')
names(new_sj) <- c('牛场编号','牛场名称','牛号','胎次','配种日期','冻精号','冻精用量','配种员','冻精类型','配种方式','本胎次配种次数')
names(new_rj) <- c('牛场编号','牛场名称','牛号','胎次','妊检日期','妊检结果','妊检方法','检查员')

old_cd <- as.data.frame(fread(paste(old_dir,'cd.csv',sep = '')))
old_sj <- as.data.frame(fread(paste(old_dir,'sj.csv',sep = '')))
old_cs <- as.data.frame(fread(paste(old_dir,'cs.csv',sep = '')))
old_rj <- as.data.frame(fread(paste(old_dir,'rj.csv',sep = '')))
old_sc <- as.data.frame(fread(paste(old_dir,'sc.csv',sep = '')))

herdnum <- read.csv('D:\\chenzw\\遗传评估\\farmcode.csv',stringsAsFactors= F)

# Step 1: calving data
new_cd <- new_cd[-which(grepl('娟姗',new_cd$牛场名称)==T),]
new_cd[,3] <- stringr::str_trim(new_cd[,3],'both')
new_cd[,1] <- stringr::str_trim(new_cd[,1],'both')
new_cd[,11]<-substr(new_cd[,3],1,nchar(new_cd[,1]))

for (i in 1:nrow(herdnum)){
  new_cd[which(grepl(herdnum[i,2],new_cd$牛场名称)),12] <- herdnum[i,3] # Some herd codes are wrong, here we correct them
}

# format the FID
new_cd$牛号 <- gsub('XP','',new_cd$牛号)
new_cd$牛号 <- gsub('XLJ','',new_cd$牛号)
new_cd$牛号 <- gsub('HL','',new_cd$牛号)
new_cd$牛号 <- gsub('BH','',new_cd$牛号)
new_cd[which(new_cd$牛场编号==new_cd$V11),13]<-substr(new_cd[which(new_cd$牛场编号==new_cd$V11),3],nchar(new_cd[which(new_cd$牛场编号==new_cd$V11),1])+1,nchar(new_cd[which(new_cd$牛场编号==new_cd$V11),3]))
new_cd[,14]<-stringr::str_pad(new_cd[,13],6,'left',pad='0')
new_cd[which(nchar(new_cd[,14])==7),14]<-substr(new_cd[which(nchar(new_cd[,14])==7),14],2,7)
new_cd[,15]<-paste('F',new_cd$V12,new_cd[,14],sep='')
new_cd[is.na(new_cd$V13),15]<-paste('F',new_cd[is.na(new_cd$V13),3],sep='')
new_cd[which(nchar(new_cd$牛号)<7),15]<-paste('F',new_cd[which(nchar(new_cd$牛号)<7),1],stringr::str_pad(new_cd[which(nchar(new_cd$牛号)<7),3],6,'left',pad='0'),sep='')
new_cd[is.na(new_cd$V14),14]<-substr(new_cd[is.na(new_cd$V14),3],7,nchar(new_cd[is.na(new_cd$V14),3]))
new_cd[,14]<-stringr::str_pad(new_cd[,14],6,'left',pad='0')
new_cd[,15]<-paste('F',new_cd$V12,new_cd[,14],sep='')
new_cd[new_cd$V14=='000000',15] <- paste('F',new_cd[new_cd$V14=='000000',1],stringr::str_pad(new_cd[new_cd$V14=='000000',3],6,'left',pad='0'),sep='')
new_cd[which(nchar(new_cd$牛号) < 10),15] <- paste('F',new_cd[which(nchar(new_cd$牛号) < 10),12],str_pad(substr(new_cd[which(nchar(new_cd$牛号) < 10),3],nchar(new_cd[which(nchar(new_cd$牛号) < 10),3])-nchar(new_cd[which(nchar(new_cd$牛号) < 10),9])-1,nchar(new_cd[which(nchar(new_cd$牛号) < 10),3])),6,'left',pad='0'),sep = '')

new_cd <- new_cd[-which(nchar(new_cd$V15)>=14),]
new_cd<-new_cd[,-c(11,13,14)]
names(new_cd)[12]<-'FID'

# extract effects
new_cd<-new_cd[,c(11,2:10,12)]
new_cd[which(new_cd$产犊难易==0),7]<-NA
new_cd[which(new_cd$产犊难易 %in% seq(2,4)),7]<-2
new_cd[which(new_cd$产犊难易==1),7]<-1
new_cd[which(new_cd$产犊难易==5),7]<-3
new_cd[which(grepl('母牛',new_cd$犊牛性别)==T),8]<-2
new_cd[which(grepl('公牛',new_cd$犊牛性别)==T),8]<-1
new_cd[which(grepl('死胎',new_cd$存活情况)==T),9]<-2
new_cd[which(grepl('正常',new_cd$存活情况)==T),9]<-1
new_cd[,c(12:20)]<-NA
new_cd<-new_cd[,c(3,5,12,13,4,14:16,10,17,1,11,7,8,9)]
names(new_cd)<-c('牛号','日期','事件','事件信息','胎次','泌乳天数','日产量','产犊方式','出生重','配种员','HERD','FID','CE','SEX','SB')

# Step 2: pregnant detect data
new_rj<-new_rj[-which(grepl('娟姗',new_rj$牛场名称)==T),]
new_rj[,3]<-stringr::str_trim(new_rj[,3],'both')
new_rj[,1]<-stringr::str_trim(new_rj[,1],'both')
new_rj[,9]<-substr(new_rj[,3],1,nchar(new_rj[,1]))

for (i in 1:nrow(herdnum)){
  new_rj[which(grepl(herdnum[i,2],new_rj$牛场名称)),10]<-herdnum[i,3]
}

# format the FID
new_rj$牛号 <- gsub('XP','',new_rj$牛号)
new_rj$牛号 <- gsub('XLJ','',new_rj$牛号)
new_rj$牛号 <- gsub('HL','',new_rj$牛号)
new_rj$牛号 <- gsub('BH','',new_rj$牛号)
new_rj[which(new_rj$牛场编号==new_rj$V9),11]<-substr(new_rj[which(new_rj$牛场编号==new_rj$V9),3],nchar(new_rj[which(new_rj$牛场编号==new_rj$V9),1])+1,nchar(new_rj[which(new_rj$牛场编号==new_rj$V9),3]))
new_rj[,12]<-stringr::str_pad(new_rj[,11],6,'left',pad='0')
new_rj[which(nchar(new_rj[,12])==7),12]<-substr(new_rj[which(nchar(new_rj[,12])==7),12],2,7)
new_rj[,13]<-paste('F',new_rj$V10,new_rj[,12],sep='')
new_rj[is.na(new_rj$V11),13]<-paste('F',new_rj[is.na(new_rj$V11),3],sep='')
new_rj[which(nchar(new_rj$牛号)<7),13]<-paste('F',new_rj[which(nchar(new_rj$牛号)<7),1],stringr::str_pad(new_rj[which(nchar(new_rj$牛号)<7),3],6,'left',pad='0'),sep='')
new_rj[is.na(new_rj$V12),12]<-substr(new_rj[is.na(new_rj$V12),3],7,nchar(new_rj[is.na(new_rj$V12),3]))
new_rj[,12]<-stringr::str_pad(new_rj[,12],6,'left',pad='0')
new_rj[,13]<-paste('F',new_rj$V10,new_rj[,12],sep='')
new_rj[new_rj$V12=='000000',13] <- paste('F',new_rj[new_rj$V12=='000000',1],stringr::str_pad(new_rj[new_rj$V12=='000000',3],6,'left',pad='0'),sep='')
new_rj[which(nchar(new_rj$牛号) < 10),13] <- paste('F',new_rj[which(nchar(new_rj$牛号) < 10),10],str_pad(substr(new_rj[which(nchar(new_rj$牛号) < 10),3],nchar(new_rj[which(nchar(new_rj$牛号) < 10),3])-nchar(new_rj[which(nchar(new_rj$牛号) < 10),9])-1,nchar(new_rj[which(nchar(new_rj$牛号) < 10),3])),6,'left',pad='0'),sep = '')

new_rj <- new_rj[-which(nchar(new_rj$V13)>=14),]
new_rj<-new_rj[,-c(9,11,12)]
names(new_rj)[10]<-'FID'

# extract effects
new_rj<-new_rj[,c(9,2:8,10)]
new_rj[which(grepl('怀孕',new_rj$妊检结果)==T),6]<-'+'
new_rj[which(grepl('空怀',new_rj$妊检结果)==T),6]<-'-'
new_rj[,10:20]<-NA
new_rj<-new_rj[,c(3,5,10,6,4,11:15,1,9)]
names(new_rj)<-c('牛号','日期','事件','事件信息','胎次','泌乳天数','日产量','产犊方式','出生重','配种员','HERD','FID')

# Step 3: insemination data
new_sj<-new_sj[-which(grepl('娟姗',new_sj$牛场名称)==T),]
new_sj[,3]<-stringr::str_trim(new_sj[,3],'both')
new_sj[,1]<-stringr::str_trim(new_sj[,1],'both')
new_sj[,12]<-substr(new_sj[,3],1,nchar(new_sj[,1]))

for (i in 1:nrow(herdnum)){
  new_sj[which(grepl(herdnum[i,2],new_sj$牛场名称)),13]<-herdnum[i,3]
}

# format the FID
new_sj$牛号 <- gsub('XP','',new_sj$牛号)
new_sj$牛号 <- gsub('XLJ','',new_sj$牛号)
new_sj$牛号 <- gsub('HL','',new_sj$牛号)
new_sj$牛号 <- gsub('BH','',new_sj$牛号)
new_sj[which(new_sj$牛场编号==new_sj$V12),14]<-substr(new_sj[which(new_sj$牛场编号==new_sj$V12),3],nchar(new_sj[which(new_sj$牛场编号==new_sj$V12),1])+1,nchar(new_sj[which(new_sj$牛场编号==new_sj$V12),3]))
new_sj[,15]<-stringr::str_pad(new_sj[,14],6,'left',pad='0')
new_sj[which(nchar(new_sj[,15])==7),15]<-substr(new_sj[which(nchar(new_sj[,15])==7),15],2,7)
new_sj[,16]<-paste('F',new_sj$V13,new_sj[,15],sep='')
new_sj[is.na(new_sj$V14),16]<-paste('F',new_sj[is.na(new_sj$V14),3],sep='')
new_sj[which(nchar(new_sj$牛号)<7),16]<-paste('F',new_sj[which(nchar(new_sj$牛号)<7),1],stringr::str_pad(new_sj[which(nchar(new_sj$牛号)<7),3],6,'left',pad='0'),sep='')
new_sj[is.na(new_sj$V15),15]<-substr(new_sj[is.na(new_sj$V15),3],7,nchar(new_sj[is.na(new_sj$V15),3]))
new_sj[,15]<-stringr::str_pad(new_sj[,15],6,'left',pad='0')
new_sj[,16]<-paste('F',new_sj$V13,new_sj[,15],sep='')
new_sj[new_sj$V15=='000000',16] <- paste('F',new_sj[new_sj$V15=='000000',1],stringr::str_pad(new_sj[new_sj$V15=='000000',3],6,'left',pad='0'),sep='')
new_sj[which(nchar(new_sj$牛号) < 10),16] <- paste('F',new_sj[which(nchar(new_sj$牛号) < 10),13],str_pad(substr(new_sj[which(nchar(new_sj$牛号) < 10),3],nchar(new_sj[which(nchar(new_sj$牛号) < 10),3])-nchar(new_sj[which(nchar(new_sj$牛号) < 10),12])-1,nchar(new_sj[which(nchar(new_sj$牛号) < 10),3])),6,'left',pad='0'),sep = '')
new_sj <- new_sj[-which(nchar(new_sj$V16)>=14),]
new_sj<-new_sj[,-c(12,14,15)]
names(new_sj)[13]<-'FID'
sj_sc <- new_sj

# extract effects
new_sj<-new_sj[,c(12,2:11,13)]
new_sj[,c(13:20)]<-NA
new_sj<-new_sj[,c(3,5,13,14,4,15:18,8,1,12)]
names(new_sj)<-c('牛号','日期','事件','事件信息','胎次','泌乳天数','日产量','产犊方式','出生重','配种员','HERD','FID')

# Step 4: birth data
new_cs<-new_cs[-which(substr(new_cs$牛号,1,1)=='G'),]
new_cs<-new_cs[-which(grepl('娟姗',new_cs$牛场名称)==T),]
new_cs[,3]<-stringr::str_trim(new_cs[,3],'both')
new_cs[,1]<-stringr::str_trim(new_cs[,1],'both')
new_cs[,7]<-substr(new_cs[,3],1,nchar(new_cs[,1]))

for (i in 1:nrow(herdnum)){
  new_cs[which(grepl(herdnum[i,2],new_cs$牛场名称)),8]<-herdnum[i,3]
}

# format the FID
new_cs$牛号 <- gsub('XP','',new_cs$牛号)
new_cs$牛号 <- gsub('XLJ','',new_cs$牛号)
new_cs$牛号 <- gsub('HL','',new_cs$牛号)
new_cs$牛号 <- gsub('BH','',new_cs$牛号)
new_cs[which(new_cs$牛场编号==new_cs$V7),9]<-substr(new_cs[which(new_cs$牛场编号==new_cs$V7),3],nchar(new_cs[which(new_cs$牛场编号==new_cs$V7),1])+1,nchar(new_cs[which(new_cs$牛场编号==new_cs$V7),3]))
new_cs[,10]<-stringr::str_pad(new_cs[,9],6,'left',pad='0')
new_cs[which(nchar(new_cs[,10])==7),10]<-substr(new_cs[which(nchar(new_cs[,10])==7),10],2,7)
new_cs[,11]<-paste('F',new_cs$V8,new_cs[,10],sep='')
new_cs[is.na(new_cs$V9),11]<-paste('F',new_cs[is.na(new_cs$V9),3],sep='')
new_cs[which(nchar(new_cs$牛号)<7),11]<-paste('F',new_cs[which(nchar(new_cs$牛号)<7),1],stringr::str_pad(new_cs[which(nchar(new_cs$牛号)<7),3],6,'left',pad='0'),sep='')
new_cs[is.na(new_cs$V10),10]<-substr(new_cs[is.na(new_cs$V10),3],7,nchar(new_cs[is.na(new_cs$V10),3]))
new_cs[,10]<-stringr::str_pad(new_cs[,10],6,'left',pad='0')
new_cs[,11]<-paste('F',new_cs$V8,new_cs[,10],sep='')
new_cs[new_cs$V10=='000000',11] <- paste('F',new_cs[new_cs$V10=='000000',1],stringr::str_pad(new_cs[new_cs$V10=='000000',3],6,'left',pad='0'),sep='')
new_cs[which(nchar(new_cs$牛号) < 10),11] <- paste('F',new_cs[which(nchar(new_cs$牛号) < 10),8],str_pad(substr(new_cs[which(nchar(new_cs$牛号) < 10),3],nchar(new_cs[which(nchar(new_cs$牛号) < 10),3])-nchar(new_cs[which(nchar(new_cs$牛号) < 10),7])-1,nchar(new_cs[which(nchar(new_cs$牛号) < 10),3])),6,'left',pad='0'),sep = '')

new_cs <- new_cs[-which(nchar(new_cs$V11)>=14),]
new_cs<-new_cs[,-c(7,9,10)]
names(new_cs)[8]<-'FID'

# extract effects
new_cs<-new_cs[,c(7,2:6,8)]
new_cs[,8:15]<-NA
new_cs<-new_cs[,c(3,4,8,5,9:14,1,7)]
names(new_cs)<-c('牛号','日期','事件','事件信息','胎次','泌乳天数','日产量','产犊方式','出生重','配种员','HERD','FID')

# Step 5: save all the updated data 
new_cd$日期 <- as.Date(new_cd$日期)
new_sj$日期 <- as.Date(new_sj$日期)
new_rj$日期 <- as.Date(new_rj$日期)
new_cs$日期 <- as.Date(new_cs$日期)
old_cd$日期 <- as.Date(old_cd$日期)
old_sj$日期 <- as.Date(old_sj$日期)
old_cs$日期 <- as.Date(old_cs$日期)
old_rj$日期 <- as.Date(old_rj$日期)

shujing_all<-rbind(old_sj,new_sj)
chusheng_all<-rbind(old_cs,new_cs)
chandu_all<-rbind(old_cd,new_cd)
chandu_all[which(chandu_all$CE == 'NULL'),13] <- NA
renjian_all<-rbind(old_rj,new_rj)
chusheng_all<-chusheng_all[!duplicated(chusheng_all$FID),]
shujing_all$aa<-as.numeric(as.Date(shujing_all$日期))
shujing_all$aa<-paste(shujing_all$aa,shujing_all$FID,sep='')
shujing_all<-shujing_all[!duplicated(shujing_all$aa),]
shujing_all<-shujing_all[,-13]
chandu_all$aa<-paste(chandu_all$FID,chandu_all$胎次,sep='')

# delete duplicated records
chandu_all%<>%
  group_by(aa)%>%
  filter(日期 %in% max(日期))%>%
  ungroup()
chandu_all<-chandu_all[!duplicated(chandu_all$aa),]
chandu_all<-chandu_all[,-16]

write.csv(chandu_all,paste(update_dir,'cd.csv',sep = ''),row.names=F)
write.csv(shujing_all,paste(update_dir,'sj.csv',sep = ''),row.names=F)
write.csv(renjian_all,paste(update_dir,'rj.csv',sep = ''),row.names=F)
write.csv(chusheng_all,paste(update_dir,'cs.csv',sep = ''),row.names=F)

# Step 6. extract sex control semen of new data 
sj_sc$aa<-paste(sj_sc$FID,as.numeric(as.Date(sj_sc$配种日期)),sep='')
sj_sc<-sj_sc[!duplicated(sj_sc$aa),]
sj_sc<-sj_sc[,-14]
b<-sj_sc[,c(13,5,9,4)]
b[which(grepl('性控',b$冻精类型)),5]<-1
b[is.na(b$V5),5]<-2
b<-b[,-3]
names(b)[2]<-'sjdate'
b$cd <- paste(b$FID,b$sjdate,sep= '')
b<-b[!duplicated(b$cd),]
b <- b [-5]
c<-b
names(c)[4]<-'SC'

sc_all <- rbind(old_sc,c)
write.csv(sc_all,paste(update_dir,'sc.csv',sep = ''),row.names=F)

rm(list=ls())

# Input data files and rename(calve, insemination, birth, pregnant detection and pedigree)--------------------------------------------------------
# Please remember to change these directories before performing a new estimation. 
raw_dir <- 'D:\\chenzw\\数据库\\北京\\格式化数据（一直更新）\\2108\\'
out_dir <- 'D:\\chenzw\\遗传评估\\北京遗传评估\\202108\\'
chandu_all <- as.data.frame(fread(paste(raw_dir,'cd.csv',sep = '')))
shujing_all <- as.data.frame(fread(paste(raw_dir,'sj.csv',sep = '')))
chusheng_all <- as.data.frame(fread(paste(raw_dir,'cs.csv',sep = '')))
renjian_all <- as.data.frame(fread(paste(raw_dir,'rj.csv',sep = '')))
sc <- as.data.frame(fread(paste(raw_dir,'sc.csv',sep = '')))

MCORID<-read.csv('D:\\chenzw\\数据库\\北京\\系谱\\公牛对照表\\2021\\公牛对照表-lwq-202012.csv',stringsAsFactors= F ,encoding='GB18030')
MCORID$sire <- paste('M',MCORID$sire,sep='')
MCORID$corid <- paste('M',MCORID$corid,sep='')

ped_all <- as.data.frame(fread('D:\\chenzw\\数据库\\北京\\系谱\\202101\\202101_1st_version_lwq.csv'))
ped_all$id <- stringr::str_trim(ped_all$id,'both')
ped_parameter <- ped_all

herdnum <- read.csv('D:\\chenzw\\遗传评估\\farmcode.csv',stringsAsFactors= F)

CE_corres <- read.csv('D:\\chenzw\\数据库\\北京\\CE_corres.csv',stringsAsFactors = F)

st <- read.csv('D:\\chenzw\\遗传评估\\北京遗传评估\\202108\\stnum.csv')

# create directories -------------------------------------------------------
dir.create(paste(out_dir,'DMU',sep= ''))
dir.create(paste(out_dir,'育种值',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系\\筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\母系\\未筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系\\筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\产犊性状\\父系\\未筛选',sep = ''))

dir.create(paste(out_dir,'育种值\\其他性状',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\母牛',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛\\未筛选',sep = ''))
dir.create(paste(out_dir,'育种值\\其他性状\\公牛\\筛选',sep = ''))

dir.create(paste(out_dir,'遗传进展',sep = ''))
# DMU——ped ----------------------------------------------------------------
ped_dmu<-ped_all[,c(4,5,6,7)]
ped_dmu$Bir_date<-as.Date(ped_dmu$Bir_date,format = '%Y/%m/%d')
ped_dmu$Bir_date<-as.numeric(ped_dmu$Bir_date)
ped_dmu[is.na(ped_dmu$Bir_date),4]<-0
write.table(ped_dmu,paste(out_dir,'DMU\\pedigree.txt',sep = ''),row.names = F,col.names = F,quote=F)

# preparation of data -----------------------------------------------------
herdrenum <- herdnum[,3:4]
names(herdrenum)[1:2]<-c('herd','herdrenum') 

# recode herd 
chandu_all<-merge(herdrenum,chandu_all,by.x='herd',by.y='HERD',all.y=T)
chandu_all<-chandu_all[,-1]
shujing_all<-merge(herdrenum,shujing_all,by.x='herd',by.y='HERD',all.y=T)
shujing_all<-shujing_all[,-1]
renjian_all<-merge(herdrenum,renjian_all,by.x='herd',by.y='HERD',all.y=T)
renjian_all<-renjian_all[,-1]
chusheng_all<-merge(herdrenum,chusheng_all,by.x='herd',by.y='HERD',all.y=T)
chusheng_all<-chusheng_all[,-1]

# format
chandu_all[which(chandu_all$胎次 == '--'),6]<-0
chandu_all$胎次<-as.numeric(chandu_all$胎次)
chandu_all[which(chandu_all$胎次 >20),6]<-NA

chusheng_all[which(chusheng_all$胎次 == '--'),6]<-0
chusheng_all$胎次<-as.numeric(chusheng_all$胎次)
chusheng_all[which(chusheng_all$胎次 >20),6]<-NA

renjian_all[which(renjian_all$胎次 == '--'),6]<-0
renjian_all$胎次<-as.numeric(renjian_all$胎次) #  It's common that NA occurs
renjian_all[which(renjian_all$胎次 >20),6]<-NA

shujing_all[which(shujing_all$胎次 == '--'),6]<-0
shujing_all$胎次<-as.numeric(shujing_all$胎次) # It's common that NA occurs
shujing_all[which(shujing_all$胎次 >20),6]<-NA

shujing_all$日期<-as.Date(shujing_all$日期)
chandu_all$日期<-as.Date(chandu_all$日期)
renjian_all$日期<-as.Date(renjian_all$日期)
chusheng_all$日期<-as.Date(chusheng_all$日期)

shujing_all$重编号<-1:length(shujing_all$herdrenum)
chandu_all$重编号<-1:length(chandu_all$herdrenum)
renjian_all$重编号<-1:length(renjian_all$herdrenum)
chusheng_all$重编号<-1:length(chusheng_all$herdrenum)

# effects
shujing_all$YMI<-paste(year(shujing_all$日期),month(shujing_all$日期),sep='')
shujing_all$HYI<-paste(shujing_all$herdrenum,as.numeric(year(shujing_all$日期)),sep='')
chandu_all$YMC<-paste(year(chandu_all$日期),month(chandu_all$日期),sep='')
chandu_all$HYC<-paste(chandu_all$herdrenum,as.numeric(year(chandu_all$日期)),sep='')
chusheng_all$YMB<-paste(year(chusheng_all$日期),month(chusheng_all$日期),sep='')
chusheng_all$HYB<-paste(chusheng_all$herdrenum,as.numeric(year(chusheng_all$日期)),sep='')

names(shujing_all)[3]<-'sjdate'
names(chandu_all)[3]<-'cddate'
names(chusheng_all)[3]<-'csdate'

# filter the time of data 
shujing_all<-shujing_all[which(year(shujing_all$sjdate)>1999),]
chandu_all<-chandu_all[which(year(chandu_all$cddate)>1999),]
chusheng_all<-chusheng_all[which(year(chusheng_all$csdate)>1999),]

# delete duplicated data
chusheng_deleted<-chusheng_all[!duplicated(chusheng_all[,c('FID','csdate')]),]
chusheng_deleted%<>%
  group_by(FID)%>%
  filter(row_number()==1)%>%
  ungroup()

shujing_all <- shujing_all %<>%
  group_by(sjdate,FID,胎次)%>%
  filter(row_number() == 1) %>%
  ungroup()

# AI technician effect (needs to be updated regularly)------------------------------------------------------------
st <- read.csv(paste(out_dir,'stnum.csv',sep = ''))
# 1. Age at first service (AFS) ---------------------------------------------------------------------
# 270d <= AFS <= 900d
shujing_first<-shujing_all[which(shujing_all$胎次 == '0'),]
shujing_first%<>%
  group_by(FID)%>%
  filter(sjdate %in% min(sjdate))%>%
  ungroup()
chusheng_AFS<-chusheng_deleted[,c('FID','csdate')]
AFS_all<-merge(shujing_first,chusheng_AFS,by='FID',all.x=T)
AFS_all$AFS<-AFS_all$sjdate-AFS_all$csdate
AFS_controled<-AFS_all[which(AFS_all$AFS %in% seq(270,900)),]
AFS_controled%<>%
  group_by(FID)%>%
  filter(重编号 %in% min(重编号))%>%
  ungroup()
rm(AFS_all)

AFS_factor<-merge(AFS_controled,st,by='配种员',all.x=T)
AFS_factor[is.na(AFS_factor$ST),18]<-999999
AFS_factor<-merge(AFS_factor,ped_all,by.x='FID',by.y='id',all.x=T)
AFS_factor$Bir_date<-as.Date(AFS_factor$Bir_date)
AFS_factor$year<-lubridate::year(AFS_factor$Bir_date)
AFS_factor$mon<-lubridate::month(AFS_factor$Bir_date)
AFS_factor$HYB<-paste(AFS_factor$herdrenum,AFS_factor$year,sep='')
AFS_factor$YMB<-paste(AFS_factor$year,AFS_factor$mon,sep='')
AFS_factor[which(grepl('NA',AFS_factor$HYB)==T),27]<-999999
AFS_factor[which(grepl('NA',AFS_factor$YMB)==T),28]<-999999
AFS_factor<-AFS_factor[complete.cases(AFS_factor$code),]
AFSdata<-AFS_factor[,c(27,28,18,21,17)]
AFSdata$HYB<-as.numeric(AFSdata$HYB)
AFSdata$YMB<-as.numeric(AFSdata$YMB)
AFSdata$AFS<-as.numeric(AFSdata$AFS)
rm(AFS_controled,AFS_factor)

# 2. Age at first calving (AFC) ----------------------------------------------
# 500d <= AFC <= 1100d
chandu_first<-chandu_all[which(chandu_all$胎次 == '1'),]
chandu_first%<>%
  group_by(FID)%>%
  filter(cddate %in% min(cddate))%>%
  filter(重编号 %in% min(重编号))%>%
  ungroup()
chusheng_AFC<-chusheng_deleted[,c('FID','csdate')]
AFC_all<-merge(chandu_first,chusheng_AFS,by='FID',all.x=T)
AFC_all$AFC<-AFC_all$cddate-AFC_all$csdate
AFC_controled<-AFC_all[which(AFC_all$AFC %in% seq(500,1100)),]
rm(chusheng_AFS,chusheng_AFC,AFC_all)
# 3. Determine the pregnant date ----------------------------
renjian_all[grep('(-)',renjian_all$事件信息),14]<-0 # not pregnant
renjian_all[grep('(－)',renjian_all$事件信息),14]<-0 # not pregnant
renjian_all[grep('(±)',renjian_all$事件信息),14]<-2 # unsure
renjian_all[is.na(renjian_all$V14),14]<-1 #pregnant
renjian_deleted<-renjian_all[!is.na(renjian_all$胎次),c('FID','胎次','日期','V14')]
names(renjian_deleted)[3]<-'rjdate'
# First step: In each parity, the last service date (20-90 d before the last detect date of which the detect result was +) is pregnant date
# Second step: For those records without detect results, regard the last service date with calving record in corresponding parity as pregnant date
chandu_deleted<-chandu_all[complete.cases(chandu_all$胎次),-c(1,2,4,7,8,10,11)]
chandu_deleted%<>%
  group_by(FID,胎次)%>%
  filter(cddate %in% min(cddate))%>%
  ungroup()

# First step
renjian_preg <- renjian_deleted[renjian_deleted$V14 == 1,] %>%
  group_by(FID,胎次)%>%
  filter(rjdate %in% max(rjdate))%>%
  ungroup()
renjian_sj <- shujing_all[,c('FID','胎次','sjdate','重编号')]
renjian_sj <- merge(renjian_sj,renjian_preg,by = c('FID','胎次'),all = F) %>%
  group_by(FID,胎次) %>%
  filter(sjdate <= (rjdate - 20) & sjdate >= (rjdate - 90))%>%
  filter(sjdate %in% max(sjdate))%>%
  ungroup()

pr1 <- renjian_sj[,c('重编号','FID','胎次')]
rm(renjian_deleted,renjian_preg,renjian_sj)

# Second step
pr2 <- shujing_all[,c('重编号','FID','胎次','sjdate')]
pr2$胎次 <- pr2$胎次 +1
pr2 <- merge(pr2,chandu_deleted,by = c('FID','胎次'),all = F)
pr2$dif <- as.numeric(pr2$cddate-pr2$sjdate)
pr2 %<>%
  group_by(FID,胎次)%>%
  filter(sjdate %in% max(sjdate) & dif >0)%>%
  ungroup()
pr2 <- pr2[,1:3]
pr2$胎次 <- pr2$胎次 -1
pr <- merge(pr1,pr2,by = c('FID','胎次'),all = T)
pr[is.na(pr$重编号),3] <- pr[is.na(pr$重编号),4]
pr <- pr[-4]
pr <- pr[!duplicated(pr$重编号),]

peiren_all <- shujing_all[shujing_all$重编号 %in% pr$重编号,]
peiren_all$cowparity <- paste(peiren_all$FID,peiren_all$胎次+1,sep = '')
peiren_all <- peiren_all[,c(3,16)]
rm(pr1,pr2,pr)

# 4. Interval from first service to last service (IFL) -----------------------
# IFL = 0d OR 19d <= IFL <= 355d
shujing_deleted<-shujing_all[complete.cases(shujing_all$胎次),]
shujing_deleted$cowparity<-paste(shujing_deleted$FID,shujing_deleted$胎次+1,sep='')
shujing_allfirst<-shujing_deleted%>%
  group_by(cowparity)%>%
  filter(sjdate %in% min(sjdate))%>%
  ungroup()

IFL_all<-merge(shujing_allfirst,peiren_all,by='cowparity',all.x=T)
IFL_all$IFL<-as.numeric(IFL_all$sjdate.y-IFL_all$sjdate.x)
IFL_all<-IFL_all[complete.cases(IFL_all$IFL),]
IFL_controled<-IFL_all[which(IFL_all$IFL %in% seq(0,230)),-17]
rm(IFL_all)

# 5. Interval from calving to first service (ICF_C)---------------------------------
# 20d <= ICF <= 230d
chandu_deleted$cowparity<-paste(chandu_deleted$FID,chandu_deleted$胎次,sep='')

# delete duplicated data
shujing_allfirst%<>%
  group_by(cowparity)%>%
  filter(sjdate %in% min(sjdate))%>%
  ungroup()
shujing_allfirst_ICF<-shujing_allfirst[,c(6,12,11,3)]
ICF_all<-merge(chandu_deleted,shujing_allfirst_ICF,by=c('FID','胎次'),all.x=T)
ICF_all$ICF<-as.numeric(ICF_all$sjdate-ICF_all$cddate)
ICF_controled<-ICF_all%>%
  filter(ICF %in% seq(20,230))

# There are some filled records (probably filled by Afifarm) and we should delete them.
# For example, they might fill the first service record as 88 days after calving.
adjust<-ICF_controled[,c(1,2,3,9)]
adjust2<-adjust
adjust2$dt1<-adjust2$cddate+lubridate::years(1)+days(1)
adjust2$dt2<-adjust2$cddate+lubridate::years(1)-days(1)
adjust2$dt3<-adjust2$cddate+lubridate::years(1)
adjust2$胎次<-adjust2$胎次+1
hb<-merge(adjust2,adjust,by=c('FID','胎次'),all.x=T)
hb[which(hb$cddate.y==hb$dt1|hb$cddate.y==hb$dt2|hb$cddate.y==hb$dt3),10]<-1
hb<-hb[complete.cases(hb$V10),]
a<-data.frame(hb[,4])
b<-data.frame(hb[,9])
names(a)<-'numa'
names(b)<-'numb'
a$V2<-1
b$V2<-1
a<-a[!duplicated(a$numa),]
b<-b[!duplicated(b$numb),]

adjust3<-merge(ICF_controled,a,by.x='重编号',by.y='numa',all.x=T)
adjust3<-adjust3[-which(adjust3$V2==1),]
adjust3<-adjust3[,-16]
ICF_adjust<-merge(adjust3,b,by.x='重编号',by.y='numb',all.x=T)
ICF_adjust<-ICF_adjust[-which(ICF_adjust$V2==1),]
ICF_adjust<-ICF_adjust[,-16]
ICF_adjust %<>%
  group_by(cowparity,ICF) %>%
  filter(row_number() == 1)%>%
  ungroup()
rm(a,b,adjust,adjust2,adjust3,ICF_controled,ICF_all,hb)

# 6. Calving ease (CE)---------------------------------------------------------
# CE: normal = 1, slight = 2, hard = 3.
CE_all<-chandu_deleted
CE_all_1<-CE_all[is.na(CE_all$产犊方式),]
CE_all_2<-CE_all[complete.cases(CE_all$产犊方式),]
CE_all_2 <- merge(CE_all_2,CE_corres,by.x = '产犊方式',by.y = 'Var1',all.x = T)
CE_all_2 <- CE_all_2[,c(2:4,1,5:13)]
names(CE_all_2)[13]<-'CE1'
CE_all_1[,13]<-CE_all_1$CE
names(CE_all_1)[13]<-'CE1'
CE_all<-rbind(CE_all_1,CE_all_2)
CE_all<-CE_all[,-c(6,8)]
names(CE_all)[11]<-'CE'
CE_deleted<-CE_all[!duplicated(CE_all$cowparity),]
CE_deleted<-CE_deleted[complete.cases(CE_deleted$CE),]

rm(CE_all,CE_all_1,CE_all_2)

# extract MID
CE_deleted$tw <- str_count(CE_deleted$事件信息,';')
CE_deleted[is.na(CE_deleted$tw),'tw'] <- 0
CE_deleted[CE_deleted$tw>2 ,6] <- 3
CE_deleted <- CE_deleted[-12]

CE_deleted_1<-CE_deleted[is.na(CE_deleted$事件信息),]
CE_deleted_2<-CE_deleted[complete.cases(CE_deleted$事件信息),]
a<-strsplit(CE_deleted_2$事件信息,'; ;')
b<-as.data.frame(t(data.frame(a[1:length(a)])))
row.names(b)<-NULL
CE_factor<-cbind(CE_deleted_2,b)
CE_factor$V2<-NA
CE_factor[which(grepl('-正常',CE_factor$V1)|grepl('-出售',CE_factor$V1)==T),13]<-2
CE_factor[which(grepl('G',CE_factor$V1)==T),13]<-1
CE_factor[is.na(CE_factor$V2),13]<-999999
CE_factor[which(CE_factor$SEX == 3),13] <- 3
CE_factor<-CE_factor[,-12]
names(CE_factor)[12]<-'SEX'
rm(a,b)
CE_factor<-CE_factor[,-c(1,4,6,7,10)]
CE_deleted_1<-CE_deleted_1[,c(2,3,5,8,9,11,6)]
CE_factor<-rbind(CE_factor,CE_deleted_1)
CE_factor$num<-1:length(CE_factor$FID)
a1<-CE_factor[which(grepl('名字',CE_factor$事件信息)==T),]
a2<-CE_factor[which(grepl('名字',CE_factor$事件信息)==F),]
a<-strsplit(a1$事件信息,'名字')
b<-as.data.frame(matrix(unlist(a),byrow=TRUE,ncol=2))
b$V3<-substring(b$V2,2)
b<-data.frame(b[,3])
c<-cbind(a1,b)
names(c)[9]<-'MID'
a2$MID<-NA
cc<-rbind(a2,c)
cc<-cc[,c(8,9)]
cc$MID<-stringr::str_trim(cc$MID,'both')
CE_factor<-merge(CE_factor,cc,by='num',all.x=T)
CE_factor<-CE_factor[,-1]
CE_factor$MID<-paste('M',CE_factor$MID,sep='')
CE_factor<-merge(CE_factor,MCORID,by.x='MID',by.y='sire',all.x=T)
CE_factor[is.na(CE_factor$corid),9]<-CE_factor[is.na(CE_factor$corid),1]
CE_factor$cowp<-paste(CE_factor$FID,CE_factor$胎次,sep='')
CE_factor<-CE_factor[!duplicated(CE_factor$cowp),-10]
CE_factor<-CE_factor[,-c(1,2)]
CE_factor$mid1<-gsub('（性','',CE_factor$corid)
CE_factor$mid1<-gsub('ET','',CE_factor$mid1)
CE_factor$mid1<-gsub('hc','',CE_factor$mid1)
CE_factor[grepl('pj$',CE_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(CE_factor[grepl('pj$',CE_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                             nchar(CE_factor[grepl('pj$',CE_factor$mid1,ignore.case = T) == T,'mid1'])-2)
CE_factor[grepl('xk$',CE_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(CE_factor[grepl('xk$',CE_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                             nchar(CE_factor[grepl('xk$',CE_factor$mid1,ignore.case = T) == T,'mid1'])-2)
CE_factor[grepl('x$',CE_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(CE_factor[grepl('x$',CE_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                            nchar(CE_factor[grepl('x$',CE_factor$mid1,ignore.case = T) == T,'mid1'])-1)

CE_factor$mid1<-gsub('后测','',CE_factor$mid1)
CE_factor$mid1<-gsub('后','',CE_factor$mid1)
CE_factor$mid1<-gsub('性控','',CE_factor$mid1)
CE_factor$mid1<-gsub('性','',CE_factor$mid1)
CE_factor$mid1<-gsub('控','',CE_factor$mid1)
CE_factor$mid1<-gsub('种公','',CE_factor$mid1)
CE_factor$mid1<-gsub('种子','',CE_factor$mid1)
CE_factor$mid1<-gsub('胚胎移植','',CE_factor$mid1)
CE_factor$mid1<-gsub('胚胎','',CE_factor$mid1)
CE_factor[which(CE_factor$mid1=='M'),8]<-'M0'
CE_factor$mid1<-gsub('ho','HO',CE_factor$mid1)
CE_factor$mid1<-gsub('h0','HO',CE_factor$mid1)
CE_factor$mid1<-gsub('H0','HO',CE_factor$mid1)
CE_factor$mid1<-gsub('（','',CE_factor$mid1)
CE_factor$mid1<-gsub('[(]','',CE_factor$mid1)
CE_factor$mid1<-gsub('）','',CE_factor$mid1)
CE_factor$mid1<-gsub(')','',CE_factor$mid1)
CE_factor$mid1<-gsub('MCAN','MHOCANM',CE_factor$mid1)
CE_factor$mid1<-gsub('MCHN','MHOCHNM',CE_factor$mid1)
CE_factor$mid1<-gsub('MUSA','MHOUSAM',CE_factor$mid1)
CE_factor$mid1<-gsub('Musa','MHOUSAM',CE_factor$mid1)
CE_factor$mid1<-gsub('HC','',CE_factor$mid1)
CE_factor$mid1<-gsub('-H','',CE_factor$mid1)
CE_factor$mid1<-gsub('-4','',CE_factor$mid1)
CE_factor$mid1<-gsub('-11','',CE_factor$mid1)
CE_factor$mid1<-gsub('克隆','',CE_factor$mid1)
CE_factor$mid1<-gsub('-8','',CE_factor$mid1)
CE_factor$mid1<-gsub('-2','',CE_factor$mid1)
CE_factor$mid1<-gsub('xk','',CE_factor$mid1)
CE_factor[grepl('-$',CE_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(CE_factor[grepl('-$',CE_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                            nchar(CE_factor[grepl('-$',CE_factor$mid1,ignore.case = T) == T,'mid1'])-1)
CE_factor$mid1<-gsub('M-','M',CE_factor$mid1)
CE_factor$mid1<-gsub('M111-','M111',CE_factor$mid1)
CE_factor$mid1<-gsub('-EI','',CE_factor$mid1)
CE_factor$mid1<-gsub('-侧','',CE_factor$mid1)
CE_factor$mid1 <- stringr::str_trim(CE_factor$mid1,'both')

CE_factor<-CE_factor[,-7]
ped<-ped_all[,c(1,4)]
ped$id <- stringr::str_trim(ped$id,'both')
ped <- ped[!duplicated(ped$id),]
CE_male<-merge(CE_factor,ped,by.x='mid1',by.y='id',all.x=T)
CE_male[is.na(CE_male$code) == T & nchar(CE_male$mid1) == 9,'mid1'] <-  paste('MHOCHN',CE_male[is.na(CE_male$code)== T & nchar(CE_male$mid1) == 9,'mid1'],
                                                                              sep='')
CE_male<-merge(CE_male,ped,by.x='mid1',by.y='id',all.x=T)
CE_male <- CE_male[,-8]
names(CE_male)[8] <- 'code'
CE_male[is.na(CE_male$code),8]<-999999
CE_factor<-CE_male[,-1]
names(CE_factor)[7]<-'MID'
rm(a,b,a1,cc,c,a2,CE_deleted,CE_deleted_1,CE_deleted_2,CE_male)

# 7. Stillbirth (SB) ---------------------------------------------------------
SB_all<-chandu_deleted
# normal = 1; stillbirth = 2
myfun<-function(x){
  if (grepl('正常',x)==T){
    1
  } else {
    if (grepl('出售',x)==T){
      1
    } else {
      if (grepl('死',x)==T){
        2
      } else {
        NA
      }
    }
  }
}
SB_all_1<-SB_all[is.na(SB_all$事件信息),]
SB_all_2<-SB_all[complete.cases(SB_all$事件信息),]
a<-data.frame(apply(matrix(SB_all_2$事件信息),1,myfun))
SB_all_2<-cbind(SB_all_2,a)
names(SB_all_2)[13]<-'SB1'
SB_all_1$SB1<-SB_all_1$SB
SB_all<-rbind(SB_all_1,SB_all_2)
SB_all<-SB_all[,-c(6,8)]
names(SB_all)[11]<-'SB'
SB_deleted<-SB_all[complete.cases(SB_all$SB),]
SB_deleted$tw <- str_count(SB_deleted$事件信息,';')
SB_deleted[is.na(SB_deleted$tw),'tw'] <- 0
SB_deleted[SB_deleted$tw>2 ,6] <- 3
SB_deleted <- SB_deleted[-12]
rm(a,myfun,SB_all)

# Extract the ID of calves and determine their sex.
# IDs begin with 'G' are male calves. 
# male = 1; female = 2; more than 1 calves = 3.
SB_deleted_1<-SB_deleted[is.na(SB_deleted$事件信息),]
SB_deleted_2<-SB_deleted[complete.cases(SB_deleted$事件信息),]
a<-strsplit(SB_deleted_2$事件信息,'; ;')
b<-as.data.frame(t(data.frame(a[1:length(a)])))
row.names(b)<-NULL
SB_factor<-cbind(SB_deleted_2,b)
SB_factor$V2<-NA
SB_factor[which(grepl('-正常',SB_factor$V1)|grepl('-出售',SB_factor$V1)==T),13]<-2
SB_factor[which(grepl('G',SB_factor$V1)==T),13]<-1
SB_factor[is.na(SB_factor$V2),13]<-999999
SB_factor[which(SB_factor$SEX == 3),13] <- 3
SB_factor<-SB_factor[,-12]
names(SB_factor)[12]<-'SEX'
rm(a,b)

# Extract MID
SB_factor<-SB_factor[,-c(1,4,6,7,10)]
SB_deleted_1<-SB_deleted_1[,c(2,3,5,8,9,11,6)]
SB_factor<-rbind(SB_factor,SB_deleted_1)
SB_factor$num<-1:length(SB_factor$FID)
a1<-SB_factor[which(grepl('名字',SB_factor$事件信息)==T),]
a2<-SB_factor[which(grepl('名字',SB_factor$事件信息)==F),]
a<-strsplit(a1$事件信息,'名字')
b<-as.data.frame(matrix(unlist(a),byrow=TRUE,ncol=2))
b$V3<-substring(b$V2,2)
b<-data.frame(b[,3])
c<-cbind(a1,b)
names(c)[9]<-'MID'
a2$MID<-NA
cc<-rbind(a2,c)
cc<-cc[,c(8,9)]
cc$MID<-stringr::str_trim(cc$MID,'both')
SB_factor<-merge(SB_factor,cc,by='num',all.x=T)
SB_factor<-SB_factor[,-1]
SB_factor$MID<-paste('M',SB_factor$MID,sep='')
SB_factor<-merge(SB_factor,MCORID,by.x='MID',by.y='sire',all.x=T)
SB_factor[is.na(SB_factor$corid),9]<-SB_factor[is.na(SB_factor$corid),1]
SB_factor$cowp<-paste(SB_factor$FID,SB_factor$胎次,sep='')
SB_factor<-SB_factor[!duplicated(SB_factor$cowp),-10]
SB_factor<-SB_factor[,-c(1,2)]
SB_factor$mid1<-gsub('（性','',SB_factor$corid)
SB_factor$mid1<-gsub('ET','',SB_factor$mid1)
SB_factor$mid1<-gsub('hc','',SB_factor$mid1)
SB_factor[grepl('pj$',SB_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(SB_factor[grepl('pj$',SB_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                             nchar(SB_factor[grepl('pj$',SB_factor$mid1,ignore.case = T) == T,'mid1'])-2)
SB_factor[grepl('xk$',SB_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(SB_factor[grepl('xk$',SB_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                             nchar(SB_factor[grepl('xk$',SB_factor$mid1,ignore.case = T) == T,'mid1'])-2)
SB_factor[grepl('x$',SB_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(SB_factor[grepl('x$',SB_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                            nchar(SB_factor[grepl('x$',SB_factor$mid1,ignore.case = T) == T,'mid1'])-1)
SB_factor$mid1<-gsub('后测','',SB_factor$mid1)
SB_factor$mid1<-gsub('后','',SB_factor$mid1)
SB_factor$mid1<-gsub('性控','',SB_factor$mid1)
SB_factor$mid1<-gsub('性','',SB_factor$mid1)
SB_factor$mid1<-gsub('控','',SB_factor$mid1)
SB_factor$mid1<-gsub('种公','',SB_factor$mid1)
SB_factor$mid1<-gsub('种子','',SB_factor$mid1)
SB_factor$mid1<-gsub('胚胎移植','',SB_factor$mid1)
SB_factor$mid1<-gsub('胚胎','',SB_factor$mid1)
SB_factor[which(SB_factor$mid1=='M'),8]<-'M0'
SB_factor$mid1<-gsub('ho','HO',SB_factor$mid1)
SB_factor$mid1<-gsub('h0','HO',SB_factor$mid1)
SB_factor$mid1<-gsub('H0','HO',SB_factor$mid1)
SB_factor$mid1<-gsub('（','',SB_factor$mid1)
SB_factor$mid1<-gsub('[(]','',SB_factor$mid1)
SB_factor$mid1<-gsub('）','',SB_factor$mid1)
SB_factor$mid1<-gsub(')','',SB_factor$mid1)
SB_factor$mid1<-gsub('MCAN','MHOCANM',SB_factor$mid1)
SB_factor$mid1<-gsub('MCHN','MHOCHNM',SB_factor$mid1)
SB_factor$mid1<-gsub('MUSA','MHOUSAM',SB_factor$mid1)
SB_factor$mid1<-gsub('Musa','MHOUSAM',SB_factor$mid1)
SB_factor$mid1<-gsub('HC','',SB_factor$mid1)
SB_factor$mid1<-gsub('-H','',SB_factor$mid1)
SB_factor$mid1<-gsub('-4','',SB_factor$mid1)
SB_factor$mid1<-gsub('-11','',SB_factor$mid1)
SB_factor$mid1<-gsub('克隆','',SB_factor$mid1)
SB_factor$mid1<-gsub('-8','',SB_factor$mid1)
SB_factor$mid1<-gsub('-2','',SB_factor$mid1)
SB_factor$mid1<-gsub('xk','',SB_factor$mid1)
SB_factor[grepl('-$',SB_factor$mid1,ignore.case = T) == T,'mid1'] <- substr(SB_factor[grepl('-$',SB_factor$mid1,ignore.case = T) == T,'mid1'],1,
                                                                            nchar(SB_factor[grepl('-$',SB_factor$mid1,ignore.case = T) == T,'mid1'])-1)
SB_factor$mid1<-gsub('M-','M',SB_factor$mid1)
SB_factor$mid1<-gsub('M111-','M111',SB_factor$mid1)
SB_factor$mid1<-gsub('-EI','',SB_factor$mid1)
SB_factor$mid1<-gsub('-侧','',SB_factor$mid1)
SB_factor$mid1 <- stringr::str_trim(SB_factor$mid1,'both')

SB_factor<-SB_factor[,-7]
ped<-ped_all[,c(1,4)]
ped$id <- stringr::str_trim(ped$id,'both')
ped <- ped[!duplicated(ped$id),]
SB_male<-merge(SB_factor,ped,by.x='mid1',by.y='id',all.x=T)
SB_male[is.na(SB_male$code) == T & nchar(SB_male$mid1) == 9,'mid1'] <-  paste('MHOCHN',SB_male[is.na(SB_male$code)== T & nchar(SB_male$mid1) == 9,'mid1'],
                                                                              sep='')
SB_male<-merge(SB_male,ped,by.x='mid1',by.y='id',all.x=T)
SB_male <- SB_male[,-8]
names(SB_male)[8] <- 'code'
SB_male[is.na(SB_male$code),8]<-999999
SB_factor<-SB_male[,-1]
names(SB_factor)[7]<-'MID'
rm(a,b,a1,cc,c,a2,SB_deleted,SB_deleted_1,SB_deleted_2,SB_male)


# 8. Effects-------------------------------------------------------------------------
# Age at first service group effect (MF)
MF<-AFSdata[,c(4,5)]
MF[which(MF$AFS %in% seq(270,439)),3]<-1
MF[which(MF$AFS %in% seq(440,469)),3]<-2
MF[which(MF$AFS %in% seq(470,499)),3]<-3
MF[which(MF$AFS %in% seq(500,529)),3]<-4
MF[which(MF$AFS %in% seq(530,900)),3]<-5
MF<-MF[,-2]
names(MF)[2]<-'MF'

# AFC
a<-shujing_first[,c(12,3)]
a<-a[!duplicated(a$FID),]
c<-merge(a,sc,by=c('FID','sjdate'),all.x=T)
c<-c[complete.cases(c$SC),]
c <- c[,c(1,4)]

AFC_factor<-merge(AFC_controled,ped_all,by.x='FID',by.y='id',all.x=T)
AFC_factor<-AFC_factor[complete.cases(AFC_factor$code),]
AFC_factor[which(grepl('性',AFC_factor$事件信息)|grepl('x',AFC_factor$事件信息)==T),27]<-1
AFC_factor<-merge(AFC_factor,c,by='FID',all.x=T)
AFC_factor[!is.na(AFC_factor$SC),27]<-AFC_factor[!is.na(AFC_factor$SC),28]
AFC_factor<-AFC_factor[,-c(13:15,28)]
names(AFC_factor)[24]<-'SC'
AFC_factor[is.na(AFC_factor$SC),24]<-2
AFC_AFS<-merge(AFC_factor,AFSdata,by='code',all.x=T)
AFC_AFS$Bir_date<-as.Date(AFC_AFS$Bir_date)
AFC_AFS$year<-lubridate::year(AFC_AFS$Bir_date)
AFC_AFS$mon<-lubridate::month(AFC_AFS$Bir_date)
AFC_AFS$HYB<-paste(AFC_AFS$herdrenum,AFC_AFS$year,sep='')
AFC_AFS$YMB<-paste(AFC_AFS$year,AFC_AFS$mon,sep='')
AFC_AFS[which(grepl('NA',AFC_AFS$HYB)==T),25]<-999999
AFC_AFS[which(grepl('NA',AFC_AFS$YMB)==T),26]<-999999
AFC_AFS[is.na(AFC_AFS$ST),27]<-999999
AFCdata<-AFC_AFS[,c(25,26,27,24,1,18)]
AFCdata$AFC <- as.numeric(AFCdata$AFC)
rm(AFC_controled,AFC_factor,AFC_AFS)

# IFL_H = HYI + YMI + ST + SC + MF + A + E
# IFL_C = HYI + YMI + ST + SC + MF + P + PE + A + E
IFL_factor<-merge(IFL_controled,st,by='配种员',all.x=T)
IFL_factor[is.na(IFL_factor$ST),18]<-999999
IFL_factor[which(grepl('性',IFL_factor$事件信息)|grepl('x',IFL_factor$事件信息)==T),19]<-1
IFL_factor[is.na(IFL_factor$V19),19]<-2
names(IFL_factor)[19]<-'SC'
IFL_factor <- merge(IFL_factor,sc,by.x = c('FID','sjdate.x'),by.y = c('FID','sjdate'),all.x = T)
IFL_factor[is.na(IFL_factor$SC.y),21] <- IFL_factor[is.na(IFL_factor$SC.y),19]
IFL_factor <- IFL_factor[-c(19,20)]
names(IFL_factor)[19]<-'SC'
IFL_factor<-merge(IFL_factor,ped_all,by.x='FID',by.y = 'id',all.x=T)
IFL_factor<-IFL_factor[complete.cases(IFL_factor$code),]
IFL_factor<-IFL_factor[,c(16,15,18,19,9,22,17)]
IFL_factor<-merge(IFL_factor,MF,by='code',all.x=T)
IFL_factor[is.na(IFL_factor$MF),8]<-999999
IFL_Hdata<-IFL_factor[which(IFL_factor$胎次.x==0),c(2,3,4,5,8,1,7)]
IFL_Cdata<-IFL_factor[which(IFL_factor$胎次.x!=0),c(2,3,4,5,8,6,1,7)]
rm(IFL_factor,IFL_controled)

# ICF = HYC + YMC + ST + MF + P + PE + A + E
ICF_adjust<-ICF_adjust[-which(ICF_adjust$胎次==0),]
ICF_factor<-merge(ICF_adjust,st,by='配种员',all.x=T)
ICF_factor<-ICF_factor[,-c(8:10)]
ICF_factor[is.na(ICF_factor$ST),13]<-999999
ICF_factor<-merge(ICF_factor,ped_all,by.x = 'FID',by.y = 'id',all.x=T)
ICF_factor<-ICF_factor[complete.cases(ICF_factor$code),]
ICF_factor<-merge(ICF_factor,MF,by='code',all.x=T)
ICF_factor[is.na(ICF_factor$MF),20]<-999999
ICFdata<-ICF_factor[,c(9,10,14,20,5,1,13)]
rm(ICF_adjust,ICF_factor)

#CE_H = HYC + YMC + MF + Sex + FA + MA + E
#CE_C = HYC + YMC + MF + Sex + P + FA + MA + FPE + MPE + E
CE_factor<-merge(CE_factor,ped_all,by.x='FID',by.y='id',all.x=T)
CE_factor<-CE_factor[complete.cases(CE_factor$code),]
CE_factor<-merge(CE_factor,MF,by='code',all.x=T)
CE_factor[is.na(CE_factor$MF),14]<-999999
CE_factor<-CE_factor[-which(CE_factor$胎次==0),]
CE_Hdata<-CE_factor[which(CE_factor$胎次==1),c(4,5,14,7,1,8,6)]
CE_Cdata<-CE_factor[which(CE_factor$胎次!=1),c(4,5,14,7,3,1,8,6)]
rm(CE_factor)

#SB_H = HYC + YMC + MF + Sex + FA + MA + E
#SB_C = HYC + YMC + MF + Sex + P + FA + MA + FPE + MPE + E
SB_factor<-merge(SB_factor,ped_all,by.x='FID',by.y='id',all.x=T)
SB_factor<-SB_factor[complete.cases(SB_factor$code),]
SB_factor<-merge(SB_factor,MF,by='code',all.x=T)
SB_factor[is.na(SB_factor$MF),14]<-999999
SB_factor<-SB_factor[-which(SB_factor$胎次==0),]
SB_Hdata<-SB_factor[which(SB_factor$胎次==1),c(4,5,14,7,1,8,6)]
SB_Cdata<-SB_factor[which(SB_factor$胎次!=1),c(4,5,14,7,3,1,8,6)]
rm(SB_factor)

# transfer formats
AFCdata%<>%dplyr::mutate_at(c('HYB','YMB','AFC'),~as.numeric(.x))
ICFdata%<>%dplyr::mutate_at(c('HYC','YMC','ICF'),~as.numeric(.x))
CE_Cdata%<>%dplyr::mutate_at(c('HYC','YMC','CE','SEX'),~as.numeric(.x))
CE_Hdata%<>%dplyr::mutate_at(c('HYC','YMC','CE','SEX'),~as.numeric(.x))
IFL_Cdata%<>%dplyr::mutate_at(c('HYI','YMI','IFL'),~as.numeric(.x))
IFL_Hdata%<>%dplyr::mutate_at(c('HYI','YMI','IFL'),~as.numeric(.x))
SB_Cdata%<>%dplyr::mutate_at(c('HYC','YMC','SB','SEX'),~as.numeric(.x))
SB_Hdata%<>%dplyr::mutate_at(c('HYC','YMC','SB','SEX'),~as.numeric(.x))


# 9. Save data-------------------------------------------------------------------------

tnames<-c('AFC','AFS','CE_C','CE_H','SB_C','SB_H','ICF','IFL_C','IFL_H')
for (i in tnames) {
  a <- get(paste(i,'data',sep=''))
  a[is.na(a)] <- 999999
  write.table(a,paste(out_dir,'DMU\\',i,'data.txt',sep=''),col.names = F,row.names = F)
}

# Run DMUAI -----------------------------------------------------------------
# !ATTENTION! Please put the dir files and pedigree for DMU into the same directory as output files before running!
setwd(paste(out_dir,'DMU',sep = ''))
for (i in tnames){
  system(paste('run_dmuai ',i,sep = ''))
}
# -----------------------calculate genetic parameters-------------------------
# other traits --------------------------------------------------
# parameters
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
write.csv(total,paste(out_dir,'\\other parameters.csv',sep = ''),row.names = F)

# unfiltered EBVs
d<-data.frame(tnames)
d$A<-NA
for (i in tnames){
  assign(i,read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA))
  a<-get(i)
  d[which(d$tnames==i),2]<-a[2,2]
}
ped_all<-ped_parameter
ped_all$Bir_date<-as.Date(ped_all$Bir_date,format='%Y/%m/%d')
ped_all<-ped_all[complete.cases(ped_all$Bir_date),]
ped_all[which(substr(ped_all$id,1,1)=='M'),8]<-1
ped_all[is.na(ped_all$V8),8]<-2
names(ped_all)[8]<-'aa'
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
  c<-c[complete.cases(c$Bir_date),]
  c<-c[,-12]
  assign(paste(i,'_EBV_ped_m',sep=''),c)
  c<-b[which(b$aa==2),]
  c<-c[complete.cases(c$Bir_date),]
  c<-c[,-11]
  assign(paste(i,'_EBV_ped_f',sep=''),c)
  a<-merge(a,ped_all,by.x='V5',by.y='code',all.x=T)
  a<-a[complete.cases(a$Bir_date),-c(2:7,12:14,16,17,18)]
  assign(paste(i,'_EBV',sep=''),a)
  rm(a,b,c)
}

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

ped_f<-ped_all[,c(4,7)]
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_f',sep=''))
  a<-a[,c(1,4:8)]
  a<-merge(a,ped_f,by.x = 'V5',by.y='code',all.x=T)
  a<-a[,c(1,6,2:5,7)]
  colnames(a)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','生日')
  write.csv(a,paste(out_dir,'育种值\\其他性状\\母牛\\',i,'.csv',sep=''),row.names=F)
}

for (i in tnames){
  a<-get(paste(i,'_EBV_ped_m',sep=''))
  a<-a[,c(1,8,4:7,11)]
  colnames(a)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','生日')
  write.csv(a,paste(out_dir,'育种值\\其他性状\\公牛\\未筛选\\',i,'.csv',sep=''),row.names=F)
}

# EBVs (born after 2009 and have more than five daughters)
for (i in tnames){
  a<-get(paste(i,'_EBV_ped_m',sep=''))
  b<-a[which(lubridate::year(a$Bir_date)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  assign(paste(i,'_EBV_ped_m',sep=''),c)
  rm(a,c)
}
pedfreq<-data.frame(table(ped_all$Sire_code))
pedfreq<-pedfreq[-1,]

for (i in tnames){
  if (i!='ICF'){
    f<-read.table(paste(out_dir,'DMU\\',i,'data.txt',sep=''))
    f$herd<-as.numeric(substr(f$V1,1,nchar(f$V1)-4))
    a<-merge(f,ped_all,by.x=paste('V',ncol(f)-2,sep=''),by.y='code',all.x=T)
    a<-a[!duplicated(a[,1]),]
    b<-data.frame(table(a$Sire_code))
    b<-b[-1,]
    h<-a[,c(ncol(a)-3,ncol(f))]
    h<-h[!duplicated(h),]
    h2<-data.frame(table(h$Sire_code))
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
    f<-read.table(paste(out_dir,'DMU\\',i,'data.txt',sep=''))
    f$herd<-as.numeric(substr(f$V2,1,nchar(f$V1)-4))
    a<-merge(f,ped_all,by.x=paste('V',ncol(f)-2,sep=''),by.y='code',all.x=T)
    a<-a[!duplicated(a[,1]),]
    b<-data.frame(table(a$Sire_code))
    b<-b[-1,]
    h<-a[,c(ncol(a)-3,ncol(f))]
    h<-h[!duplicated(h),]
    h2<-data.frame(table(h$Sire_code))
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

# calving traits ----------------------------------------------------------
# parameters
name<-c('CE_H','SB_H','CE_C','SB_C')
a<-as.data.frame(matrix(NA,ncol = 9,nrow = 4))  
a[,1]<-name
names(a)<-c('trait','fh2','fh2se','fre','frese','mh2','mh2se','mre','mrese')
for (i in name){
  df<-read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA)
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
write.csv(a,paste(out_dir,'\\calving parameters.csv',sep = ''),row.names=F)

# extract EBV (unfiltered)
a<-data.frame(name)
a$af<-NA
a$am<-NA

for (i in name){
  df<-read.table(paste(out_dir,'DMU\\',i,'.PAROUT_STD',sep=''),col.names = c('V1','V2','V3','V4'),fill=NA)
  all<-read.table(paste(out_dir,'DMU\\',i,'.SOL',sep=''))
  a[which(a$name==i),2]<-df[2,2]
  a[which(a$name==i),3]<-df[4,2]
  # dam 
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
  # service sire
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
for (i in name){
  a<-get(paste('mx_',i,'_male',sep=''))
  b<-a[which(lubridate::year(a$Bir_date)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  assign(paste('mx_',i,'_male',sep=''),c)
  rm(a,c)
}
pedfreq<-data.frame(table(ped_all$Sire_code))
pedfreq<-pedfreq[-1,]

for (i in name){
  f<-read.table(paste(out_dir,'DMU\\',i,'data.txt',sep=''))
  f$herd<-as.numeric(substr(f$V1,1,nchar(f$V1)-4))
  a<-merge(f,ped_all,by.x=paste('V',ncol(f)-3,sep=''),by.y='code',all.x=T)
  a<-a[!duplicated(a[,1]),]
  b<-data.frame(table(a$Sire_code))
  h<-a[,c(ncol(a)-3,ncol(f))]
  h<-h[!duplicated(h),]
  h2<-data.frame(table(h$Sire_code))
  c<-get(paste('mx_',i,'_male',sep=''))
  e<-merge(b,c,by.x='Var1',by.y='code',all.x=T)
  e<-merge(e,pedfreq,by='Var1',all.x=T)
  e<-e[complete.cases(e$EBV),]
  rm(b,c)
  e<-merge(e,h2,by='Var1',all.x=T)
  e<-e[,c(1,7,9,10,2,11,3,4,5,6)]
  names(e)<-c('系谱编号','公牛号','出生日期','总女儿数','有表型的女儿数','群体数','估计育种值EBV','标准误','可靠性','准确性')
  e$出生年份<-lubridate::year(e$出生日期)
  write.csv(e,paste(out_dir,'育种值\\产犊性状\\母系\\筛选\\',i,'.csv',sep=''),row.names = F)
  rm(f,e,a)
} 

for (i in name){
  a<-get(paste('fx_',i,'_male',sep=''))
  b<-a[which(lubridate::year(a$Bir_date)>2009),]
  c<-b
  c<-c[which(c$REL>mean(b$REL)),-c(2,3,10)]
  names(c)[1:3]<-c('code','EBV','SE')
  c<-c[,c(1,6,2:5,8)]
  colnames(c)<-c('系谱编号','ID','育种值','育种值标准误','可靠性','准确性','出生日期')
  write.csv(c,paste(out_dir,'育种值\\产犊性状\\父系\\筛选\\',i,'.csv',sep=''),row.names = F)
  rm(a,c)
}

# other results -----------------------------------------------------------
# 【statistical summary of phenotypes】
tnames<-c('AFC','AFS','CE_C','CE_H','SB_C','SB_H','ICF','IFL_C','IFL_H')
d<-data.frame(tnames)
d$A<-NA
d$num<-NA
d$per <- NA
for (i in 1:9){
  f<-read.table(paste(out_dir,'DMU\\',tnames[i],'data.txt',sep=''))
  d[i,2]<-paste(round(mean(f[,ncol(f)]),4),'±',round(sd(f[,ncol(f)]),4))
  d[i,3]<-nrow(f)
  if (i %in% 3:6){
     d[i,4] <- paste(round(100*(nrow(f[which(f[,ncol(f)]==2),])+nrow(f[which(f[,ncol(f)]==3),]))/(nrow(f[which(f[,ncol(f)]==1),])+nrow(f[which(f[,ncol(f)]==2),])+nrow(f[which(f[,ncol(f)]==3),])),3),'%')
  }
}
names(d) <- c('性状','均值±标准差','数据量','百分数')
write.csv(d,paste(out_dir,'描述性统计.csv',sep = ''),row.names = F)

# 【range of EBVs and reliabilities】 
trait <- c('AFS','AFC','ICF','IFL_C','IFL_H')
t <- data.frame(trait)
t$trait <- as.character(t$trait)
t$mrange <- NA
t$mrel <- NA
t$frange <- NA
t$frel <- NA
for (i in 1:5){
  m <- read.csv(paste(out_dir,'育种值\\其他性状\\公牛\\未筛选\\',trait[i],'.csv',sep=''),stringsAsFactors = F)
  m <- m[m$可靠性 != 1&m$可靠性 != 0,]
  t[i,2] <- paste(round(min(m$育种值),2),'~',round(max(m$育种值),2),sep='')
  t[i,3] <- round(mean(m$可靠性),4)
  f <- read.csv(paste(out_dir,'育种值\\其他性状\\母牛\\',trait[i],'.csv',sep=''),stringsAsFactors = F)
  f <- f[f$可靠性 != 1&f$可靠性 != 0,]
  t[i,4] <- paste(round(min(f$育种值),2),'~',round(max(f$育种值),2),sep='')
  t[i,5] <- round(mean(f$可靠性),4)
}
write.csv(t,paste(out_dir,'其他性状-总结部分.csv',sep = ''),row.names = F)

trait <- c('SB_C','SB_H','CE_C','CE_H')
t <- data.frame(trait)
t$trait <- as.character(t$trait)
t$mrange_dam <- NA
t$mrel_dam <- NA
t$frange_dam <- NA
t$frel_dam <- NA
t$mrange_sire <- NA
t$mrel_sire <- NA
t$frange_sire <- NA
t$frel_sire <- NA
for (i in 1:4){
  m_dam <- read.csv(paste(out_dir,'育种值\\产犊性状\\母系\\未筛选\\',trait[i],'_公牛育种值','.csv',sep=''),stringsAsFactors = F)
  t[i,2] <- paste(round(min(m_dam$育种值),3),'~',round(max(m_dam$育种值),3),sep='')
  t[i,3] <- round(mean(m_dam$育种值可靠性),4)
  f_dam <- read.csv(paste(out_dir,'育种值\\产犊性状\\母系\\未筛选\\',trait[i],'_母牛育种值','.csv',sep=''),stringsAsFactors = F)
  t[i,4] <- paste(round(min(f_dam$育种值),3),'~',round(max(f_dam$育种值),3),sep='')
  t[i,5] <- round(mean(f_dam$育种值可靠性),4)
  m_sire <- read.csv(paste(out_dir,'育种值\\产犊性状\\父系\\未筛选\\',trait[i],'_公牛育种值','.csv',sep=''),stringsAsFactors = F)
  t[i,6] <- paste(round(min(m_sire$育种值),3),'~',round(max(m_sire$育种值),3),sep='')
  t[i,7] <- round(mean(m_sire$育种值可靠性),4)
  f_sire <- read.csv(paste(out_dir,'育种值\\产犊性状\\父系\\未筛选\\',trait[i],'_母牛育种值','.csv',sep=''),stringsAsFactors = F)
  t[i,8] <- paste(round(min(f_sire$育种值),3),'~',round(max(f_sire$育种值),3),sep='')
  t[i,9] <- round(mean(f_sire$育种值可靠性),4)
}
write.csv(t,paste(out_dir,'产犊性状-总结部分.csv',sep = ''),row.names = F)

# 【plots of genetic gain】
windowsFonts(TNM = windowsFont("Times New Roman")) 
mytheme <- theme_bw()+
  theme(legend.position = 'top', 
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.key =element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(size = 18,family ='TNM',colour = 'black'),
        axis.text = element_text(size = 14,family ='TNM',colour = 'black'),
        plot.title = element_text(size = 18),
        axis.line = element_line(color = 'black',size = 1),
        axis.ticks = element_line(color = 'black',size = 1)
  )
ggplot2.two_y_axis <- function(g1, g2){
  g1 <- ggplotGrob(g1)
  g2 <- ggplotGrob(g2)}

trait <- c('AFS','AFC','ICF','IFL_C','IFL_H')
for (i in 1:5){
  m <- read.csv(paste(out_dir,'育种值\\其他性状\\公牛\\未筛选\\',trait[i],'.csv',sep=''),stringsAsFactors = F)
  m <- m[m$可靠性 != 1&m$可靠性 != 0,]
  m$year <- lubridate::year(as.Date(m$生日))
  m <- m[m$year >= 2000,]
  m <- m[rank(m$可靠性) >= 0.5*nrow(m),]
  m$year <- as.factor(m$year)
  df <- data.frame(table(m$year))
  df[,1] <- as.numeric(as.character(df[,1]))
  df$mean <- NA
  for (x in 1:nrow(df)){
    df[x,3] <- mean(m[m$year == df[x,1],3],na.rm = T)
  }
  p1 <- ggplot(df,aes(x = as.factor(Var1),y = mean,group = 1))+geom_line(size =1)+mytheme+geom_point(size=3)+
    labs(x = '出生年份',y = 'EBV均值',title = paste(trait[i],'遗传进展',sep=''))
  ggsave(paste(out_dir,'遗传进展\\',trait[i],'遗传进展','.jpeg',sep=''),p1,width = 10,height = 5)
}

trait <- c('SB_C','SB_H','CE_C','CE_H')
for (i in 1:4){
  m <- read.csv(paste(out_dir,'育种值\\产犊性状\\母系\\未筛选\\',trait[i],'_公牛育种值','.csv',sep=''),stringsAsFactors = F)
  m <- m[m$育种值可靠性 != 1&m$育种值可靠性 != 0,]
  m$year <- lubridate::year(as.Date(m$出生日期,format = '%Y-%m-%d'))
  m <- m[m$year >= 2000,]
  m <- m[rank(m$育种值可靠性) >= 0.5*nrow(m),]
  m$year <- as.factor(m$year)
  df <- data.frame(table(m$year))
  df[,1] <- as.numeric(as.character(df[,1]))
  df$mean <- NA
  for (x in 1:nrow(df)){
    df[x,3] <- mean(m[m$year == df[x,1],3],na.rm = T)
  }
  p1 <- ggplot(df,aes(x = as.factor(Var1),y = mean,group = 1))+geom_line(size =1)+mytheme+geom_point(size=3)+
    labs(x = '出生年份',y = 'EBV均值',title = paste(trait[i],'遗传进展_母系',sep=''))
  ggsave(paste(out_dir,'遗传进展\\',trait[i],'遗传进展_母系','.jpeg',sep=''),p1,width = 10,height = 5)
}
for (i in 1:4){
  m <- read.csv(paste(out_dir,'育种值\\产犊性状\\父系\\未筛选\\',trait[i],'_公牛育种值','.csv',sep=''),stringsAsFactors = F)
  m <- m[m$育种值可靠性 != 1&m$育种值可靠性 != 0,]
  m$year <- lubridate::year(as.Date(m$出生日期,format = '%Y-%m-%d'))
  m <- m[m$year >= 2000,]
  m <- m[rank(m$育种值可靠性) >= 0.5*nrow(m),]
  m$year <- as.factor(m$year)
  df <- data.frame(table(m$year))
  df[,1] <- as.numeric(as.character(df[,1]))
  df$mean <- NA
  for (x in 1:nrow(df)){
    df[x,3] <- mean(m[m$year == df[x,1],3],na.rm = T)
  }
  p1 <- ggplot(df,aes(x = as.factor(Var1),y = mean,group = 1))+geom_line(size =1)+mytheme+geom_point(size=3)+
    labs(x = '出生年份',y = 'EBV均值',title = paste(trait[i],'遗传进展_父系',sep=''))
  ggsave(paste(out_dir,'遗传进展\\',trait[i],'遗传进展_父系','.jpeg',sep=''),p1,width = 10,height = 5)
}
