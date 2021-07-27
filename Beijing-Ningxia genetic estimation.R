# >>>>>>>>>>>>>>Genetic estimation for population from Beijing<<<<<<<<<<<<<<<<<<<<<
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!READ ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!You must change the input and output directories!!!!!!!!
# !!!!Save your raw data in a safe space or have another copy!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------data formatting-----------------------------
# Models: AFS = HYB + YMB + ST + A + E
#         AFC = HYB + YMB + ST + SC + A + E
#         ICF = HYC + YMC + ST + MF + P + PE + A + E
#         IFL_H = HYI + YMI + ST + SC + MF + A + E
#         IFL_C = HYI + YMI + ST + SC + MF + P + PE + A + E
#         CE_H = HYC + YMC + MF + Sex + FA + MA + E
#         CE_C = HYC + YMC + MF + Sex + P + FA + MA + FPE + MPE + E
#         SB_H = HYC + YMC + MF + Sex + FA + MA + E
#         SB_C = HYC + YMC + MF + Sex + P + FA + MA + FPE + MPE + E
# Load packages -----------------------------------------------------------
library(data.table)

# Read data --------------------------------------------------------
# Please remember to change these directories before performing a new estimation. 
nx_dir <- 'D:\\chenzw\\遗传评估\\宁夏遗传评估\\202108\\'
bj_dir <- 'D:\\chenzw\\遗传评估\\北京遗传评估\\202108\\'
out_dir <- 'D:\\chenzw\\遗传评估\\北京宁夏联合\\202108\\'

for (i in c('AFC','AFS','CE_C','CE_H','SB_C','SB_H','ICF','IFL_C','IFL_H')){
  assign(paste(i,'_nx',sep = ''),read.table(paste(nx_dir,'DMU\\',i,'.txt',sep = ''),header = F))
  assign(paste(i,'_bj',sep = ''),read.table(paste(bj_dir,'DMU\\',i,'data.txt',sep = ''),header = F))
}

ped_nx <- as.data.frame(data.table::fread('D:\\chenzw\\数据库\\宁夏\\系谱\\202107\\pedigree_v2_format.csv'))
ped_bj <- as.data.frame(fread('D:\\chenzw\\数据库\\北京\\系谱\\202101\\202101_1st_version_lwq.csv'))
ped_all <- as.data.frame(fread('D:\\chenzw\\数据库\\北京-宁夏\\系谱\\202101\\北京宁夏合并系谱20210215.csv'))


# Create directories ------------------------------------------------------
dir.create(paste(out_dir,'DMU',sep = ''))
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
# Merge formatted data from Ningxia and Beijing  --------------------------
ped_nx <- ped_nx[,c('id','code')]
ped_nx$loc <- regexpr('F',ped_nx$id)
ped_nx[ped_nx$loc == -1,3] <- 1
ped_nx$FID <- substr(ped_nx$id,ped_nx$loc,nchar(ped_nx$id))
ped_nx[ped_nx$loc > 7,'FID'] <- ped_nx[ped_nx$loc > 7,'id']
ped_nx <- ped_nx[,c('FID','code')]
names(ped_nx)[1] <- 'id' 

ped_bj <- ped_bj[,c('id','code')]
ped_all <- ped_all[,c('FID','CODE')]
ped_all <- ped_all[complete.cases(ped_all$CODE),]

for (i in c('AFC','AFS','ICF','IFL_C','IFL_H')){
  nx <- get(paste(i,'_nx',sep = ''))
  bj <- get(paste(i,'_bj',sep = ''))
  names(nx)[ncol(nx)-1] <- 'code'
  names(bj)[ncol(bj)-1] <- 'code'
  nx <- merge(nx,ped_nx,by = 'code',all = F)
  bj <- merge(bj,ped_bj,by = 'code',all = F)
  all <- rbind(nx,bj)
  all <- merge(all,ped_all,by.x = 'id',by.y = 'FID',all = F)
  all <- all[,c(3:(ncol(all)-2),ncol(all),(ncol(all)-1))]
  write.table(all,paste(out_dir,'DMU\\',i,'data.txt',sep = ''),row.names= F,col.names = F,quote = F)
}

for (i in c('CE_H','SB_H','CE_C','SB_C')){
  nx <- get(paste(i,'_nx',sep = ''))
  bj <- get(paste(i,'_bj',sep = ''))
  names(nx)[(ncol(nx)-2):(ncol(nx)-1)] <- c('fcode','mcode')
  names(bj)[(ncol(bj)-2):(ncol(bj)-1)] <- c('fcode','mcode')
  nx <- merge(nx,ped_nx,by.x = 'fcode',by.y = 'code',all = F)
  bj <- merge(bj,ped_bj,by.x = 'fcode',by.y = 'code',all = F)
  nx <- merge(nx,ped_nx,by.x = 'mcode',by.y = 'code',all.x = T)
  bj <- merge(bj,ped_bj,by.x = 'mcode',by.y = 'code',all.x = T)
  all <- rbind(nx,bj)
  all <- merge(all,ped_all,by.x = 'id.x',by.y = 'FID',all = F)
  all <- merge(all,ped_all,by.x = 'id.y',by.y = 'FID',all.x = T)
  all[is.na(all)] <- 999999
  all <- all[c(5:(ncol(all)-3),(ncol(all)-1):ncol(all),(ncol(all)-2))]
  write.table(all,paste(out_dir,'DMU\\',i,'data.txt',sep = ''),row.names= F,col.names = F,quote = F)
}


# Run DMUAI ---------------------------------------------------------------
# !ATTENTION! Please put the dir files and pedigree for DMU into the same directory as output files before running!
tnames <- c('AFC','AFS','CE_C','CE_H','SB_C','SB_H','ICF','IFL_C','IFL_H')
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
