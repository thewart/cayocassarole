source('~/Dropbox/monkeybris/rscript/ohbehave.R')
source("~/Dropbox/monkeybris/rscript/getRank.R")
source("~/code/cayocassarole/personality2014.R")
source('~/Dropbox/monkeybris/rscript/getAge.R')
dp <- "~/Dropbox/monkeybris/focaldata/"
d <- paste0(dp,c("Group_F_Focal data_MASTERCOPY_2010.csv",
                 "Group_F_Focal data_MASTERCOPY_2011.csv",
                 "Group_S_FOCAL DATA 2011.csv"))
bdyear <- c("2010","2011","2011")

census <- as.data.table(read.xlsx2("~/Dropbox/Cayo Census/2011.11.Nov_Animales in CS ALL.xls",1,colIndex = c(1,6)))
femID <- census[SEX=="f",animal_id2]

safez <- function(x)
{
  if (sd(x)==0)
    return(rep(0,length(x)))
  
  return((x-mean(x))/sd(x))
}

out <- list()
for (i in 1:length(d))
{
  bd <- as.data.table(behave.preproc(read.csv(d[i])))
  bd <- bd[(FocalID %in% femID) &
             (PartnerID %in% c("NOANIMAL","",femID) | grepl("female",PartnerID,ignore.case = T))]
  
  yrank <- subset(rank,year==bdyear[i])
  tmp <- bd[,list(passive=passcont(.SD)/600, alone=foreveralone(.SD),
           app_high=approach(FocalID,.SD,rank)[1], app_low=approach(FocalID,.SD,rank)[2],
           vocal_high=vocal(FocalID,.SD,yrank)[1], vocal_low=vocal(FocalID,.SD,yrank)[2],
           agg=agg(.SD,"old"), submit=submit(.SD), selfdir=selfie(Behavior),
           obslength=(max(StartTime)-min(StartTime))/600),
     by=.(FocalID,Observation)]
  
  tmp <- merge(
    tmp[,lapply(.SD,function(x,y) sum(x)/sum(y),obslength),FocalID,.SDcols=c(3,5:12)],
    tmp[,.(alone=mean(alone)),FocalID],by = "FocalID")[,obslength := NULL]
  tmp <- tmp[,lapply(.SD,sqrt),FocalID]
  tmp[,2:ncol(tmp) :=lapply(.SD,safez),.SDcols=2:ncol(tmp)]
  out[[i]] <- merge.data.frame(tmp,yrank,by.x="FocalID",by.y="ID")
}
out <- as.data.table(plyr::ldply(out))
out[,age := getage(FocalID,year)]
XY <- out[,lapply(.SD,mean),FocalID,.SDcols=which(sapply(out,is.numeric))]



source("~/Dropbox/monkeybris/rscript/pedigree.preproc.batch.R")
reped <- ped.matchnames(as.character(unique(X$FocalID)),pedigree$id)
if (!is.na(reped))
{
  pedigree <- ped.replace(pedigree,reped$oldID,reped$ID)
  if (any(is.na(reped$pedind)))
    XY <- subset(XY,!(ID %in% subset(reped,is.na(pedind))$ID))
}

source("~/Dropbox/tools/cheetahmm/cheetahmm.R")
A <- cheetah.spkin(pedigree)
A <- as.matrix(A[colnames(A) %in% X$FocalID,colnames(A) %in% X$FocalID])
A <- A[match(X$FocalID,rownames(A)),match(X$FocalID,rownames(A))]

X <- XY[,poly(cbind(nout,age),degree=2,simple=T)]
Y <- XY[,2:10,with=F]

write.table(X,file="~/analysis/cayocassarole/X.csv",sep=",",row.names = F,col.names=F)
write.table(A,file="~/analysis/cayocassarole/A.csv",sep=",",row.names = F,col.names=F)
write.table(Y,file="~/analysis/cayocassarole/Y.csv",sep=",",row.names = F,col.names=F)
write.table(diag(nrow(XY)),file="~/analysis/cayocassarole/Z.csv",sep=",",row.names = F,col.names=F)
