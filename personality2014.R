passcont <- function(bdi)
{
  pass <- subset(bdi,Behavior=="passcont")
  npass <- nrow(pass)
  if (npass==0) {
    passtime <- 0
  } else if (npass==1) {
    passtime <- pass$Duration
  } else {
    contsec <- vector()
    setnames(pass,tolower(names(pass)))
    for (j in 1:npass) contsec <- union(contsec,pass$starttime[j]:pass$stoptime[j])
    passtime <- as.double(length(contsec))
  }
  return(passtime)
}


approach <- function(ID,bdi,rank)
{
  app <- subset(bdi,Behavior=="Approach")
  app <- subset(app,behave.screen(list(ALL=c("give","given","focal")),app))
  
  if (nrow(app)==0 | !(ID%in%rank$ID))
    return(c(0,0))

  higher <- lower <- 0
  rank_foc <- rank$rank[match(ID,rank$ID)]
  for (i in 1:nrow(app))
  {
    partnr <- app$PartnerID[i]
    t <- app$StartTime[i]
    bad <- with(bdi,any(Behavior %in% c("Agg","contactAgg","noncontactAgg","threat","displace") &
           PartnerID==partnr & StartTime <= (t + 120) & StartTime >= (t-120)))
    
    if (!bad) 
    {
      rank_part <- rank$rank[match(partnr,rank$ID)]
      if (is.na((rank_part)))
        next
      if (rank_part > rank_foc) {
        higher <- higher + 1
      } else if (rank_part < rank_foc)
        lower <- lower + 1
    }
  }
  return(c(higher,lower))
}

vocal <- function(ID,bd,rank)
{
  voc <- subset(bd,Behavior=="AffVoc" & PartnerID %in% rank$ID)
  voc <- subset(voc,behave.screen(list(ALL=c("give","given")),voc))
  
  if (nrow(voc)==0)
    return(as.integer(c(0,0)))
  
  focrank <- rank$rank[match(ID,rank$ID)]
  partrank <- rank$rank[match(voc$PartnerID,rank$ID)]
  
  higher <- focrank < partrank
  lower <- focrank > partrank
  
  higher[is.na(higher)] <- 0L
  lower[is.na(lower)] <- 0L
  
  return(c(sum(higher),sum(lower)))
}

foreveralone <- function(bdi)
{
  noint <- !( (stringr::str_length(bdi$PartnerID) == 3) | grepl("adult",bdi$PartnerID,ignore.case = T) )
  scan <- subset(bdi,Behavior=="scan")
  try(noprox <- with(scan,str_extract(ProxScan,"\\(\\d+\\)") %in% c("(0)","(2)","(6)","(7)")),silent = T)
  try(noprox <- with(scan,In2m_ID_code %in% c("no animal","juvenile","#N/A","")),silent = T)
  return(all(c(noprox,noint)))
}

selfie <- function(Behavior)
  return(sum(Behavior %in% c("SelfGrm","Scratch")))

agg <- function(bdi,style="new")
{
  if (style == "old")
  {
    agg <- subset(bdi,Behavior=="Agg")
    exclude <- list()
    exclude$ALL <- c("avoid","displace")
    exclude$PartnerID <- c("^\\?","^$","unknown","infant","juvenile","juvenille","#N/A","human","researcher")
    modifier <- list(ALL = c("given","give"))
  } else if (style == "old")
  {
    agg <- subset(bdi,Behavior %in% c("contactAgg","noncontactAgg","threat"))
    modifier <- list(Winner="foc",Direction="give")
    exclude <- list(PartnerID <- c("^\\?","^$","unknown","infant","juvenile","juvenille","#N/A","human","researcher","self","not in file"))
  }
  
  if (nrow(agg) == 0)
  {
    return(as.integer(0))
  } else
  {
    inc <- behave.screen(modifier,agg)
    exc <- behave.screen(exclude,agg)
    return (sum(inc & !exc))
  }
}

submit <- function(bdi)
{
  subm <- subset(bdi,Behavior =="Submit")
  if (nrow(subm) == 0){
    return(as.integer(0))
  }else
  {
    modifier <- list(ALL = c("give","given"))
    exclude <- list(PartnerID = c("^\\?","^$","unknown","infant","juvenile","juvenille","#N/A","human","researcher","self","not in file"))
    inc <- behave.screen(modifier,subm)
    exc <- behave.screen(exclude,subm)
    return (sum(inc & !exc))
  }
}