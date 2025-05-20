month_number=04; year_number=2016; current_year=2025;  

nadres_func=function(current_year,year_number,month_number)
  
  # args = commandArgs(trailingOnly=TRUE)
  # if (length(args)<3) {
  #   stop("Correct number of arguments must be supplied", call.=FALSE)
  # }
  # 
  # 
  # current_year=args[1]
  # year_number=args[2]
  # month_number=args[3]
  
print(current_year)
print(year_number)
print(month_number)

library(RMySQL)
library(rgdal)
library(RColorBrewer)
library(sqldf)
library(data.table)  

library(reshape2)
library(imputeMissings)
require(sp)
require(spdep)
require(rms) 
library(xlsx)
library(plyr)
library(dplyr)
library(randomForest)
library(dismo)
library(tidyr)

library(psych)
library(pROC)
library(SDMTools)
library(BIOMOD)
library(ROCR)
library(caret)
library(MLmetrics)
library(s2)



df_total<-NULL
#mydb = dbConnect(MySQL(), user='', password='', dbname='', host='')
#mydb = dbConnect(MySQL(), user='', password='', dbname='', host='')
month_name=data.frame(
  month=c(1:12),
  month_names=c("January","February","March","April","May","June","July","August","September","October","November","December")
)

##################################################################################################  
ss_final<-fread(file="NADRES_PCA_February_2025.csv",header=T,check.names = F)
#ss_final<- ss_final[ss_final$year >= 2014 & ss_final$year <=2023, ] 
#ss_final<-subset(ss_final, select = -c(1,8) )
ss=ss_final[ss_final$month==month_number,]
#write.csv(ss,"ss_dec_data.csv")
ss_lag=ss_final[ss_final$month==month_number - 1,]
colnames(ss_lag)[18:69]=paste0(colnames(ss_lag)[18:69],"_lag")
ss=cbind(ss,ss_lag[,18:69])
names(ss)

##############################################################################################
# ss_final=read.csv("./append_DB_NEW/NEW_DB_UPTOMARCH/Averaged_13_Final_DB.csv")
# ss=ss_final[ss_final$month==month_number,]
# ss_lag=read.csv("./append_DB_NEW/NEW_DB_UPTOMARCH/Averaged_lag.csv")
# colnames(ss_lag)[18:40]=paste0(colnames(ss_lag)[18:40],"_lag")
# ss=cbind(ss,ss_lag[,18:40])
# names(ss)
###############################################################################################  
col_pars=names(ss)
vars= paste(col_pars[7:ncol(ss)],collapse = "+")
vars=paste0(vars,"+rating", "+Month_score")
options(verbose = F)
rs<-dbSendQuery(mydb,"SELECT state.state_name,state.state_id,district.district_id, district.district_name, dval_ob_district_final.year_id,
dval_ob_district_final.month_id,species.species_name, disease.disease_id,
disease.disease_name, dval_ob_district_final.number_outbreak, dval_ob_district_final.number_susceptible,
dval_ob_district_final.number_attack, dval_ob_district_final.number_death
FROM dval_ob_district_final
INNER JOIN  state on state.state_id=dval_ob_district_final.state_id
INNER JOIN  district on district.district_id=dval_ob_district_final.district_id
INNER JOIN  disease on disease.disease_id=dval_ob_district_final.disease_id
INNER JOIN  species on species.species_id=dval_ob_district_final.species_id")
data1 = fetch(rs, n=-1)
colnames(data1)=c("state_name","state_id","district_id","district_name","year","month","species_name","disease_id","disease_name",
                  "number_of_outbreaks","number_susceptible","number_of_attacks","number_of_deaths")
#data1=read.csv("databasefile2.csv")
#disease=8
k1=readOGR("India_shapefile_2025/India_shape_file2025.shp")
k1@data
names(k1)[1]<-"DISTRICT"
names(k1)[2]<-"ST_NM"
names(k1)[3]<-"ST_CEN_CD"
names(k1)[4]<-"DT_CEN_CD"
ll_coord=data.frame(coordinates(k1))
final_eval=NULL
# final_eval=final_eval[-c(13:15),]
fwrite(data1,paste0("dist_out_nadres_",gsub("\\:","_",Sys.time()),".csv"))


for(disease in c(8,10,11,12,31,35,37,48,60,65,70,72,79,146,189)) 
  
  
{
  k=k1
  
  rat_df=c(1:nrow(k))  
  df=data1
  
  if(disease==12)
  {
    d1=df[df$year>=1987 & df$year<=current_year & df$disease_id==disease & df$month==month_number,]
  } else  d1=df[df$year>=year_number & df$year<=current_year & df$disease_id==disease & df$month==month_number,]
  d2=d1[,c("state_name","district_name","disease_name","month","year")]
  d2=d2[!duplicated(d2),]
  
  
  yr_rt=data.frame(year=c(year_number:current_year),rating=c(1:10))
  
  st_dt=data.frame(k@data[,c("ST_NM","DISTRICT")])
  d2$state_name=toupper(d2$state_name)
  st_dt$rating=0
  for (i in 1:nrow(st_dt)) {
    rt=yr_rt[yr_rt$year==max(d2[which(d2$state_name==as.character(st_dt[i,"ST_NM"]) & 
                                        d2$district_name==as.character(st_dt[i,"DISTRICT"])),"year"]),"rating"]
    if(sum(rt)!=0)
      st_dt[i,"rating"]=rt
  }
  rat_df=cbind(rat_df,st_dt)
  colnames(rat_df)[c(2,3)]=c("state_name","district_name")
  rat_df=rat_df[-1]
  
  
  # Create the score table
  score_table <- data.frame( disease_name = c("African Swine Fever", "Anthrax", "Babesiosis", "Black quarter", "Bluetongue", "Classical Swine fever", "Enterotoxaemia", "Fascioliasis", "Foot and mouth disease", "Haemorrhagic septicaemia", "Lumpy Skin Disease", "Peste des petits ruminants", "Sheep & Goat pox", "Theileriosis", "Trypanosomiasis"),
              January = c(2,	1,	3,	2,	8,	2,	2,	4,	3,	2,	10,	3,	2,	2,	3),
              February = c(2,	1,	6,	2,	10,	3,	3,	9,	6,	4,	7,	9,	5,	4,	7),
              March = c(3,	1,	6,	2,	2,	3,	3,	9,	5,	3,	3,	10,	4,	4,	8),
              April = c(4,	1,	4,	2,	2,	5,	3,	6,	4,	5,	10,	10,	4,	4,	5),
              May = c(2,	1,	2,	2,	2,	2,	2,	2,	2,	2,	10,	2,	2,	2,	2),
              June = c(2,	2,	2,	2,	1,	2,	2,	2,	2,	2,	10,	2,	2,	2,	2),
              July = c(9,	2,	3,	2,	1,	2,	2,	5,	5,	2,	10,	4,	2,	3,	3),
              August = c(7,	2,	3,	2,	1,	2,	2,	6,	10,	2,	7,	3,	2,	3,	4),
              September = c(3,	1,	3,	2,	4,	2,	2,	5,	10,	2,	6,	4,	2,	2,	3),
              October = c(4,	1,	4,	2,	9,	2,	2,	6,	4,	2,	4,	10,	2,	4,	5),
              November = c(3,	1,	4,	2,	10,	2,	2,	6,	7,	7,	2,	10,	2,	3,	4),
              December = c(2,	1,	2,	2,	10,	2,	2,	2,	2,	2,	2,	3,	2,	2,	3)
  )
  
  
  score_long <- score_table %>%
    pivot_longer(cols = January:December, names_to = "month", values_to = "Score") %>%
    mutate(month = match(month, month.name))
  
  d2_with_scores <- d2 %>%
    left_join(score_long, by = c( "disease_name" = "disease_name","month"= "month"))
  d2_with_scores <- d2_with_scores[, -which(names(d2_with_scores) == "year")]
  
  st_dt$Month_score <- 0
  for (i in 1:nrow(st_dt)) {
    matching_score <- d2_with_scores %>%
      filter(state_name == as.character(st_dt[i, "ST_NM"]) &
               district_name == as.character(st_dt[i, "DISTRICT"])) %>%
      .$Score
    
    # If a matching score exists and is greater than 0, assign it to Month_score
    if (length(matching_score) > 0 && !is.na(matching_score[1]) && matching_score[1] > 0) {
      # your code here
    }
  }
  
  mn_score <- cbind(rat_df, st_dt)
  mn_score <- mn_score[-(4:6)]  
  
  data<-subset(data1,data1$year>=year_number & data1$disease_id==disease)
  
  #fwrite(data,paste0("NDR_",Sys.Date(),".csv"))
  
  df<-sqldf("SELECT state_id,state_name,district_id,district_name,disease_id,disease_name,month,sum(number_of_outbreaks)as outbreak FROM data GROUP BY state_id,district_id,state_name,district_name,month,disease_id,disease_name",drv="SQLite")
  ss1<-subset(ss,ss$disease_id==disease)
  
  dd<-merge(ss1, df, by = c("state_id","district_id","disease_id","month"),all.x=TRUE)
  attach(dd,warn.conflicts = F)
  out<-data.frame(outbreak)
  out<-ifelse(outbreak>=1,1,0)
  out[is.na(out)]<-0
  final<-cbind(dd,out)
  
  final1<-final[which(final$disease_id==disease & final$month==month_number),]
  cat("For disease: ",as.character(unique(ss1[,"disease_name"])),"\n")
  ncs= ncol(final1)-5
  temp = data.frame(final1[,8:ncs])
  for(i in 1:ncol(temp)){
    temp[is.na(temp[,i]), i] <- mean(temp[,i], na.rm = TRUE)
  }
  
  final2<-cbind(final1$state_id,final1$state_name.x,final1$district_id,final1$district_name.x,final1$disease_id,final1$disease_name.x,final1$out,final1$month,temp)
  setnames(final2,old=c("final1$state_id","final1$state_name.x","final1$district_id","final1$district_name.x","final1$disease_id","final1$disease_name.x","final1$out","final1$month"),new=c("state_id","state_name","district_id","district_name","disease_id","disease_name","out","month"))
  
  final2 <- final2 %>% left_join(mn_score[, c("state_name", "district_name", "rating", "Month_score")],
                                 by = c("state_name", "district_name"))
  final2 <- final2 %>%
    mutate( rating = if_else(is.na(rating), 0, rating),
            Month_score = if_else(is.na(Month_score), 0, Month_score))
}
  
  write.csv(final2, "final2.csv") # This script processes one disease at a time within a loop; only the data extraction and preprocessing logic for individual diseases is shown here as part of the overall pipeline.
