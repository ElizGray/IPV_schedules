#Coverage models 2018-2022

setwd("C:/Users/egray/OneDrive - Imperial College London/IPV_impact_codes_and_files")
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
#library(binsmooth)
library(reshape2)

#schedule<- read_csv("POLIO_WIISE_AD_SCHEDULES_2018_22.csv")

historical_schedules=read_csv("POLIO_WIISE_AD_SCHEDULES_2018_22.csv")%>%filter(VACCINECODE=="IPV")%>%filter(is.na(TARGETPOP))

historical_schedules$COUNTRY[which(historical_schedules$COUNTRY=="Bolivia")]="Bolivia (Plurinational State of)"
historical_schedules$COUNTRY[which(historical_schedules$COUNTRY=="Lao People's  Democratic Republic")]="Lao People's Democratic Republic"




#schedule_1a=read_csv("vaccine-schedule2021.csv")
schedule_1=read_csv("schedules_2023.csv")
schedule_1$COUNTRYNAME=as.character(schedule_1$COUNTRYNAME)
schedule=schedule_1%>%filter(TARGETPOP_DESCRIPTION=="General/routine")


schedule$COUNTRYNAME[ grep("Ivoire",schedule$COUNTRYNAME)]="Cote d'Ivoire"
schedule$COUNTRYNAME[ grep("Cura",schedule$COUNTRYNAME)]="Curacao"
schedule$COUNTRYNAME[ grep("kiye",schedule$COUNTRYNAME)]="Turkey"
schedule$COUNTRYNAME[ grep("alestin",schedule$COUNTRYNAME)]="Palestine, N.A. (West Bank and Gaza)" 
schedule$COUNTRYNAME[ grep("renadines",schedule$COUNTRYNAME)]="Saint Vincent and The Grenadines"
schedule$COUNTRYNAME[ grep("aicos",schedule$COUNTRYNAME)]="Turks and Caicos" 
schedule$COUNTRYNAME[ grep("Venezuela",schedule$COUNTRYNAME)]="Venezuela" 
schedule$COUNTRYNAME[ grep("Kosovo",schedule$COUNTRYNAME)]="Kosovo" 

opv_using=unique(unlist(read_csv("POLIO_WIISE_AD_SCHEDULES_2018_22.csv")%>%filter(VACCINECODE=="OPV")%>%select(COUNTRY)))
opv_using2=unique(unlist(schedule%>%filter(VACCINECODE=="OPV")%>%select(COUNTRYNAME)))
opv_using=unique(c(opv_using,opv_using2))
opv_using[which(opv_using=="Bolivia")]="Bolivia (Plurinational State of)"
opv_using[which(opv_using=="Côte d´Ivoire")]="Cote d'Ivoire"
opv_using[which(opv_using=="Lao People's  Democratic Republic")]="Lao People's Democratic Republic"
opv_using[ grep("Cura",opv_using)]="Curacao"
opv_using[ grep("kiye",opv_using)]="Turkey"
opv_using[ grep("alestine",opv_using)]="Palestine, N.A. (West Bank and Gaza)" 
opv_using[ grep("renadines",opv_using)]="Saint Vincent and The Grenadines"
opv_using[ grep("aicos",opv_using)]="Turks and Caicos"
opv_using[ grep("enezuela",opv_using)]="Venezuela"
opv_using[ grep("osovo",opv_using)]="Kosovo"
opv_using[ grep("Ivoire",opv_using)]="Cote d'Ivoire"
#="Cote d'Ivoire"

# "occupied Palestinian territory, including east Jerusalem"   
#"Palestine, N.A. (West Bank and Gaza)"  



schedule=schedule%>%filter(is.element(COUNTRYNAME, opv_using))

schedule_countries= unique(schedule$COUNTRYNAME)

n=length(schedule_countries)
#AFRO_COUNTRIES=unique(as.vector(unlist(schedule%>%filter(WHO_REGION=="AFRO")%>%select(COUNTRYNAMENAME))))

date2=schedule$AGEADMINISTERED

#take the earliest poss date of administration
date2=word(date2,sep=fixed("-"))
date2=str_replace(date2, ">=","")
date2=str_replace(date2, "=>","")
date2=str_replace(date2, ">","")

#now the "below" ones -not sure what to do with these 
date2=str_replace(date2, "<","")

plusones=rep(0,length(date2))
plusones[grep("\\+",date2)]=1

date2=str_replace(date2, "\\+","")

date2=str_replace(date2, ">=A60 ","Y60")



date2[which(date2=="B")]=0
#date2[which(date2=="1st contact")]=0
#date2[which(date2=="1st Contact")]=0

year_ind=rep(0,length(date2))
year_ind[grep("Y",date2)]=1
date2=str_replace(date2, "Y","")


month_ind=rep(0,length(date2))
month_ind[grep("M",date2)]=1
date2=str_replace(date2, "M","")

week_ind=rep(0,length(date2))
week_ind[grep("W",date2)]=1
date2=str_replace(date2, "W","")

day_ind=rep(0,length(date2))
day_ind[grep("D",date2)]=1
date2=str_replace(date2, "D","")

#Take out the equals signs
#What are the question marks for? 

date3=as.numeric(date2)
date4=round(date3*day_ind*(1/7)+date3*week_ind+date3*month_ind*4.3+date3*year_ind*52.1)

#now the plusses 

rounds2=as_tibble(data.frame(index=1:length(plusones),plusones=plusones,rounds=schedule$SCHEDULEROUNDS))
rounds2=rounds2%>%arrange(rounds)%>%filter(plusones==1)
plusones_sorted=rounds2$index



#need to go from 

date5=date4
# 
# for(j in 1:length(plusones_sorted)){
#   i=plusones_sorted[j]
#   COUNTRYNAME_i=schedule$COUNTRYNAME[i]
#   year_i=schedule$YEAR
#   vaccinecode_i=schedule$VACCINECODE[i]
#   round_i=schedule$SCHEDULEROUNDS[i]
#   
#   whichone=which(schedule$COUNTRYNAME==COUNTRYNAME_i & YEAR==year_i & schedule$VACCINECODE==vaccinecode_i & schedule$SCHEDULEROUNDS==(round_i-1))
#   
#   date_prev=date5[whichone]
#   date_to_add=date5[i]
#   newdate=date_prev+date_to_add
#   date5[i]=newdate}
# 

schedule$AGEWEEKS=date5

schedule=schedule%>%filter(AGEADMINISTERED!="1st contact")
schedule=schedule%>%filter(!is.na(AGEWEEKS))

#now synthesising with coverage 
#coverage1=read_csv("coverage2022.csv")
coverage1=read_csv("coverage-data-2023.csv")
coverage1$NAME[ grep("Ivoire",coverage1$NAME)]="Cote d'Ivoire"
coverage1$NAME[ grep("Cura",coverage1$NAME)]="Curacao"
coverage1$NAME[ grep("alestin",coverage1$NAME)]="Palestine, N.A. (West Bank and Gaza)" 
coverage1$NAME[ grep("Venezuela",coverage1$NAME)]="Venezuela" 
coverage1$NAME[ grep("aicos",coverage1$NAME)]="Turks and Caicos"
coverage1$NAME[ grep("incent",coverage1$NAME)]="Saint Vincent and The Grenadines"
coverage1$NAME[ grep("kiye",coverage1$NAME)]="Turkey"
coverage1$NAME[ grep("osovo",coverage1$NAME)]="Kosovo"




coverage1=coverage1%>%filter(is.element(NAME,schedule_countries)) 




introdata=read_csv("vaccine-introduction-data.csv")
intro2=introdata%>%filter(INTRO=="Yes")%>%group_by(COUNTRYNAME,DESCRIPTION)%>%summarise(my=min(YEAR))%>%filter(my>2018)
intro2$COUNTRYNAME[ grep("Ivoire",intro2$COUNTRYNAME)]="Cote d'Ivoire"
intro2$COUNTRYNAME[ grep("Cura",intro2$COUNTRYNAME)]="Curacao"
intro2$COUNTRYNAME[ grep("kiye",intro2$COUNTRYNAME)]="Turkey"
intro2$COUNTRYNAME[ grep("alestin",intro2$COUNTRYNAME)]="Palestine, N.A. (West Bank and Gaza)" 
intro2$COUNTRYNAME[ grep("renadines",intro2$COUNTRYNAME)]="Saint Vincent and The Grenadines"
intro2$COUNTRYNAME[ grep("aicos",intro2$COUNTRYNAME)]="Turks and Caicos" 
intro2$COUNTRYNAME[ grep("Venezuela",intro2$COUNTRYNAME)]="Venezuela" 
intro2$COUNTRYNAME[ grep("Kosovo",intro2$COUNTRYNAME)]="Kosovo" 


yearsvec=2019:2023

cpk_list=vector(length=5, mode="list")
for(k in 1:5){
  currentyear=yearsvec[k]

  intro_current_year=intro2%>%filter(my==currentyear)
  
coverage=coverage1%>%filter(YEAR==currentyear)
#schedule=schedule%>%filter(YEAR==2020)
#coverage=coverage%>%filter(is.element(NAME, AFRO_COUNTRIES))
coverage_WUENIC=coverage%>%filter(COVERAGE_CATEGORY=="WUENIC")
coverage_nonwue=coverage%>%filter(COVERAGE_CATEGORY=="ADMIN", is.element(NAME,setdiff(unique(coverage$NAME), unique(coverage_WUENIC$NAME))))%>%filter(!is.na(COVERAGE))%>%filter(COVERAGE<=100)
coverage_nonwue$COVERAGE[which(coverage_nonwue$COVERAGE==100)]=99.99
coverage_WUENIC=bind_rows(coverage_WUENIC,coverage_nonwue)




#1 BCG - in WUENIC THIS IS THE BIRTH DOSE - just one - should we take out later ones? 
cover_BCG=coverage_WUENIC%>%filter(ANTIGEN=="BCG")
cover_BCG2=cover_BCG%>%select(NAME,COVERAGE)
cover_BCG2=cover_BCG2%>%rename(COUNTRYNAME=NAME)
schedule_BCG=schedule%>%filter(VACCINECODE=="BCG", SCHEDULEROUNDS==1,AGEWEEKS<150)
BCG_schedule_coverage=right_join(schedule_BCG,cover_BCG2)

#2 DTPCV1 - DTP containing vaccine, one dose 

DTP_vaccines=c("DTAPHIBIPV",
               "DTAPIPV",  
               "DTWPHIBHEPB",  
               "DTAPHIBHEPB",  
               "DTWP", 
               "DTAPHIBHEPBIPV", 
               "DTAP",    
               "TDAP_S_IPV",
               "TDAP_S", 
               "DTAP", 
               "DTWPHIB",
               "DTAPHEPBIPV",  
               "DTWPHIBHEPBIPV",
               "DTAPHIB", 
               "DTWPHEPB",  
               "DTWPHIBHEPBIPV") 

schedule_DTP1=schedule%>%filter(is.element(VACCINECODE,DTP_vaccines),AGEWEEKS<150, SCHEDULEROUNDS==1)
schedule_DTP1=schedule_DTP1%>%filter(!duplicated(COUNTRYNAME))
cover_DTP1=coverage_WUENIC%>%filter(ANTIGEN=="DTPCV1")
cover_DTP1_2=cover_DTP1%>%select(COUNTRYNAME=NAME,COVERAGE)
DTP1_schedule_coverage=left_join(schedule_DTP1,cover_DTP1_2)

# DTP3 
schedule_DTP3=schedule%>%filter(is.element(VACCINECODE,DTP_vaccines),AGEWEEKS<150, SCHEDULEROUNDS==3)
schedule_DTP3=schedule_DTP3%>%filter(!duplicated(COUNTRYNAME))
cover_DTP3=coverage_WUENIC%>%filter(ANTIGEN=="DTPCV3")
cover_DTP3_2=cover_DTP3%>%select(COUNTRYNAME=NAME,COVERAGE)
DTP3_schedule_coverage=left_join(schedule_DTP3,cover_DTP3_2)


#"HEPB_BD" Hep B birth does 
HepBBD=c("HEPB_PEDIATRIC",
         "DTWPHIBHEPB",
         "DTAPHIBHEPB",   
         "HEPA_HEPB", 
         "DTWPHIBHEPBIPV",
         "DTAPHEPBIPV",
         "DTAPHIBHEPBIPV",  
         "DTWPHIBHEPB", 
         "DTWPHEPB")

schedule_HEPB_BD=schedule%>%filter(is.element(VACCINECODE,HepBBD),AGEWEEKS<6, SCHEDULEROUNDS==1) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_HEP_BD=schedule_HEPB_BD%>%arrange(AGEWEEKS)
schedule_HEPB_BD=schedule_HEPB_BD%>%filter(!duplicated(COUNTRYNAME))
cover_HEPB_BD=coverage_WUENIC%>%filter(ANTIGEN=="HEPB_BD")
cover_HEPB_BD_2=cover_HEPB_BD%>%select(COUNTRYNAME=NAME,COVERAGE)
HEPB_BD_schedule_coverage=left_join(schedule_HEPB_BD,cover_HEPB_BD_2)

excl=(intro_current_year%>%filter(DESCRIPTION=="HepB birth dose"))$COUNTRYNAME
HEPB_BD_schedule_coverage=HEPB_BD_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))


#"HIB3" 


HIB_vaccines=c("DTAPHIBIPV",
               "DTWPHIB",
               "DTAPHIBHEPB", 
               "DTWPHIBHEPBIPV", 
               "DTAPHIBHEPBIPV",
               "DTAPHIB", 
               "HIB",   
               "HIB_MEN_C_CONJ", 
               "DTWPHIBHEPB")

schedule_HIB3=schedule%>%filter(is.element(VACCINECODE,HIB_vaccines), SCHEDULEROUNDS==3) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_HIB3=schedule_HIB3%>%arrange(AGEWEEKS)
schedule_HIB3=schedule_HIB3%>%filter(!duplicated(COUNTRYNAME))
cover_HIB3=coverage_WUENIC%>%filter(ANTIGEN=="HIB3")
cover_HIB3_2=cover_HIB3%>%select(COUNTRYNAME=NAME,COVERAGE)
HIB3_schedule_coverage=left_join(schedule_HIB3,cover_HIB3_2)

excl=(intro_current_year%>%filter(DESCRIPTION=="Hib (Haemophilus influenzae type B) vaccine"))$COUNTRYNAME
HIB3_schedule_coverage=HIB3_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))



#"IPV1" 
IPV_vaccines=c(
  "DTAPHIBIPV",
  "IPV_FRAC" ,
  "DTAPIPV",
  "DTIPV",
  "DTAPHEPBIPV",
  "DTWPHIBHEPBIPV",
  "DTAPHIBHEPBIPV",
  "TDIPV_S",
  "IPV",
  "TDAP_S_IPV")

#NOW IPV2 is collected


#schedule_IPV=schedule%>%filter(is.element(VACCINECODE,IPV_vaccines), SCHEDULEROUNDS==1)

#DO WE CHANGE THIS TO JUST BIRTH DOSE?

schedule_IPV=schedule%>%filter(is.element(VACCINECODE,IPV_vaccines))
schedule_IPV=schedule_IPV%>%arrange(AGEWEEKS)
schedule_IPV=schedule_IPV%>%filter(!duplicated(COUNTRYNAME))

schedule_IPV=schedule_IPV%>%arrange(AGEWEEKS)
schedule_IPV=schedule_IPV%>%filter(!duplicated(COUNTRYNAME))
cover_IPV=coverage_WUENIC%>%filter(ANTIGEN=="IPV1")
cover_IPV_2=cover_IPV%>%select(COUNTRYNAME=NAME,COVERAGE)
IPV_schedule_coverage=left_join(schedule_IPV,cover_IPV_2)

excl=(intro_current_year%>%filter(DESCRIPTION=="IPV (Inactivated polio vaccine)"))$COUNTRYNAME
if(currentyear==2021){
  excl=c(excl,"Nigeria")
}else if(currentyear==2022){
  excl=c(excl,c("Uganda","Azerbaijan"))
}else if(currentyear==2023){
  excl=c(excl,c("Thailand","Cote d'Ivoire","Senegal","Sudan","Suriname"))
}
  
IPV_schedule_coverage=IPV_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))




#IPV second dose
schedule_IPV=schedule%>%filter(is.element(VACCINECODE,IPV_vaccines))
schedule_IPV=schedule_IPV%>%arrange(SCHEDULEROUNDS)
schedule_IPV2=schedule_IPV%>%distinct(COUNTRYNAME,AGEWEEKS,.keep_all = TRUE)%>%filter(duplicated(COUNTRYNAME))%>%filter(!duplicated(COUNTRYNAME))

cover_IPV22=coverage_WUENIC%>%filter(ANTIGEN=="IPV2")
cover_IPV_22=cover_IPV22%>%select(COUNTRYNAME=NAME,COVERAGE)
IPV2_schedule_coverage=left_join(schedule_IPV2,cover_IPV_22)%>%filter(!is.na(COVERAGE))

excl=(intro_current_year%>%filter(DESCRIPTION=="IPV (Inactivated polio vaccine) 2nd dose"))$COUNTRYNAME
IPV2_schedule_coverage=IPV2_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))

"MCV1" 

measles_vaccines=c(
  "MMR",
  "MMRV" ,
  "MR"  ,
  "MEASLES")            

schedule_measles=schedule%>%filter(is.element(VACCINECODE,measles_vaccines))%>%filter(SCHEDULEROUNDS==1) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_measles=schedule_measles%>%arrange(AGEWEEKS)
schedule_measles=schedule_measles%>%filter(!duplicated(COUNTRYNAME))
cover_measles=coverage_WUENIC%>%filter(ANTIGEN=="MCV1")
cover_measles_2=cover_measles%>%select(COUNTRYNAME=NAME,COVERAGE)
measles_schedule_coverage=left_join(schedule_measles,cover_measles_2)


#"MCV2" 


schedule_measles2=schedule%>%filter(is.element(VACCINECODE,measles_vaccines), SCHEDULEROUNDS==2) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_measles2=schedule_measles2%>%arrange(AGEWEEKS)
schedule_measles2=schedule_measles2%>%filter(!duplicated(COUNTRYNAME))
cover_measles2=coverage_WUENIC%>%filter(ANTIGEN=="MCV2")
cover_measles_2_2=cover_measles2%>%select(COUNTRYNAME=NAME,COVERAGE)
measles_schedule_coverage2=left_join(schedule_measles2,cover_measles_2_2)

excl=(intro_current_year%>%filter(DESCRIPTION=="Measles-containing vaccine 2nd dose"))$COUNTRYNAME
measles_schedule_coverage2=measles_schedule_coverage2%>%filter(!is.element(COUNTRYNAME,excl))


#"PCV3" pneumococcal conjugate - are these the right ones 
pneu_vaccines=c("PCV13", 
                "PCV10")

schedule_pneu=schedule%>%filter(is.element(VACCINECODE,pneu_vaccines), SCHEDULEROUNDS==3) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_pneu=schedule_pneu%>%arrange(AGEWEEKS)
schedule_pneu=schedule_pneu%>%filter(!duplicated(COUNTRYNAME))
cover_pneu=coverage_WUENIC%>%filter(ANTIGEN=="PCV3")
cover_pneu_2=cover_pneu%>%select(COUNTRYNAME=NAME,COVERAGE)
pneu_schedule_coverage=left_join(schedule_pneu,cover_pneu_2)

excl=(intro_current_year%>%filter(DESCRIPTION=="PCV (Pneumococcal conjugate vaccine)"))$COUNTRYNAME
pneu_schedule_coverage=pneu_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))


#Polio 


polio_vaccines=c(
  "DTAPHIBIPV",
  "IPV_FRAC" ,
  "DTAPIPV",
  "DTIPV",
  "DTAPHEPBIPV",
  "DTWPHIBHEPBIPV",
  "DTAPHIBHEPBIPV",
  "TDIPV_S",
  "IPV",
  "TDAP_S_IPV",
  "OPV")


schedule_polio=schedule%>%filter(is.element(VACCINECODE,polio_vaccines), SCHEDULEROUNDS==3) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_polio=schedule_polio%>%arrange(AGEWEEKS)%>%arrange(VACCINECODE)
schedule_polio=schedule_polio%>%filter(!duplicated(COUNTRYNAME))
cover_polio=coverage_WUENIC%>%filter(ANTIGEN=="POL3")
cover_polio_2=cover_polio%>%select(COUNTRYNAME=NAME,COVERAGE)
polio_schedule_coverage=left_join(schedule_polio,cover_polio_2)


#"ROTAC" 
rotavirus_vaccines=c("ROTAVIRUS_1","ROTAVIRUS_5")
schedule_rotavirus=schedule%>%filter(is.element(VACCINECODE,rotavirus_vaccines))%>%filter(is.element(SCHEDULEROUNDS,c(3,2))) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_rotavirus=schedule_rotavirus%>%arrange(AGEWEEKS)
schedule_rotavirus=schedule_rotavirus%>%arrange(desc(SCHEDULEROUNDS))
schedule_rotavirus=schedule_rotavirus%>%filter(!duplicated(COUNTRYNAME))
cover_rotavirus=coverage_WUENIC%>%filter(ANTIGEN=="ROTAC")
cover_rotavirus_2=cover_rotavirus%>%select(COUNTRYNAME=NAME,COVERAGE)
rotavirus_schedule_coverage=left_join(schedule_rotavirus,cover_rotavirus_2) 

excl=(intro_current_year%>%filter(DESCRIPTION=="Rotavirus vaccine"))$COUNTRYNAME
rotavirus_schedule_coverage=rotavirus_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))

#Yellow fever YFV
yellowfever_vaccine=c("YF")
schedule_yellowfever=schedule%>%filter(is.element(VACCINECODE,yellowfever_vaccine)) #DO WE CHANGE THIS TO JUST BIRTH DOSE?
schedule_yellowfever=schedule_yellowfever%>%arrange(AGEWEEKS)
schedule_yellowfever=schedule_yellowfever%>%arrange(desc(SCHEDULEROUNDS))
schedule_yellowfever=schedule_yellowfever%>%filter(!duplicated(COUNTRYNAME))
cover_yellowfever=coverage_WUENIC%>%filter(ANTIGEN=="YFV")
cover_yellowfever_2=cover_yellowfever%>%select(COUNTRYNAME=NAME,COVERAGE)
yellowfever_schedule_coverage=left_join(schedule_yellowfever,cover_yellowfever_2) 
yellowfever_schedule_coverage=yellowfever_schedule_coverage%>%filter(GEOAREA=="NATIONAL")%>%filter(!is.element(COUNTRYNAME, c("Uganda","Equatorial Guinea")))

excl=(intro_current_year%>%filter(DESCRIPTION=="YF (Yellow fever) vaccine"))$COUNTRYNAME
yellowfever_schedule_coverage=yellowfever_schedule_coverage%>%filter(!is.element(COUNTRYNAME,excl))

#Take out polio and yellow fever?


#ALL WUENIC COVERAGE ESTIMATES

all_wuenic=rbind(IPV_schedule_coverage,IPV2_schedule_coverage,yellowfever_schedule_coverage,rotavirus_schedule_coverage,HEPB_BD_schedule_coverage,measles_schedule_coverage,BCG_schedule_coverage,measles_schedule_coverage2,DTP1_schedule_coverage,DTP3_schedule_coverage,pneu_schedule_coverage,HIB3_schedule_coverage)
all_wuenic=all_wuenic[which(!duplicated(all_wuenic)),]

all_wuenic=all_wuenic%>%filter(GEOAREA=="NATIONAL")
# if(k==5){
# all_wuenic=all_wuenic[-which(all_wuenic$YEAR=="2022" & all_wuenic$ISO_3_CODE=="GNB" & all_wuenic$COVERAGE<10),]}
#all_wuenic=all_wuenic[which(is.na(all_wuenic$COMMENTORIGINAL)),]


all_wuenic=all_wuenic%>%filter(COVERAGE>9) #likely anomalies eg just starting 

lm_list_k=vector(length=n, mode="list")
country_predictions_k=matrix(NA,nrow=n,ncol=150)


for(i in 1:n){
  COUNTRYNAME_wuenic=all_wuenic%>%filter(COUNTRYNAME==schedule_countries[i])
  COUNTRYNAME_df=data_frame(cover=qlogis(COUNTRYNAME_wuenic$COVERAGE/100), weeks=COUNTRYNAME_wuenic$AGEWEEKS)
  COUNTRYNAME_df=COUNTRYNAME_df[which(exp(COUNTRYNAME_df$cover)>0),]
  
  if(prod(is.na(COUNTRYNAME_df$cover))+prod(is.na(COUNTRYNAME_df$weeks))>0 ){lm_list_k[[i]]=NA
  
}else{
    plot(COUNTRYNAME_wuenic$AGEWEEKS,qlogis(COUNTRYNAME_wuenic$COVERAGE/100))
    plot(COUNTRYNAME_wuenic$AGEWEEKS,(COUNTRYNAME_wuenic$COVERAGE/100))                                   
    lm1=lm(cover~weeks, data=COUNTRYNAME_df)
    lm_list_k[[i]]=lm1
    
    
    
    print(c(dim(COUNTRYNAME_df)[1],i))
    
    if(!is.na(lm1$coefficients[2])){
    if(lm1$coefficients[2]<0){country_predictions_k[i,]=as.numeric(plogis(predict.lm(lm1, newdata=data.frame(weeks=c(1:150)))))}else{
      
      country_predictions_k[i,]=mean(as.numeric(unlist(COUNTRYNAME_wuenic%>%select(COVERAGE))),na.rm=TRUE)/100}}else{
        
        country_predictions_k[i,]=mean(as.numeric(unlist(COUNTRYNAME_wuenic%>%select(COVERAGE))),na.rm=TRUE)/100
      }
  
} 
}





cpk_list[[k]]=country_predictions_k
}



schedule_countries[which(schedule_countries=="occupied Palestinian territory, including east Jerusalem")]="Palestine, N.A. (West Bank and Gaza)"



cp19=as.data.frame(cpk_list[[1]])
rownames(cp19)=schedule_countries
colnames(cp19)=paste0("Week ", 1:150)

cp20=as.data.frame(cpk_list[[2]])
rownames(cp20)=schedule_countries
colnames(cp20)=paste0("Week ", 1:150)

cp21=as.data.frame(cpk_list[[3]])
rownames(cp21)=schedule_countries
colnames(cp21)=paste0("Week ", 1:150)

cp22=as.data.frame(cpk_list[[4]])
rownames(cp22)=schedule_countries
colnames(cp22)=paste0("Week ", 1:150)

cp23=as.data.frame(cpk_list[[5]])
rownames(cp23)=schedule_countries
colnames(cp23)=paste0("Week ", 1:150)

list_of_cover_mats=list(cp19=cp19,cp20=cp20,cp21=cp21,cp22=cp22,cp23=cp23)

save(list_of_cover_mats, file="list_of_cover_mats.Rda")

list_for_coverage_decline_plot=list(list_of_cover_mats=list_of_cover_mats,schedule_countries=schedule_countries,all_wuenic=all_wuenic, vector2023=cpk_list[[5]] )
save(list_for_coverage_decline_plot,file="list_for_coverage_decline_plot.Rda")

weekvec=NULL
for(i in 1:length(schedule_countries)){
  weekvec=c(weekvec,rep(i,150))
}
t2=as_tibble(list_of_cover_mats[[5]])
rownames(t2)=schedule_countries

t2=data.frame(country=rep(schedule_countries, 150),Week=weekvec,coverage=as.vector(cpk_list[[5]]))

t3=t2%>%filter(is.element(country,c("Nigeria","Uganda","Democratic Republic of Congo","Turkmenistan","Yemen","Afghanistan","Ghana","Pakistan","Malawi","Bangladesh") ))%>%
  rename(COUNTRYNAME=country, AGEWEEKS=Week, COVERAGE=coverage)%>%
  filter(AGEWEEKS<81)
#

all_wuenic_subset=all_wuenic%>%
  filter(is.element(COUNTRYNAME, c("Nigeria","Uganda","Democratic Republic of Congo","Turkmenistan","Yemen","Afghanistan","Ghana","Pakistan","Malawi","Bangladesh")))%>%
  filter(AGEWEEKS<81)


ggplot(all_wuenic_subset)+
  geom_point(aes(x=AGEWEEKS,y=COVERAGE, colour=VACCINECODE))+
  facet_wrap(~COUNTRYNAME)+ylab("% Coverage")+
  xlab("Weeks of age at scheduled administration")+
  geom_line(data=t3, aes(x=AGEWEEKS,y=100*(COVERAGE)),linetype="dashed")
           
            