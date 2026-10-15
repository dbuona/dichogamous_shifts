setwd("GitHub/dichogamous_shifts/")
library(dplyr)

d<-read.csv("Data/occurrence.csv")

colnames(d)

cols2Keep<-c("gbifID","license",
             "references","rightsHolder"
             ,"reproductiveCondition","eventDate",
             "startDayOfYear","year","month","day",
             "stateProvince","decimalLatitude","decimalLongitude",
             "subgenus","infragenericEpithet",
             "specificEpithet","elevation"
             )

d2<-d %>% select(all_of(cols2Keep))

table(d2$reproductiveCondition)

d2$spike<-NA
d2$pistillate<-NA
d2$staminate<-NA
d2$seed<-NA

acrocystis<-c("pensylvanica","inops","lucorum","albicans","floridana",
              "nigromarginata","peckii","brevicaulis","deflexa","geophila","pityophila",
              "rossii","tonsa","umbellata","brainerdii","communis","globosa","novae-angliae",
              "turbinata","serpenticola")

acro<-filter(d2,specificEpithet %in% acrocystis)
acro %>% group_by (specificEpithet) %>% count() %>% arrange(-n)

careyanae<-c("plantaginea","careyana","platyphylla","laxiculmis","digitalis","austrocaroliniana",
             "cumberlandensis","abscondita")
carey<-filter(d2,specificEpithet %in% careyanae)

carey %>% group_by (specificEpithet) %>% count() %>% arrange(-n)

laxiflorae<-c("albursina","blanda","chapmanii","crebriflora","gracilescens",
              "hendersonii","kraliana","laxiflora","leptonervia","manhartii",
              "ormostachya","purpurifera","radfordii",
              "striatula","styloflexa")

laxi<-filter(d2,specificEpithet %in% laxiflorae)

laxi %>% group_by (specificEpithet) %>% count() %>% arrange(-n)

write.csv(d2,"Input/carexInput.csv",row.names = FALSE)
write.csv(acro,"Input/acrocystisInput.csv",row.names = FALSE)
write.csv(carey,"Input/careyanaeInput.csv",row.names = FALSE)
write.csv(laxi,"Input/laxifloraeInput.csv",row.names = FALSE)
nrow(laxi)+nrow(acro)+nrow(carey)
