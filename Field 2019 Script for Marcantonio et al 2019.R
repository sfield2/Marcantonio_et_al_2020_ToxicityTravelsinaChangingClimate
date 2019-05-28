### Script for all anlyses written by Sean Field M.A., reported in
### R. Marcantonio, S. Field, P. Regan. 2019. Toxcity Travels In A Changing Climate. Submitted to the Journal of Global Enviornmental Change


############# REQUIRED LIBRARIES ################
library('units')
library('sf')
library('sp')
library('rgdal')
library('dplyr')
library('measurements')
library('raster')
library('rgeos')
library('spdep')
library('maptools')

#setwd
#################################### NATIONAL ASSESSMENT ##################
##### READ DATA
#census tract spatial data
sub_tract <- readOGR("C:/Users/seanp/Documents/R/ENVIRON/DATA",layer="sub-tract")
UAA_sub <- read.csv("C:/Users/seanp/Documents/R/ENVIRON/DATA/UAA_Data/Sub-City Indicators_1.csv",header=T)

#contaminated sites listed by CIMC
FC <- read.csv("C:/Users/seanp/Documents/R/ENVIRON/DATA/CIMC/CONTANIMATED_SITE_FULL.csv",header=T)
FC_datum <- matrix(NA,nrow=nrow(FC),ncol=2)
FC_datum[,1]<-FC$Longitude
FC_datum[,2]<-FC$Latitude
FC_site <- SpatialPointsDataFrame(coords=FC_datum,FC,proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

########################################## SORT DATA ##########################################################

#query full list by specified subsets downloaded from CIMC list
#superfund sites
SF <- read.csv("C:/Users/seanp/Documents/R/ENVIRON/DATA/CIMC/SUPERFUND_CIMC.csv",header=T)
SF_site <- subset(FC_site, Map.Site.CSV %in% SF$Map.Site.CSV)
SF_ID <- c(1:1417)
SF_site_ID <- cbind(SF_site,SF_ID)
#writeOGR(SF_site,".","Superfund_site",driver='ESRI Shapefile')

#brownfield sites
BF <- read.csv("C:/Users/seanp/Documents/R/ENVIRON/DATA/CIMC/BROWNFIELD_CIMC.csv",header=T)
BF_site <- subset(FC_site, Map.Site.CSV %in% BF$Map.Site.CSV)
BF_ID <- c(1:24304)
BF_site_ID <- cbind(BF_site,BF_ID)
#writeOGR(BF_site,".","Brownfield_site",driver='ESRI Shapefile')

#RCRA corrective action sites 
RCRA <- read.csv("C:/Users/seanp/Documents/R/ENVIRON/DATA/CIMC/RCRA_CA_CIMC.csv",header=T)
RCRA_site <- subset(FC_site, Map.Site.CSV %in% RCRA$Map.Site.CSV)
RCRA_ID<-c(1:3723)
RCRA_site_ID <- cbind(RCRA_site,RCRA_ID)
#writeOGR(RCRA_site,".","RCRA_site",driver='ESRI Shapefile')

#merge all subsets 
ALL_site <- bind(RCRA_site,BF_site,SF_site)
site_ID <- c(1:29444)
ALL_site_ID <- cbind(ALL_site,site_ID)
#writeOGR(ALL_site,".","Allcontaminatedsite",driver='ESRI Shapefile')

# sub_tract by UAA_sub poly
sub_tract_UAA <- subset(sub_tract, GEOID_Data %in% UAA_sub$Geo.ID)
#UAA sub social data
sum(UAA_sub$Population)
sum(UAA_sub$Population.in.High.Risk.Flood.Zones)
mean(UAA_sub$Median.Household.Income) 
mean(as.numeric(UAA_sub$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

##################################################### ANALYSIS  #######################################################################
##### SUPERFUND SITES IN UAA SUB #####
SF_sub_sites <- over(SF_site_ID,sub_tract_UAA)
# NUMBER OF SUBTRACTS
SF_sub_withsites_tot <- SF_sub_sites %>% distinct()
SF_sub_withsites_unique <- SF_sub_withsites_tot[complete.cases(SF_sub_withsites_tot),]
# plotable
SF_sub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% SF_sub_withsites_unique$GEOID_Data)
# NUMBER OF SITES
SF_sub_sites_ID <- cbind(SF_sub_sites,SF_ID)
SF_subsites <- SF_sub_sites_ID[complete.cases(SF_sub_sites_ID),]
# plotable
SF_sites_in_sub <- subset(SF_site_ID, X1.1417 %in% SF_subsites$SF_ID)
#writeOGR(SF_sites_in_sub,".","Superfundsite_insub",driver='ESRI Shapefile')
# SOCIAL VARIABLES
SF_sub_withsites_soc <- subset(UAA_sub, Geo.ID %in% SF_sub_withsites_unique$GEOID_Data)
sum(SF_sub_withsites_soc$Population)
sum(SF_sub_withsites_soc$Population.in.High.Risk.Flood.Zones)
mean(SF_sub_withsites_soc$Median.Household.Income) 
mean(as.numeric(SF_sub_withsites_soc$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

##### BROWNFIELD SITES IN UAA SUB #####
BF_sub_sites <- over(BF_site_ID,sub_tract_UAA)
# NUMBER OF SUBTRACTS
BF_sub_withsites_tot <- BF_sub_sites %>% distinct()
BF_sub_withsites_unique <- BF_sub_withsites_tot[complete.cases(BF_sub_withsites_tot),]
# plotable
BF_sub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% BF_sub_withsites_unique$GEOID_Data)
# NUMBER OF SITES
BF_sub_sites_ID <- cbind(BF_sub_sites,BF_ID)
BF_subsites <- BF_sub_sites_ID[complete.cases(BF_sub_sites_ID),]
# plotable
BF_sites_in_sub <- subset(BF_site_ID, X1.24304 %in% BF_subsites$BF_ID)
#writeOGR(BF_sites_in_sub,".","Brownfieldsite_insub",driver='ESRI Shapefile')
# SOCIAL VARIABLES
BF_sub_withsites_soc <- subset(UAA_sub, Geo.ID %in% BF_sub_withsites_unique$GEOID_Data)
sum(BF_sub_withsites_soc$Population)
sum(BF_sub_withsites_soc$Population.in.High.Risk.Flood.Zones)
mean(BF_sub_withsites_soc$Median.Household.Income) 
mean(as.numeric(BF_sub_withsites_soc$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

##### RCRA_CA SITES IN UAA SUB #####
RCRA_sub_sites <- over(RCRA_site_ID,sub_tract_UAA)
# NUMBER OF SUBTRACTS
RCRA_sub_withsites_tot <- RCRA_sub_sites %>% distinct()
RCRA_sub_withsites_unique <- RCRA_sub_withsites_tot[complete.cases(RCRA_sub_withsites_tot),]
# plotable
RCRA_sub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% RCRA_sub_withsites_unique$GEOID_Data)
# NUMBER OF SITES
RCRA_sub_sites_ID <- cbind(RCRA_sub_sites,RCRA_ID)
RCRA_subsites <- RCRA_sub_sites_ID[complete.cases(RCRA_sub_sites_ID),]
# plotable
RCRA_sites_in_sub <- subset(RCRA_site_ID, X1.3723 %in% RCRA_subsites$RCRA_ID)
#writeOGR(RCRA_sites_in_sub,".","RCRAsite_insub",driver='ESRI Shapefile')
# SOCIAL VARIABLES
RCRA_sub_withsites_soc <- subset(UAA_sub, Geo.ID %in% RCRA_sub_withsites_unique$GEOID_Data)
sum(RCRA_sub_withsites_soc$Population)
sum(RCRA_sub_withsites_soc$Population.in.High.Risk.Flood.Zones)
mean(RCRA_sub_withsites_soc$Median.Household.Income) 
mean(as.numeric(RCRA_sub_withsites_soc$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

##### ALL (SUPERFUND,RCRA,BROWNFIELD) SITES IN UAA SUB  #####
sub_sites <- over(ALL_site_ID,sub_tract_UAA)
# NUMBER OF SUBTRACTS
sub_withsites_tot <- sub_sites %>% distinct()
sub_withsites_unique <- sub_withsites_tot[complete.cases(sub_withsites_tot),]
# plotable
sub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% sub_withsites_unique$GEOID_Data)
# NUMBER OF SITES
sub_sites_ID <- cbind(sub_sites,site_ID)
subsites <- sub_sites_ID[complete.cases(sub_sites_ID),]
# plotable
sites_in_sub <- subset(ALL_site_ID, site_ID %in% subsites$site_ID)
#writeOGR(sites_in_sub,".","Allcontaminatedsite_insub",driver='ESRI Shapefile')
# SOCIAL VARIABLES
sub_withsites_soc <- subset(UAA_sub, Geo.ID %in% sub_withsites_unique$GEOID_Data)
sum(sub_withsites_soc$Population)
sum(sub_withsites_soc$Population.in.High.Risk.Flood.Zones)
mean(sub_withsites_soc$Median.Household.Income)
mean(as.numeric(sub_withsites_soc$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)



##### ALL SUB WITH ANY TYPE OF CONTAMINATED SITE AND THEIR NEIGHBORS ######
sub_ID <- c(1:25569)
sub_tract_ID <- cbind(sub_tract_UAA,sub_ID)

allsubsite_UTM<- spTransform(sub_with_sites,CRS("+proj=utm +datum=NAD83 +zone=12 +ellps=GRS80 +towgs84=0,0,0"))
allsubsite_buff<-gBuffer(allsubsite_UTM,width=1000,byid = TRUE)
all_buff <- spTransform(allsubsite_buff,CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
all_neighbors <- over(sub_tract_UAA,all_buff)
all_neighbors_ID <- cbind(all_neighbors,sub_ID)
all_neighbors_unique <- all_neighbors_ID[complete.cases(all_neighbors_ID),]
all_neighbors_all <- subset(sub_tract_ID,X1.25569 %in% all_neighbors_unique$sub_ID)
all_neighbors_sub_soc <- subset(UAA_sub, Geo.ID %in% all_neighbors_all$GEOID_Data)
all_neighbors_UAA_poly <- subset(sub_tract_UAA, GEOID_Data %in% all_neighbors_sub_soc$Geo.ID)

##### JUST THE NEIGHBORS 
only_allneighbors_sub_social <- subset(all_neighbors_sub_soc, !(Geo.ID %in% sub_with_sites$GEOID_Data))
only_allneighbors_UAA_poly <- subset(sub_tract_UAA, GEOID_Data %in% only_allneighbors_sub_social$Geo.ID)
# SOCIAL
sum(only_allneighbors_sub_social$Population)
sum(only_allneighbors_sub_social$Population.in.High.Risk.Flood.Zones)
mean(as.numeric(only_allneighbors_sub_social$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

#### ALL OTHER SUBS WITHOUT CONTAMINATED SITE#####
UAA_sub_non <- subset(UAA_sub, !(Geo.ID %in% sub_with_sites$GEOID_Data))
sum(UAA_sub_non$Population)
sum(UAA_sub_non$Population.in.High.Risk.Flood.Zones)
mean(as.numeric(UAA_sub_non$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)

### ALL SUBS THAT DO NOT HAVE CONTAMINATED SITE AND ARE NOT A NEIGHBOR ####
UAA_sub_far <- subset(UAA_sub, !(Geo.ID %in% all_neighbors_sub_soc$Geo.ID))
sum(UAA_sub_far$Population)
sum(UAA_sub_far$Population.in.High.Risk.Flood.Zones)
mean(as.numeric(UAA_sub_far$Percent.of.Pop.in.High.Risk.Flood.Zone),na.rm=T)



############################################# HOUSTON CASE STUDY  #####################################################
# FLOOD DATA
harvey <- readOGR("C:/Users/seanp/Documents/R/ENVIRON/DATA",layer="houston_flood_boundaries")
harvey_CRS <-spTransform(harvey,CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# SUB TRACTS IN HOUSTON 
houston_sub_UAA <- subset(UAA_sub, City == 'Houston')
sub_tract_UAA_houston <- subset(sub_tract, GEOID_Data %in% houston_sub_UAA$Geo.ID)
writeOGR(sub_tract_UAA_houston,".","houston_sub",driver='ESRI Shapefile')

### SITES IN HOUSTON
FC_houston <- over(FC,sub_tract_UAA_houston)
FC_houston_ID <- cbind(FC_houston,site_ID)
sites_houston <- FC_houston_ID[complete.cases(FC_houston_ID),]
#plotables
sites_in_houston <- subset(FC_site_ID, X1.29444 %in% sites_houston$site_ID)
writeOGR(sites_in_houston,".","houston_sites",driver='ESRI Shapefile')
### BY SUBTRACT
h_tot <- FC_houston %>% distinct()
h_unique <- h_tot[complete.cases(h_tot),]
# plotable
hsub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% h_unique$GEOID_Data)
writeOGR(hsub_with_sites,".","houstonsub_sites",driver='ESRI Shapefile')
# SOCIAL VARIABLES
h_soc <- subset(UAA_sub, Geo.ID %in% h_unique$GEOID_Data)
sum(h_soc$Population)
sum(h_soc$Population.in.High.Risk.Flood.Zones)
mean(h_soc$Median.Household.Income)

### SITES IN HARVEY 
FC_site_harvey_all <- over(FC,harvey_CRS)
FC_site_harveyall_ID <-cbind(FC_site_harvey_all,site_ID)
sites_harvey <- FC_site_harveyall_ID[complete.cases(FC_site_harveyall_ID),]
#plotable
sites_in_harvey <- subset(FC_site_ID, X1.29444 %in% sites_harvey$site_ID)
writeOGR(sites_in_harvey,".","harvey_sites",driver='ESRI Shapefile')
harvey_ID <- c(1:31)
sites_in_harvey_ID <- cbind(sites_in_harvey,harvey_ID)

### SITES IN HARVEY & HOUSTON
FC_hh <- over(sites_in_harvey_ID,sub_tract_UAA_houston)
FC_hh_ID <- cbind(FC_hh,harvey_ID)
sites_hh <- FC_hh_ID[complete.cases(FC_hh_ID),]
#plotable
sites_in_hh <- subset(sites_in_harvey_ID, X1.31 %in% sites_hh$harvey_ID)
writeOGR(sites_in_hh,".","houstonharvey_sites",driver='ESRI Shapefile')
### BY SUBTRACT
hh_tot <- FC_hh %>% distinct()
hh_unique <- hh_tot[complete.cases(hh_tot),]
# plotable
hhsub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% hh_unique$GEOID_Data)
writeOGR(hhsub_with_sites,".","houstonharveysub_sites",driver='ESRI Shapefile')
# SOCIAL VARIABLES
hh_soc <- subset(UAA_sub, Geo.ID %in% hh_unique$GEOID_Data)
sum(hh_soc$Population)
sum(hh_soc$Population.in.High.Risk.Flood.Zones)
mean(hh_soc$Median.Household.Income)

### SITES NOT IN HARVEY BUT IN HOUSTON
FC_hnh <- subset(sites_in_houston, !(X1.29444 %in% sites_in_hh$X1.29444))
writeOGR(FC_hnh,".","houstonnotharvey_sites",driver='ESRI Shapefile')
# BY SUBTRACT (SOME OF THESE SUBTRACTS MAY HAVE MULTIPLE SITES, ONLY WITH A PERCENTAGE OF THOSE SITES IN HARVEY)
hnh_sub <- over(FC_hnh,sub_tract_UAA_houston)
hnh_tot <- hnh_sub %>% distinct()
hnh_unique <- hnh_tot[complete.cases(hnh_tot),]
#plotable 
hnhsub_with_sites <- subset(sub_tract_UAA, GEOID_Data %in% hnh_unique$GEOID_Data)
writeOGR(hnhsub_with_sites,".","houstonnotharveysub_sites",driver='ESRI Shapefile')
# SOCIAL VARIABLES
hnh_soc <- subset(UAA_sub, Geo.ID %in% hnh_unique$GEOID_Data)
sum(hnh_soc$Population)
sum(hnh_soc$Population.in.High.Risk.Flood.Zones)
mean(hnh_soc$Median.Household.Income)

### SUBTRACT WITH SITE(S) IN HOUSTON NONE OF WHICH ARE IN HARVEY
hnhsub_unique <- subset(hnh_unique, !(GEOID_Data %in% hh_unique$GEOID_Data))
# plotable
hnhsub_with_sites_unique <- subset(sub_tract_UAA, GEOID_Data %in% hnhsub_unique$GEOID_Data)
writeOGR(hnhsub_with_sites_unique, ".","houstonnotharveysub_sites_unique",driver='ESRI Shapefile')
# SOCIAL VARIABLES
hnh_unique_soc <- subset(UAA_sub, Geo.ID %in% hnhsub_unique$GEOID_Data)
sum(hnh_unique_soc$Population)
sum(hnh_unique_soc$Population.in.High.Risk.Flood.Zones)
mean(hnh_unique_soc$Median.Household.Income)

### ALL OTHER SUBTRACTS THAT DO NOT HAVE SITES 
houston_nosite <- subset(sub_tract_UAA_houston, !(GEOID_Data %in% hsub_with_sites$GEOID_Data))
writeOGR(houston_nosite, ".","houstonsub_nosites",driver='ESRI Shapefile')
#SOCIAL VARIABLES
houstonnosite_soc <- subset(UAA_sub, Geo.ID %in% houston_nosite$GEOID_Data)
sum(houstonnosite_soc$Population)
sum(houstonnosite_soc$Population.in.High.Risk.Flood.Zones)
mean(houstonnosite_soc$Median.Household.Income)





