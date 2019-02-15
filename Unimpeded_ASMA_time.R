
library(dplyr)
library(ROracle)
library(geosphere)
library(lubridate)

Sys.setenv(TZ = "UTC")
Sys.setenv(ORA_SDTZ = "UTC")

Airport="EIDW"

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
Flx_data=dbGetQuery(con, paste0("SELECT ID, LOBT, ADEP, ADES, AIRCRAFT_TYPE_ICAO_ID
                                FROM FLX.FLIGHT
                                WHERE LOBT >= '01-jan-2016'
                                AND LOBT < '01-jan-2017'
                                AND FLT_RULES='I'
                                AND ADES='", Airport, "'"))
Circle_data=dbGetQuery(con, paste0("SELECT SAM_ID, LOBT, ENTRY_TIME, EXIT_TIME, ENTRY_LAT, ENTRY_LON, AIRSPACE_ID
                                   FROM FSD.ALL_FT_CIRCLE_PROFILE
                                   WHERE LOBT >= '01-jan-2016'
                                   AND LOBT < '01-jan-2017'
                                   AND model_type = 'CPF'
                                   AND AIRSPACE_ID IN ('L40','L100')"))
APT_data <- dbGetQuery(con, "SELECT * FROM SP_AIRPORT_INFO")
dbDisconnect(con)

con2 <- dbConnect(drv, "PRU_AIRPORT", "ria7pa", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
ASMA_sectors=dbGetQuery(con2, "SELECT * FROM ASMA_SECTOR") %>% 
  mutate(ASMA_RADIUS=paste0("L", ASMA_RADIUS))
dbDisconnect(con2)

Flight_data=inner_join(Flx_data, Circle_data, by=c("ID"="SAM_ID", "LOBT"="LOBT")) %>% 
  arrange(ADES, AIRSPACE_ID, EXIT_TIME)
Acft_groups=readRDS("Data/Acft_groups.RDS")

# Split ASMA sectors which include 0°
ASMA_sectors=mutate(ASMA_sectors, FROM_BEARING=ifelse(FROM_BEARING==360, 0, FROM_BEARING), TO_BEARING=ifelse(TO_BEARING==360, 0, TO_BEARING)) %>% 
  filter(!is.na(AIRPORT)) %>% 
  arrange(AIRPORT, ASMA_RADIUS, FROM_BEARING)
ASMA_sectors_ext=data.frame()
for (APT in unique(ASMA_sectors$AIRPORT)) {
  for (RADIUS in c("L40", "L100")) {
    temp=filter(ASMA_sectors, AIRPORT==APT, ASMA_RADIUS==RADIUS) %>% 
      arrange(FROM_BEARING)
    bearing_min=min(temp$TO_BEARING)
    
    if (bearing_min!=0) {
      temp[nrow(temp)+1,]=temp[nrow(temp),]
      temp$TO_BEARING[nrow(temp)-1]=360
      temp$FROM_BEARING[nrow(temp)]=0
    } else {
      temp$TO_BEARING[nrow(temp)]=360
    }
    
    ASMA_sectors_ext=rbind(ASMA_sectors_ext, temp)
    
  }
}

## Step 1: Filtering

Flight_data_filter=filter(Flight_data, !is.na(ENTRY_TIME) & !is.na(EXIT_TIME) & ADES %in% ASMA_sectors_ext$AIRPORT) %>% 
  mutate(AcASMA=difftime(EXIT_TIME, ENTRY_TIME, units="hours")) %>% 
  filter(AcASMA<2
         # & !ACFT_CAT=="H"
  )


## Step 2 + Step 3b: Computations at flight level: ASMA Time (see Step 1), Congestion level + Calculation of Arrival Throughput

Flight_data_filter$Cong_lvl=NA
Flight_data_filter$Hourly_rate=NA
Flight_data_cong=data.frame()
for (APT in unique(Flight_data_filter$ADES)) {
  for (RADIUS in c("L40", "L100")) {
    temp=filter(Flight_data_filter, ADES==APT, AIRSPACE_ID==RADIUS)
    if (nrow(temp)>0) {
      for (i in seq(1, nrow(temp))) {
        temp$Cong_lvl[i]=nrow(filter(temp, temp$ENTRY_TIME[i]<=EXIT_TIME & EXIT_TIME<=temp$EXIT_TIME[i]))-1
        temp$Hourly_rate[i]=(nrow(filter(temp, temp$EXIT_TIME[i]-20*60<=EXIT_TIME & EXIT_TIME<=temp$EXIT_TIME[i]))-1)/
          as.numeric(difftime(temp$EXIT_TIME[i],min(filter(temp, temp$EXIT_TIME[i]-20*60<=EXIT_TIME & EXIT_TIME<=temp$EXIT_TIME[i])$EXIT_TIME), units="hours"))
      }
      Flight_data_cong=rbind(Flight_data_cong, temp)
      print(paste0(APT, "-", RADIUS, "-", nrow(temp)))
    }
  }
}

## Step 3: Computation of the Saturation level

cl=0.5

Peak_hourly_arr_throughput=group_by(Flight_data_cong, ADES) %>% 
  summarise(R=quantile(Hourly_rate, probs=0.9, na.rm=TRUE))

Flight_data_group=left_join(Flight_data_cong, select(APT_data, ICAO_CODE, ARP_LAT, ARP_LON), by=c("ADES"="ICAO_CODE")) %>% 
  mutate(Bearing=bearingRhumb(cbind(ARP_LON, ARP_LAT), cbind(ENTRY_LON, ENTRY_LAT))) %>% 
  left_join(ASMA_sectors_ext, by=c("ADES"="AIRPORT", "AIRSPACE_ID"="ASMA_RADIUS")) %>% 
  filter(Bearing>=FROM_BEARING & Bearing<TO_BEARING) %>% 
  left_join(Acft_groups, by=c("AIRCRAFT_TYPE_ICAO_ID"="ARCTYP")) %>% 
  mutate(FLT_GROUP=paste0(AIRSPACE_ID, "-", ASMA_SECTOR, "-", AC_CLASS))

Flight_data_sat=group_by(Flight_data_group, ADES, FLT_GROUP) %>% 
  summarise(U1=as.numeric(quantile(AcASMA, probs = 0.2)))

Sat_lvl=left_join(Peak_hourly_arr_throughput, Flight_data_sat, by=c("ADES")) %>% 
  mutate(L=round(R*U1))

## Step 4: Identification of unimpeded flights

Flight_data_unimp=left_join(Flight_data_group, Sat_lvl, by=c("ADES", "FLT_GROUP")) %>% 
  mutate(Unimpeded=ifelse(Cong_lvl<=(cl*L), TRUE, FALSE))


## Step 5: Computation of unimpeded time per grouping

Unimp_ASMA=filter(Flight_data_unimp, Unimpeded==TRUE,
         (as.numeric(format(EXIT_TIME, "%H")) + as.numeric(format(EXIT_TIME, "%M"))/60 + as.numeric(format(EXIT_TIME, "%S"))/3600)>=6.5,
         (as.numeric(format(EXIT_TIME, "%H")) + as.numeric(format(EXIT_TIME, "%M"))/60 + as.numeric(format(EXIT_TIME, "%S"))/3600)<=22) %>% 
  group_by(ADES, FLT_GROUP) %>% 
  summarise(UASMA=median(AcASMA),
            Nbr_flights=n()) %>% 
  filter(Nbr_flights>=20)
saveRDS(Unimp_ASMA, file="Results/Unimpeded_ASMA.RDS")



