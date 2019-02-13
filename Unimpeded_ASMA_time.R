
library(dplyr)
library(ROracle)
library(geosphere)

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
Flx_data=dbGetQuery(con, paste0("SELECT ID, LOBT, ADEP, ADES
                                FROM FLX.FLIGHT
                                WHERE LOBT >= '01-jan-2016'
                                AND LOBT < '01-jan-2017'
                                AND FLT_RULES='I'"))
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

Flight_data=inner_join(Flx_data, Circle_data, by=c("ID"="SAM_ID", "LOBT"="LOBT"))

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

Flight_data_filter=filter(Flight_data, !is.na(ENTRY_TIME) & !is.na(EXIT_TIME)) %>% 
  mutate(AcASMA=EXIT_TIME-ENTRY_TIME) %>% 
  filter(AcASMA<120*60
         # & !ACFT_CAT=="H"
  )


## Step 2: Computations at flight level: ASMA Time (see Step 1), Congestion level

Cong_lvl=data.frame()
for (flight_id in Flight_data_filter$ID) {
  
  temp=filter(Flight_data_filter, ID==flight_id)
  
}


## Step C: Calculation of Additional ASMA Time per flight

ASMA_results=mutate(Flight_data_group, AdASMA=AcASMA-UASMA)


## Step D: Calculation of the Additional ASMA Time per airport

ASMA_results_airport=group_by(ASMA_results, ADES) %>% 
  summarise(AdASMA_APT=mean(AdASMA))



