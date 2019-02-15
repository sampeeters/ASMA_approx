
library(ROracle)

con <- dbConnect(drv, "PRU_AIRPORT", "ria7pa", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
Acft_groups=dbGetQuery(con, "SELECT AC_CLASS, ARCTYP FROM STAT_MAIN_APT_DATA
                       WHERE SRC_DATE_FROM>='01-jan-2017'") %>% 
  unique()
dbDisconnect(con)

saveRDS(Acft_groups, file="Results/Acft_groups.RDS")