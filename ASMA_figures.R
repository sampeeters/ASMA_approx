
library(ggplot2)
library(scales)
library(dplyr)
library(ROracle)

Airport="EIDW"

ASMA_results_airport_daily=readRDS('Results/ASMA_results_airport_daily.RDS') %>% 
  filter(ADES==Airport)
ASMA_results_airport_monthly=readRDS('Results/ASMA_results_airport_monthly.RDS') %>% 
  filter(ADES==Airport) %>% 
  mutate(Date=as.Date(strptime(paste0("01-", Month, "-", Year),"%d-%b-%Y", tz="UTC")),
         Source="Traj",
         UASMA_APT=as.numeric(UASMA_APT)*60,
         AdASMA_APT=as.numeric(AdASMA_APT)*60) %>% 
  ungroup()
ASMA_results_airport_yearly=readRDS('Results/ASMA_results_airport_yearly.RDS') %>% 
  filter(ADES==Airport)

drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRU_AIRPORT", "ria7pa", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
ASMA_results_APT=rbind(dbGetQuery(con, paste0("SELECT * FROM ASMA_MM_RESULTS_2016 WHERE AIRPORT='", Airport, "'")),
                       dbGetQuery(con, paste0("SELECT * FROM ASMA_MM_RESULTS_2017 WHERE AIRPORT='", Airport, "'")),
                       dbGetQuery(con, paste0("SELECT * FROM ASMA_MM_RESULTS_2018 WHERE AIRPORT='", Airport, "'")),
                       dbGetQuery(con, paste0("SELECT * FROM ASMA_MM_RESULTS_2019 WHERE AIRPORT='", Airport, "'"))) %>% 
  mutate(Date=as.Date(strptime(paste0("01-", MONTH, "-", YEAR),"%d-%m-%Y", tz="UTC")),
         Source="APDF")
dbDisconnect(con)

res=rbind(select(ASMA_results_airport_monthly, Date, UASMA=UASMA_APT, AdASMA=AdASMA_APT, Source),
          select(ASMA_results_APT, Date, UASMA=UNIMPEDED_TIME, AdASMA=ADD_TIME_PER_ARR, Source))

ggplot() +
  geom_line(data = res, aes(x=Date, y=AdASMA, colour=Source), size=2) +
  theme_bw() +
  theme(axis.text=element_text(size=30),
        plot.title = element_text(size=40, face="bold", hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=36),
        legend.key.size = unit(2.5, 'lines'),
        legend.position="bottom",
        axis.title=element_text(size=38)) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  labs(x="", y="Additional ASMA time (min)\n", title=paste0("Additional ASMA time (", Airport,")")) +
  ylim(c(0, max(res$AdASMA)))
ggsave(paste0("Figures/Additional_ASMA_", Airport, ".png"), width = 50, height = 30, units = "cm", dpi=200)
