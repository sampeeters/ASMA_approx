
library(ggplot2)
library(scales)

Airport="EIDW"

ASMA_results_airport_daily=readRDS('Results/ASMA_results_airport_daily.RDS') %>% 
  filter(ADES==Airport)
ASMA_results_airport_monthly=readRDS('Results/ASMA_results_airport_monthly.RDS') %>% 
  filter(ADES==Airport) %>% 
  mutate(Date=as.Date(strptime(paste0("01-", Month, "-", Year),"%d-%b-%Y", tz="UTC")))
ASMA_results_airport_yearly=readRDS('Results/ASMA_results_airport_yearly.RDS') %>% 
  filter(ADES==Airport)

ggplot() +
  geom_line(data = ASMA_results_airport_monthly, aes(x=Date, y=as.numeric(AdASMA_APT)*60), size=2, colour="blue") +
  theme_bw() +
  theme(axis.text=element_text(size=30),
        plot.title = element_text(size=40, face="bold", hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=36),
        legend.key.size = unit(2.5, 'lines'),
        legend.position="bottom",
        axis.title=element_text(size=38)) +
  scale_x_date(labels = date_format("%m-%Y")) +
  labs(x="", y="Additional ASMA time (min)\n", title=paste0("Additional ASMA time (", Airport,")")) +
  ylim(c(0, max(as.numeric(ASMA_results_airport_monthly$AdASMA_APT*60))))
ggsave(paste0("Figures/Additional_ASMA_", Airport, ".png"), width = 50, height = 30, units = "cm", dpi=200)
