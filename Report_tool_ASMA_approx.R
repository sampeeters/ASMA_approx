
library(devtools)
library(mailR)
library(ROracle)
library(reshape2)
library(rmarkdown)
suppressMessages(library(dplyr))

dir="//hhbruna30/dgof-pru$/Project/Vertical_flight_efficiency/2015/ASMA_approx/"
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
drv <- dbDriver("Oracle")
con <- dbConnect(drv, "PRUTEST", "test", dbname='//porape5.ops.cfmu.eurocontrol.be:1521/pe5')
Allowed_apts=dbGetQuery(con, paste0("SELECT *
                                FROM SP_TOP_APTS
                                WHERE YEAR>2014
                                ORDER BY year, -nbr_mvmts"))
dbDisconnect(con)

Airport="EIDW"

ASMA_results_airport_monthly=readRDS('Results/ASMA_results_airport_monthly.RDS') %>% 
  filter(ADES==Airport, AIRSPACE_ID=="L40") %>% 
  ungroup() %>% 
  mutate(MONTH_NUM=match(Month, month.abb), Year=as.numeric(as.character(Year)))
Last_av_year<<-max(ASMA_results_airport_monthly$Year)
Last_av_month<<-filter(ASMA_results_airport_monthly, Year==Last_av_year) %>% 
  select(MONTH_NUM) %>% 
  as.numeric()


if (Airport %in% Allowed_apts$AIRPORT) {
  
  attempt=1
  while(!file.exists(paste0(dir, 'Reports/ASMA approximation ', Airport, ' ',
                            ifelse(Last_av_month>=10, Last_av_month, paste0("0", Last_av_month)), '_', Last_av_year, '.pdf')) && attempt <= 3) {
    attempt <- attempt + 1
    tryCatch(
      {
        
        rmarkdown::render(paste0(dir, 'Report_Add_ASMA_approx.Rmd'),
                          output_file = paste0(dir, 'Reports/ASMA approximation ', Airport, ' ',
                                               ifelse(Last_av_month>=10, Last_av_month, paste0("0", Last_av_month)), '_', Last_av_year, '.pdf'))
        
      },
      error = function(e){
        if (attempt==4) {
          
          send.mail(from = "pru-support@eurocontrol.int",
                    to = "sam.peeters@eurocontrol.int",
                    subject = paste0("FAILED Report ASMA approximation ", Airport),
                    body = paste0("Automatic generation of ASMA approximation report for ", Airport, " failed!"),
                    smtp = list(host.name = "mailservices.eurocontrol.int", port = 25),
                    authenticate = FALSE,
                    send = TRUE)
          
        } else {
          
          print(e)
          
        }
      }
    )
  }
  
  # Send report to requester
  
  send.mail(from = "pru-support@eurocontrol.int",
            to = "sam.peeters@eurocontrol.int",
            cc=c("sam.peeters@eurocontrol.int","rainer.koelle@eurocontrol.int", "pru-support@eurocontrol.int"),
            replyTo = "sam.peeters@eurocontrol.int",
            subject = paste0("ASMA approximation report ", Airport),
            body = paste0("Dear all,\n\nPlease find enclosed the ASMA approximation report for ", Airport,
                          ".\n\nPlease don't hesitate to contact us if you have any feedback or further questions.\n\n",
                          "Kind regards\n\nPRU support"),
            smtp = list(host.name = "mailservices.eurocontrol.int", port = 25),
            attach.files=paste0(dir, 'Reports/ASMA approximation ', Airport, ' ',
                                ifelse(Last_av_month>=10, Last_av_month, paste0("0", Last_av_month)), '_', Last_av_year, '.pdf'),
            authenticate = FALSE,
            send = TRUE)
  
}

print(Sys.time())
