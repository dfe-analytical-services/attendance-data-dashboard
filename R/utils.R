extend_dummy_data <- function(nweeks,file='data/Weekly_dummy_data.csv',outfile='data/Weekly_dummy_extended.csv'){
  dfDummy3weeks <- read.csv(file,stringsAsFactors = FALSE)
  dfDummyExtended <- dfDummy3weeks
  nweeks_init <- length(unique(dfDummy3weeks$time_identifier))
  nrows       <- dfDummy3weeks %>% filter(time_identifier=='Week 1') %>% nrow()
  for (i in 1:nweeks){
    cat(paste(i,nrows),fill=TRUE)
    dfDummyExtended <- dfDummyExtended %>% 
      rbind(dfDummy3weeks %>% 
              filter(time_identifier=='Week 1') %>%
              mutate(
                time_identifier=paste("Week",i+nweeks_init),
                attendance_date=format(as.Date(attendance_date,"%d/%m/%Y")+
                  (i+nweeks_init)*7,"%d/%m/%Y"),
                possible_sessions=possible_sessions+sample.int(i*80000,nrows,replace=TRUE)))
  }
  write.csv(dfDummyExtended,file=outfile,row.names = FALSE)
  return(dfDummyExtended)
}