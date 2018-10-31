library(rvest)

year = 2012:2018
url = "https://www.sports-reference.com/cfb/years/2012-schedule.html"

dat = list()
for(y in year){
  temp_url =  gsub('[[:digit:]]+', y, url)
  temp_tab = temp_url %>% read_html() %>% html_nodes("table") %>% html_table(header=T)
  temp_tab = temp_tab[[1]]
  temp_tab = temp_tab[,names(temp_tab)%in%c("Wk","Date","Day","Winner","Pts","Loser")]
  dat[[paste(y)]] = temp_tab
}
dat = do.call(rbind, dat)

# write to .csv
write.csv(dat, "data/scores.csv", row.names=F)
