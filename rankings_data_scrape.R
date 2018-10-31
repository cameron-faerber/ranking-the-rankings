library(stringr)
library(rvest)


# extremely hacky way of scraping data
year = 2012:2018
# load url
url = 'https://www.footballdb.com/college-football/rankings.html?yr=2018'
dat = list()
for(y in year){
  temp_url = sub("yr=.*", paste("yr=", y, sep=""), url)
  doc = readLines(temp_url)
  # find where poll AND week are defined for the given year
  index = grep("btn-group", doc) + 1
  # extract all href thingies
  pattern = 'href=".[^"]*"'
  href_poll = gsub('\"', "", sub("href=", "", unlist(str_extract_all(doc[index[1]], pattern))))
  href_poll = sub("rankings[.]html\\?yr=\\d{4}", "", href_poll)
  href_week = gsub('\"', "", sub("href=", "", unlist(str_extract_all(doc[index[2]], pattern))))
  href_week = sub("rankings[.]html\\?yr=\\d{4}&ranktype=.", "", href_week)
  # all pairwise combinations
  combinations = expand.grid(href_poll,href_week)
  combinations$url_final = paste(url, combinations$Var1, combinations$Var2, sep="")
  combinations$year = y
  combinations$poll = ifelse(gsub("&ranktype=", "", combinations$Var1)=="A","AP","Coaches")
  combinations$week = gsub("&rankdate=", "", combinations$Var2)
  # go through each URL's and obtain table for data extraction
  tab = combinations$url_final[1] %>% read_html() %>% html_nodes("table") %>% html_table(header=T)
  tab = tab[[1]]
  tab$year = combinations$year[1]
  tab$poll = combinations$poll[1]
  tab$week = combinations$week[1]
  for(i in 2:nrow(combinations)){
    temp_tab = combinations$url_final[i] %>% read_html() %>% html_nodes("table") %>% html_table(header=T)
    temp_tab = temp_tab[[1]]
    if(nrow(temp_tab)>0){
      temp_tab$year = combinations$year[i]
      temp_tab$poll = combinations$poll[i]
      temp_tab$week = combinations$week[i]
      tab = rbind(tab,temp_tab)
    }
  }
  # wide to long
  
  
  dat[[paste(y)]] = tab
}

rankings = do.call(rbind, dat)

# save as .csv
write.csv(rankings, "data/rankings.csv", row.names=F)
