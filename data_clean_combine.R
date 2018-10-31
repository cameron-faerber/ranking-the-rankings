library(stringr) # for cleaning whitespace
library(anytime) # for formatting dateTime

# read in data
rankings = read.csv("data/rankings.csv", stringsAsFactors=F)
scores = read.csv("data/scores.csv", stringsAsFactors=F)


### clean rankings data ###
## team variable cleanup
# remove numbers and lower case
rankings$Team = gsub(" \\(\\d*\\)", "", rankings$Team) %>% tolower()
## dateTime
rankings$week = as.Date(rankings$week)
# separate AP and Coaches
coaches = rankings[rankings$poll=="Coaches",c("Rank","Team","week")]
names(coaches) = c("rank_coaches","team","week")
ap = rankings[rankings$poll=="AP",c("Rank","Team","week")]
names(ap) = c("rank_ap","team","week")
rankings = merge(coaches, ap, by.x=c("week", "team"), by.y=c("week", "team"), all=T)
### ###


### clean scores data ###
## winner / loser cleanup
scores$Winner = gsub("\\(\\d*\\)", "", scores$Winner) %>% str_trim()  %>% tolower()
scores$Loser = gsub("\\(\\d*\\)", "", scores$Loser) %>% str_trim()  %>% tolower()
## clean out pointless rows
scores = scores[-which(scores$Wk=="Wk"),]
## dateTime
temp = gsub(", ","-",scores$Date) %>% gsub(pattern=" ", replacement="-")
temp[grep("\\-\\d\\-", temp)] = paste(substring(temp[grep("\\-\\d\\-", temp)],1,4),"0",substring(temp[grep("\\-\\d\\-", temp)],5),sep="")
scores$Date = anydate(temp)
## pts rename
names(scores)[c(5,7)] = c("pts_w","pts_l")
### ###


### clean school names so they can merge ###
# rankings uses shorthand, scores does not; so we must look at these and change them by hand
sort(unique(rankings$team))
# byu, lsu, tcu, ucf, usc
rankings$team = ifelse(rankings$team == "byu", "brigham young", rankings$team)
rankings$team = ifelse(rankings$team == "lsu", "louisiana state", rankings$team)
rankings$team = ifelse(rankings$team == "tcu", "texas christian", rankings$team)
rankings$team = ifelse(rankings$team == "ucf", "central florida", rankings$team)
rankings$team = ifelse(rankings$team == "usc", "southern california", rankings$team)
rankings$team = ifelse(rankings$team == "louisiana-lafayette", "lafayette", rankings$team)
rankings$team = ifelse(rankings$team == "miami", "miami (fl)", rankings$team)

# check if they are all found ; (1) 
mean(unique(rankings$team) %in% unique(c(scores$Winner, scores$Loser)))
### ###


### crazy merge thingie-mabob ###
# for each week of rankings, identify week for the "scores"
weeks = sort(unique(rankings$week))
scores$week = Sys.Date()-365.25*2018
for(i in 1:(length(weeks)-1)){
  index = which(scores$Date>=weeks[i] & scores$Date<=weeks[i+1])
  scores$week[index] = weeks[i]
}
index = which(scores$Date>=weeks[length(weeks)])
scores$week[index] = weeks[length(weeks)]

# merge on week and winner
scores_merged = merge(scores, rankings, by.x=c("week","Winner"), by.y=c("week","team"), all.x=T)
names(scores_merged)[c(9,10)] = paste("w_",names(scores_merged)[c(9,10)],sep="")
scores_merged = merge(scores_merged, rankings, by.x=c("week","Loser"), by.y=c("week","team"), all.x=T)
names(scores_merged)[c(11,12)] = paste("l_",names(scores_merged)[c(11,12)],sep="")
# remove duplicated
scores_merged = scores_merged[!duplicated(scores_merged[,c("week","Loser","Winner","Day")]),]
### ###


### add in winner ranked boolean variable ###
# coaches
scores_merged$coach = ifelse(is.na(scores_merged$w_rank_coaches)==T & is.na(scores_merged$l_rank_coaches)==F, 0, ifelse(is.na(scores_merged$w_rank_coaches)==F & is.na(scores_merged$l_rank_coaches)==T, 1, ifelse(is.na(scores_merged$w_rank_coaches)==T & is.na(scores_merged$l_rank_coaches)==T, NA, ifelse(scores_merged$w_rank_coaches<scores_merged$l_rank_coaches, 1, 0))))
# ap
scores_merged$ap = ifelse(is.na(scores_merged$w_rank_ap)==T & is.na(scores_merged$l_rank_ap)==F, 0, ifelse(is.na(scores_merged$w_rank_ap)==F & is.na(scores_merged$l_rank_ap)==T, 1, ifelse(is.na(scores_merged$w_rank_ap)==T & is.na(scores_merged$l_rank_ap)==T, NA, ifelse(scores_merged$w_rank_ap<scores_merged$l_rank_ap, 1, 0))))
### ###


### add in week of season variable ###


### ###

# write to .csv
write.csv(scores_merged, "data/full_data.csv", row.names=F)
