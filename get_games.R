library(XML)

setwd('C:/Users/A097092/Desktop/Extra/NCAAB Elo/games')

files <- dir()

all_games <- lapply(files, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
get_col <- function(col_num) sapply(xpathSApply(the_tree, paste0('//table/tbody/tr/td[',col_num,']')),function(y) xmlValue(y, trim=T))

heads <- sapply(xpathSApply(the_tree, '//table/thead/tr[2]/th/text()'),function(y) xmlValue(y, trim=T))
heads_prefix <- c(rep('',7),rep('tm ',18),rep('opp ',18)) 
heads <- paste0(heads_prefix,heads)

gameid <- xpathSApply(the_tree, '//table/tbody/tr/td[1]/a/@href')

df_all <- data.frame(gameid,sapply(1:42, get_col),stringsAsFactors=F)
names(df_all)[-c(1)] <- heads[-c(1)]
names(df_all)[4] <- 'Home'

df_all
})

df_games <- data.frame(do.call(rbind,all_games))

df_games[,7:43] <- lapply(df_games[,7:43],as.numeric)
df_games$Date <- as.Date(df_games$Date)

begin_szn <- sapply(2011:2018, function(x) min(df_games$Date[which(df_games$Date >= as.Date(paste0(x-1,'-06-01')) & df_games$Date < as.Date(paste0(x,'-06-01')))]))
end_szn <- sapply(2011:2018, function(x) max(df_games$Date[which(df_games$Date >= as.Date(paste0(x-1,'-06-01')) & df_games$Date < as.Date(paste0(x,'-06-01')))]))
Schools <- sort(names(rev(sort(table(df_games$Schl))))[1:351])

df_games$Date[which(df_games$gameid=='/cbb/boxscores/2012-12-23-hawaii.html')] <- as.Date('2012-12-24')
df_games$Date[which(df_games$gameid=='/cbb/boxscores/2014-11-14-tennessee-tech.html')] <- as.Date('2014-11-15')
df_games$Date[which(df_games$gameid=='/cbb/boxscores/2014-12-22-portland.html')] <- as.Date('2014-12-23')

df_games$Margin <- df_games$tm.PTS - df_games$opp.PTS
df_games$Win <- ifelse(df_games$Margin>0, 1, 0)

setwd('C:/Users/A097092/Desktop/Extra/NCAAB Elo')
elo_ratings <- read.csv('2012-13 Final Elo Ratings.csv')
###
#sapply(seq(90,110,by=5),function(z) {
home_adv <- 95
df_games$HomeAdv <- ifelse(df_games$Home=='@',-home_adv,ifelse(df_games$Home=='',home_adv,0))

K <- 50
margin_adj <- function(mar, elo_d) ((abs(mar)+3)^.8)/(7.5+.006*ifelse(mar*elo_d>0,abs(elo_d),-abs(elo_d)))

szn <- 6
old_elo <- mean(elo_ratings$Current)*(1/3) + elo_ratings$Current*(2/3)
all_elo <- elo_calc_df[-c(1:nrow(elo_calc_df)),]
#old_elo <- 1500
elo_ratings <- data.frame('Schl'=Schools, Begin=old_elo, Current=old_elo)
###
games_byDay <- lapply(begin_szn[szn]:end_szn[szn], function(x) df_games[which(df_games$Date==x),])

#check for double headers
which(sapply(games_byDay, function(x) length(x$Schl) - length(unique(x$Schl)))>0)
#sort(table(games_byDay[[39]]$Schl))
#games_byDay[[39]][which(games_byDay[[39]]$Schl=='Lewis & Clark'),]

for (day in 1:length(games_byDay)) {
elo_calc_df <- merge(games_byDay[[day]], elo_ratings[,c('Schl','Current')],all.x=T)
elo_calc_df <- merge(elo_calc_df, elo_ratings[,c('Schl','Current')], by.x=c('Opp'), by.y=c('Schl'),all.x=T)
names(elo_calc_df)[(ncol(elo_calc_df)-1):ncol(elo_calc_df)] <- c('MyElo', 'OppElo')

elo_calc_df$MyElo[is.na(elo_calc_df$MyElo)] <- 900
elo_calc_df$OppElo[is.na(elo_calc_df$OppElo)] <- 900

elo_calc_df$Elo_Diff <- elo_calc_df$MyElo - elo_calc_df$OppElo + elo_calc_df$HomeAdv
elo_calc_df$Elo_WinProb <- 1/(10^(-elo_calc_df$Elo_Diff/400)+1)

elo_calc_df$Elo_New <- elo_calc_df$MyElo + (elo_calc_df$Win - elo_calc_df$Elo_WinProb) * K * margin_adj(elo_calc_df$Margin, elo_calc_df$Elo_Diff)

elo_ratings <- merge(elo_calc_df[,c('Schl','Elo_New')], elo_ratings, all.y=T)
elo_ratings$Elo_New[which(is.na(elo_ratings$Elo_New))] <- elo_ratings$Current[which(is.na(elo_ratings$Elo_New))]
elo_ratings$Current <- elo_ratings$Elo_New
names(elo_ratings)[which(names(elo_ratings)=='Elo_New')] <- paste0('Day_',day)

all_elo <- rbind(all_elo,elo_calc_df)
}

#sum(all_elo$Elo_WinProb)*2
#nrow(all_elo)/2
all_elo <- all_elo[match(unique(all_elo$gameid),all_elo$gameid),]

#rsq
get_psudo_RSQ <- function(rows) {
res.dev <- -2 * sum(ifelse(all_elo$Win[rows]==1, log(all_elo$Elo_WinProb[rows]), log(1-all_elo$Elo_WinProb[rows])))
null.dev <- -2 * sum(ifelse(all_elo$Win[rows]==1, log(mean(all_elo$Elo_WinProb[rows])), log(1-mean(all_elo$Elo_WinProb[rows]))))
1-res.dev/null.dev
}
#LogLoss
get_LogLoss <- function(rows) {
sum(all_elo$Win[rows]*log(all_elo$Elo_WinProb[rows]) + (1-all_elo$Win[rows])*log(1-all_elo$Elo_WinProb[rows]))/-length(all_elo$Win[rows])
}
#})

get_LogLoss()
#####
get_LogLoss(which(all_elo$Home=='@'))
get_LogLoss(which(all_elo$Home=='N'))
get_LogLoss(which(all_elo$Home==''))
get_LogLoss(which(all_elo$Date >= '2015-03-15'))

get_LogLoss(which(all_elo$Date >= '2015-03-19' & all_elo$Home=='N'))

get_psudo_RSQ(which(all_elo$Home=='@'))
get_psudo_RSQ(which(all_elo$Home=='N'))
get_psudo_RSQ(which(all_elo$Home==''))

get_psudo_RSQ(which(all_elo$Date >= '2014-01-01'))

Team_RSQ <- sapply(Schools, function(x) get_psudo_RSQ(which(all_elo$Schl==x)))
Szn_Proj <- t(sapply(Schools,function(x) cbind(sum(all_elo$Elo_WinProb[which(all_elo$Schl==x)]),sum(all_elo$Win[which(all_elo$Schl==x)]))))

team_diff <- cbind(Szn_Proj,Team_RSQ,Szn_Proj[,1]-Szn_Proj[,2])
team_diff[order(-team_diff[,3]),]

merge(Team_RSQ,Szn_Proj)

cbind(1:10,elo_ratings[rev(order(elo_ratings$Current))[1:10],c(1:2)])

cuts <- cut(all_elo$Elo_WinProb,seq(0,1,.05))
cbind(levels(cuts),sapply(1:20, function(x) mean(all_elo$Win[which(cuts==levels(cuts)[x])])))

write.csv(elo_ratings,paste0(szn+2009,'-',szn+10,' Final Elo Ratings.csv'))
str(all_elo)

summary(elo_ratings$Current)
