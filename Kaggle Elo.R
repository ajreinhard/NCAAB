options(scipen=999)
setwd('C:/Users/A097092/Desktop/Extra/NCAAB Elo/Kaggle Data')

get_LogLoss <- function(rows) {
sum(log(Elo_games$Elo_WinProb[rows]))/-length(Elo_games$Elo_WinProb[rows])
}

Teams <- read.csv('Teams.csv',stringsAsFactor=F)
Reg_Szn <- read.csv('RegularSeasonDetailedResults.csv')
MM_Szn <- read.csv('TourneyDetailedResults.csv')
conf <- read.csv('conf map 2003-18.csv',stringsAsFactors=F)

all_games <- rbind(MM_Szn,Reg_Szn)
all_games <- merge(all_games,Teams,by.x='Wteam',by.y='Team_Id',all.x=T)
all_games <- merge(all_games,Teams,by.x='Lteam',by.y='Team_Id',all.x=T)

names(all_games)[which(names(all_games)=='Team_Name.x')] <- 'Winner'
names(all_games)[which(names(all_games)=='Team_Name.y')] <- 'Loser'
all_games$Margin <- all_games$Wscore - all_games$Lscore
all_games$Poss <- .5 * (all_games$Wfga + .475 * all_games$Wfta - all_games$Wor + all_games$Wto) + .5 * (all_games$Lfga + .475 * all_games$Lfta - all_games$Lor + all_games$Lto)
all_games$Pace <- 40 * (all_games$Poss / (.2 * (200+all_games$Numot*25)))
all_games$WOffRtg <- all_games$Wscore/all_games$Poss * 100
all_games$WDefRtg <- all_games$Lscore/all_games$Poss * 100
all_games$WNetRtg <- all_games$WOffRtg - all_games$WDefRtg

all_games$Loser <- as.factor(all_games$Loser)
all_games$Winner <- as.factor(all_games$Winner)

#sapply(seq(30,45,by=5), function(K) {
###########
home_adv <- 105
all_games$HomeAdv <- ifelse(all_games$Wloc=='A',-home_adv,ifelse(all_games$Wloc=='H',home_adv,0))

K <- 15
K_mar <- 60
#margin_adj <- function(mar, elo_d) ((abs(mar)+3)^0.8)/(7.5+.006*ifelse(mar*elo_d>0,abs(elo_d),-abs(elo_d)))
margin_adj <- function(mar, elo_d)  pnorm(mar/10) - 1/(10^(-elo_d/400)+1)

reversion <- 1/3

#######
elo_ratings <- data.frame(team=Teams$Team_Name, Begin=1500, Current=1500,stringsAsFactors=F) 
Elo_data <-c()

for (y in 2003:2016) {

conf_merge <- merge(conf[,c('team',paste0('X',y))],elo_ratings[,c('team','Current')])
names(conf_merge)[2] <- 'conf_name'
conf_mean <- aggregate(Current~conf_name, data=conf_merge, mean)
names(conf_mean)[2] <- 'conf_avg'
elo_ratings <- merge(conf_merge,conf_mean)

old_elo <- elo_ratings$conf_avg*(reversion) + elo_ratings$Current*(1-reversion)
#old_elo <- mean(elo_ratings$Current)*(reversion) + elo_ratings$Current*(1-reversion)
elo_ratings <- data.frame(team=elo_ratings$team, Begin=old_elo, Current=old_elo,stringsAsFactors=F) 
all_elo <- c()

games_byDay <- lapply(0:154, function(x) all_games[which(all_games$Daynum==x & all_games$Season==y),])

#check for double headers
#which(sapply(games_byDay, function(x) length(c(x$Winner,x$Loser)) - length(unique(c(x$Winner,x$Loser))))>0)

for (day in 1:155) {
elo_calc_df <- merge(games_byDay[[day]], elo_ratings[,c('team','Current')],by.x='Winner',by.y='team',all.x=T)
elo_calc_df <- merge(elo_calc_df, elo_ratings[,c('team','Current')],by.x='Loser',by.y='team',all.x=T)
names(elo_calc_df)[which(names(elo_calc_df)=='Current.x')] <- 'Win.Curr'
names(elo_calc_df)[which(names(elo_calc_df)=='Current.y')] <- 'Los.Curr'

elo_calc_df$Elo_Diff <- elo_calc_df$Win.Curr - elo_calc_df$Los.Curr + elo_calc_df$HomeAdv
elo_calc_df$Elo_WinProb <- 1/(10^(-elo_calc_df$Elo_Diff/400)+1)

#+ K_mar 
elo_calc_df$Win.New <- elo_calc_df$Win.Curr + (1-elo_calc_df$Elo_WinProb) * K + K_mar  * margin_adj(elo_calc_df$WNetRtg, elo_calc_df$Elo_Diff)
elo_calc_df$Los.New <- elo_calc_df$Los.Curr + (elo_calc_df$Elo_WinProb-1) * K + K_mar  * margin_adj(-elo_calc_df$WNetRtg, -elo_calc_df$Elo_Diff)

Upd_Elo <- data.frame(team=c(elo_calc_df$Winner,elo_calc_df$Loser),Elo.New=c(elo_calc_df$Win.New,elo_calc_df$Los.New))

elo_ratings <- merge(Upd_Elo, elo_ratings, all.y=T)
elo_ratings$Elo.New[which(is.na(elo_ratings$Elo.New))] <- elo_ratings$Current[which(is.na(elo_ratings$Elo.New))]
elo_ratings$Current <- elo_ratings$Elo.New
names(elo_ratings)[which(names(elo_ratings)=='Elo.New')] <- paste0('Day_',day)

all_elo <- rbind(all_elo,elo_calc_df)
}

Elo_data <- c(Elo_data,list(all_elo,elo_ratings))
}

Elo_games <- lapply(seq(1,27,by=2), function(x) Elo_data[[x]])
Elo_games <- do.call(rbind,Elo_games)

#szn15 <- Elo_data[[26]][,c('team','Begin','Current')]
#szn15[order(-szn15$Begin),]

get_LogLoss()
get_LogLoss(which(Elo_games$Daynum>=136 & Elo_games$Season==2015))
get_LogLoss(which(Elo_games$Daynum>=136 & Elo_games$Season==2016))
cbind(2003:2016,sapply(2003:2016, function(x) get_LogLoss(which(Elo_games$Season==x))))

sum(table(Elo_games$Daynum[which(Elo_games$Season==2011)]))
#})

#plot(table(cut(Elo_games$Elo_WinProb,seq(0,1,.05))))
#plot(Elo_games$Elo_Diff/28.5,Elo_games$Margin)

toun16 <- Elo_games[which(Elo_games$Daynum>=136 & Elo_games$Season==2016),]
#toun16[order(toun16$Elo_WinProb),]

#Elo_data[[28]][,c('team','Day_135')]
#names(Elo_data[[28]])


#cuts <- cut((abs(Elo_games$Elo_WinProb-.5)+.5),(50:100)/100)
#plot(aggregate(Elo_WinProb~cuts,Elo_games,mean))
#########
##compare to good entry
e_name <- 'bayz_290360_2707967'
e_name <- 'mallorqui_289145_2704001'
e_name <- 'all-05-benchmark_291868_2700286'
good_entry <- read.csv(paste0('C:/Users/A097092/Desktop/Extra/NCAAB Elo/Kaggle Data/predictions/predictions/',e_name,'.csv'),stringsAsFactors=F)

actual_games1 <- paste0('2016_',Elo_games$Lteam[which(Elo_games$Daynum>=136 & Elo_games$Season==2016)],'_',Elo_games$Wteam[which(Elo_games$Daynum>=136 & Elo_games$Season==2016)])
actual_games2 <- paste0('2016_',Elo_games$Wteam[which(Elo_games$Daynum>=136 & Elo_games$Season==2016)],'_',Elo_games$Lteam[which(Elo_games$Daynum>=136 & Elo_games$Season==2016)])

his_pred <- ifelse(is.na(match(actual_games1,good_entry$Id)),good_entry$Pred[match(actual_games2,good_entry$Id)],1-good_entry$Pred[match(actual_games1,good_entry$Id)])
his_pred <- med_pick

compare <- cbind(toun16[,c('Winner','Loser','Elo_WinProb')],his_pred,diff=his_pred-toun16$Elo_WinProb)
compare$LLDiff <- (log(compare$Elo_WinProb)/-63)-(log(compare$his_pred)/-63)
compare[order(-compare$LLDiff),]

sum((log(compare$his_pred)/-63))

###get all competitors
actual_rows <- ifelse(is.na(match(actual_games1,good_entry$Id)),match(actual_games2,good_entry$Id),match(actual_games1,good_entry$Id))
actual_fav_win <- ifelse(is.na(match(actual_games1,good_entry$Id)),0,1)
setwd('C:/Users/A097092/Desktop/Extra/NCAAB Elo/Kaggle Data/predictions/predictions/')
all_pred <- sapply(dir(), function(x) read.csv(x,stringsAsFactors=F)[actual_rows,c('Pred')])
all_pred <- apply(all_pred, 2, function(x) abs(actual_fav_win-x))
all_predLL <- apply(all_pred, 2, function(x) -sum(log(x))/63)
med_pick <- apply(all_pred, 1, median)


###look at point spread
summary(glm(Elo_Diff~Margin, data=Elo_games))
pred_sp <- glm(WNetRtg~Elo_Diff, data=Elo_games)

glm(WNetRtg~Elo_Diff, data=Elo_games, subset=(Elo_Diff <= -100))

NetRateFav <- ifelse(Elo_games$Elo_Diff<=0,-Elo_games$WNetRtg,Elo_games$WNetRtg)
EloDiffFav <- abs(Elo_games$Elo_Diff)
real_pred <- lm(NetRateFav~EloDiffFav)
summary(real_pred)
confint(real_pred)
predict(real_pred,Elo_games,se.fit=T)$fit[1:10]
predict(real_pred,Elo_games,se.fit=T)$se.fit[1:10]

Elo_cuts <- cut(EloDiffFav,0:7*100)
aggregate(NetRateFav~Elo_cuts, data=Elo_games, FUN= function(x) c(mn=mean(x), n=length(x),md=median(x),sd=sd(x)))

library(rpart)
rpart(WNetRtg~Elo_Diff+Wloc, data=Elo_games,cp=0.001)

###logistic model
cbind(1:43,names(all_games))

season_all <- lapply(2003:2016, function(x) all_games[which(all_games$Season==x & all_games$Daynum<=135),])

winner_df <- data.frame(Team=all_games$Winner,all_games[,c('Season','Daynum')],Vic=1,Def=0,all_games[,c(5,6,38,9:21,22:34)])
loser_df <- data.frame(Team=all_games$Loser,all_games[,c('Season','Daynum')],Vic=0,Def=1,all_games[,c(6,5,38,22:34,9:21)])
w_col <- which(substr(names(winner_df),1,1)=='W')
l_col <- which(substr(names(winner_df),1,1)=='L')
names(winner_df)[w_col] <- sub('W','TM',names(winner_df)[w_col])
names(winner_df)[l_col] <- sub('L','OPP',names(winner_df)[l_col])
names(loser_df) <- names(winner_df)

teamgm_df <- rbind(winner_df,loser_df)

aggregate(.~Team, data=teamgm_df[,-c(2:3)] ,subset = teamgm_df$Season==2015 & teamgm_df$Daynum<=135, sum)



cbind(table(season_all[[13]]$Winner),table(season_all[[13]]$Loser))
season_avg_wins <- aggregate(.~Winner, data=all_games[,c(35,9:34,37:42)],subset = all_games$Season==2015 & all_games$Daynum<=135, mean)
season_avg_loss <- aggregate(.~Loser, data=all_games[,c(36,9:34,37:42)],subset = all_games$Season==x & all_games$Daynum<=135, mean)

###YUSAG imitiation
SZN <- all_games[which(all_games$Season==2016),]


mx_col <- length(levels(SZN$Winner))
mx_row <- length(SZN$Winner)
mx_str <- rep(0,mx_col*mx_row)

mx_str[seq(0,mx_col*mx_row-1,mx_col) + match(SZN$Winner,levels(SZN$Winner))] <- 1
mx_str[seq(0,mx_col*mx_row-1,mx_col) + match(SZN$Loser,levels(SZN$Winner))] <- -1
mx_done <- matrix(mx_str, mx_row, mx_col, byrow=T)
colnames(mx_done) <- levels(SZN$Winner)

YUSAG <- glm(SZN$WNetRtg~mx_done)

str(SZN)




