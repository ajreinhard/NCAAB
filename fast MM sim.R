pred <- read.csv('C:/Users/A097092/Desktop/Extra/NCAAB Elo/MM Scoring/FTE 2016-17 predictions.csv',stringsAsFactors=F)
pred <- pred[which(pred$gender=='mens' & pred$forecast_date=='3/15/2017'),]

n <- 100000

pred <- pred[pred$rd1_win==1,]
pred$team_seed[pred$team_seed=='16a']<-16
pred$team_seed[pred$team_seed=='16b']<-16
pred$team_seed[pred$team_seed=='11a']<-11
pred$team_seed[pred$team_seed=='11b']<-11

mapping <- expand.grid(team_seed=c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15),team_region=c('East','West','Midwest','South'))
mapping$rd2 <- c(sapply(1:32,function(x) rep(x,2)))
mapping$rd3 <- c(sapply(33:48,function(x) rep(x,4)))
mapping$rd4 <- c(sapply(49:56,function(x) rep(x,8)))
mapping$rd5 <- c(sapply(57:60,function(x) rep(x,16)))
mapping$rd6 <- c(sapply(61:62,function(x) rep(x,32)))
mapping$rd7 <- c(sapply(63,function(x) rep(x,64)))

pred_all <- merge(mapping,pred,by=c('team_region','team_seed'))

prob_matrix <- sapply(pred_all$team_name, function(x) {
win_prob <- rep(0,63)
win_prob[unlist(pred_all[which(pred_all$team_name==x),3:8])] <- pred_all[which(pred_all$team_name==x),13:18]
unlist(win_prob)
})

raw_simulation <- apply(prob_matrix,1,function(x) sample(1:64,n,x,replace=T))

for (i in 1:n) raw_simulation[i,unlist(pred_all[raw_simulation[i,63],c(3:7)])] <- raw_simulation[i,63]
for (j in 61:62) for (i in 1:n) raw_simulation[i,unlist(pred_all[raw_simulation[i,j],c(3:6)])] <- raw_simulation[i,j]
for (j in 57:60) for (i in 1:n) raw_simulation[i,unlist(pred_all[raw_simulation[i,j],c(3:5)])] <- raw_simulation[i,j]
for (j in 49:56) for (i in 1:n) raw_simulation[i,unlist(pred_all[raw_simulation[i,j],c(3:4)])] <- raw_simulation[i,j]
for (j in 33:48) for (i in 1:n) raw_simulation[i,unlist(pred_all[raw_simulation[i,j],c(3)])] <- raw_simulation[i,j]

#head(raw_simulation)


apply(


head(raw_simulation)

rnd_7 <- sample(1:64,n,prob=pred$rd7_win,replace=T)

rnd <- 'rd6'

odds <- sapply(1:2, function(x) sample(1:32,n,prob=pred_all[which(pred_all[rnd]==x),paste0(rnd,'_win')],replace=T))

aggregate(rd4_win~rd4,data=pred_all,sum)


apply(pred[,3:10],2,sum)

,n,prob=pred$rd7_win,replace=T)


pred$rd7_win








