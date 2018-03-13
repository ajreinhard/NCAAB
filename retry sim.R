rm(list=ls())
options(scipen=999)
library(data.tree)

######begin function that makes sim
clean_frame <- function(pred) {

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

pred_m <- merge(mapping,pred,by=c('team_region','team_seed'))
pred_m[order(pred_m$rd2),]
}

do_sim <- function(pred_all, n) {

#make quick matrix of gameid
game_mx <- matrix(unlist(pred_all[,c(3:8)]),nrow=64)

#gameid by teamid matrix of prob
prob_matrix <- sapply(pred_all$team_name, function(x) {
win_prob <- rep(0,63)
win_prob[unlist(pred_all[which(pred_all$team_name==x),3:8])] <- pred_all[which(pred_all$team_name==x),13:18] - c(pred_all[which(pred_all$team_name==x),14:18],0)
unlist(win_prob)
})

sim_str <- rep(NA, n*63)

sim_rd7 <- sample(1:64,n,prob_matrix[63,],replace=T)
sim_str[c(game_mx[sim_rd7,1:6]+(0:(n-1))*63)] <- c(matrix(sim_rd7,n,6))
sim_mx <- matrix(sim_str,n,63,byrow=T)

rd6_gms <- lapply(61:62, function(x) which(is.na(sim_mx[,x])))
sim_rd6 <- unlist(lapply(1:2, function(x) sample(1:64,length(rd6_gms[[x]]),prob_matrix[60+x,],replace=T)))
sim_str[c(game_mx[sim_rd6,1:5]+(unlist(rd6_gms)-1)*63)] <- c(matrix(sim_rd6,n,5))
sim_mx <- matrix(sim_str,n,63,byrow=T)

rd5_gms <- lapply(57:60, function(x) which(is.na(sim_mx[,x])))
sim_rd5 <- unlist(lapply(1:4, function(x) sample(1:64,length(rd5_gms[[x]]),prob_matrix[56+x,],replace=T)))
sim_str[c(game_mx[sim_rd5,1:4]+(unlist(rd5_gms)-1)*63)] <- c(matrix(sim_rd5,n*2,4))
sim_mx <- matrix(sim_str,n,63,byrow=T)

rd4_gms <- lapply(49:56, function(x) which(is.na(sim_mx[,x])))
sim_rd4 <- unlist(lapply(1:8, function(x) sample(1:64,length(rd4_gms[[x]]),prob_matrix[48+x,],replace=T)))
sim_str[c(game_mx[sim_rd4,1:3]+(unlist(rd4_gms)-1)*63)] <- c(matrix(sim_rd4,n*4,3))
sim_mx <- matrix(sim_str,n,63,byrow=T)

rd3_gms <- lapply(33:48, function(x) which(is.na(sim_mx[,x])))
sim_rd3 <- unlist(lapply(1:16, function(x) sample(1:64,length(rd3_gms[[x]]),prob_matrix[32+x,],replace=T)))
sim_str[c(game_mx[sim_rd3,1:2]+(unlist(rd3_gms)-1)*63)] <- c(matrix(sim_rd3,n*8,2))
sim_mx <- matrix(sim_str,n,63,byrow=T)

rd2_gms <- lapply(1:32, function(x) which(is.na(sim_mx[,x])))
sim_rd2 <- unlist(lapply(1:32, function(x) sample(1:64,length(rd2_gms[[x]]),prob_matrix[x,],replace=T)))
sim_str[c(game_mx[sim_rd2,1]+(unlist(rd2_gms)-1)*63)] <- c(matrix(sim_rd2,n*16,1))
sim_mx <- matrix(sim_str,n,63,byrow=T)

return(sim_mx)
}
######end function that makes sim


pred_sample_1 <- read.csv('C:/Users/A097092/Desktop/Extra/NCAAB Elo/MM Scoring/FTE 2016-17 predictions.csv',stringsAsFactors=F)
pred_sample <- clean_frame(pred_sample_1[which(pred_sample_1$gender=='mens' & pred_sample_1$forecast_date=='3/15/2017'),])
actual <- clean_frame(pred_sample_1[which(pred_sample_1$gender=='mens' & pred_sample_1$forecast_date=='4/1/2017'),])
pred_ESPN <- clean_frame(read.csv('C:/Users/A097092/Desktop/Extra/NCAAB Elo/MM Scoring/ESPN_sim_2017.csv',stringsAsFactors=F))

actual_res <- c(which(actual$rd2_win==1),which(actual$rd3_win==1),which(actual$rd4_win==1),which(actual$rd5_win==1),which(actual$rd6_win==1),33)

scoring <- c(rep(10,32),rep(20,16),rep(40,8),rep(80,4),rep(160,2),320)
#scoring <- c(rep(1,32),rep(2,16),rep(3,8),rep(5,4),rep(8,2),13)

# Start the clock!
ptm <- proc.time()

ESPN_sim <- do_sim(pred_ESPN, 100)
sample_sim <- do_sim(pred_sample, 10)
scoring_rep <- c(matrix(scoring,nrow(ESPN_sim),63,byrow=T))
sample_sim <- rbind(actual_res,sample_sim)

#seed scoring system
#seeds <- matrix(pred_ESPN$team_seed[sample_sim],nrow(sample_sim))
#seed_diff_max <- function(gm, play_in) apply(cbind(seeds[,gm]-seeds[,play_in+1],seeds[,gm]-seeds[,play_in]),1,max)
#prev_gm <- cbind(63:33,seq(61,1,-2))
#seed_diff2 <- sapply(31:1,function(x) seed_diff_max(prev_gm[x,1],prev_gm[x,2]))
#seed_diff1 <- ifelse(seeds[,1:32]>=9,seeds[,1:32]*2-17,0)
#seed_diff_full <- cbind(seed_diff1,seed_diff2)


bracket_scoring <- sapply(1:nrow(sample_sim), function(x) {
sim_scores <- rep(0, nrow(ESPN_sim)*63)
correct_games <- which(c(ESPN_sim)==c(matrix(sample_sim[x,],nrow(ESPN_sim),63,byrow=T)))
sim_scores[correct_games] <- scoring_rep[correct_games]
#+c(matrix(seed_diff_full[x,],nrow(ESPN_sim),63,byrow=T))[correct_games]
apply(matrix(sim_scores,63,byrow=T), 2, sum)
})

rnk <- apply(bracket_scoring, 2, rank)/nrow(bracket_scoring)
top_prob <- rnk^9
top_prob_tot <- apply(top_prob, 1, sum)/nrow(sample_sim)

pick_adv <- apply(ESPN_sim,2,function(x) {
champ <- aggregate(top_prob_tot~pred_ESPN$team_name[x],FUN=mean)
champ[order(-champ[,2]),]
})

my_picks <- sapply(pick_adv,function(x) x[1,1])

best_brack <- match(my_picks,pred_ESPN$team_name)
best_brack_scores <- apply(ifelse(matrix(best_brack,nrow(sample_sim),63,byrow=T)==sample_sim,matrix(scoring,nrow(sample_sim),63,byrow=T),0),1,sum)
#+seed_diff_full

pred_ESPN$pathString <- paste(my_picks[pred_ESPN[,8]],my_picks[pred_ESPN[,7]],my_picks[pred_ESPN[,6]],my_picks[pred_ESPN[,5]],my_picks[pred_ESPN[,4]],my_picks[pred_ESPN[,3]],pred_ESPN$team_name,sep='/')
print(as.Node(pred_ESPN),limit=1000)
best_brack_scores[1]

proc.time() - ptm

my_picks <- pred_ESPN$team_name[ESPN_sim[rev(order(top_prob_tot))[3],]]

cbind(my_picks,ifelse(actual_res==best_brack,scoring+seed_diff_full[1,],0))
######



prob_matrix[1:63,ESPN_sim[1,]]
prob_pred <- sapply(1:63, function(x) prob_matrix_2[x,ESPN_sim[1,x]])
sum(prob_pred*scoring)
prod(prob_pred)


pred_all<-pred_sample
str(pred_all)

prob_matrix_2 <- sapply(pred_all$team_name, function(x) {
win_prob <- rep(0,63)
win_prob[unlist(pred_all[which(pred_all$team_name==x),3:8])] <- pred_all[which(pred_all$team_name==x),13:18]
unlist(win_prob)
})








#######
table(bracket_scoring[1,])

grp_cnt <- 10
grp_tot <- nrow(ESPN_sim)/grp_cnt

grp <- rep(1:grp_tot,grp_cnt)
grp <- c(matrix(1:grp_tot,grp_cnt,grp_tot,byrow=T))
grped <- lapply(1:grp_tot, function(x) bracket_scoring[which(grp==x),])
all_wins <- c(sapply(grped,function(x) table(factor(apply(x, 2, order)[grp_cnt,],1:grp_cnt))))

pick_adv <- apply(ESPN_sim,2,function(x) {
champ <- aggregate(all_wins/nrow(sample_sim)~pred_ESPN$team_name[x],FUN=mean)
champ[order(-champ[,2]),]
})





head(rnk)
str(bracket_scoring)

head(bracket_scoring)

bb1 <- best_brack
best_brack <- bb1

pick17min <- my_picks

which(my_picks!=pick17min)


disagree <- unique(c(which(pick10min!=pick17min),which(pick10min2!=pick17min)))
cbind(my_picks,pick17min,1:63)[disagree,]


round((table(ESPN_sim[,63])/20000)*1000,1)/10


sim_scores <- ifelse(c(ESPN_sim)==c(matrix(sample_sim[1,],nrow(ESPN_sim),63,byrow=T)),scoring_rep,0)
sum(matrix(sim_scores,63,byrow=T)[,1])

scoring_mx <- matrix(scoring,nrow(ESPN_sim),63,byrow=T)
sim_scores <- ifelse(matrix(sample_sim[1,],nrow(ESPN_sim),63,byrow=T)==ESPN_sim,scoring_mx,0)
str(sim_scores)

matrix(scoring,nrow(ESPN_sim),63,byrow=T)[1:10,]
.28*200/60






head(bracket_scoring)




#table(factor(sim_mx[,1:32],1:64))/n
#cbind((table(factor(sim_mx[,49:56],1:64))/n - pred_all$rd3_win)*100)








