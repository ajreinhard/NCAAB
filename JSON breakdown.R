library(RJSONIO)

setwd('C:/Users/A097092/Desktop/Extra/NCAAB Elo/NCAA Direct/board json')

json_files <- dir()[which(file.info(dir())$size!=0)]

game_info <- sapply(c('id','conference','startDate','startTime','gameState','location','url'),function(col_n) {
all_items_NULL <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) is.null(x[[col_n]]))))
all_items <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) x[[col_n]])))
final <- rep(NA,length(all_items))
final[which(!all_items_NULL)] <- all_items
final
})

team_info <- lapply(c('shortname','currentScore','nameRaw','winner'),function(col_n) {
all_items_NULL_H <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) is.null(x$home[[col_n]]))))
all_items_H <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) x$home[[col_n]])))
all_items_NULL_A <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) is.null(x$away[[col_n]]))))
all_items_A <- unlist(lapply(json_files,function(file) lapply(fromJSON(file)[[1]][[1]][[2]],function(x) x$away[[col_n]])))
cbind(home=ifelse(all_items_NULL_H,NA,all_items_H),away=ifelse(all_items_NULL_A,NA,all_items_A))
final_H <- rep(NA,length(all_items_H))
final_H[which(!all_items_NULL_H)] <- all_items_H
final_A <- rep(NA,length(all_items_A))
final_A[which(!all_items_NULL_A)] <- all_items_A
cbind(home=final_H,away=final_A)
})

all_game_info <- data.frame(team_info,game_info,stringsAsFactors=F)
all_game_info$startDate <- as.Date(all_game_info$startDate)

str(all_game_info)


yearly <- all_game_info[which(all_game_info$startDate >= as.Date('2010/11/8') & all_game_info$startDate <= as.Date('2011/4/4')),]
yearly <- yearly[which(yearly$gameState=='final'),]

sum(table(yearly$startDate))

all_game_info[which(is.na(all_game_info$startDate)),]
all_game_info$url[which(is.na(all_game_info$startDate))]

team_sch <- yearly[which(yearly$home=='ARK ST' | yearly$away=='ARK ST'),]
cbind(1:nrow(team_sch),ifelse(team_sch$home=='ARK ST',team_sch$away,team_sch$home))
teams <- names(rev(sort(table(c(yearly$home.2,yearly$away.2))))[2:346])


records <- t(sapply(teams,function(x) rev(table(c(yearly$home.3[yearly$home.2==x],yearly$away.3[yearly$away.2==x])))))
records[order(row.names(records)),]

apply(records,2,sum)

sort(teams)

table(yearly$away.1)

fromJSON(json_files[2])[[1]][[1]][[2]][[1]][['away']][['shortname']]
sapply(fromJSON(json_files[27])[[1]][[1]][[2]],function(x) x$url)

fromJSON(json_files[27])[[1]][[1]][[2]][[11]]

length(unique(all_urls))
sort(table(all_urls))

write.table(yearly$url,'C:/Users/A097092/Desktop/Extra/NCAAB Elo/NCAA Direct/2010-11 url.txt',row.names=F,col.names=F,quote=F)
write.table(teams,'C:/Users/A097092/Desktop/Extra/NCAAB Elo/DirectTeams.txt',row.names=F,col.names=F,quote=F)