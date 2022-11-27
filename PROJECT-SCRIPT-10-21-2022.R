library(lubridate); library(tidyverse); library(ggplot2); library(officer); 
library(cfbfastR); library(leaps); library(GGally); library(flextable); library(cowplot)
Sys.setenv(CFBD_API_KEY = 'wSIaYgKNpw/e0GVlEjj+30R0lhT3pKNHen4AywtCBTUVlbH8Wf6RXtAgTfxNpoxS')
start = Sys.time()

for (year in 2020:2022) {
  lines = cfbd_betting_lines(year = year, season_type = 'regular')
  if (year == 2020) {
    database = lines
    }else{
      database = rbind(database, lines, fill = TRUE)
    }
  }

database = data.frame(matrix(unlist(database), nrow=nrow(database)),stringsAsFactors=FALSE) %>% setNames(., names(lines))

allData = database[complete.cases(database), ] %>% setNames(., names(lines)) %>% mutate(start_date = as.Date(start_date)) %>% mutate_at(c(8, 11, 13, 15:ncol(database)), as.numeric)
allData = allData[!allData$home_moneyline == allData$away_moneyline, ]
allData = allData[!allData$spread_open == 0, ]
allData$realTotal = allData$home_score + allData$away_score
allData$realSpred = abs(allData$home_score - allData$away_score)

smolData = allData %>% select(game_id, season, season_type, week, start_date, home_team, home_conference, home_score, away_team, away_conference, away_score, spread_open, over_under_open, home_moneyline, away_moneyline) %>%
  mutate(fav = ifelse(allData$home_moneyline < allData$away_moneyline, home_team, away_team)) %>% mutate(underdog = ifelse(allData$home_moneyline > allData$away_moneyline, home_team, away_team)) %>% 
  mutate(winner = ifelse(allData$home_score > allData$away_score, home_team, away_team)) %>% mutate(confmatch = ifelse(allData$home_conference == allData$away_conference, 1, 0)) %>% 
  mutate(over = ifelse(allData$home_score + allData$away_score > allData$over_under_open, 1, 0))

quadTwo = smolData[smolData$home_moneyline < 0 & smolData$spread_open > 0, ] 
quadTwo = quadTwo %>% mutate(spread_open = ifelse(quadTwo$home_moneyline < away_moneyline, quadTwo$spread_open * -1, quadTwo$spread_open))
quadFor = smolData[smolData$home_moneyline > 0 & smolData$spread_open < 0, ]
quadFor = quadFor %>% mutate(spread_open = ifelse(quadFor$away_moneyline < home_moneyline, quadFor$spread_open * -1, quadFor$spread_open))
quadOne = smolData[smolData$home_moneyline > 0 & smolData$spread_open > 0, ]
quadOne = quadOne %>% mutate(cover = ifelse(quadOne$away_score - quadOne$home_score < quadOne$spread_open | quadOne$home_score > quadOne$away_score, 1, 0))
quadTre = smolData[smolData$home_moneyline < 0 & smolData$spread_open < 0, ] 
quadTre = quadTre %>% mutate(cover = ifelse(quadTre$home_score - quadTre$away_score < quadTre$spread_open | quadTre$away_score > quadTre$home_score, 1, 0))

smolData = rbind(quadOne, quadTre)

smolData$histry = 0
smolData$favPct = 0

for (game in 1:nrow(smolData)) {
  
  record = cfbd_team_matchup_records(smolData$fav[game], smolData$underdog[game], max_year = as.numeric(smolData$season[game]) - 1)
  
  if (length(record) > 6) {
    
    favWins = as.numeric(record$team1_wins)
    undWins = as.numeric(record$team2_wins)
    favPctg = round(favWins / (favWins + undWins), digits = 3)
    smolData$histry[game] = favWins + undWins
    smolData$favPct[game] = favPctg
    
  } else {
    
    smolData$favPct[game] = NA
    
  }
}

smolData = smolData %>% mutate(upset = ifelse(smolData$fav == smolData$winner, 0, 1))
homeFavs = smolData[smolData$home_moneyline < smolData$away_moneyline, ]
homeFavs$home = 1
awayFavs = smolData[smolData$away_moneyline < smolData$home_moneyline, ]
awayFavs$home = 0

homeFavs = homeFavs %>% select(season, week, home_team, away_team, home_moneyline, away_moneyline, spread_open, over_under_open, confmatch, histry, favPct, home, upset) %>% setNames(., c('Season', 'Week', 'Favorite', 'Underdog', 'fMoneyline', 'uMoneyline', 'Spread', 'OverUnder', 'Conference', 'History', 'fWinPct', 'fHome', 'Upset'))
awayFavs = awayFavs %>% select(season, week, away_team, home_team, away_moneyline, home_moneyline, spread_open, over_under_open, confmatch, histry, favPct, home, upset) %>% setNames(., c('Season', 'Week', 'Favorite', 'Underdog', 'fMoneyline', 'uMoneyline', 'Spread', 'OverUnder', 'Conference', 'History', 'fWinPct', 'fHome', 'Upset'))
awayFavs = awayFavs %>% mutate(Spread = ifelse(awayFavs$Spread > 0, awayFavs$Spread * -1, awayFavs$Spread))

favData = rbind(homeFavs, awayFavs) %>% na.omit()
favData$fProbRaw = round(1 / (1 + (100 / abs(favData$fMoneyline))), digits = 2)
favData$uProbRaw = round(1 / (1 + (abs(favData$uMoneyline) / 100)), digits = 2)
favData$sumProbRaw = favData$fProbRaw + favData$uProbRaw
favData$fProbFair = round(favData$fProbRaw / favData$sumProbRaw, digits = 2)
favData$uProbFair = round(favData$uProbRaw / favData$sumProbRaw, digits = 2)

favData = favData %>% select(Season, Week, Favorite, Underdog, fMoneyline, fProbRaw, fProbFair, uMoneyline, uProbRaw, uProbFair, uProbRaw, Spread, OverUnder, History, fWinPct, Conference, fHome, Upset)

newNames = c(names(favData), 'total_yds', 'rush_yds', 'net_pass_yds', 'pass_TDs', 'rush_TDs', 'first_downs', 'total_TDs')
projData = data.frame(matrix(ncol = length(newNames), nrow = nrow(favData)))
names(projData) = newNames

for (row in 1:nrow(favData)) {
  matchup = favData[row, ]
  
  favStat = cfbd_stats_season_team(year = matchup$Season, team = matchup$Favorite, end_week = matchup$Week)
  if(length(favStat) < 1) next
  
  favStat = data.frame(favStat %>% select(games, total_yds, rush_yds, net_pass_yds, pass_TDs, rush_TDs, first_downs)) %>% mutate(total_TDs = rush_TDs + pass_TDs)
  favPrGm = round(favStat[2:ncol(favStat)] / favStat$games, 2)
  
  undStat = cfbd_stats_season_team(year = matchup$Season, team = matchup$Underdog, end_week = matchup$Week)
  if(length(undStat) < 1) next
  
  undStat = data.frame(undStat %>% select(games, total_yds, rush_yds, net_pass_yds, pass_TDs, rush_TDs, first_downs)) %>% mutate(total_TDs = rush_TDs + pass_TDs)
  undPrGm = round(undStat[2:ncol(undStat)] / undStat$games, 2)
  
  tacData = favPrGm - undPrGm
  output = cbind(matchup, tacData)
  projData[row, ] = output
}

data = projData %>% select(fMoneyline, fProbFair, uMoneyline, uProbFair, Spread, OverUnder, fWinPct, total_yds, rush_yds, net_pass_yds, first_downs, pass_TDs, rush_TDs, total_TDs, Conference, fHome, Upset) %>% 
  setNames(., c('fML', 'fProb', 'uML', 'uProb', 'Spread', 'OvUn', 'fwPct', 'totYds', 'rushYds', 'passYds', 'fDowns', 'passTDs', 'rushTDs', 'totTDs', 'Conf', 'Home', 'Upset')) %>% na.omit()

bookData = data %>% select(c(1:6, 15:17))
perfData = data %>% select(c(7:14, 17))

ggpairs(bookData, ggplot2::aes(color = factor(Upset)))
ggpairs(perfData, ggplot2::aes(color = factor(Upset)))

head(bookData) %>% flextable() %>% align(align = 'center', part = 'all') %>% width(width = 6.5)
head(perfData) %>% flextable() %>% align(align = 'center', part = 'all') %>% width(width = 6.5)

bookSubsets = regsubsets(Upset ~ ., bookData)
bookSubSumm = summary(bookSubsets)


set.seed(1)
trainInd = sample(nrow(bookData), 0.80 * nrow(bookData), replace = FALSE)
bookTrain = bookData[trainInd, ]
bookTest = bookData[-trainInd, ]

bookVariables = names(bookData)[1:ncol(bookData) - 1]
bookPredError = list()
bookPredVarbs = list()

for (num in 1:nrow(bookSubSumm$outmat)) {
  bookPredictors = which(bookSubSumm$outmat[num, ] == '*')
  bookModVariabs = bookVariables[bookPredictors]
  bookModFormula = as.formula(paste('Upset', paste(bookModVariabs, collapse = '+'), sep = '~'))
  bookModel = glm(bookModFormula, data = bookTrain, family = 'binomial')
  bookPreds = predict(bookModel, bookTest, type = 'response')
  bookLabel = rep(0, nrow(bookTest))
  bookLabel[bookPreds > 0.5] = 1
  bookTable = table(bookLabel, bookTest$Upset)
  bookError = round(1 - (sum(diag(bookTable)) / sum(bookTable)), 2)
  bookPredVarbs[[num]] = bookModVariabs
  bookPredError[num] = bookError
}

bookResults = data.frame(matrix(ncol = 2, nrow = length(bookPredVarbs)))
bookResults$X1 = bookPredVarbs
bookResults$X2 = bookPredError
names(bookResults) = c('Predictor Subset', 'Prediction Error')
bookResults[, 1] = paste(bookResults[, 1])
bookResults[, 2] = as.numeric(paste(bookResults[, 2]))
flextable(bookResults) %>% width(width = 6.5) %>% align(j = 2, align = 'center') %>% align(align = 'center', part = 'header') %>% bg(i = ~ `Prediction Error` == min(`Prediction Error`), bg = 'light blue', part = 'body')


perfSubsets = regsubsets(Upset ~ ., perfData)
perfSubSumm = summary(perfSubsets)

perfTrain = perfData[trainInd, ]
perfTest = perfData[-trainInd, ]

perfVariables = names(perfData)[1:ncol(perfData) - 1]
perfPredError = list()
perfPredVarbs = list()

for (numb in 1:nrow(perfSubSumm$outmat)) {
  perfPredictors = which(perfSubSumm$outmat[numb, ] == '*')
  perfModVariabs = perfVariables[perfPredictors]
  perfModFormula = as.formula(paste('Upset', paste(perfModVariabs, collapse = '+'), sep = '~'))
  perfModel = glm(perfModFormula, data = perfTrain, family = 'binomial')
  perfPreds = predict(perfModel, perfTest, type = 'response')
  perfLabel = rep(0, nrow(perfTest))
  perfLabel[perfPreds > 0.5] = 1
  perfTable = table(perfLabel, perfTest$Upset)
  perfError = round(1 - (sum(diag(perfTable)) / sum(perfTable)), 2)
  perfPredVarbs[[numb]] = perfModVariabs
  perfPredError[numb] = perfError
}

perfResults = data.frame(matrix(ncol = 2, nrow = length(perfPredVarbs)))
perfResults$X1 = perfPredVarbs
perfResults$X2 = perfPredError
names(perfResults) = c('Predictor Subset', 'Prediction Error')
perfResults[, 1] = paste(perfResults[, 1])
perfResults[, 2] = as.numeric(paste(perfResults[, 2]))
flextable(perfResults) %>% width(width = 6.5) %>% align(j = 2, align = 'center') %>% align(align = 'center', part = 'header') %>% bg(i = ~ `Prediction Error` == min(`Prediction Error`), bg = 'light blue', part = 'body')


dataSubsets = regsubsets(Upset ~ ., data)
dataSubSumm = summary(dataSubsets)


dataTrain = data[trainInd, ]
dataTest = data[-trainInd, ]

dataVariables = names(data)[1:ncol(data) - 1]
dataPredError = list()
dataPredVarbs = list()

for (numbs in 1:nrow(dataSubSumm$outmat)) {
  dataPredictors = which(dataSubSumm$outmat[numbs, ] == '*')
  dataModVariabs = dataVariables[dataPredictors]
  dataModFormula = as.formula(paste('factor(Upset)', paste(dataModVariabs, collapse = '+'), sep = '~'))
  dataModel = glm(dataModFormula, data = dataTrain, family = 'binomial')
  dataPreds = predict(dataModel, dataTest, type = 'response')
  dataLabel = rep(0, nrow(dataTest))
  dataLabel[dataPreds > 0.5] = 1
  dataTable = table(dataLabel, dataTest$Upset)
  dataError = round(1 - (sum(diag(dataTable)) / sum(dataTable)), 2)
  dataPredVarbs[[numbs]] = dataModVariabs
  dataPredError[numbs] = dataError
}

dataResults = data.frame(matrix(ncol = 2, nrow = length(dataPredVarbs)))
dataResults$X1 = dataPredVarbs
dataResults$X2 = dataPredError
names(dataResults) = c('Predictor Subset', 'Prediction Error')
dataResults[, 1] = paste(dataResults[, 1])
dataResults[, 2] = as.numeric(paste(dataResults[, 2]))
flextable(dataResults) %>% width(width = 6.5) %>% align(j = 2, align = 'center') %>% align(align = 'center', part = 'header') %>% bg(i = ~ `Prediction Error` == min(`Prediction Error`), bg = 'light blue', part = 'body')


profitData = data %>% select(fML, uML, fProb, uProb, totTDs, Upset)
profitTrain = profitData[trainInd, ]
profitTest = profitData[-trainInd, ]


profitModel = glm(Upset ~ uProb + totTDs, data = profitTrain, family = 'binomial')
profitPreds = predict(profitModel, profitTest, type = 'response')
profitLabel = rep(0, nrow(profitTest))
profitLabel[profitPreds > 0.5] = 1
profitTable = table(profitLabel, profitTest$Upset)

profitBind = cbind(profitTest, profitLabel) %>% filter(profitLabel == 1)
profitBind$Payout = ifelse(profitBind$uML > 0, round(profitBind$uML / 100, 2), round(100 / abs(profitBind$uML), 2))
profitBind$Winnings100 = ifelse(profitBind$Upset == 1, round(100 * profitBind$Payout), -100)

bankroll = 100
profitBind$Bankroll = 0

for (bet in 1:nrow(profitBind)) {
  outcome = profitBind$Winnings100[bet]
  bankroll = bankroll + outcome
  profitBind$Bankroll[bet] = bankroll
}

ggplot(data = profitBind, mapping = aes(x = 1:nrow(profitBind), y = Bankroll)) + geom_line(linewidth = 1) + geom_point(shape = 21, color = 'black', fill = 'light blue', size = 5) +
  ylim(0, 2000) + geom_hline(yintercept = 0, col = 'red') + ggtitle('Cumulative Change in Bankroll in Simulated Betting with $100 per Game') + 
  geom_text(aes(label = paste(Winnings100, ' : ', '$', Bankroll, sep = ''), color = case_when(Winnings100 > 0 ~ 'red', Winnings100 < 0 ~ 'blue')), angle = 90, hjust = -0.25, size = 4.5, fontface = 'bold') + 
  xlab('Predicted Upset Game') + ylab('Cumulative Bankroll') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')


profitBind$Payout2 = round(100 / abs(profitBind$fML), 2)
profitBind$WinningsFav = ifelse(profitBind$Upset == 0, round(100 * profitBind$Payout2), -100)
profitBind$Bankroll2 = 0

bankroll2 = 100
profitBind$Bankroll2 = 0

for (bet2 in 1:nrow(profitBind)) {
  outcome2 = profitBind$WinningsFav[bet2]
  bankroll2 = bankroll2 + outcome2
  profitBind$Bankroll2[bet2] = bankroll2
}

ggplot(data = profitBind, mapping = aes(x = 1:nrow(profitBind))) + geom_point(aes(y = Bankroll), shape = 21, size = 3.5, fill = 'blue') + geom_line(aes(y = Bankroll)) +
  geom_point(aes(y = Bankroll2), shape = 21, size = 3.5, fill = 'red') + geom_line(aes(y = Bankroll2)) + geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') +
  ylim(-2000, 2000) + ggtitle('Cumulative Change in Bankroll ($100/Game) with Upset Prediction (Blue) vs. Naive Betting Favorites (Red)') + xlab('Game Number') +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(data = profitBind, mapping = aes(x = 1:nrow(profitBind))) + geom_point(aes(y = Bankroll), shape = 21, size = 3.5, fill = 'blue') + geom_line(aes(y = Bankroll), color = 'blue') +
  geom_point(aes(y = Bankroll2), shape = 21, size = 3.5, fill = 'red') + geom_line(aes(y = Bankroll2), color = 'red') + geom_hline(yintercept = 0, col = 'red', linetype = 'dashed', linewidth = 1) +
  geom_text(aes(y = Bankroll, label = paste(Winnings100, ' : ', '$', Bankroll, sep = '')), angle = 90, hjust = -0.25, size = 4, fontface = 'bold') + 
  geom_text(aes(y = Bankroll2, label = paste(WinningsFav, ' : ', '$', Bankroll2, sep = '')), angle = 90, hjust = 1.25, size = 4, fontface = 'bold') + 
  ylim(-2500, 2500) + ggtitle('Cumulative Change in Bankroll ($100/Game) with Upset Prediction (Blue) vs. Naively Betting Favorites (Red)') + xlab('Game Number') +
  theme(plot.title = element_text(hjust = 0.5))

overall = data %>% group_by(Upset) %>% summarize(n()) %>% ggplot(aes(x = factor(Upset), y = `n()`, fill = factor(Upset))) + geom_bar(stat = 'identity') + scale_x_discrete(labels = c('No Upset', 'Upset')) + 
  ggtitle('Prevalence of Upsets - All Data') + geom_label(aes(label = paste(`n()`, ' (', round(`n()` / sum(`n()`), 2) * 100, '%)', sep = '')), fill = 'white', size = 8, position = position_stack(vjust = 0.75)) + 
  theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_text(size = 10, face = 'bold'), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5, size = 18))

predictedUpsets = profitBind %>% group_by(Upset) %>% summarize(n()) %>% ggplot(aes(x = factor(Upset), y = `n()`, fill = factor(Upset))) + geom_bar(stat = 'identity') + scale_x_discrete(labels = c('No Upset', 'Upset')) + 
  ggtitle('Prevalence of Upsets - Predicted') + geom_label(aes(label = paste(`n()`, ' (', round(`n()` / sum(`n()`), 2) * 100, '%)', sep = '')), fill = 'white', size = 8, position = position_stack(vjust = 0.75)) + 
  theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_text(size = 10, face = 'bold'), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5, size = 18))

plot_grid(overall, predictedUpsets, ncol = 2)
