"
  function: compareTeams.R
  usage: compare teams by looking at the total goal differential for games played b/w them.
    This function is intended for use only within a call of the function nhlMCMC.
    Use of this function in other environments may likely lead to errors or incorrect results.
  input: team 1 abbreviation, team 1 season data, team 2 abbreviation, team 2 season data
    Team abbreviations should be given in quotations and all-caps (assuming that the expected
      data sets, or data sets formatted in the same manner, are given as input).
    Uses the dplyr library.
  output: returns a probability that team 1 is better than team 2
"
compareTeams <- function (team1, dta1, team2, dta2) {
  # get games played b/w team1 & team2
  library(dplyr)
  games <- filter(dta1, visitor == team2 | home == team2)
  
  # get total goals scored for each team in the relevant games
  home <- games[which(games$home == team1),]
  away <- games[which(games$visitor == team1),]
  team1Goals <- sum(home$hgoals,away$vgoals)
  team2Goals <- sum(home$vgoals,away$hgoals)
  
  # calculate and return final result
  totalGoals <- sum(team1Goals,team2Goals)
  result <- team1Goals / totalGoals
  return(result)
}