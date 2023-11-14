trackMacGuffinFromStartToFinish <- function(start_season) {
  token_holder <- "Leeds United"
  token_movements <- list()
  
  matches <- all_matches[all_matches$Season >= start_season, ]
  
  for (i in 1:nrow(matches)) {
    match <- matches[i,]
    
    if (token_holder == "Leeds United" && !is.na(match$FTR)) {
      if (match$HomeTeam == "Leeds United" && match$FTR == "A") {
        token_holder <- match$AwayTeam
        token_movements[[as.character(match$Date)]] <- token_holder
      } else if (match$AwayTeam == "Leeds United" && match$FTR == "H") {
        token_holder <- match$HomeTeam
        token_movements[[as.character(match$Date)]] <- token_holder
      }
    } else if (!is.na(match$FTR) && (match$HomeTeam == token_holder || match$AwayTeam == token_holder)) {
      if (match$FTR != "D") {
        winner <- ifelse(match$FTR == "H", match$HomeTeam, match$AwayTeam)
        if (token_holder != winner) {
          token_holder <- winner
          token_movements[[as.character(match$Date)]] <- token_holder
        }
      }
    }
  }
  
  # Add tracking from Leicester City starting from 2001
  token_holder_leicester <- "Leicester City"
  matches_from_2001 <- all_matches[as.Date(all_matches$Date, format="%m/%d/%y") > as.Date("01/01/01", format="%m/%d/%y"), ]
  matches_from_2001 <- matches_from_2001[order(as.Date(matches_from_2001$Date, format="%m/%d/%y")), ]
  
  for (i in 1:nrow(matches_from_2001)) {
    match <- matches_from_2001[i,]
    
    if (token_holder_leicester == "Leicester City" && !is.na(match$FTR)) {
      if (match$HomeTeam == "Leicester City" && match$FTR == "A") {
        token_holder_leicester <- match$AwayTeam
        token_movements[[as.character(match$Date)]] <- token_holder_leicester
      } else if (match$AwayTeam == "Leicester City" && match$FTR == "H") {
        token_holder_leicester <- match$HomeTeam
        token_movements[[as.character(match$Date)]] <- token_holder_leicester
      }
    } else if (!is.na(match$FTR) && (match$HomeTeam == token_holder_leicester || match$AwayTeam == token_holder_leicester)) {
      if (match$FTR != "D") {
        winner <- ifelse(match$FTR == "H", match$HomeTeam, match$AwayTeam)
        if (token_holder_leicester != winner) {
          token_holder_leicester <- winner
          token_movements[[as.character(match$Date)]] <- token_holder_leicester
        }
      }
    }
  }
  
  return(token_movements)
}

# Example: Track the movement of the singular MacGuffin token from Season 1993 to the last game in the dataset, merging both tracking methods
combined_macguffin_tracking <- trackMacGuffinFromStartToFinish(1993)
print(combined_macguffin_tracking)
