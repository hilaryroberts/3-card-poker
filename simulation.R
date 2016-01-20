val <- rep(1:5,4)
suit <- c(rep("D",5),rep("S",5),rep("C",5),rep("H",5))
deck<- data.frame(val,suit)
set.seed(pi)

draws <- list()
for (d in 1:10000) {
  draws[[d]] <- deck[sample(20, 3),]
}

check_score <- function(draw){
  flush <- FALSE
  straight <- FALSE
  threekind <- FALSE
  pair <- FALSE
  
  if(draw[1,2] == draw[2,2] && draw[1,2] == draw[3,2]){flush<-TRUE}
  
  if(draw[1,1] == draw[2,1] && draw[1,1] == draw[3,1]){threekind<-TRUE}
  else if(draw[1,1] == draw[2,1]|draw[1,1] == draw[3,1]|draw[3,1] == draw[2,1]){pair<-TRUE}
  
  sorted <- sort(draw[,1], decreasing = TRUE)
  if(sorted[1] == sorted[2]+1 && sorted[1]==sorted[3]+2){straight<-TRUE}
  
  if(straight&&flush){return("straight flush")}
  else if(threekind){return("3 of a kind")}
  else if(straight){return("straight")}
  else if(flush){return("flush")}
  else if(pair){return("pair")}
  else{return("High Card")}
}

for (i in 1:length(draws)){
  results[i] <- check_score(draws[[i]])
}

frequencies <- table(results)
barplot(frequencies)
