# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser


#class Game
setClass("Game",
         slots = c(
           #gametype??
           parties = "character", #To-Do
           game_tate = "character" #To-Do
         ),
         
         prototype = c(
           game_state = "not started"
         ),#To-Do
)
#class SetGame
setClass("SetGame",
         slots = c(
           rounds_to_win_game = "numeric", #To-Do
           points_to_win_round = "numeric", #To-Do
           roundScores = "numeric", #To-Do
           scores = "numeric", #To-Do
           
           point_gains = "numeric", #To-Do
           point_gains_probabilities = "numeric" #To-Do
         ),
          contains = "Game",       
         prototype = c(
            round_scores = c(0,0),
            scores = c(0,0) 
         ),#To-Do
)
#class TimeGame
setClass("TimeGame",
         slots = c(
           game_duration = "numeric", #To-Do
           scores = "numeric", #To-Do
           expected_points_per_game = "numeric" #To-Do
         ),
          contains = "Game",
         prototype = c(
            scores = c(0,0) 
         ),#To-Do
)


setGeneric("simulate_round", function(x) standardGeneric("simulate_round"))
setMethod("simulate_round", "SetGame", function(x) {
  
  while(all(x@round_scores < x@points_to_win_round)) {
    
    # sample whether a point occurs (use your weights)
    point_value <- sample(x@point_gains, 1, prob = x@point_gains_probabilities)
    
    # sample which player scores
    scorer <- sample(1:2, 1)
    
    x@round_scores[scorer] <- x@round_scores[scorer] + point_value
  }
  
  # Winner of the round
  winner <- which.max(x@round_scores)
  x@scores[winner] <- x@scores[winner] + 1
  
  # Reset for next round
  x@round_scores <- c(0,0)
  
  return(x)
})
setMethod("simulate_round", "TTGame", function(x) {
  while(all(x@round_scores < x@points_to_win_round)) {
    
    #when both parties at 10 or more points, have to win by 2 
    overtime <- all(x@round_scores >= 10) && abs(diff(x@round_scores)) < 2
    if(overtime){
      x@points_to_win_round <- max(x@points_to_win_round, min(x@round_scores) + 2)
    }
    
    # sample whether a point occurs (use your weights)
    point_value <- sample(x@point_gains, 1, prob = x@point_gains_probabilities)
    
    # sample which player scores
    scorer <- sample(1:2, 1)
    
    x@round_scores[scorer] <- x@round_scores[scorer] + point_value
  }
  
  # Winner of the round
  winner <- which.max(x@round_scores)
  x@scores[winner] <- x@scores[winner] + 1
  
  # Reset for next round
  x@round_scores <- c(0,0)
  x@points_to_win_round <- 11
  
  return(x)
})

setGeneric("simulate_minute", function(x) standardGeneric("simulate_minute"))
setMethod("simulate_minute", "TimeGame", function(x) {
  
  lambda <- x@expected_points_per_game / x@game_duration
  points_scored <- rpois(1, lambda)
  
  for(i in seq_len(points_scored)) {
    scorer <- sample(1:2, 1)
    x@scores[scorer] <- x@scores[scorer] + 1
  }
  
  return(x)
})

setGeneric("play_game", function(x) standardGeneric("play_game"))
setMethod("play_game", "Game", function(x) {
  x@game_state <- "in progress"
  x
})
setMethod("play_game", "SetGame", function(x) {
  
  x <- callNextMethod()    # sets state
  
  while(all(x@scores < x@rounds_to_win_game)) {
    x <- simulate_round(x)
  }
  
  x@game_state <- "finished"
  x
})
setMethod("play_game", "TimeGame", function(x) {
  
  x <- callNextMethod()
  
  for(i in 1:x@game_duration) {
    x <- simulate_minute(x)
  }
  
  x@game_state <- "finished"
  x
})

setMethod("play_game", "TTGame", function(x) {
  callNextMethod()   # automatically uses SetGame version
})

setMethod("show", "Game", function(x) {
  cat("Game between:", paste(x@parties, collapse=" vs "), "\n")
  cat("State:", x@game_state, "\n")
})

setMethod("show", "SetGame", function(x) {
  callNextMethod()
  cat("Score:", paste(x@scores, collapse=" - "), "\n")
})

setMethod("show", "TimeGame", function(x) {
  callNextMethod()
  cat("Score:", paste(x@scores, collapse=" - "), "\n")
})


setClass(
  "TTGame",
  contains = "SetGame",
  prototype = list(
    rounds_to_win_game  = 3,
    points_to_win_round = 11,
    
    point_gains         = 1,
    point_gains_probabilities = 1
  )
)

tt1 <- new(TTGame, parties = c("A. B.", "C. D."))
show(tt1)
tt1 <- play_game(tt1)
show(tt1)

