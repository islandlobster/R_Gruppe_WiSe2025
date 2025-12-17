# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser


#class Game
setClass("Game",
         slots = c(
           #gametype??
           parties = "character",
           game_state = "character",
           scores = "numeric",
           print = "logical" 
         ),
         
         prototype = list(
           game_state = "not started",
           scores = c(0,0),
           print = FALSE
         ),
)
#class SetGame
setClass("SetGame",
         slots = c(
           rounds_to_win_game = "numeric", 
           points_to_win_round = "numeric", 
           round_scores = "numeric", 
           sub_results= "list", 
           point_gains = "numeric", 
           point_gains_probabilities = "numeric"
         ),
          contains = "Game",       
         prototype = list(
            round_scores = c(0,0)
         ),#To-Do
)
#class TimeGame
setClass("TimeGame",
         slots = list(
           game_duration = "numeric", 
           expected_points_per_game = "numeric" 
         ),
          contains = "Game",
         prototype = list(
         ),
)


setGeneric("simulate_round", function(x) standardGeneric("simulate_round"))
setMethod("simulate_round", "SetGame", function(x) {
  while(all(x@round_scores < x@points_to_win_round)) {
    
    # sample whether a point occurs (use your weights)
    point_value <- sample(x@point_gains, 1, prob = x@point_gains_probabilities)
    
    # sample which player scores
    scorer <- sample(1:2, 1)
    
    x@round_scores[scorer] <- x@round_scores[scorer] + point_value
    if(x@print){cat(x@parties[scorer], " ")}
  }
  
  # Winner of the round
  winner <- which.max(x@round_scores)
  x@scores[winner] <- x@scores[winner] + 1
  sub_result <- paste0(x@round_scores[1], "-", x@round_scores[2])
  x@sub_results <- c(x@sub_results, sub_result)
  
  if(x@print){cat("\n")}
  
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
    if(x@print){cat(x@parties[scorer], " ")}
  }
  
  # Winner of the round
  winner <- which.max(x@round_scores)
  x@scores[winner] <- x@scores[winner] + 1
  sub_result <- paste0(x@round_scores[1], "-", x@round_scores[2])
  x@sub_results <- c(x@sub_results, sub_result)
  
  if(x@print){cat("\n")}
  
  # Reset for next round
  x@round_scores <- c(0,0)
  
  return(x)
})
setMethod("simulate_round", "Volleyball", function(x) {
  while(all(x@round_scores < x@points_to_win_round)) {
    #when both parties at 24 or more points, have to win by 2 
    overtime <- all(x@round_scores >= 24) && abs(diff(x@round_scores)) < 2
    if(overtime){
      x@points_to_win_round <- max(x@points_to_win_round, min(x@round_scores) + 2)
    }
    
    # sample whether a point occurs (use your weights)
    point_value <- sample(x@point_gains, 1, prob = x@point_gains_probabilities)
    
    # sample which player scores
    scorer <- sample(1:2, 1)
    
    x@round_scores[scorer] <- x@round_scores[scorer] + point_value
    if(x@print){cat(x@parties[scorer], " ")}
  }
  
  # Winner of the round
  winner <- which.max(x@round_scores)
  x@scores[winner] <- x@scores[winner] + 1
  sub_result <- paste0(x@round_scores[1], "-", x@round_scores[2])
  x@sub_results <- c(x@sub_results, sub_result)
  
  if(x@print){cat("\n")}
  
  # Reset for next round
  x@round_scores <- c(0,0)
  
  return(x)
})

setGeneric("simulate_minute", function(x) standardGeneric("simulate_minute"))
setMethod("simulate_minute", "TimeGame", function(x) {
  
  lambda <- x@expected_points_per_game / x@game_duration
  points_scored <- rpois(1, lambda)
  
  for(i in seq_len(points_scored)) {
    scorer <- sample(1:2, 1)
    x@scores[scorer] <- x@scores[scorer] + 1
    if(x@print){cat(x@parties[scorer], " at minute ", i, "\n")}
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
  if(x@print){cat("Game started\n")}
  while(all(x@scores < x@rounds_to_win_game)) {
    if(x@print){cat("Set ", sum(x@scores) + 1, "\n")}
    x <- simulate_round(x)
    sub_result <- x@sub_results[[length(x@sub_results)]]
    if(x@print){cat(sub_result, "\n")}
  }
  if(x@print){cat("Game ended\n")}
  x@game_state <- "finished"
  x
})
setMethod("play_game", "TimeGame", function(x) {
  
  x <- callNextMethod()
  if(x@print){cat("Game started\n")}
  for(i in 1:x@game_duration) {
    x <- simulate_minute(x)
  }
  if(x@print){cat("Game ended\n")}
  
  x@game_state <- "finished"
  x
})

setMethod("play_game", "TTGame", function(x) {
  callNextMethod()   # automatically uses SetGame version
})

setMethod("play_game", "Volleyball", function(x) {
  
  if(x@print){cat("Game started\n")}
  while(all(x@scores < x@rounds_to_win_game)) {
    if(x@print){cat("Set ", sum(x@scores) + 1, "\n")}
    if(all((x@scores == x@rounds_to_win_game-1))) {
      x@points_to_win_round <- 15
    }
    x <- simulate_round(x)
    sub_result <- x@sub_results[[length(x@sub_results)]]
    if(x@print){cat(sub_result, "\n")}
  }
  
  if(x@print){cat("Game ended\n")}
  x@game_state <- "finished"
  x
})

setGeneric("show", function(x) standardGeneric("show"))
setMethod("show", "Game", function(x) {
  cat("Game between:", paste(x@parties, collapse=" vs "), "\n")
  cat("State:", x@game_state, "\n")
  if(x@game_state == "in progress" || x@game_state == "finished") {
    cat("Score:", paste(x@scores, collapse=" - "), "\n")
  }
  if(x@game_state == "not started") {
    cat("No results yet. \n")
  }
  return(invisible(x))
})

setMethod("show", "SetGame", function(x) {
  cat(x@game_state, "\n")
  cat(paste(x@parties, collapse = " vs."),"\n")
  if(x@game_state == "finished") {
    cat(paste(x@scores, collapse=" - "), "\n")
    cat(paste(x@sub_results, collapse=", "), "\n")
  } else {
    cat("No results yet.\n")
  }
  return(invisible(x))
})

setMethod("show", "TimeGame", function(x) {
  callNextMethod()
  return(invisible(x))
})

setMethod("show", "TTGame", function(x) {
  cat("A Game of Table Tennis: ")
  callNextMethod()
  return(invisible(x))
})
setMethod("show", "Volleyball", function(x) {
  cat("A Game of Volleyball: ")
  callNextMethod()
  return(invisible(x))
})
setGeneric("show_game_course", function(x) standardGeneric("show_game_course"))
setMethod("show_game_course", "Game", function(x) {
  if(x@game_state != "not started") {
    cat("Game has already concluded \n")
    return()
  }
  cat("Game between:", paste(x@parties, collapse=" vs "), "\n")
  x@print <- TRUE
  x <- play_game(x)
  x@print <- FALSE
  cat("Final Score:", paste(x@scores, collapse=" - "), "\n")
  return(x)
})

setClass(
  "TTGame",
  contains = "SetGame",
  prototype = list(
    rounds_to_win_game  = 2,
    points_to_win_round = 11,
    
    point_gains         = 1,
    point_gains_probabilities = 1
  )
)

setClass(
  "Volleyball",
  contains = "SetGame",
  prototype = list(
    rounds_to_win_game  = 3,
    points_to_win_round = 25,
    
    point_gains         = 1,
    point_gains_probabilities = 1
  )
)

setClass(
  "Football",
  contains = "TimeGame",
  prototype = list(
    game_duration = 90,
    expected_points_per_game = 3.7
  )
)


#Example Usage
tt1 <- new("TTGame", parties = c("A. B.", "C. D."))
show(tt1)
tt1 <- play_game(tt1)
show(tt1)
tt2 <- new("TTGame", parties = c("E. F.", "G. H."))
tt2 <- show_game_course(tt2)


vb1 <- new("Volleyball", parties = c("I", "J"))
show(vb1)
vb1 <- play_game(vb1)
show(vb1)
vb2 <- new("Volleyball", parties = c("A", "B"))
vb2 <- show_game_course(vb2)


fb1 <- new("Football", parties = c("Team X", "Team Y"))
show(fb1)
fb1 <- play_game(fb1)
show(fb1)
fb2 <- new("Football", parties = c("Team A", "Team B"))
fb2 <- show_game_course(fb2)
