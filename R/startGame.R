#' Start the Castle of R game
#'
#' This function will start the Castle of R game. If you played the game before,
#' ended the game in the middle and chose to save your progress - this
#' function will identify this and will give you the option to pick up where
#' you left. It will also suggest to clean your workspace and plots entirely,
#' for a better game experience. Be careful before accepting this suggestion.
#' 
#' @examples
#' startGame()
#' 
#' @export
startGame <- function(...){
  if (exists(".gameOn") && .gameOn) {
    message("Game id already on, you can endGame() and come back later.")
    invisible()
  } else {
    removeTaskCallback("CastleOfR")
    .gameOn <<- TRUE
    message("Before you start, can I clean your workspace and plots?")
    cleanAns <- menu(c("yes", "no")) == 1
    if (cleanAns) {
      graphics.off()
      tryCatch(rm(list = ls(envir = globalenv()), envir = globalenv()),
               warning = function(w) {invisible()})
    }
    continue <- FALSE
    playerLevel <- NULL
    if (file.exists(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))) {
      message("You've been here before. Pick up from where you left?")
      continue <- menu(c("yes", "no")) == 1
    }
    game <- if (continue) {
      readRDS(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
    } else {
      message("You have been cordially invited to have tea with Lady R, at the Castle of R.")
      message("How would you describe your level of proficiency in R?")
      playerLevel <- menu(c("My dear, I'm the Master.",
                            "I'm pretty good actually.",
                            "I get by.",
                            "What is R?"))
      initializeGame(ifelse(playerLevel == 0, 4, playerLevel))
    }
    cb <- function(expr, val, ok, vis, data = game){
      game$expr <- expr
      game$val <- val
      game$ok <- ok
      game$vis <- vis
      return(react(game, ...))
    }
    if (continue && game$currentRoom$name != "lounge") {
      game$currentRoom$greet()
    } else {
      game$currentRoom$startScenario()
    }
    game$roomStartTime <- Sys.time()
    addTaskCallback(cb, name = "CastleOfR")
    invisible()
  }
}