#' Start the Castle of R game
#'
#' This function will start the Castle of R game.
#' @export
#' @examples
#' startGame()
startGame <- function(...){
  removeTaskCallback("CastleOfR")
  message("Before you start, can I clean your workspace?")
  cleanAns <- menu(c("yes", "no")) == 1
  if (cleanAns) {
    graphics.off()
    tryCatch(rm(list = ls(envir = globalenv()), envir = globalenv()),
             warning = function(w) {invisible()})
  }
  continue <- FALSE
  if (file.exists(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))) {
    message("You've been here before. Continue where you left?")
    continue <- menu(c("yes", "no")) == 1
  } else {
    message("You have been cordially invited to have tea with Lady R, at the Castle of R.")
    message("How would you describe your level of proficiency in R?")
    playerLevel <- menu(c("My dear, I'm the Master.",
           "I'm pretty good actually.",
           "I get by.",
           "What is R?"))
  }
  game <- initializeGame(continue, playerLevel)
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