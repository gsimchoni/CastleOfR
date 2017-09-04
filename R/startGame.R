#' Start the Castle of R game
#'
#' This function will start the Castle of R game.
#' @export
#' @examples
#' startGame()
startGame <- function(...){
  removeTaskCallback("CastleOfR")
  continue <- FALSE
  if (file.exists(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))) {
    message("You've been here before. Continue where yous left?")
    continue <- menu(c("yes", "no")) == 1
  }
  game <- initializeGame(continue)
  cb <- function(expr, val, ok, vis, data = game){
    game$expr <- expr
    game$val <- val
    game$ok <- ok
    game$vis <- vis
    return(react(game, ...))
  }
  if (continue) {
    game$currentRoom$greet()
  } else {
    game$currentRoom$startScenario()
  }
  game$roomStartTime <- Sys.time()
  addTaskCallback(cb, name = "CastleOfR")
  invisible()
}