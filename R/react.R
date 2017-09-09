#' React to player's actions during the Castle of R game
#'
#' This function is called in the background after each of the player's actions
#' and responds accordingly. It is the "engine" of the game, modifying the game
#' environment according to the game's logic.
react <- function(game, ...){
  
  # if time is up
  if (difftime(Sys.time(), game$roomStartTime, units = "mins") >
      game$currentRoom$timeLimit) {
    game$loseScenario("Too late.")
    return(TRUE)
  }
  
  game$deparsedExpr <- deparse(game$expr)
  
  if (game$compareExpression("endGame()")) {
    gameEnded <- game$endGame("Ending game.")
    if (gameEnded) {
      return(TRUE)
    }
  }
  
  if (game$compareExpression("wtf()")) {
    game$wtf()
  }
  
  if (game$compareExpression("timeLeft()")) {
    game$timeLeft()
  }
  
  if (game$compareExpression("seePassword()")) {
    game$plotPwd()
  }
  
  if (game$compareExpression("whatDoIHave()")) {
    game$whatDoIHave()
  }
  
  if (game$compareExpression("hint()") ||
      game$compareExpression("solution()")) {
    type <- regmatches(game$deparsedExpr, gregexpr("[a-z]+",
                                                   game$deparsedExpr))[[1]]
    game$hintSolution(type)
  }
  
  if (game$compareExpression("whatWasTheQuestion()")) {
    game$whatWasTheQuestion()
  }
  
  if (game$compareExpression("summonRDragon()")) {
    lostOrWon <- game$summonRDragon()
    if (lostOrWon) {
      return(TRUE)
    }
  }
  
  if (game$compareExpression("^seeMap\\([0-9]+\\)$", TRUE)) {
    game$seeMap()
  }
  
  if (game$compareExpression("^openDoor\\([0-9]+\\)$", TRUE)) {
    game$openDoor()
  }
  
  if (game$compareExpression("^lockDoor\\([0-9]+\\)$", TRUE)) {
    game$lockDoor()
  }
  
  if (game$compareExpression("^takeObject\\([0-9]+\\)$", TRUE)) {
    game$takeObject()
  }
  
  if (!is.null(game$mode)) {
    game$reactToCall()
  }
  
  return(TRUE)
}