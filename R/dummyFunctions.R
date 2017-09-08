#' End the Castle of R game
#'
#' This function will end the Castle of R game. Before ending the game it will
#' give the player the option of saving the game and coming back later. It will
#' also suggest to clean your workspace and plots entirely, for a better game
#' experience. Be careful before accepting this suggestion.
#' 
#' @note In some scenarios, the player will not be able to end the game until
#' the scenario is over.
#' 
#' @examples
#' endGame()
#' 
#' @export
endGame <- function() {
  invisible()
}

#' Open door to another room
#'
#' This function will transfer the player to another room if she answers a
#' question correctly.
#' 
#' @param n Door number, as it appears next to the door description, when the
#' player enters the room, or with the \code{wtf} function)
#' 
#' @note You may ask to \code{openDoor}, see the question, then decide you wish
#' to do something else, e.g. open a different door or taking an object.
#' 
#' @examples
#' openDoor(1)
#' 
#' @export
openDoor <- function(n) {
  invisible() 
}

#' Lock door to another room
#'
#' This function will lock the door to another room, if it is open.
#' 
#' @param n Door number, as it appears next to the door description, when the
#' player enters the room, or with the \code{wtf} function)
#' 
#' @note A locked door means another question if you ever wish to go back from
#' that door to the room from which you came from. However, it also means more
#' time before Lady R gets into the room.
#' 
#' @examples
#' lockDoor(1)
#' 
#' @export
lockDoor <- function(n) {
  invisible() 
}

#' Take an object in a room
#'
#' This function lets you take a specific object while in a room.
#' 
#' @param n Object number, as it appears next to the object description, when the
#' player enters the room, or with the \code{wtf} function)
#' 
#' @note Some objects might give you valuable clues on how to escape the Castle.
#' 
#' @examples
#' takeObject(1)
#' 
#' @export
takeObject <- function(n) {
  invisible() 
}

#' Debrief a player of his overall whereabouts
#'
#' This function repeats the greeting message of a room, detailing the doors
#' and objects (if any) around you. It also summarizes how much R Power you
#' have and the objects you have taken in your satchel.
#' 
#' @examples
#' wtf()
#' 
#' @export
wtf <- function() {
  invisible() 
}

#' Remind a player of the current question
#'
#' This function will remind you the current question in case you forgot.
#' 
#' @examples
#' whatWasTheQuestion()
#' 
#' @export
whatWasTheQuestion <- function() {
  invisible() 
}

#' Summarise a player's resources
#'
#' This function will remind you the current content of your satchel, how much R
#' Power you have and other resources.
#' 
#' @examples
#' whatDoIHave()
#' 
#' @export
whatDoIHave <- function() {
  invisible() 
}

#' Look at a map
#'
#' This function lets you look at a map, if you have one. The map is of an
#' entire floor in the Castle.
#' 
#' @param n Floor number (1 for 1st floor, etc.)
#' 
#' @examples
#' seeMap(1)
#' 
#' @export
seeMap <- function(n) {
  invisible()
}

#' Look at the password
#'
#' This function will let you look at the current situation of the password.
#' 
#' @examples
#' seePassword()
#' 
#' @export
seePassword <- function() {
  invisible()
}

#' @export
summonRDragon <- function() {
  invisible()
}

#' Give the player a hint for the current question.
#'
#' This function will give you a short hint regarding the solution to the
#' current question.
#' 
#' @note Viewing a hint costs R Power.
#' 
#' @examples
#' hint()
#' 
#' @export
hint <- function() {
  invisible()
}

#' Give the player the solution for the current question.
#'
#' This function will give you the solution to the current question.
#' 
#' @note Viewing a solution costs R Power.
#' 
#' @examples
#' solution()
#' 
#' @export
solution <- function() {
  invisible()
}

#' Give the player the solution for the current question.
#'
#' This function will give you the solution to the current question.
#' 
#' @note Viewing a solution costs R Power.
#' 
#' @examples
#' solution()
#' 
#' @export
timeLeft <- function() {
  invisible()
}