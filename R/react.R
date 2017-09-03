react <- function(game, ...){
  deparsedExpr <- deparse(game$expr)
  if (game$expr == "endGame()") {
    if (class(game$currentRoom)[1] %in% c("TimeRoom", "DarkRoom")) {
      message("Can't end game in this room.")
    } else {
      game$endGame("Ending game.")
      #removeTaskCallback("CastleOfR")
      return(TRUE)
    }
  }
  if (difftime(Sys.time(), game$roomStartTime, units = "mins") > game$currentRoom$timeLimit) {
    game$endGame("Too late, Lady R is here.")
    return(TRUE)
  }
  if (game$expr == "debrief()") {
    game$debrief()
  }
  if (game$expr == "seePassword()") {
    game$plotPwd()
  }
  if (game$expr == "whatWasTheQuestion()") {
    if (!is.null(game$riddle)) {
      game$riddle$askQuestion()
    } else {
      message("No  question.")
    }
  }
  if (game$expr == "summonRDragon()") {
    if (game$escapeRoom$name == game$currentRoom$name) {
      message("The R Dragon comes flying. She's asking for the password.\nUse password(\"12345\") to give it to her.")
      message(paste0("pwd is: ", paste0(game$pwd, collapse = "")))
    } else {
      message("You have not reached the Room from which you can escape.")
    }
  }
  if (grepl("^password\\(\"?[A-Za-z0-9]+\"?\\)$", deparsedExpr)) {
    if (game$escapeRoom$name == game$currentRoom$name) {
      inputPwd <- unlist(regmatches(deparsedExpr, gregexpr("[A-Za-z0-9]+", deparsedExpr)))[2]
      message(paste0("input pwd is: ", inputPwd))
      game$password(inputPwd)
    } else {
      message("You have not reached the Room from which you can escape.")
    }
  }
  if (grepl("^seeMap\\([0-9]+\\)$", deparsedExpr)) {
    map_idx <- as.numeric(unlist(regmatches(deparsedExpr, gregexpr("[0-9]+", deparsedExpr))))
    if (map_idx > 0 && map_idx <= length(game$floorMapsAvailable)) {
      if (toString(map_idx) %in% names(game$floorMapsPlayer)) {
        game$plotMap(game$floorMapsPlayer[[toString(map_idx)]])
      } else {
        message("You don't have the map to floor ", map_idx)
      }
    } else {
      message("Bad index number for floor.")
    }
    
  }
  if (grepl("^openDoor\\([0-9]+\\)$", deparsedExpr)) {
    game$door_idx <- as.numeric(unlist(regmatches(deparsedExpr, gregexpr("[0-9]+", deparsedExpr))))
    if (game$door_idx > 0 && game$door_idx <= length(game$currentRoom$door)) {
      game$nextRoom <- game$currentRoom$door[[game$door_idx]]$getNextRoom(game$currentRoom$name)
      if (!game$currentRoom$door[[game$door_idx]]$open) {
        game$mode <- "door"
        game$riddle <- game$currentRoom$door[[game$door_idx]]$getRiddle(game$currentRoom$name)
        game$riddle$askQuestion()
      } else {
        message("Door is open.")
        game$directionChosen <- game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        game$previousRoom <- game$currentRoom
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$roomStartTime <- Sys.time()
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$currentRoom$greet(game$directionChosen)
          if (game$RPower >= game$currentRoom$RPower) {
            # subtract necessary R Power and go back to previous room
            game$RPower <- game$RPower - game$currentRoom$RPower
            game$directionChosen <- game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
            message("Yes you do! You're drawn back to the previous room.\n")
            game$currentRoom <- game$previousRoom
            game$previousRoom <- NULL
            game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                             game$currentRoom$countLockedDoors() *
                                             game$lockedDoorDelay)
            game$currentRoom$greet(game$directionChosen)
            game$roomStartTime <- Sys.time()
            game$mode <- NULL
            game$riddle <- NULL
            game$nextRoom <- NULL
          } else {
            game$endGame(paste0("Oh no, you only have ", game$RPower,
                                " points. She's got you.\n\nGame over."))
            return(TRUE)
          }
        } else {
          game$currentRoom$greet(game$directionChosen)
          game$mode <- NULL
          game$riddle <- NULL
          game$nextRoom <- NULL
        }
      }
    } else {
      message("Bad index number for door.")
    }
  }
  if (grepl("^lockDoor\\([0-9]+\\)$", deparsedExpr)) {
    idx <- as.numeric(unlist(regmatches(deparsedExpr, gregexpr("[0-9]+", deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$door)) {
      game$nextRoom <- game$currentRoom$door[[idx]]$getNextRoom(game$currentRoom$name)
      game$currentRoom$door[[idx]]$lockDoor()
      game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                       game$currentRoom$countLockedDoors() *
                                       game$lockedDoorDelay)
    } else {
      message("Bad index number for door.")
    }
  }
  if (grepl("^takeObject\\([0-9]+\\)$", deparsedExpr)) {
    idx <- as.numeric(unlist(regmatches(deparsedExpr, gregexpr("[0-9]+", deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$object)) {
      if (!game$currentRoom$object[[idx]]$taken) {
        game$mode <- "object"
        game$object_idx <- idx
        game$riddle <- game$currentRoom$object[[idx]]$riddle
        game$riddle$askQuestion()
      } else {
        message("object taken")
      }
    } else {
      message("Bad index number of object.")
    }
  }
  if (!is.null(game$mode)) {
    if (game$expr == game$riddle$solution || (!is.null(game$val) && game$val == game$riddle$val)) {
      message("Correct!")
      if (game$mode == "door") {
        game$directionChosen <- game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        message(paste0("Door to ", game$directionChosen, " opens."))
        game$previousRoom <- game$currentRoom
        game$currentRoom$door[[game$door_idx]]$openDoor()
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$nextRoom <- NULL
        #game$door_idx <- NULL
        #game$currentRoom$greet(game$directionChosen)
        game$roomStartTime <- Sys.time()
        game$mode <- NULL
        game$riddle <- NULL
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$currentRoom$greet(game$directionChosen)
          if (game$RPower >= game$currentRoom$RPower) {
            # subtract necessary R Power and go back to previous room
            message("Yes you do! You're drawn back to the previous room.\n")
            game$RPower <- game$RPower - game$currentRoom$RPower
            game$directionChosen <- game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
            game$currentRoom <- game$previousRoom
            game$previousRoom <- NULL
            game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                             game$currentRoom$countLockedDoors() *
                                             game$lockedDoorDelay)
            game$currentRoom$greet(game$directionChosen)
            game$roomStartTime <- Sys.time()
            game$mode <- NULL
          } else {
            game$endGame(paste0("Oh no, you only have ", game$RPower,
                                " points. She's got you.\n\nGame over."))
            return(TRUE)
          }
        } else {
          game$currentRoom$greet(game$directionChosen)
        }
      } else if (game$mode == "object") {
        #message(paste0(game$currentRoom$object[[game$object_idx]]$name, " in your satchel."))
        game$currentRoom$object[[game$object_idx]]$takeObject()
        game$satchel <- c(game$satchel, game$currentRoom$object[[game$object_idx]])
        objType <- game$currentRoom$object[[game$object_idx]]$type
        if (objType == "power") {
          message(paste0("You gained ",
                         game$currentRoom$object[[game$object_idx]]$points,
                         " points of R Power!"))
          game$RPower <- game$RPower + game$currentRoom$object[[game$object_idx]]$points
        } else if (objType == "pwd") {
          message(paste0("There's something written on the ",
                         game$currentRoom$object[[game$object_idx]]$name,
                         ". Look at the plot window."))
          game$pwdExposedIdx <- c(game$pwdExposedIdx, 
                                  sample(setdiff(1:length(game$pwd), game$pwdExposedIdx), 1))
          game$plotPwd()
        } else if (objType == "tip") {
          message(paste0("There's something written on the ",
                         game$currentRoom$object[[game$object_idx]]$name,
                         ": ", game$currentRoom$object[[game$object_idx]]$riddle$tip))
        } else if (objType == "map") {
          newMapIdx <- game$currentRoom$object[[game$object_idx]]$riddle$floorMapsIdx
          mapsNames <- names(game$floorMapsPlayer)
          if (!newMapIdx %in% mapsNames) {
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <- game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            game$plotMap(game$floorMapsPlayer[[newMapIdx]])
            message("It's a map! Look at the plot window.")
          } else {
            message("It's a map, but you already have it.")
          }
        }
        game$object_idx <- NULL
        game$mode <- NULL
        game$riddle <- NULL
      } else if (game$mode == "time") {
        if (game$trRiddleIdx == length(game$currentRoom$riddle)) {
          # give player map if not already there
          if (!game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer)) {
            mapsNames <- names(game$floorMapsPlayer)
            newMapIdx <- game$currentRoom$floorMapsIdx
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <- game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            game$plotMap(game$floorMapsPlayer[[newMapIdx]])
            message("You got a map!\n\nReturning to previous room.")
          } else {
            message("You already have a map...\n\nReturning to previous room.")
          }
          # return to previous room
          game$directionChosen <- game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
          game$currentRoom <- game$previousRoom
          game$previousRoom <- NULL
          game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                           game$currentRoom$countLockedDoors() *
                                           game$lockedDoorDelay)
          game$currentRoom$greet(game$directionChosen)
          game$roomStartTime <- Sys.time()
          game$mode <- NULL
          game$riddle <- NULL
        } else {
          game$trRiddleIdx <- game$trRiddleIdx + 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          game$riddle$askQuestion()
        }
      }
    }
  }
  return(TRUE)
}