#' Initialize the Castle of R game environment
#' 
#' This function reads in the various text files containing the data and
#' configuration of the various game components such as rooms, doors, objects
#' and riddles.
initializeGame <- function(playerLevel) {
  
  game <- new.env(globalenv())
    
  #lounge
  lounge <- Lounge$new("lounge", "The Lounge", "1", NA)
  lounge$set_objects(list(Object$new("teacup", "on the table", "power", 3,
                                     Riddle$new("what is 0 + 0?", "0 + 0", 0,
                                                "zero..."))))
  
  #bridge
  bridge <- Bridge$new("bridge", "The Bridge of Doom", "4", NA)
  
  # rooms
  rooms_file <- system.file("extdata", "CastleOfR_Rooms.txt",
                            package = "CastleOfR")
  rooms_df <- read.table(rooms_file, stringsAsFactors = FALSE, header = TRUE,
                         sep = "\t", comment.char = "|")
  rooms_list <- apply(rooms_df, 1, function(room) Room$new(room[["name"]],
                                                           room[["title"]],
                                                           room[["floor"]],
                                                           room[["comment"]]))
  names(rooms_list) <- rooms_df$name
  list2env(rooms_list, envir = environment())
  
  # time rooms
  timeRooms_file <- system.file("extdata", "CastleOfR_TimeRooms.txt",
                                package = "CastleOfR")
  timeRooms_df <- read.table(timeRooms_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  timeRooms_list <- apply(timeRooms_df, 1,
                          function(room) TimeRoom$new(room[["name"]],
                                                      room[["title"]],
                                                      room[["timeLimit"]],
                                                      room[["floorMapsIdx"]]))
  names(timeRooms_list) <- timeRooms_df$name
  list2env(timeRooms_list, envir = environment())
  
  # dark rooms
  darkRooms_file <- system.file("extdata", "CastleOfR_DarkRooms.txt",
                                package = "CastleOfR")
  darkRooms_df <- read.table(darkRooms_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  darkRooms_list <- apply(darkRooms_df, 1, function(room) 
    DarkRoom$new(room[["name"]], room[["title"]], room[["nObjectsLeave"]]))
  names(darkRooms_list) <- darkRooms_df$name
  list2env(darkRooms_list, envir = environment())
  
  # doors
  doors_file <- system.file("extdata", "CastleOfR_Doors.txt",
                            package = "CastleOfR")
  doors_df <- read.table(doors_file, stringsAsFactors = FALSE, header = TRUE,
                         sep = "\t", comment.char = "|")
  
  doors_list <- apply(doors_df, 1, function(door) {
    riddle1 <- Riddle$new(door[["question1"]], door[["solution1"]],
                          door[["val1"]], door[["hint1"]],
                          NA, NA,
                          door[["prepare1"]], door[["cleanup1"]])
    riddle2 <- Riddle$new(door[["question2"]], door[["solution2"]],
                          door[["val2"]], door[["hint2"]],
                          NA, NA,
                          door[["prepare2"]], door[["cleanup2"]])
    Door$new(door[["direction_1to2"]],
             list(get(door[["room1"]]), get(door[["room2"]])),
             list(riddle1, riddle2))
  })
  names(doors_list) <- doors_df$name
  list2env(doors_list, envir = environment())
  
  # objects
  objects_file <- system.file("extdata", "CastleOfR_Objects.txt",
                              package = "CastleOfR")
  objects_df <- read.table(objects_file, stringsAsFactors = FALSE,
                           header = TRUE, sep = "\t", comment.char = "|")
  
  objects_list <- apply(objects_df, 1, function(object) {
    riddle <- Riddle$new(object[["question"]], object[["solution"]],
                         object[["val"]], object[["hint"]], object[["tip"]],
                         object[["floorMapsIdx"]], object[["prepare"]],
                         object[["cleanup"]])
    Object$new(object[["objName"]], object[["location"]], object[["type"]],
               object[["points"]], riddle)
  })
  names(objects_list) <- objects_df$name
  list2env(objects_list, envir = environment())
  
  # time rooms riddles
  trRiddles_file <- system.file("extdata", "CastleOfR_TimeRoomsRiddles.txt",
                                package = "CastleOfR")
  trRiddles_df <- read.table(trRiddles_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  trRiddles_list <- apply(trRiddles_df, 1, function(trRiddle)
    Riddle$new(trRiddle[["question"]], trRiddle[["solution"]],
               trRiddle[["val"]], trRiddle[["hint"]],
               NA, NA,
               trRiddle[["prepare"]], trRiddle[["cleanup"]]))
  names(trRiddles_list) <- trRiddles_df$name
  list2env(trRiddles_list, envir = environment())
  
  # maps
  mapsNames <- c("CastleOfR_floor1.png", "CastleOfR_floor2.png", "CastleOfR_floor3.png", "CastleOfR_Towers.png")
  mapsNames <- sapply(mapsNames, function(mapName) system.file("extdata", mapName, package = "CastleOfR"))
  floorMapsAvailable <- lapply(mapsNames, png::readPNG)
  names(floorMapsAvailable) <- 1:4
  
  # set doors to rooms
  getDoorsObjectsForRoom <- function(l, roomName) {
    mget(names(l)[which(grepl(roomName, names(l)))], inherits = TRUE)
  }
  invisible(lapply(rooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  invisible(lapply(darkRooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  invisible(lapply(timeRooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  lounge$set_doors(getDoorsObjectsForRoom(doors_list, lounge$name))
  bridge$set_doors(getDoorsObjectsForRoom(doors_list, bridge$name))
  
  # set objects to rooms
  invisible(lapply(rooms_list, function(room) room$set_objects(getDoorsObjectsForRoom(objects_list, room$name))))
  
  # set timeLimits to regular rooms
  game$playerLevel <- playerLevel
  
  roomTimeLimit <- switch(game$playerLevel,
                          "1" = 1,
                          "2" = 3,
                          "3" = 5,
                          "4" = 10)
  lockedDoorDelay <- roomTimeLimit / 5
  invisible(lapply(rooms_list, function(room) room$set_timeLimit(roomTimeLimit +
                                                                   lockedDoorDelay *
                                                                   room$countLockedDoors())))
  lounge$set_timeLimit(roomTimeLimit +
                         lockedDoorDelay * lounge$countLockedDoors())
  
  # set riddles to time rooms
  invisible(lapply(timeRooms_list, function(room) room$set_riddles(getDoorsObjectsForRoom(trRiddles_list, room$name))))
  
  # set password (no no uppercase I, lowercase L)
  pwd <- sample(c(LETTERS[-9], letters[-12], 0:9), 7, replace = TRUE)
  
  # game internal functions
  compareExpression <- function(targetExp, regex = FALSE) {
    if (regex) {
      grepl(targetExp, game$deparsedExpr)
    } else {
      !is.null(game$expr) && game$deparsedExpr != "NA" && game$expr == targetExp
    }
  }
  
  wtf <- function() {
    game$currentRoom$greet()
    game$whatDoIHave()
    game$timeLeft()
  }
  
  whatDoIHave <- function() {
    if (length(game$satchel) > 0) {
      message(paste0("In your satchel: ",
                     paste(lapply(game$satchel, function(obj) obj$name),
                           collapse = ", "),
                     "."))
    } else {
      "You have an empty satchel."
    }
    message(paste0("You have ", game$RPower, " points of R Power."))
    if (length(game$floorMapsPlayer) > 0) {
      if (length(game$floorMapsPlayer) == 1) {
        message(paste0("And a map to floor ", names(game$floorMapsPlayer)[1], "."))
      } else {
        message(paste0("And maps to floors: ",
                       paste0(names(game$floorMapsPlayer), collapse = ", "), "."))
      }
    } else {
      message("And no maps.")
    }
  }
  
  whatWasTheQuestion <- function() {
    if (!is.null(game$riddle)) {
      game$riddle$askQuestion()
    } else {
      message("You haven't been asked a question. Yet.")
    }
  }
  
  removeNObjectsFromSatchel <- function(n) {
    for (obj in 1:n) {
      message(paste0("Please select object ", obj, ":"))
      objIdx <- menu(lapply(game$satchel, function(obj) obj$name))
      game$satchel <- game$satchel[-objIdx]
    }
  }
  
  endGame <- function(endMessage = NULL) {
    if (class(game$currentRoom)[1] %in% c("TimeRoom", "DarkRoom")) {
      message("Can't end game in this room.")
      return(FALSE)
    } else {
      message(endMessage)
      if (is.null(game$mode) || !game$mode %in% c("time", "dark", "lose", "win")) {
        message("Save game so you can come back later and pick up from where you left?")
        saveAns <- menu(c("yes", "no")) == 1
        if (saveAns) {
          saveRDS(game, file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
        }
      }
      message("Before you go, can I clean your workspace and plots?")
      cleanAns <- menu(c("yes", "no")) == 1
      if (cleanAns) {
        graphics.off()
        tryCatch(rm(list = ls(envir = globalenv()), envir = globalenv()),
                 warning = function(w) {invisible()})
      }
      removeTaskCallback("CastleOfR")
      return(TRUE)
    }
  }
  
  plotMap <- function(map) {
    plot(1:2, type = "n", main = "", xlab = "", ylab = "", bty = "n",
         xaxt = "n", yaxt = "n")
    lim <- par()
    rasterImage(map, lim$usr[1], lim$usr[3], lim$usr[1] +
                  (dim(map)[1]/dim(map)[2]) *(lim$usr[2] - lim$usr[1]), lim$usr[4])
  }
  
  seeMap <- function() {
    map_idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                            gregexpr("[0-9]+",
                                                     game$deparsedExpr))))
    if (map_idx > 0 && map_idx <= length(game$floorMapsAvailable)) {
      if (toString(map_idx) %in% names(game$floorMapsPlayer)) {
        game$plotMap(game$floorMapsPlayer[[toString(map_idx)]])
      } else {
        message("You don't have the map to floor ", map_idx)
      }
    } else {
      message("No such floor.")
    }
  }
  
  plotPwd <- function(...) {
    defaultBGColor <- par(bg = "black")
    plot(1:length(game$pwd), 1:7, type = "n", main = "", xlab = "", ylab = "",
         bty = "n", xaxt = "n", yaxt = "n", ...)
    for (i in game$pwdExposedIdx) text(i, 4, labels = game$pwd[i],
                                       col = "white", cex = 3)
    on.exit(par(defaultBGColor))
  }
  
  isObjectInSatchel <- function(objName) {
    if (length(game$satchel) > 0) {
      objName %in% sapply(game$satchel, function(obj) obj$name)
    } else {
      FALSE
    }
  }
  
  wasObjectInSatchel <- function(objName) {
    if (length(game$satchelHist) > 0) {
      objName %in% sapply(game$satchelHist, function(obj) obj$name)
    } else {
      FALSE
    }
  }
  
  teacupScenario <- function() {
    message("\"Do you have my teacup?\"")
    ansTeacup <- menu(c("yes", "no", "I need to check"))
    isTeacup <- isObjectInSatchel("teacup")
    wasTeacup <- wasObjectInSatchel("teacup")
    if (ansTeacup == 1) {
      if (isTeacup) {
        game$winScenario()
        return(TRUE)
      } else {
        if (wasTeacup) {
          game$loseScenario("\"Of course not! You gave it away in one of the Dark Rooms!\"\nThe R dragon flaps her wings and off she goes, leaving you behind.")
          return(TRUE)
        } else {
          message("You're lying! If you ever want to escape this castle I suggest you go get me my teacup!")
          return(FALSE)
        }
      }
    } else if (ansTeacup == 2) {
      if (isTeacup) {
        game$winScenario()
        return(TRUE)
      } else {
        if (wasTeacup) {
          game$loseScenario("\"Of course not! You gave it away in one of the Dark Rooms!\"\nThe R dragon flaps her wings and off she goes, leaving you behind.")
          return(TRUE)
        } else {
          message("\"Well, if you ever want to escape this castle I suggest you go get me my teacup!\", says the R Dragon as she flies away.")
          return(FALSE)
        }
      }
    } else {
      message("\"Please do. When you're ready, summon the R Dragon again\", says the R Dragon as she flies away.")
      return(FALSE)
    }
  }
  
  summonRDragon <- function() {
    if (game$escapeRoom$name == game$currentRoom$name) {
      if (game$dragonSeen) {
        message("The R Dragon flies to the Castle roof again.")
      } else {
        game$dragonSeen <- TRUE
        message("Look throught the Tower window.")
        message("A magnificent dragon is flying your way. You thought the stories weren't true...")
        message("\nThe R Dragon!\n She is landing with all her glory on the Castle roof. She is asking you:")
      }
      message("\"What is the password?\"")
      message(paste0("pwd is: ", paste0(game$pwd, collapse = "")))
      inputPwd <- readline()
      if (game$isPasswordCorrect(inputPwd)) {
        message("Password is correct.")
        return(game$teacupScenario())
      } else {
        message("\"That's not the right password! Are you trying to fool the R dragon?!\" The R Dragon flies away.")
        return(FALSE)
      }
    } else {
      message("You have not reached the Room from which you can escape.")
      return(FALSE)
    }
  }
  isPasswordCorrect <- function(inputPwd) {
    inputPwdSplit <- strsplit(inputPwd, "")[[1]]
    identical(inputPwdSplit, game$pwd)
  }
  
  hintSolution <- function(type) {
    if (!is.null(game$riddle)) {
      RPowerCost <- ifelse(type == "hint", game$hintRPower, game$solutionRPower)
      if (game$RPower >= RPowerCost) {
        message(paste0("Are you sure you want to spend R Power on this ",
                       type, " (", RPowerCost, " points)?"))
        payRPower <- menu(c("yes", "no")) == 1
        if (payRPower) {
          if(type == "hint") {
            message(paste0("Hint: ", game$riddle$hint))
          } else {
            message(paste0("Solution: ", game$riddle$solution))
          }
          game$RPower <- game$RPower - RPowerCost
        }
      } else {
        message(paste0("You do not have enough R Power for this", type, " (",
                       RPowerCost, " point)"))
      }
    } else {
      message("No  question.")
    }
  }
  
  timeLeft <- function() {
    message(paste0("Lady R is coming to this room in ",
                   strftime(
                     as.POSIXct(
                       as.numeric(
                         difftime(game$roomStartTime +
                                    60 * game$currentRoom$timeLimit,
                                  Sys.time(), units = "sec")),
                       origin = Sys.Date()),
                     format = "%M:%S"), " minutes!"))
  }
  
  loseScenario <- function(loseMessage = NULL) {
    message(loseMessage)
    message("You can hear Lady R's crazy laughter right behind you.")
    message("You turn around and you see her waiving that knife.")
    message("\"Game over!\" she's shouting with her screeching voice, \"Move my lovely R gimp, move!\"")
    message("She leads you through the Castle to the Prison Tower, where you will spend the rest of your days multiplying matrices and munging data for the Lady.")
    message("That is, until the next R gimp comes...")
    game$mode <- "lose"
    game$endGame()
  }
  
  winScenario <- function() {
    message("\"Ah, my wonderful teacup. Did you know dragons absolutley love tea?\"")
    message("The R Dragon lets you climb on her back. While she sets off from the Castle roof you can see Lady R waving here hands and cursing \"You'll be back! They all do!\"")
    message("Congratulations. You escaped the Castle of R at the last minute.\n\nYou are truly a knight of R.")
    game$mode <- "win"
    game$endGame()
    return(TRUE)
  }
  
  openDoor <- function() {
    game$door_idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                                  gregexpr("[0-9]+",
                                                           game$deparsedExpr))))
    if (game$door_idx > 0 && game$door_idx <= length(game$currentRoom$door)) {
      game$nextRoom <-
        game$currentRoom$door[[game$door_idx]]$getNextRoom(game$currentRoom$name)
      if (!game$currentRoom$door[[game$door_idx]]$open) {
        game$mode <- "door"
        game$riddle <-
          game$currentRoom$door[[game$door_idx]]$getRiddle(game$currentRoom$name)
        if (!is.na(game$riddle$prepare)) {
          eval(parse(text = game$riddle$prepare))
        }
        game$riddle$askQuestion()
      } else {
        message("Door is open.")
        game$directionChosen <-
          game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        game$previousRoom <- game$currentRoom
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$roomStartTime <- Sys.time()
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(game$currentRoom$floorMapsIdx %in%
                                   names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("Question ", game$trRiddleIdx, " out of ",
                         length(game$currentRoom$riddle), ":"))
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$mode <- "dark"
          game$currentRoom$greet(game$directionChosen)
          if (length(game$satchel) >= game$currentRoom$nObjectsLeave) {
            # subtract necessary objects and go back to previous room
            message("Yes you do!")
            game$removeNObjectsFromSatchel(game$currentRoom$nObjectsLeave)
            message("Great! You're drawn back to the previous room.\n")
            game$directionChosen <-
              game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
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
            loseMessage <- paste0("Oh no, ", ifelse(length(game$satchel) == 0,
                                                    "you don't have any",
                                                    paste0("you only have ",
                                                           length(game$satchel))),
                                  " objects in your satchel.")
            game$loseScenario(loseMessage)
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
      message("No such door.")
    }
  }
  
  lockDoor <- function() {
    idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                        gregexpr("[0-9]+",
                                                 game$deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$door)) {
      game$currentRoom$door[[idx]]$lockDoor()
      game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                       game$currentRoom$countLockedDoors() *
                                       game$lockedDoorDelay)
      message("Door locked.")
    } else {
      message("No such door.")
    }
  }
  
  takeObject <- function() {
    idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                        gregexpr("[0-9]+",
                                                 game$deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$object)) {
      if (!game$currentRoom$object[[idx]]$taken) {
        game$mode <- "object"
        game$object_idx <- idx
        game$riddle <- game$currentRoom$object[[idx]]$riddle
        if (!is.na(game$riddle$prepare)) {
          eval(parse(text = game$riddle$prepare))
        }
        game$riddle$askQuestion()
      } else {
        message("object taken")
      }
    } else {
      message("No such object.")
    }
  }
  
  reactToCall <- function() {
    if (game$deparsedExpr == game$riddle$solution ||
        (is.numeric(game$val) && !is.na(game$riddle$val) &&
         game$val == game$riddle$val)) {
      message("Correct!")
      if (!is.na(game$riddle$cleanup)) {
        eval(parse(text = game$riddle$cleanup))
      }
      if (game$mode == "door") {
        game$directionChosen <- 
          game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        
        doorOpensMessage <- switch(game$directionChosen,
                                   "up" = "Hatch to an upper floor opens.",
                                   "down" = "Hatch to a lower floor opens.",
                                   paste0("Door to ", game$directionChosen, " opens."))
        message(doorOpensMessage)
        game$previousRoom <- game$currentRoom
        game$currentRoom$door[[game$door_idx]]$openDoor()
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$nextRoom <- NULL
        game$roomStartTime <- Sys.time()
        game$mode <- NULL
        game$riddle <- NULL
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(
            game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("Question ", game$trRiddleIdx, " out of ",
                         length(game$currentRoom$riddle), ":"))
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$mode <- "dark"
          game$currentRoom$greet(game$directionChosen)
          if (length(game$satchel) >= game$currentRoom$nObjectsLeave) {
            # subtract necessary R Power and go back to previous room
            message("Yes you do!")
            game$removeNObjectsFromSatchel(game$currentRoom$nObjectsLeave)
            message("Great! You're drawn back to the previous room.\n")
            game$directionChosen <-
              game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
            game$currentRoom <- game$previousRoom
            game$previousRoom <- NULL
            game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                             game$currentRoom$countLockedDoors() *
                                             game$lockedDoorDelay)
            game$currentRoom$greet(game$directionChosen)
            game$roomStartTime <- Sys.time()
            game$mode <- NULL
          } else {
            loseMessage <- paste0("Oh no, ", ifelse(length(game$satchel) == 0,
                                                    "you don't have any",
                                                    paste0("you only have ",
                                                           length(game$satchel))),
                                  " objects in your satchel.")
            game$loseScenario(loseMessage)
            return(TRUE)
          }
        } else {
          game$currentRoom$greet(game$directionChosen)
        }
      } else if (game$mode == "object") {
        message(paste0("You take the ", game$currentRoom$object[[game$object_idx]]$name,
                       " and put it in your satchel."))
        game$currentRoom$object[[game$object_idx]]$takeObject()
        game$satchel <- c(game$satchel, game$currentRoom$object[[game$object_idx]])
        game$satchelHist <- game$satchel
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
                                  sample(setdiff(1:length(game$pwd),
                                                 game$pwdExposedIdx), 1))
          game$plotPwd()
        } else if (objType == "tip") {
          message(paste0("There's something written on the ",
                         game$currentRoom$object[[game$object_idx]]$name,
                         ": ", game$currentRoom$object[[game$object_idx]]$riddle$tip))
        } else if (objType == "map") {
          newMapIdx <- game$currentRoom$object[[game$object_idx]]$riddle$floorMapsIdx
          mapsNames <- names(game$floorMapsPlayer)
          if (!newMapIdx %in% mapsNames) {
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <-
              game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            message(paste0("It's a map! To see it enter seeMap(", newMapIdx, ")"))
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
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <-
              game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            message(paste0("You got a map! To see it enter seeMap(",
                           newMapIdx, ")\n\nReturning to previous room."))
          } else {
            message("You already have this map...\n\nReturning to previous room.")
          }
          # return to previous room
          game$directionChosen <-
            game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
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
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("Question ", game$trRiddleIdx, " out of ",
                         length(game$currentRoom$riddle), ":"))
          game$riddle$askQuestion()
        }
      }
    }
  }
  
  # additional stuff
  currentRoom = lounge
  nextRoom = NULL
  previousRoom = NULL
  deparsedExpr = NULL
  directionChosen = NULL
  roomStartTime = NULL
  satchel = list()
  satchelHist = list()
  mode = NULL
  door_idx = NULL
  object_idx = NULL
  riddle = NULL
  RPower = 0
  trRiddleIdx = NULL
  floorMapsPlayer = list()
  pwdExposedIdx = NULL
  escapeRoom = osTower
  hintRPower = 1
  solutionRPower = 2
  dragonSeen = FALSE
  
  list2env(mget(ls()), envir = game)
  
  return(game)
}