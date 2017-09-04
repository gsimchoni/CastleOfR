initializeGame <- function(continue) {
  #source("castleOfR_intro.R")
  #library(R6)
  
  # if game environment exists, "game <- readRDS("CastleOfR_game.RData")" else 
  if (continue) {
    game <- readRDS(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
  } else {
    game <- new.env(globalenv())
    #lounge
    lounge <- Lounge$new("lounge", "The Lounge")
    # rooms
    rooms_file <- system.file("extdata", "CastleOfR_Rooms.txt", package = "CastleOfR")
    rooms_df <- read.table(rooms_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    rooms_list <- apply(rooms_df, 1, function(room) Room$new(room[["name"]], room[["title"]]))
    names(rooms_list) <- rooms_df$name
    list2env(rooms_list, envir = environment())
    
    # time rooms
    timeRooms_file <- system.file("extdata", "CastleOfR_TimeRooms.txt", package = "CastleOfR")
    timeRooms_df <- read.table(timeRooms_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    timeRooms_list <- apply(timeRooms_df, 1,
                            function(room) TimeRoom$new(room[["name"]],
                                                        room[["title"]],
                                                        room[["timeLimit"]],
                                                        room[["floorMapsIdx"]]))
    names(timeRooms_list) <- timeRooms_df$name
    list2env(timeRooms_list, envir = environment())
    
    # dark rooms
    darkRooms_file <- system.file("extdata", "CastleOfR_DarkRooms.txt", package = "CastleOfR")
    darkRooms_df <- read.table(darkRooms_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    darkRooms_list <- apply(darkRooms_df, 1, function(room) 
      DarkRoom$new(room[["name"]], room[["title"]], room[["RPower"]]))
    names(darkRooms_list) <- darkRooms_df$name
    list2env(darkRooms_list, envir = environment())
    
    # doors
    doors_file <- system.file("extdata", "CastleOfR_Doors.txt", package = "CastleOfR")
    doors_df <- read.table(doors_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    
    doors_list <- apply(doors_df, 1, function(door) {
      riddle1 <- Riddle$new(door[["question1"]], door[["solution1"]],
                            door[["val1"]], door[["hint1"]])
      riddle2 <- Riddle$new(door[["question2"]], door[["solution2"]],
                            door[["val2"]], door[["hint2"]])
      Door$new(door[["direction_1to2"]],
               list(get(door[["room1"]]), get(door[["room2"]])),
               list(riddle1, riddle2))
    })
    names(doors_list) <- doors_df$name
    list2env(doors_list, envir = environment())
    
    # objects
    objects_file <- system.file("extdata", "CastleOfR_Objects.txt", package = "CastleOfR")
    objects_df <- read.table(objects_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    
    objects_list <- apply(objects_df, 1, function(object) {
      riddle <- Riddle$new(object[["question"]], object[["solution"]],
                           object[["val"]], object[["hint"]], object[["tip"]],
                           object[["floorMapsIdx"]])
      Object$new(object[["objName"]], object[["location"]], object[["type"]],
                 object[["points"]], riddle)
    })
    names(objects_list) <- objects_df$name
    list2env(objects_list, envir = environment())
    
    # time rooms riddles
    trRiddles_file <- system.file("extdata", "CastleOfR_TimeRoomsRiddles.txt", package = "CastleOfR")
    trRiddles_df <- read.table(trRiddles_file, stringsAsFactors = FALSE, header = TRUE, sep = "\t")
    trRiddles_list <- apply(trRiddles_df, 1, function(trRiddle)
      Riddle$new(trRiddle[["question"]], trRiddle[["solution"]], trRiddle[["val"]], trRiddle[["hint"]]))
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
    
    # set objects to rooms
    invisible(lapply(rooms_list, function(room) room$set_objects(getDoorsObjectsForRoom(objects_list, room$name))))
    
    # set timeLimits to rooms
    roomTimeLimit <- 2
    lockedDoorDelay <- 1
    invisible(lapply(rooms_list, function(room) room$set_timeLimit(roomTimeLimit +
                                                                     lockedDoorDelay *
                                                                     room$countLockedDoors())))
    
    # set riddles to time rooms
    invisible(lapply(timeRooms_list, function(room) room$set_riddles(getDoorsObjectsForRoom(trRiddles_list, room$name))))
    
    # password
    pwd <- sample(c(LETTERS[-9], letters[-12], 0:9), 7, replace = TRUE)
    
    # game internal functions
    debrief <- function() {
      game$currentRoom$greet()
      #message(paste0("In your satchel: ", paste(lapply(game$satchel, function(obj) obj$name), collapse = ", ")))
      message(paste0("R Power: ", game$RPower, " points"))
      message(paste0("Time left in room: ",
                     strftime(
                       as.POSIXct(
                         as.numeric(
                           difftime(game$roomStartTime +
                                      60 * game$currentRoom$timeLimit,
                                    Sys.time(), units = "sec")),
                         origin = Sys.Date()),
                       format = "%M:%S")))
    }
    
    endGame <- function(endMessage) {
      message(endMessage)
      if (is.null(game$mode) || !game$mode == "end") {
        message("Save game so you can come back later?")
        saveAns <- menu(c("yes", "no")) == 1
        if (saveAns) {
          saveRDS(game, file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
        }
      }
      removeTaskCallback("CastleOfR")
    }
    
    plotMap <- function(map) {
      plot(1:2, type = "n", main = "", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
      lim <- par()
      rasterImage(map, lim$usr[1], lim$usr[3], lim$usr[1] +
                    (dim(map)[1]/dim(map)[2]) *(lim$usr[2] - lim$usr[1]), lim$usr[4])
    }
    plotPwd <- function(...) {
      defaultBGColor <- par(bg = "black")
      plot(1:length(game$pwd), 1:7, type = "n", main = "", xlab = "", ylab = "",
           bty = "n", xaxt = "n", yaxt = "n", ...)
      for (i in game$pwdExposedIdx) text(i, 4, labels = game$pwd[i], col = "white", cex = 3)
      on.exit(par(defaultBGColor))
    }
    password <- function(inputPwd) {
      inputPwdSplit <- strsplit(inputPwd, "")[[1]]
      if (identical(inputPwdSplit, game$pwd)) {
        game$endGame("Great! You did it! R Dragon flies away. Lady R is waving to the air furiously.")
      } else {
        message("That's not the right password.")
      }
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
    # set environment
    list2env(list(currentRoom = lounge,
                  nextRoom = NULL,
                  previousRoom = NULL,
                  directionChosen = NULL,
                  roomStartTime = NULL,
                  satchel = list(),
                  mode = NULL,
                  door_idx = NULL,
                  object_idx = NULL,
                  riddle = NULL,
                  RPower = 0,
                  trRiddleIdx = NULL,
                  floorMapsAvailable = floorMapsAvailable,
                  floorMapsPlayer = list(),
                  lockedDoorDelay = lockedDoorDelay,
                  roomTimeLimit = roomTimeLimit,
                  pwd = pwd,
                  pwdExposedIdx = NULL,
                  escapeRoom = osTower,
                  debrief = debrief,
                  endGame = endGame,
                  plotMap = plotMap,
                  plotPwd = plotPwd,
                  password = password,
                  hintRPower = 1,
                  solutionRPower = 3,
                  hintSolution = hintSolution),
             envir = game)
  }
  return(game)
}