#library(R6)

Room <- R6::R6Class("Room",
                public = list(
                  name = NULL,
                  title = NULL,
                  door = NULL,
                  object = NULL,
                  timeLimit = Inf,
                  object_numbers = NULL,
                  initialize = function(name = NA, title = NA) {
                    self$name <- name
                    self$title <- title
                  },
                  set_doors = function(doors) {
                    self$door <- doors
                  },
                  set_objects = function(objects) {
                    self$object <- objects
                    self$object_numbers <- 1:length(objects)
                  },
                  set_timeLimit = function(timeLimit) {
                    self$timeLimit <- as.numeric(timeLimit)
                  },
                  countLockedDoors = function() {
                    sum(sapply(self$door, function(door) !door$open))
                  },
                  doorsList_toString = function(directionChosen) {
                    l <- self$door
                    if (!is.null(l) && is.list(l) && length(l) > 0) {
                      paste0(1:length(l), ": ",
                             lapply(l, function(x) {
                               currentDirection <- x$getDirection(self$name)
                               s <- currentDirection
                               if (!is.null(directionChosen)) {
                                 reverseDirectionChosen <- x$reverseDirection(directionChosen)
                                 if (reverseDirectionChosen == currentDirection) {
                                   s <- paste0(s, " (where you just came from)")
                                 }
                               }
                               s
                             }),
                             collapse = "\n\t")
                    } else {
                      ""
                    }
                  },
                  objectsList_toString = function() {
                    l <- self$object
                    numbers <- self$object_numbers
                    if (!is.null(l) && is.list(l) && length(l) > 0) {
                      takenObjectsIdx <- which(sapply(l, function(obj) !obj$taken))
                      l <- l[takenObjectsIdx]
                      numbers <- numbers[takenObjectsIdx]
                      if (length(l) > 0) {
                        paste0(numbers, ": ",
                               lapply(l, function(x) x$toString()),
                               collapse = "\n\t")
                      } else {
                        ""
                      }
                    } else {
                      ""
                    }
                  },
                  greet = function(directionChosen = NULL) {
                    message(paste0("You are in ", self$title, ".\nList of Objects:\n\t",
                                   self$objectsList_toString(),
                                   "\nList of Doors:\n\t", self$doorsList_toString(directionChosen)))
                  }
                )
)

Door <- R6::R6Class("Door",
                public = list(
                  direction = list(NULL, NULL),
                  room = list(NULL, NULL),
                  riddle = list(NULL, NULL),
                  open = FALSE,
                  initialize = function(direction = NA,
                                        room = list(NA, NA),
                                        riddle = list(NA, NA)) {
                    if (!direction %in% c("north", "south", "east", "west", "up", "down")) {
                      stop("invalid direction")
                    }
                    self$direction[[1]] <- direction
                    self$direction[[2]] <- self$reverseDirection(self$direction[[1]])
                    self$room <- room
                    self$riddle <- riddle
                    names(self$riddle) <- self$getRoomsNames()
                    names(self$direction) <- self$getRoomsNames()
                  },
                  getRoomsNames = function() {
                    lapply(self$room, function(room) room$name)
                  },
                  toString = function(room) {
                    paste0(self$direction[[room]])
                  },
                  openDoor = function() {
                    self$open <- TRUE
                  },
                  lockDoor = function() {
                    self$open <- FALSE
                  },
                  reverseDirection = function(direction) {
                    switch(direction,
                           "north" = "south",
                           "south" = "north",
                           "east" = "west",
                           "west" = "east",
                           "up" = "down",
                           "down" = "up")
                  },
                  getRiddle = function(room) {
                    self$riddle[[room]]
                  },
                  getRoom = function(room) {
                    self$room[[room]]
                  },
                  getNextRoom = function(room) {
                    self$room[[which(self$getRoomsNames() != room)]]
                  },
                  getDirection = function(room) {
                    self$direction[[room]]
                  }
                ))

Object <- R6::R6Class("Object",
                  public = list(
                    name = NULL,
                    location = NULL,
                    riddle = NULL,
                    taken = FALSE,
                    type = NULL,
                    points = NULL,
                    initialize = function(name = NA, location = NA, type = NA,
                                          points = NA, riddle = NA) {
                      self$name <- name
                      self$location <- location
                      self$type <- type
                      self$points <- as.numeric(points)
                      self$riddle <- riddle
                    },
                    toString = function() {
                      paste0(self$name, " ", self$location)
                    },
                    takeObject = function() {
                      self$taken <- TRUE
                    }
                  ))

Riddle <- R6::R6Class("Riddle",
                  public = list(
                    question = NULL,
                    solution = NULL,
                    val = NULL,
                    hint = NULL,
                    tip = NULL,
                    floorMapsIdx = NULL,
                    initialize = function(question = NA, solution = NA, val = NA,
                                          hint = NA, tip = NA, floorMapsIdx = NA) {
                      self$question <- question
                      self$solution <- solution
                      self$val <- as.numeric(val)
                      self$hint <- hint
                      self$tip <- tip
                      self$floorMapsIdx <- floorMapsIdx
                    },
                    askQuestion = function() {
                      message(self$question)
                    },
                    getSolution = function() {
                      message(self$solution)
                    },
                    getHint = function() {
                      message(self$hint)
                    }
                  ))


TimeRoom <- R6::R6Class("TimeRoom",
                    inherit = Room,
                    public = list(
                      riddle = NULL,
                      timeLimit = NULL,
                      floorMapsIdx = NULL,
                      initialize = function(name = NA, title = NA, timeLimit = NA, floorMapsIdx = NA) {
                        self$name <- name
                        self$title <- title
                        self$timeLimit <- as.numeric(timeLimit)
                        self$floorMapsIdx <- floorMapsIdx
                      },
                      set_riddles = function(riddles) {
                        self$riddle <- riddles
                      },
                      set_timeLimit = function(timeLimit) {
                        invisible()
                      },
                      greet = function(alreadyHasMap) {
                        message(paste0("Oh oh. You reached ", self$title,
                                       ".\n\nThis means you have ",
                                       self$timeLimit, " minutes to answer these ",
                                       length(self$riddle), " questions.\nIf you don't make it - Lady R will get you.\nBut if you do - you will return to the previous room ", ifelse(!alreadyHasMap, "with a valuable\npiece of information.", "(with nothing 'cause you've already been here!).")))
                      }
                    )
)

DarkRoom <- R6::R6Class("DarkRoom",
                    inherit = Room,
                    public = list(
                      RPower = NULL,
                      #timeLimit = Inf,
                      initialize = function(name = NA, title = NA, RPower = NA) {
                        self$name <- name
                        self$title <- title
                        self$RPower <- as.numeric(RPower)
                      },
                      greet = function(directionChosen = NULL) {
                        message(paste0("Damn! You reached ", self$title,
                                       "!\n\nYou can't see a thing.\n\nSuddenly, you see a small candle light approaching towards you.\nOh no! It's Lady R! She's laughing maniacally and is coming towards you!\n\nQuick! Do you have enough R power to go back? (",
                                       self$RPower, " points)"))
                      }
                    )
)

gameStartScenario <- function() {
  message("Welcome to the Castle of R!")
  message("\nCastle of R is a text-based adventure.\n\nThe purpose of this game is to test your skills in base R. Let's begin.")
  message("\n\nYou're in the Castle lounge.\nYou're sitting in front of Lady R, an elderly and pleasant woman, who is the owner of the Castle of R.\nBehind you there's a window with a lovely view to the gardens of the Castle of R.\nIn front of you, over Lady R's shoulder, you can see a door.")
  message("Lady R is asking for your name.")
  playerName <- readline()
  message(paste0("\"Pleased to meet you ", playerName, ". Would like some tea?\""))
  tea <- menu(c("yes", "no")) == 1
  if (tea) {
    message("Lady R pours you a cup of tea. You drink the tea and at the bottom of the cup you see a strange message:\n      \"Visit all rooms.\"\n")
  }
  message("Lady R says:\"Biscuits?\"")
  biscuits <- menu(c("yes", "no")) == 1
  if (biscuits) {
    message("Lady R serves you a small plate with biscuits. You eat the biscuits and at the bottom of the plate it says:\n      \"Run.\"")
  } else {
    message("\"Quite the ascetic, aren't we? Very well then.\"")
  }
  message("\nSuddenly Lady R gets off her chair.\nShe no longer looks pleasant. In fact she's holding a knife and saying:\n\"I'm so glad you could join me at this beautiful day. I need me a fresh R gimp!\"\nYou realize you need to run. But where?")
  message("\nYou move towards the northern door, quick! Write \"openDoor(1)\"! Hurry!")
  startTime <- Sys.time()
}

Lounge <- R6::R6Class("Lounge",
                  inherit = Room,
                  public = list(
                    startScenario = function() {
                      gameStartScenario()
                    }
                  )
)