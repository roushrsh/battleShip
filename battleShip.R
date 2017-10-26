#Soroush Hajizadeh Completed running model on October 19 2017
#BattleShip Like Game made using R

#Creating Matrix
#options(warn=1)
sizeM = 400
battleShip <- c(rep(0,sizeM))  #can be changed to be made larger
nrow = 20 #can be changed to match new sizes
ncol = 20 #can be changed to match new sizes
bShipMatrix <- matrix(c(battleShip), nrow, ncol)

#numberofShipsandTheirFeatures
#Below are values the user can tinker with. Number of ships, minimum ship length (assuming they don't fuse), number of bombs, and coordinates of the bombs
numberOfShips = 12
myShip <- c(1,1,1,1,1,1) #default smallest ship size, can be changed to make ships larger or smaller. Also ships can overlap to become "larger" through probability
numberOfBombs <- 20 #can't be more than length of vector, so 20 in original case
#you can also input them manual ex: XHit <- c(1,2,3,4,5,6)
XHit <- sample(1:ncol, numberOfBombs) #X coordinates of Bombs
YHit <- sample(1:nrow, numberOfBombs) #Y coordinates of Bombs

#### Things user shouldn't touch below
Empty <- list()
shipFeature <- c("x","y","position")  #how we initially store each ship
list<- c(Empty, list(shipFeature))
arrayOfShips <- list()
shipNumberStart <- 1 #just used for looping, starts at first ship in the list

 #Methods are below

#As the name implies, this determines if the ships are vertical or horizontal and gives them their first position
shipGenerator <- function(shipNumberStart, arrayOfShips){
  test <- sample(0:1,1) #randomly choosing vert or hor
  if (test == 0){ #ifTrue, then it's vertical
    positionY <- sample(1:(nrow-length(myShip)), 1)
    positionX <- sample(1:length(myShip), 1)
    temporaryShip <- c(positionX, positionY, "vertical") #placing positions into vector
    arrayOfShips[[shipNumberStart]] = temporaryShip #placing vector into list
  }else{
  #horizontal ship case, do same as above
    positionX <- sample(1:(ncol-length(myShip)), 1)
    positionY <- sample(1:length(myShip), 1)
    temporaryShip <- c(positionX, positionY, "horizontal")
    arrayOfShips[[shipNumberStart]] = temporaryShip
  }
  shipNumberStart = shipNumberStart+1
  if(shipNumberStart <= numberOfShips){
    shipGenerator(shipNumberStart, arrayOfShips) #call again if there are more ships
  }else{
    return(arrayOfShips) #else return our new list/array of ships
  }
}

#This extends the ships to their full length and places them all into a list
placeShipsToList <- function(shipsMade){
  ship <- list()
  set = 0
  for (x in shipsMade){
    position = 0
    if (x[3] == "horizontal"){ #if horizontal then we extend by X, and keep Y the same
      for(i in 1:length(myShip)){
        ship[[i + (set)]] = c(( as.integer(x[1])+position), x[2])
        position = position+1
      }
    }else{ ##vertical
      for(i in 1:length(myShip)){ #if vertical then we extend by Y and keep X the same
        ship[[i+ (set)]] = c(x[1], ( as.integer(x[2])+position))
        position = position+1
      }
    }
    set = set +length(myShip) #this is to to basically make up for i reseting.
  }
  return (ship)
}

#This places the newly created ships onto the Matrix by making the 0's 1's where the ships are
placeOntoMatrix <- function(placedShipsInList, matrix){
  for (ship in listOfShips){
    matrix[ (as.integer(ship[1])), (as.integer(ship[2]))] = 1
  }
  return (matrix)
}

#This simulates bombing of the ships by placing 2's if it finds a 1 in the positions it is given
bombMatrix <- function(matrix, XHit, YHit){
  for (hit in 1:length(XHit)){
    if (matrix[(XHit[hit]), (YHit[hit])] == 1){  #if bomb is it, change it to 2
      matrix[(XHit[hit]), (YHit[hit])] = 2
      #do your own bombs here
    }
  }
  return (matrix)
}


#This finally checks and prints out each ship and if they've been hit or not
seeIfHit <- function(list, matrix){
  shipNum = 1
  position = 0
  for (shipPiece in list){
    if ((position)%%(length(myShip)) == 0){  #if gone through every ship.
      cat ("\n")
      cat("Ship ", shipNum, ": ")
      shipNum = shipNum +1
    }
    if (matrix[as.integer(shipPiece[1]), as.integer(shipPiece[2])] == 1){
      cat ("miss ")
      position = position+1
    }else if (matrix[as.integer(shipPiece[1]), as.integer(shipPiece[2])] == 2) {
      cat ("hit ")
      position = position+1
    }
  }
}


shipsMade <- shipGenerator(shipNumberStart, arrayOfShips)
listOfShips <- placeShipsToList(shipsMade)
shipsOnMatrix <- placeOntoMatrix(listOfShips, bShipMatrix)
bombedMatrix <- bombMatrix(shipsOnMatrix, XHit, YHit)
seeIfHit(listOfShips, bombedMatrix)
write.table(bombedMatrix, file="myMatrix.txt", row.names=FALSE, col.names=FALSE) #prints
