##Columns and Rows: on Rooks
# To solve the problem of Rows and columns
# First we create a data.frame of 8 rows and 8 variables
# Them, we create vectors of unique values from 1 to 8
# And we apply a value of 1 to the positions where the Queens stay without attacking each other
# ... if they are not able to attack diagonally as rooks do!
# for (i in 1:8){
#   chess.board[i,(VECTOR WITH VALUES 1 TO 8)[i]]<-1}

#####
#Install combintal package if not installed
if(!("combinat" %in% installed.packages()))
  {
  install.packages("combinat")
}
#####
#Load combinat package
library(combinat)
# Using permn function from the package "combinat", 
#I create a list that contains 8! (40320) vectors of unique combinations of vectors 1:8

rooks<-permn(1:8)
#I create a blank list that contains data frames with chess setups
#The value of 1 represent the queens of the eight queen challenges
eight.queen.solutions<-list()
#I create a numeric object of value that counts the number of valid solutions
n<-0
n.1<-0
#I use the function system.time to calculate the time the loop runs
#The last time it took 101 seconds to run

# I create a loop from 1 to 8! (using the function gamma(9))
system.time(
for (m inz1:gamma(9)){
  n.1 <- n.1+1
# At the beginning of the loop I create a data frame with 8 variables and 8 rows
# each row has 0 values in each row
chess.board<-data.frame(column.1=rep(0, 8), column.2=rep(0, 8),
                          column.3=rep(0, 8), column.4=rep(0, 8),
                          column.5=rep(0, 8), column.6=rep(0, 8),
                          column.7=rep(0, 8), column.8=rep(0, 8))
#I extract the mth vector from rooks and I call it base.vector
base.vector<-rooks[[m]]
#I create a logical (a nice R name for booleans) variable with value FALSE
check.diag<-F
#I add an if statement that verifies that the logical variabe is FALSE
if(!check.diag){
###Diagonals!
#I start an outer loop from 1 to 7
for (i in 1:7){
#I start an inner loop from 1 to 7
  for (j in 1:7){
    #I add a condition to execute both loops (i+j should be 8 or less)
    if (i+j < 9){
      #if the condition is satisfied, then I add another condition
      #if the ith place of the base.vector is equal to the ith+ the jth condition plus j
      #it means that the queens are attacking via diagonals (as bishops do)
      #So such solution is discarded and the check.diag become TRUE
      #And the loops ends and the loop picks another vector from the rooks list
    if(base.vector[i]  == (base.vector[j+i]+j)
       |base.vector[i] == (base.vector[j+i]-j))
      {
      check.diag<-T
      }
  }}}}
#if the vector solution is NOT discarded check.diag is still FALSE
#then the loop continues
if(!check.diag)
  
{
  #A new loop starts from 1 to 8
   for (i in 1:8){
     #The loop adds a 1 valuethe ith element of the used solution to the ith row of the dataframe
     
chess.board[i,rooks[[m]][i]] <-1}   
    #After the loop ends the value of n increases by one
  n<- n +1
    #It adds the nth solution to list containing the eight.queen solution list
  eight.queen.solutions[[n]]<-chess.board
}

})