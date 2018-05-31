#################################
## ARTIFICIAL NEURAL NETWORKS  ##
#################################

## import data and install packages
path_to_my_data <- "path_to_my_data"
path_output <- "path_to_results"
setwd(path_to_my_data)

#install.packages("neuralnet")
#install.packages("relimp")
library("neuralnet")
library("relimp")


## check the data
morpho.data <- read.csv("morpho.example.NN.csv", header=T, sep=";")
showData(morpho.data) ## check if the matrix is correctly loaded
## columns of information about cases [including one column with the code of the species as numeric]
## columns of belonging to a class (1=YES,-1=NO)
## columns of morphometric data

morpho.data<-na.omit(morpho.data) ## delete rows with  NAs

col_info <- 6 ## number of columns which are NOT morphometric data
num_spp <- 4 ## number of entities (species and infraspecific taxa in this case)

names(morpho.data) ## check variables names
unique(morpho.data$SP) ## check number of species

## generate the formula
## first term of the formula: dependent variable (output layer)
cods <- paste(names(morpho.data[((col_info-num_spp)+1):col_info]), collapse="+")
## second termn of the formula (independent variables; input layers)
vars <- paste(names(morpho.data[(col_info+1):ncol(morpho.data)]), collapse="+")
vars
cods
formula <- paste(cods, vars, sep="~") 
formula


## standarization max-min
Maxs <- apply(morpho.data[,(col_info+1):ncol(morpho.data)], 2, max) 
Mins <- apply(morpho.data[,(col_info+1):ncol(morpho.data)], 2, min)

stand <- as.data.frame(scale(morpho.data[,(col_info+1):ncol(morpho.data)], center = Mins, scale = Maxs - Mins))

## split the dataset into training/test sets
percent <- 0.6 # percentage of training set (in decimals)


Training <- sample(nrow(morpho.data), 
                   percent * nrow(morpho.data))

TrainSet <- stand[Training,]
TestSet <- stand[-Training,]
#TrainSet

## add the columns indicating the code for the species
CodsTrainSet <- morpho.data[Training,((col_info-num_spp)+1):col_info]
CodsTestSet <- morpho.data[-Training,((col_info-num_spp)+1):col_info]

TrainSet <- cbind(CodsTrainSet, TrainSet)
TestSet <- cbind(CodsTestSet, TestSet)

## generate the Artificial Neural Net (or several if num_reps>1)
num_reps=1
nnet <- neuralnet(formula, data=TrainSet, hidden=4, linear.output=F, rep=num_reps) 
## if more than one hidden layer is desired, change hidden arg (example hidden=c(4,6))

#TestSet

## Test the NN over TestSet
pred <- compute(nnet, TestSet[,(num_spp+1):ncol(TestSet)])$net.result
#pred


## assign to the class that shows the highest probability
AssignToAClass <- function(x) {
  return(which(x == max(x)))
}

ClassResult_inspection <- apply(pred, 1, AssignToAClass) ## by rows
#ClassResult_inspection ## factors

## visual inspection of the results (or to calculate per species result)
########################################################################
unique(morpho.data$SP)

for (i in 1:length(unique(morpho.data$SP))){
  ClassResult_inspection[ClassResult_inspection== i] <- as.character(unique(morpho.data$SP))[i]
}

isTRUE(length(ClassResult_inspection)==nrow(CodsTestSet)) ## checkpoint

results_table <- cbind.data.frame(CodsTestSet, ClassResult_inspection)
showData(results_table) ## print
write.csv2(results_table, file= paste(path_output,"test1.csv")) ## save


## automated MCR calculation
############################

ClassResult_auto <- apply(pred, 1, AssignToAClass) ## by rows
#ClassResult_auto

InitialClass <- morpho.data[-Training,1] ## first column: species (as.numeric)
#InitialClass


## calculate the misclassification rate
CalculateMCR <- function(y) {
  MCR.vector<-as.numeric(y) - as.numeric(InitialClass) 
  misclassified.cases <- length(MCR.vector[MCR.vector != 0])
  
  print(paste("total number of cases in the test set",nrow(TestSet), sep= " = "))
  print(paste("number of misclassified cases",misclassified.cases, sep= " = "))
  result <- round(misclassified.cases/nrow(TestSet),3) ## number of decimals
  return(paste("MCR = ", result))
}


CalculateMCR(ClassResult_auto)


## error/reachd treshold/steps of the NN
########################################

nnet$result.matrix[1:3,1:num_reps]

############
## GRAPHS ##
############

## ANNs VISUALIZATION (S1_Fig)
##############################
#https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/

#install.packages("devtools")
library(devtools)

## import the function from Github
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


plot.nnet(nnet, bias=F) ## default settings
plot.nnet(nnet, pos.col = 'darkgreen', neg.col = 'darkblue', alpha.val = 0.7, rel.rsc = 10,
          circle.cex = 3, cex = 1.4,
          circle.col ='brown') ## change colors, thick of the lines, text size...


## MCR vs. number of spp (S2_Fig)
#################################
library(ggplot2)
library(relimp)

## working directory
setwd("my_working_directory")

## load data
final_result <- read.table("example_MCRvsNUM.spp.txt", header = T) 
showData(final_result) ## in the example the values correspond to the correctly classified (1-MCR)

str(final_result) ## check if it is numeric
corrClass <- apply(final_result[,2:ncol(final_result)], 1, mean)
corrClass <- round(corrClass,3)
corrClass

## in case you directly have MCR, skip this part (--> go to line 177)
invert <- function(x) {1-x}
misClass <- sapply(corrClass, invert)
misClass

## we create the data.frame and proceed to generate the graphic
misClass <- as.data.frame(cbind(final_result$no.spp, misClass))
names(misClass) <- c("no.spp","MCrate")
showData(misClass)

## checkpoint
unique(misClass$no.spp) ## check categories
names(misClass)

## graph
graph <- ggplot(misClass, aes(x=no.spp, y=MCrate)) + 
  geom_point(shape=1)+
  #geom_smooth(method=lm, color="black")+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, level=0.99) +
  labs(title="Misclassification Rate vs. Number of Species",
       x="number of species", y = "misclassification rate")+
  theme_light()  

graph +  ggtitle("Misclassification Rate vs. Number of Species") +
  labs(x="number of species",y="misclassification rate\n") + 
  theme(plot.title = element_text( 
    color="#666666", face="bold", size=18, hjust=0)) +
  theme(axis.title.x = element_text(size=16, color="#666666", face="bold"),
        axis.title.y = element_text(size=16, color="#666666", face="bold")) +
  theme(axis.text.x = element_text(size=14, angle=0, hjust=0.5),
        axis.text.y = element_text(size=14))

## END