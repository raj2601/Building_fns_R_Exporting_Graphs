# Function 2:
# Here the function Graphs() is used to plot and save the graphs based on the variables(either categorical or numerical) 
# and save it to the given directory.
# Inside the function(dataframe,variable,catergorical_cutoff, directory)is mentioned.
# Variable is an optional argument where specific variables alone could be sent to generate graphs
# Categorical_cutoff is used to seperate categorical values based on their unique values.Default value is set at 2% of total values
# Directory specifies the directory into which the graphs has to be saved.
# This function incorporates all the prescribed suggestions. 


getwd()
directory=setwd("C:\\Users\\MY PC\\Desktop\\e-files\\Trimester 2\\R\\Assignment")



Graphs <- function(data,variable=c(1:length(data)),cat_cutoff=0.02, 
                   directory="C:\\Users\\MY PC\\Desktop\\e-files\\Trimester 2\\R\\Assignment")
{
  setwd(directory)
  
  if(!is.data.frame(data))  #Allows only if dataframe object is fed or stops.
    stop("The given object is not a data frame")
  
  for(i in variable)  #fn to classify variables(cat or num) based on their unique values.We are setting the condition of seperating using proportion of unique values.  
  {
    a = length(unique(data[,i]))
    b = cat_cutoff*length(data[,i])
    
    if(is.numeric(data[,i]) & (!a<b)) #Allows the variable only with numeric & continuous values.
    {
      png(paste(names(data)[i], ".png", sep="")) 
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T) #Plots boxplot
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], col = "lightgreen", border=F)  #Plots histogram
      
      
      
      dev.off()  
      
    }
    else
    {
      png(paste(names(data)[i], ".png", sep="")) 
      
      par(mfrow=c(1,2))
      barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]), 
              ylab = names(data)[i], col = "yellow", border = "grey5") #Plots barplot
      
      pie(table(data[,i]), main = paste("Pie-chart of", names(data)[i]), 
           xlab = names(data)[i],radius=1) #Plots pie-chart
      dev.off()
    }
  }
}


cars=read.csv("C:\\Users\\MY PC\\Desktop\\e-files\\Trimester 2\\R\\cars.csv")

Graphs(cars,directory="C:\\Users\\MY PC\\Desktop\\e-files\\Trimester 2\\R\\Assignment\\cars")
  




























