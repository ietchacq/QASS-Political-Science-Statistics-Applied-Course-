# R DATA (and )####

## 1. 'Built-in' data ####

# base R has some already loaded data in it, for example:
cars # The data give the speed of cars and the distances taken to stop. Note that the data were recorded in the 1920s.
?cars #same as looking for help
help(cars)

iris
?iris #measurements in centimeters of the variables sepal length and width and petal length and width, respectively, 
      # for 50 flowers from each of 3 species of iris.

# We can also assign them to objects in our session environment if wantint/needing to
cars <- cars
iris <- iris
cars   
iris # now, they are the same, but once we call them we'll be calling our created objects, not the originals
     # so if we change something in the newly created car objects, that'll be reflcted there
# so, sometimes probably best to change names. More on this next...

## 2. Data Packages ####
# more on this example: https://github.com/saschagobel/legislatoR
install.packages("legislatoR") #installing a package
library("legislatoR") # loading it to your library

deu_politicians <- get_core(legislature = "deu") # assign some of their data to object called 'deu_politicians'
deu_politicians # now we have a a data set of german politicians

typeof(deu_politicians) #it's a list
class(deu_politicians) #which's actually a data frame

## 3. Bringing in Data ####

# Different data types (csv, spss, stata, json, etc.)
# while R (base R functions) can open most data types, see e.g. https://www.datafiles.samhsa.gov/get-help/format-specific-issues/how-do-i-read-data-
# There are some spec. packages used to best open those files in R, e.g. R package "foreign"

# We'll mostly use .csv files, so we'll use the following function: read.csv()
# if using R data file, then the function you use if load()

# Before reading any data, you must set the R working directory to the location of the data
# setwd(“…”) will set the current working directory to a specific location
# getwd() will print out the current directory.

setwd("C:/Users/irise/Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/")
data <- read.csv("02_data.csv")

#use this form for general userprofile
setwd(file.path(Sys.getenv("USERPROFILE"),"Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/")) #useful if you change PCs

# Otherwise, you just type the entire location:
data2 <- read.csv("C:/Users/ie7/Box/0 Teaching/Intro to R/2023_Rice_POLI102/materials/02_data.csv")

## 4. Creating Data (and more Data Structures) ####

### i. R Matrices ####
# Check more basic stuff on matrices: https://www.w3schools.com/r/r_matrices.asp

# Ex 1: numeric matrix
thismatrix <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2) # Create a matrix
thismatrix # print it 

# Ex 2: string matrix
thismatrix <- matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol = 2)
thismatrix

# Access matrix items by using [ ] brackets
# The first number "1" in the bracket specifies the row-position, while the second number "2" specifies the column-position
thismatrix[1, 2]
thismatrix[2,] # whole row can be accessed if you specify a comma after the number in the bracket
thismatrix[,2] # whole column can be accessed if you specify a comma before the number in the bracket

####  ii. Data Frames ####
# As we've already seen Data Frames are data displayed in a format as a table
# But beyond importing them from somewhere else, we can create them ourselves

# Create a data frame from scratch
Data_Frame <- data.frame (
  Training = rnorm(5916, 1, 23),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

Data_Frame

# Create a data frame using as.data.frame() function, which changes the type and class of an object and forces it to be a df
Data_Frame2 <- as.data.frame(thismatrix)
Data_Frame2
