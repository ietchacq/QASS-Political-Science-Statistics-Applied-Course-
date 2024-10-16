# For the most part of this code and tutorial I'm using the first two sections of the following, feel free to check it out:
# https://www.w3schools.com/r/default.asp

# Header level 1 ####

## Header level 2 ####

### Header level 3 ####

#### Header level 4 ####

##### Header level 5 ####

###### Header level 6 ####

# Number of pounds at the beginning determines level, for at the end the fact that is a header

# FUNDAMENTALS I ####

## 1.	Syntax (coding), printing, commenting  ####

# This is the Source/script where you code 
## Ctrl+Enter to run line or click 'Run' upper-right corner of script

# Console shows what you're doing/output of everything you run (except plots)/error messages, etc.
## You can clean console clicking the broom @upper-right corner of console

# Environment, etc. most important 'Environment' all the objects you have in the session
## You can remove objects in your environment by running the function rm(x), where 'x' references the object name

# Files, Plots, etc. most important 'Plots' and 'Help', Plots shows output of plots, help of help(...) function

## Example plotting outputs

plot("hello") # Will produce nothing and error, but so you get the idea
  
cars <- c(1, 3, 6, 4, 9) # Define the cars vector with 5 values
                         # See now you have an object in your environment

plot(cars) # Graph the cars vector with all defaults

## Parenthesis on environment comments above now that we have an object

cars # you can check it out (check console), the object, by just typing its name and running the line

Cars # and see, R is case sensitive (check console again)

rm(cars) # and you can remove it

# Example help outputs
help()
help(plot) # example of function that's present in more than one package
help(abs) # only in one package

# To output text in R, use single or double quotes:
"Hello World!"

# To output numbers, just type the number (without quotes):
5
10
25

# To do simple calculations, add numbers together:
5 + 5

# Printing output

"Hello World!" # You can output code in R without using a print function

print("Hello World!") # But there's also a print() function if you WANT to use it

# Sometimes you NEED to use print() to output code, e.g. with for loops (next lab)
    
for (x in 1:10) { 
  print(x)
}

# Commenting

## Comments can be used to explain R code, and to make it more readable. 
## It can also be used to prevent execution when testing alternative code.
## Comments starts with a #. When executing code, R will ignore anything that starts with #.

## DO comment your code! You're making your life way easier, not just your readers'.

# I've been commenting all this time, but still some examples:

# EX 1: comment before a line of code

# This is a comment
"Hello World!"

# EX 2: comment at the end of a line of code
"Hello World!" # This is a comment

# EX 3: comments can also be used to prevent R from executing the code, not just to explain the code
# "Good morning!"
"Good night!"

# FUNDAMENTALS II ####

## 2. Packages, Functions, Objects (and Data Structures)####

### i. Packages ####

#R works based off packages, which are a collection of R functions, complied code, and sample data
#Most of the ones we'll be using are 'built-in' packages , they are base R packages, i.e., they come with the app
#and we won't need to install them

help(abs)
help(base)
help(stats)

# Others, we do need to install and then call each time we start a session in order to use its functions
# Actually, one of these is quite an important one: tidyverse https://www.tidyverse.org/
# We'll learn both base-R and tidyverse code

install.packages("tidyverse") # to install a package you use the install.packages() function 
                              #and insert the name of the package quoted inside the brackets
library("tidyverse") # to 'call' the package when you want to use its functions you use the library() function
                   # this will make the package be availble to you in your current session
                   # (if you terminate R session you'll need to call it again)
                   # package name can go both quoted and unquoted 

# Example (we'll be able to code and interpret these things later on :))
box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length)) # the ggplot() function is part of tidyverse
box + geom_boxplot(aes(fill=Species)) +                  # if we run all this before calling the package, we'll get an error
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

### ii. Functions ####
help(plot) # we've seen this already, like in math, functions are expressions that tell you/something to do something
           # formally, they relate an input to an output; here they are the same thing, they tell R to do something
           # and they also have arguments which we use to define the specs. of what the function is doing
           # arguments are separated by commas (as operators in math functions)

# Example creating two objects(vectors in this case), to plot them, they'll be some of the plot() function arguments 
x <- (1:10)
y <- seq(1,50,by=5)

plot(x) #plotting only x as 1 argument
plot(x, y) #adding y as another argument, so now plotting x against y vectors
plot(x, y,
     main = "In this case, 'main' is the argument that defines the plot title") # adding a third argument
                                                                                # in the case of plot(), 
                                                                              #'main' is an argument that defines the plot title 

# Remember help() command we'll help you know what arguments a function has and what we can do with them + what are the defaults


### iii. Objects (and Data Structures) ####
# general, more here: https://cran.r-project.org/doc/manuals/r-devel/R-lang.html#Basic-types
# objects are data storage things/elements/well, objects... in R

#### Vectors ####
# ~ contiguous cells containing data
x # our previous x object, which is a vector containing 10 numbers from 1 to 10

#### Lists ####
# more: https://www.datamentor.io/r-programming/list/
# ~ lists (or “generic vectors”) another kind of data storage, which containts elements


l <- list("a" = 2.5,
          "b" = TRUE, 
          "c" = 1:3) #e.g., list can be created using the list() function
l # our l list

c("potatos","books","sun")


#### Functions  ####
# more: https://www.w3schools.com/r/r_functions.asp
# ~ functions are objects 2! and can be manipulated in much the same way as any other object
# functions that are in packages won't appear listed in our environment
# but if we create our own functions (which we can!), they will appear there too

# Creating function
my_function <- function() { # create a function with the name my_function
  print("Hello World!")
}

# Calling it 
my_function <- function() {
  print("Hello World!")
}

my_function() # call the function named my_function

#### NULL ####
# ~special object, mostly used to perform some tasks not as an object itself
# used whenever there is a need to indicate or specify that an object is absent, but we need to create that 'space'
n <- NULL
n

## 3.	Variables and Basic Data Types ####

### i. Variables ####
 
# Creating variables, as we've been doing before, i.e., assigning values to named objects with: <- 
name <- "John"
age <- 40

### ii. Data ####

# class() vs typeof() 
# "typeof determines the (R internal) type or storage mode of any object
# while class is an attribute of an object that can be assigned regardless of its internal storage mode

# What object type is x? I.e., what type of vector is x? ~ what's its storage mode?
typeof() #learn what type the object is
typeof(x)
x <- (1:10)
typeof(y)
typeof(l) # ok this is a list, what if we want to also know what type of things 
    
str(l) # lists' structure can be examined with the str() function
       # now in the output we can see the 'typeof' each element in the list
       # each element of a list can be contain any type of R object, i.e. the 
       # elements of a list do not have to be of the same type

typeof(n) # the NULL object has no type and no modifiable properties.

# What's its actual 'type', or attribute?
class(x)
class(y)
y <- as.character(y)
class(n)
class(l)

## 4.	R Math, Booleans, Operators  ####

### i. Math ####

#Simple
10 + 5
10 - 5
2*4
60/2
2*6+4/2 # it computes as it should given only addition/subtraction symbols separate arguments
(2*6)+(4/2) # see, same result
2*(6+4)/2 # but we can change that using brakets, as we normally do

#Basic math functions
# More here: https://www.javatpoint.com/r-built-in-functions

# Beyond these examples, all of these we can use for variables in data sets too

# min() and max() functions can be used to find the lowest or highest number in a set
max(5, 10, 15)
min(5, 10, 15)

# square root of a number
sqrt(16) 

# absolute (positive) value of a number
abs(-4.7)

# ceiling() and floor() for rounding numbers
ceiling(1.4) #ceiling() function rounds a number upwards to its nearest integer
floor(1.4) #floor() function rounds a number downwards to its nearest integer
# you can also use round() which will do it following standard .5 definition of whether is up or down
round(1.4)
round(1.6)

### ii. Booleans ####
# booleans are logical values, i.e., is this true/false? greatear/smaller thatn this? etc.
10 > 9    # TRUE because 10 is greater than 9
10 == 9   # FALSE because 10 is not equal to 9
10 < 9    # FALSE because 10 is greater than 9
10 != 10  # different from

# Again, we can also test this with variables
a <- 10
b <- 9
a > b
c <- a > b
c

# We can also run a boolean condition in an if statement, which we see below
a <- 200
b <- 33 #if we change this to > 200 then the other message appears
if (b > a) { # if value of b is greater than value of a
  print ("b is greater than a") #then print "b is greater than a"
} else { # if whatever else happens
  print("b is not greater than a") #then print "b is not greater than a"
}

### iii. Operators ####

# R divides the operators in the following groups:
  
  # Arithmetic operators (math ones we've seen above +,-,*,...)
  
  # Assignment operators (the one/s we use to assign values to objects: <-)
  my_var <- 3 # we can actually define it in several ways
  my_var <<- 3
  3 -> my_var # the only important thing is that the arrow goes from the value to the object name
  3 ->> my_var
  my_var 
  my_var -> 3 # See, this throws an error 
  # print my_var
  
  # Comparison operators (the ones we've seen above in 'Booleans' >,<,==,!=, ...)
  
  # Logical operators (to combine conditional statements, e.g. in if conditions below &,|,!,etc.)
  
  # Miscellaneous operators (others use to manipulate data, e.g. %in% (something is in/inside something),
                            # %>% this is a pipe, we'll see piping in a bit is part of the tidyverse language/coding)

## 5.	R Strings  ####
  
  #Regular expressions 

  #We won't see strings given time (+ not really needed in the context of this class nor SOC302)
  #BUT if you want to analyze text data, I'll cover it with you as needed of course
  #For now, if interested, I'd recommend following this intro
    # First, these basic links:
      # https://www.w3schools.com/r/r_strings.asp
      # https://www.w3schools.com/r/r_strings_esc.asp
    # Some basic commands here: https://www.javatpoint.com/r-built-in-functions
    # Then try to follow up by doing whatever manipulation you'd like to perform in text data/strings by yourself + googling how to solutions
    # Want to follow a more thorough intro? https://r4ds.had.co.nz/strings.html
  
## 6.	R If…Else and Loops ####
  
### i. If...Else Statements ####
# For if statements and generally loops too, we use the comparison operators we saw above to establish conditions
  # What's an 'if statement': a statement/code in which we tell R to do something IF a certain condition is met
                              # and something else if not. We can also state several 'ifs' at the same time, as well as 'otherwise' 
                              # (i.e. else) conditions

#### If ####
# "if statement" is written with the if keyword, and it is used to specify a block of code to be executed if a condition is TRUE
a <- 33 # 2 variables defined as follows
b <- 200

if (b > a) { #IF b is greater than a
  print("b is greater than a") #THEN do whatever inside the curly brackets, in this case, print that text
} # curly brackets { } are used to define the scope in the code

      # SO, note: if keyword used at the begging to specify we'll run an if statement
                # () within standard brackets we state the condition (e.g., b is greater than a)
                # {} within curly brackets we tell R what it should do if that condition is met, i.e., the action to execute

#### Else_if #### 
# else if keyword is R's way of saying "if the previous conditions were not true, then try this condition"
a <- 33
b <- 33

if (b > a) { # if b greater than a
  print("b is greater than a") # print this
} else if (a == b) { # if that not true but b and a are equal
  print ("a and b are equal") #then print this
}

# You can use as many else if statements as you want in R
if (b > a) {
  print("b is greater than a")
} else if (a == b) {
  print ("a and b are equal")
}  else if (a > b) {
  print ("a greater than b")
}

#### Else #### 
# else keyword catches anything which isn't caught by the preceding conditions
a <- 200
b <- 33

if (b > a) {
  print("b is greater than a")
} else if (a == b) { #you can actually use else without an intermediate else_if before, as we did in the first if else statement 
  print("a and b are equal")
} else {
  print("a is greater than b")
}

#### Nested If Statements #### 
# You can also have if statements inside if statements, this is called nested if statements.
x <- 41

if (x > 10) {
  print("Above ten")
  if (x > 20) {
    print("and also above 20!")
  } else {
    print("but not above 20.")
  }
} else {
  print("below 10.")
}

#### CHANGE TITLE #### 
# We also logical operators to combine different conditions

#  & symbol (and) is a logical operator, and is used to combine conditional statements
# Ex: Test if a is greater than b, AND, if c is greater than a:
a <- 200
b <- 33
c <- 500

if (a > b & c > a) {print("Both conditions are true")}

# | symbol (or) is a logical operator, and is used to combine conditional statements
# Ex: Test if a is greater than b, OR, if c is greater than a
a <- 200
b <- 33
c <- 500

if (a > b | a > c) {
  print("At least one of the conditions is true")
}

### ii. Loops ####  
  # What's a loop?: a piece of code that runs 'looping' until a certain condition is met
  # What's the use?  save time, reduce errors, and they make code more readable
  # What's pseudo-code? something like a written explanation of each line of your code in kind of 'execute' terms
                        # pretty much what I've been doing with my comments on the if...else statements above
                        # it's often useful to first plan out what you want to do, and then more eaisly coding it

# two loop commands: while loops, for loops

#### R While Loops ####

# With the while loop we can execute a set of statements as long as a condition is TRUE

# Print i as long as i is less than 6:
i <- 1 # while loop requires relevant variables to be ready, in this example we need to define an indexing variable, i, which we set to 1
while (i < 6) { # the loop will continue to produce numbers ranging from 1 to 5. The loop will stop at 6 because 6 < 6 is FALSE.
  print(i)
  i <- i + 1 # re define i 
}

# With the break statement, we can stop the loop even if the while condition is TRUE
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) { 
    break #Exit the loop if i is equal to 4
  }
} #loop will stop at 3 because we have chosen to finish the loop by using the break statement when i is equal to 4 (i == 4)

# With the next statement, we can skip an iteration without terminating the loop
# Skip the value of 3:
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
} # When the loop passes the value 3, it will skip it and continue to loop.

#### R For Loop ####

# A for loop is used for iterating over a sequence

# Ex 1: back to an example used at the beggining... 
# so, now we pseudo code we can say we are saying:

for (x in 1:10) {  # for a series of x going from 1:10, let's iterate the following each time (until series comes to an end/last item)
  print(x) # in this example: print that x item
}

x # in this example, x will end up having the value of the last iteration, but we can do all sorts of things with for loops

# Ex 2: Print every item in a list:
fruits <- list("apple", "banana", "cherry")

for (x in fruits) {
  print(x)
}

# as before, break to stop the loop before it has looped through all the items
for (x in fruits) {
  if (x == "cherry") {
    break
  }
  print(x)
}

# next to skip an iteration without terminating the loop
for (x in fruits) {
  if (x == "banana") {
    next
  }
  print(x)
}

# Nested loop: loop inside a loop
# Ex: Print the adjective of each fruit in a list
for (x in fruits) {
  if (x == "banana") {
    next
  }
  print(x)
}
