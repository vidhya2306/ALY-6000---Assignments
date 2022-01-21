#1. Print your name at the top of the script
print('My Name is Vidhya Murugan')

#2. Install the vcd package (pg 19)
install.packages("vcd")

#3. Import the vcd library
library(vcd)

#4. Plot a sales ~ temp scatter
sales <- c(7,11,15,20,19,11,18,10,6,22)
temperature <- c(69,81,77,84,80,97,87,70,65,90)
plot(sales,temperature,type="p",main="Scatter Plot",sub = "Sales ~ Temperature",xlab = "Sales",ylab = "Temperature")

#5. Find the mean temperature
mean(temperature)

#6. Delete the 3rd element from the sales vector
newSales1 <- sales[-3]
newSales1

#7. Insert 16 as the 3rd element into the sales vector
newSales2 <- append(newSales,16,after=2)
newSales2

#8. Create a vector <names> with elements Tom, Dick, Harry
names <- c("Tom","Dick","Harry")
names

#9. Create a 5 row and 2 column matrix of 10 integers
?matrix
matrix <- matrix(1:10, nrow=5, ncol=2, byrow=TRUE)
matrix

#10. Create a data frame <icSales> with sales and temp attributes
?data.frame
rowNumber <- c(1:length(sales))
icSales <- data.frame(sales,temperature,row.names = rowNumber,check.rows = TRUE)

#11. Display the data frame structure of icSales
icSales
str(icSales)

#12. Display a summary of the icSales data frame
summary(icSales)

#13. Import the dataset Student.csv
dataset <- read.csv("Student.csv")

#14. Display only the variable names of the Student.csv dataset
colnames(dataset)

#15. Commit your code in your github/gitlab repo
#github link:https://github.com/vidhya2306/Module-1-Assignment/blob/main/Assignment%201.R
