#1. Print your name at the top of the script. Include the prefix: "Plotting Basics:" such that it appears "Plotting Basics: Lastname" 
print("Plotting Basics: MURUGAN")

#2. Import libraries including: FSA, FSAdata, magrittr,  dplyr, plotrix, ggplot2, and moments
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#3. Load the BullTroutRML2 dataset (BullTroutRML2.csv) 
BullTroutRML2FullData <- BullTroutRML2

#4. Print the first and last 3 records from the BullTroutRMS2 dataset 
head(BullTroutRML2FullData, n=3)
tail(BullTroutRML2FullData, n=3)

#5. Remove all records except those from Harrison Lake
NewData <- filter(BullTroutRML2FullData,lake == "Harrison")

#6. Display the first and last 5 records from the filtered BullTroutRML2 dataset
head(NewData, n=5)
tail(NewData, n=5)

#7. Display the structure of the filtered BullTroutRML2dataset
str(NewData)

#8. Display the summary of the filtered BullTroutRML2dataset 
summary(NewData)

#9. Create a scatterplot for "age" (y variable) and "fl" (x variable)  with the following specifications: 
# ??? Limit of x axis is (0,500) 
# ??? Limit of y axis is (0,15) 
# ??? Title of graph is "Plot 1: Harrison Lake Trout 
# ??? Y axis label is "Age (yrs)" 
# ??? X axis label is "Fork Length (mm)" 
# ??? Use a small filled circle for the plotted data points

ggplot(NewData, aes(x = fl, y = age)) +
  geom_point(color="blue") +
  xlim(0,500) +
  ylim(0,15)+
  labs(                
    title = "Plot 1: Harrison Lake Trout",
    x = "Fork Length (mm)",      
    y = "Age (yrs)"   
  )


# 10. Plot an "Age" histogram with the following specifications 
# ??? Y axis label is "Frequency" 
# ??? X axis label is "Age (yrs)" 
# ??? Title of the histogram is "Plot 2: Harrison Fish Age Distribution" 
# ??? X and Y axis limits is 0, 15 
# ??? The color of the frequency plots is "cadetblue" 
# ??? The color of the Title is "cadetblue" 
hist(NewData$age, ylab="Frequency", xlab="Age (yrs)", 
     main="Plot 2: Harrison Fish Age Distribution", 
     col.main= "cadetblue",
     xlim=c(0,15), ylim=c(0,15), 
     col="cadetblue")


# 11. Create an overdense plot using the same specifications as the previous scatterplot. But,  
# ??? Title the plot "Plot 3: Harrison Density Shaded by Era" 
# ??? Y axis label is "Age (yrs)" 
# ??? Y axis limits are 0 to 15 
# ??? X axis label is "Fork Length (mm)" 
# ??? X axis limits are 0 to 500  
# ??? include two levels of shading for the "green" data points.  
# ??? Plot solid circles as data points 


plot(x= NewData$fl, y = NewData$age, xlim = c(0,500), ylim = c(0,15), 
     pch = 16,
     col=  hcl.colors(100, palette = "greens"),
     ylab = "Age(yrs)",
     xlab = "Fork Length(mm)", 
     main = "Plot 3: Harrison Density Shaded by Era")

#12.Create a new object called "tmp" that includes the first 3 and last 3 records of the BullTroutRML2 data set. 
tmp <- headtail(NewData, n = 3, nh = 3, nt = 3)
rownames(tmp) = seq(length=nrow(tmp))
tmp


#13. Display the "era" column (variable) in the new "tmp" object
data.frame(tmp$era)


#14. Create a pchs vector with the argument values for + and x.  
pchs <- c("+","x")


#15. Create a cols vector with the two elements "red" and "gray60" 
cols <- c("red","gray60")
cols


#16. Convert the tmp era values to numeric values. 
as.numeric(tmp$era)


#17. Initialize the cols vector with the tmp era values 
colsEra <- cols[factor(tmp$era)]
colsEra


#18. Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the following specifications: 
#??? Title of graph is "Plot 4: Symbol & Color by Era" 
#??? Limit of x axis is (0,500) 
#??? Limit of y axis is (0,15) 
#??? X axis label is "Age (yrs)" 
#??? Y axis label is "Fork Length (mm)" 
#??? Set pch equal to pchs era values 
#??? Set col equal to cols era values 
plot(x= NewData$fl, y = NewData$age, xlim = c(0,500), ylim = c(0,15), 
     pch = ifelse(NewData$era ==  "1977-80", pchs[1], pchs[2]),
     col = ifelse(NewData$era == "1977-80", unique(colsEra)[1], unique(colsEra)[2]), 
     ylab = "Age(yrs)",
     xlab = "Fork Length(mm)", 
     main = "Plot 4: Symbol & Color by Era")


#19. Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay". 
plot(x= NewData$fl, y = NewData$age, xlim = c(0,500), ylim = c(0,15), 
     pch = ifelse(NewData$era ==  "1977-80", pchs[1], pchs[2]),
     col = ifelse(NewData$era == "1977-80", unique(colsEra)[1], unique(colsEra)[2]), 
     ylab = "Age(yrs)",
     xlab = "Fork Length(mm)", 
     main = "Plot 5: Regression Overlay")
abline(lm(NewData$age ~ NewData$fl,data=NewData),col='blue')


#20. Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay" 
plot(x= NewData$fl, y = NewData$age, xlim = c(0,500), ylim = c(0,15), 
     pch = ifelse(NewData$era ==  "1977-80", pchs[1], pchs[2]),
     col = ifelse(NewData$era == "1977-80", unique(colsEra)[1], unique(colsEra)[2]), 
     ylab = "Age(yrs)",
     xlab = "Fork Length(mm)", 
     main = "Plot 6: :Legend Overlay")
abline(lm(NewData$age ~ NewData$fl,data=NewData),col='blue')
legend("topleft",
       legend=levels(NewData$era), 
       inset = 0.02,
       title = "Legend",
       title.col = "black",
       pch = pchs,
       col = unique(colsEra),
       bty="n",
       text.col = unique(colsEra))