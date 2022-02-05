# 1.Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr,dplyr, tidyr plyr and tidyverse 
print("Last Name: MURUGAN")

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

# 2.Import the inchBio.csv and name the table <bio> 
bio <- read.csv("inchBio.csv")
view(bio)

# 3. Display the head, tail and structure of <bio> 
head(bio)
tail(bio)

# 4. Create an object, <counts>, that counts and lists all the species records 
counts <-c(bio$species)

# 5. Display just the 8 levels (names) of the species 
unique(bio$species)

# 6. Create a <tmp> object that displays the different species and the number of record of each species in the dataset. Include this information in your report.- 
tmp <- data.frame(table(bio$species))
tmp
######### Converted the variable into data frame for better aesthetic output

# 7. Create a subset, <tmp2>, of just the species variable and display the first five records 
tmp2 <- tmp[1]
head(tmp2, n = 5)

# 8. Create a table, <w>, of the species variable. Display the class of w
w <- table(bio$species)
class(w)

# 9. Convert <w> to a data frame named <t> and display the results 
t <- data.frame(w)
t

# 10. Extract and display the frequency values from the <t> data frame 

#### Method 1:
t$Freq

#### Method 2:
t[2]

# 11. Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio> 
cSpec <- table(bio$species)
class(cSpec)                      ### To confirm table is created
cSpec

# 12. Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class. 
cSpecPct <- round(100 * prop.table(cSpec),digits = 2)
class(cSpecPct)
cSpecPct

# 13. Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame 
u <- data.frame(cSpecPct)
class(u)

# 14. Create a barplot of <cSpec> titled Fish Count with the following specifications: 
# . Title: Fish Count 
# . Y axis is labeled "COUNTS" 
# . Color the bars Light Green 
# . Rotate Y axis to be horizontal 
# . Set the X axis font magnification to 60% of nominal 
barplot(cSpec, horiz = TRUE,  col = "Light Green", border = "Dark Green",
        main = "Fish Count", xlab = "COUNTS", cex.names = 0.6, las = 1)

# 15. Create a barplot of <cSpecPct>, with the following specifications: 
# . Y axis limits of 0 to 40 ### have corrected the value from 0 to 4 to 0 to 40 as cSpecPct is a relative frequency table.
# . Y axis label color of Light Blue 
# . Title of  "Fish Relative Frequency" 
par(mar = c(8,4,4,3))
barplot(cSpecPct, ylim = c(0,40), ylab="Relative Frequency",
        col.lab = "Light Blue", col = "Light Blue",   main = "Fish Relative Frequency", las=2)

# 16. Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d> 
d <- u[order(u$Freq,decreasing  = TRUE),]
d

# 17. Rename the <d> columns Var 1 to Species, and Freq to RelFreq 
d <- setNames(d,c("Species","RelFreq"))
d

# 18. Add new variables to <d> and call them cumfreq, counts, and cumcounts
dcount <- t[order(t$Freq,decreasing  = TRUE),]
d <- transform(d,cumfreq = round(cumsum(d$RelFreq),digits = 2) ,counts = dcount$Freq, cumcounts = cumsum(dcount$Freq))
d

# 19. Create a parameter variable <def_par> to store parameter variables 
def_par <- par() 
par(mar = c(8, 4, 4, 3))

# 20. Create a barplot, <pc>, with the following specifications:
# . d$counts of width 1, spacing of .15
# . no boarder
# . Axes: F
# . Yaxis limit 0,3.05*max
# . d$counts na.rm is true
# . y label is Cummulative Counts
# . scale x axis to 70%
# . names.arg: d$Species
# . Title of the barplot is "Species Pareto"
# . las: 2)
par(mar = c(8, 4, 4, 3))
pc <- barplot(d$counts, width = 1, space = 0.15, border = NA,
              axes = F, ylim = c(0,3.05*max(d$counts, na.rm = TRUE)),
              ylab = "Cummulative Counts", cex.axis = 0.7, 
              names.arg= d$Species, main = "Species Pareto", las = 2)

# 21. Add a cumulative counts line to the <pc> plot with the following:
# . Spec line type is b
# . Scale plotting text at 70%
# . Data values are solid circles with color cyan4

lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

# 22. Place a grey box around the pareto plot

box(col = "grey")

# 23. Add a left side axis with the following specifications
# . Horizontal values at tick marks at cumcounts on side 2
# . Tickmark color of grey62
# . Color of axis is grey62
# . Axis scaled to 80% of normal

axis(side = 2, at = c(0, d$cumcounts), tick = TRUE,
     las = 1, col.axis = "grey62", col.ticks = "grey62", cex.axis = 0.8)

# 24. Add axis details on right side of box with the specifications:
# . Spec: Side 4
# . Tickmarks at cumcounts with labels from 0 to cumfreq with %,
# . Axis color of cyan5 and label color of cyan4
# . Axis font scaled to 80% of nominal

axis(side = 4, tick = TRUE, at = c (0, d$cumcounts), las = 1,
     labels =paste(round( c(0,d$cumfreq),digits = 0),'%'),
     col.axis = "cyan4", col = "cyan4", cex.axis = "0.8")           

#### have used axis color as "cyan4" as "cyan5" is not availlable

# 25. Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot
par(mar = c(8, 4, 4, 3))
pc <- barplot(d$counts, width = 1, space = 0.15, border = NA,
              axes = F, ylim = c(0,3.05*max(d$counts, na.rm = TRUE)),
              ylab = "Cummulative Counts", cex.axis = 0.7, 
              names.arg= d$Species,  las = 2)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey")
axis(side = 2, at = c(0, d$cumcounts), tick = TRUE,
     las = 1, col.axis = "grey62", col.ticks = "grey62", cex.axis = 0.8)
axis(side = 4, tick = TRUE, at = c (0, d$cumcounts), las = 1,
     labels =paste(round( c(0,d$cumfreq),digits = 0),'%'),
     col.axis = "cyan4", col = "cyan4", cex.axis = "0.8")   
title(main = "Murugan")

