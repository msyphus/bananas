library(readxl)

# Read in raw data
data <- read_excel("rawData.xlsx")
data2 <- read_excel("rawData.xlsx", sheet = "Second")
data3 <- read_excel("rawData.xlsx", sheet = "Third")
# Data is an unusable mess. The employees you asked to collect the data didn't follow your instructions. (Except whoever did the third sheet did an awesome job!)
# You can't use this data because of all the repeating column names, titles, blank rows and columns, the percent signs 
# create formatting issues, and the fact that multiple data sets are put in the same file (multiple sets in one sheet, 
# and multiple sheets in the file).  Data is not aligned so R can use it.  Additionally, the column names are difficult 
# to work with since they have spaces and special characters in them.

#Clean up raw data to make it usable
#Original reference test data
refTest <- data.frame(data[, c(1:4)])
refTest <- na.omit(refTest)
colnames(refTest) <- c("totalWeight", "fruitWeight", "peelWeight", "peelProportion")
refTest$peelProportion <- refTest$peelProportion * 100
#This is how the data should look
refTest

#One Sample T Test data (compare against reference value)
oneTTest <- data.frame(data[, c(6:9)])
oneTTest <- na.omit(oneTTest)
colnames(oneTTest) <- c("totalWeight", "fruitWeight", "peelWeight", "peelProportion")
oneTTest$peelProportion <- oneTTest$peelProportion * 100
#This is how the data should look
oneTTest

#Two Sample T Test data (compare Vendor A vs Vendor B)
twoTTest <- data.frame(data[, c(11:14)])
vendorA <- na.omit(twoTTest[c(1:21),])
vendorB <- na.omit(twoTTest[c(24:44),])
colnames(vendorA) <- c("totalWeight", "fruitWeight", "peelWeight", "peelProportion")
colnames(vendorB) <- c("totalWeight", "fruitWeight", "peelWeight", "peelProportion")
vendorA <- vendorA[-1,]
vendorB <- vendorB[-1,]
vendorA$peelProportion <- as.numeric(vendorA$peelProportion)
vendorA$peelProportion <- round(vendorA$peelProportion * 100, 2)
vendorB$peelProportion <- as.numeric(vendorB$peelProportion)
vendorB$peelProportion <- round(vendorB$peelProportion * 100, 2)
#This is what the data should look like
vendorA
vendorB
#I could touch up the totalWeight, fruitWeight, and peelWeight columns, but I won't actually be using them, so I left 
#them as is.

#Paired T Test data
pairedTTest <- data.frame(na.omit(data[, c(16:18)]))
colnames(pairedTTest) <- c("banana", "week1", "week2")
#This is what the data should look like
pairedTTest

#ANOVA test data
(combined <- unlist(data2[, c(1:5)], use.names = FALSE))
anovaTest <- data.frame(peelProportion = combined, color = rep(c("green", "yellowing", "yellow", "browning", "brown"), each = 6))
#This is what the data should look like
anovaTest

### Analyses

# Mean, standard deviation, confidence interval of the reference test.
(refAvg <- round(mean(refTest$peelProportion), 2))
(refSd <- round(sd(refTest$peelProportion), 2))
(refCI <- t.test(refTest$peelProportion))
refCIText <- "The printout shows a 95% confidence interval that ranges between 13.75 and 15.88. Although the calculated mean was 14.82, the real mean could be anywhere between 13.75 and 15.88 due to variation."

#Mean and standard deviation of second sampling.
(checkAvg <- round(mean(oneTTest$peelProportion), 2))
(checkSd <- round(sd(oneTTest$peelProportion), 2))

#One sample T Test comparing against and average of 14.82
(oneSampT <- t.test(oneTTest$peelProportion, alternative = "two.sided", mu = 14.82))
oneTText <- "The p-value is greater than 0.05, so we assume there is no difference between our sample mean and 14.82. You can also see this by looking at the 95% confidence interval.  14.82 is between 13.97 and 16.70, so it's reasonable to assume 14.82 could be the real average of our sample."

#Sample size calculation
power.t.test(n = NULL, delta = 3, sd = 2.85, sig.level = 0.05, power = 0.80, type = "two.sample")
#"n = NULL" indicates that you are trying to solve for n (your sample size).
#"delta" is what you consider a real difference between the means would be. There's a lot of discretion here, but 
#in this case, we are saying that if the means are different from each other by 2.5, that's a big deal. That's based 
#on the fact that our last confidence interval had a range of 2.73 and there could be some measurement uncertainty 
#with the scale.  We could have put any value for delta as long as you have a logical reason for it.
#sd = standard deviation; sig.level and power can usually be set like this as a rule of thumb.  We want to perform a 
#two sample T test, so we set type = "two.sample".
#The result shows n = 15.19, meaning we need 15 or 16 samples to achieve the power we need.
#Since we're not all that confident about our delta level, we decide to make it an even 20 to increase the ability 
#of our test to detect a difference.
#We could have also done this:
power.t.test(n = 20, delta = NULL, sd = 2.85, sig.level = 0.05, power = 0.80, type = "two.sample")
#Now we are solving for delta. If we take 20 samples, we could detect a difference as small as 2.59.  Since our 
#standard deviation is 2.85, and the confidence interval range is 2.73, you might be ok with that.

#Two sample T Test comparing means of Vendor A and Vendor B
(twoSampT <- t.test(vendorA$peelProportion, vendorB$peelProportion, alternative = "two.sided"))
twoTText <- "The p-value greater than 0.05 indicates that the mean banana peel proportion for each vendor is not different from the other. The confidence interval shows that the difference between Vendor A's average and Vendor B's average could be anywhere between -3.96 and 6.26. Since 0.00 is located within that range, it is reasonable to assume 0.00 could be the real average difference between the two vendors."

#F Test comparing variation between Vendor A and Vendor B
(fTest <- var.test(vendorA$peelProportion, vendorB$peelProportion))
#p-value < 0.05 so there is a difference between the two vendors.  Can also see that the ratio of variances is not 1.00.
#Think of it this way, if the variation of Vendor A is 25 and the variation of Vendor B is 25, the two are the same.
#25/25 = 1.  So if the ratio of variances is not 1, it's different.  The confidence interval ranges from 0.03 to 0.20,
#so it is not reasonable to assume that the ratio is 1.00.
fText <- "The p-value less than 0.05 indicates that the variation between the two vendors is different. This is also shown with the ratio of variances. If each vendor had the same variance, dividing them would equal 1.0. Our ratio is not 1.0, so they aren't the same. The confidence interval ranges from 0.03 to 0.20, so 1.0 so it is not reasonable to assume that the ratio is 1.00."
fPlotText <- "The plot shows an obvious difference between the curvature of the two bell curves. Samples with similar variation would have similar bell curves."

#Paired T Test
#First you find the difference between Week 1 and Week 2. I suspect the peel may lose weight over time, meaning the 
#Week 2 proportions would be less than the Week 1 proportions, so I'm subtracting Week 2 from Week 1.  If the number 
#is negative, it means the peel lost weight.
differences <- pairedTTest$week2 - pairedTTest$week1
#Now you run the T Test on the differences to see if the average difference equals 0.00 or not.
(pairedT <- t.test(differences))
#But I want to point out something here.  We suspected the peel would lose weight.  In all the other tests, we were 
#just seeing if there was a difference or not.  But now we really want to know if the peel lost weight or not.
(pairedT <- t.test(differences, alternative = "less"))
pairedText <- "As shown by the small p-value and negative confidence interval, Week 2 was definitely less than Week 1 (the peels lost weight)."

#ANOVA Test
aovModel <- aov(peelProportion ~ color, data = anovaTest)
aovTest <- summary(aovModel)
TukeyHSD(aovModel, conf.level = 0.90)
anovaText <- "The p-value (Pr(>F) column) is greater than 0.05, so there is no difference between the groups of bananas."
#Since the overall model was not significant, there's no need to run the Tukey test, but I wanted to put it on here
#for educational purposes.  If the overall model had a significant p-value (<0.05), you'd want to run the Tukey test 
#to see where the differences are.

aov2Model <- aov(Proportion ~ Color * Season, data = data3)
aov2Test <- summary(aov2Model)
anova2Text <- "The p-value for Color is greater than 0.05, so there's no difference between banana ripeness groups. Similarly, there's no difference between seasons. The interaction between Color and Season (the Color:Season row) is also not significant, which means you don't have an interaction."


#Linear Regression
(linMod <- summary(lm(fruitWeight ~ totalWeight, data = refTest)))
regText <- "The p-value of the totalWeight row is less than 0.05, so there is a linear trend. The linear equation is determined by the first column in the table. The Intercept row is your y-intercept. The totalWeight row is your slope."
regText2 <- "y = mx + b turns into y = 0.86x - 1.22" 
regText3 <- "The positive slope indicates an upward trend (positive relationship).  This is how you'd predict the fruit weight.  If you weigh a whole banana that was 170 g.  y = (0.86 * 170) - 1.22. So you'd only charge the customer for 144.98 g."

#Please note that I did not cover assumption testing.  All of these tests are parametric tests, meaning the data must 
#certain criteria in order for the tests to be reliable.  I'm purposely keeping this on an introductory, beginner 
#level.  In general, you'll be ok without doing the assumption tests.  Start with this, then learn assumption testing 
#as you're able.

#Visuals
x <- seq(5, 25, length = 5000)
refY <- dnorm(x, mean = 14.82, sd = 2.85)
refPlot <- function() {
  plot(x, refY, type = "l", lwd = 2, col = "blue", ylab = "Probability Density", xlab = "Banana Peel Proportion (%)")
  abline(v = c(13.75, 14.82, 15.88), col = c("purple", "red", "purple"), lty = 2, lwd = 2)
  text(x = c(13.25, 14.32, 15.88), y = -0.015, labels = c("13.75", "14.82", "15.88"), col = c("purple", "red", "purple"), srt = 45, xpd = TRUE)
}
refPlotText <- "The bell curve represents the distribution of our data. The probability density indicates how likely the x-value (our peel proportion) is to occur."

oneTPlot <- function() {
  oneY <- dnorm(x, mean = 15.33, sd = 3.66)
  plot(x, refY, type = "l", lwd = 2, col = "blue", ylab = "Probability Density", xlab = "Banana Peel Proportion (%)")
  lines(x, oneY, type = "l", lwd = 2, col = "green")
  abline(v = c(13.97, 14.82, 15.33, 16.70), col = c("purple", "red", "orange", "purple"), lty = 2, lwd = 2)
  text(x = c(13.47, 16.20), y = -0.015, labels = c("13.97", "16.70"), col = "purple", srt = 45, xpd = TRUE)
}
oneTPlotText <- "The plot shows the reference value of 14.82 (red line) and the average of our current sample (orange line). The purple lines indicate the confidence interval for our current sample. As long as the red line is between the purple lines, our sample is likely to be no different from the reference."

twoTPlot <- function() {
  x2  <- seq(2, 30, length = 5000)
  y1 <- dnorm(x2, mean = 15.5, sd = 3.00)
  y2 <- dnorm(x2, mean = 14.35, sd = 10.60)
  plot(x2, y1, type = "l", lwd = 2, col = "blue", ylab = "Probability Density", xlab = "Banana Peel Proportion (%)")
  lines(x2, y2, type = "l", lwd = 2, col = "green")
  abline(v = c(9.39, 14.10, 14.35, 15.50, 16.90, 19.31), col = c("skyblue", "purple", "orange", "red", "purple", "skyblue"), lty = 2, lwd = 2)
}
twoTPlotText <- "Vendor A is the blue curve with red line for the mean and purple lines for the confidence interval. Vendor B is the green curve with orange line for the mean and light blue lines for the confidence interval. Because the confidence intervals overlap, the means are not considered different."

pairedPlot <- function() {
  par(mfrow = c(1, 2))
  par(mar = c(4, 4, 1, 0))
  pairedY1 <- dnorm(x, mean = 15.50, sd = 3.00)
  pairedY2 <- dnorm(x, mean = 15.20, sd = 2.99)
  plot(x, pairedY1, type = "l", lwd = 2, col = "blue", ylab = "Probability Density", xlab = "Banana Peel Proportion (%)")
  lines(x, pairedY2, type = "l", lwd = 2, col = "green")
  abline(v = c(13.80, 14.10, 15.20, 15.50, 16.60, 16.90), col = c("skyblue", "purple", "orange", "red", "skyblue", "purple"), lty = 2, lwd = 2)
  par(mar = c(4, 0, 1, 1))
  realx <- seq(-1, 1, length = 5000)
  realy <- dnorm(realx, mean = -0.30, sd = 0.23)
  plot(realx, realy, type = "l", lwd = 2, col = "blue", yaxt = "n", ylab = "", xlab = "Difference Week 2-Week 1")
  abline(v = c(-0.30, -0.21), col = c("red", "purple"), lty = 2, lwd = 2)
}
pairedPlotText <- "For illustration purposes, the left plot shows a two sample T Test. Week 1 and Week 2 were almost identical. There is a visible shift to the left (Week 2 had less peel weight), but there's no significant difference between the two weeks. This is why a Paired T Test calculates the differences first then evaluates to see if the average difference is 0.00. The plot to the right is the correct one. The confidence interval does not cover 0.00 and doesn't even have a lower end; there is a negative difference."

anovaBoxplot <- function() {
  boxplot(peelProportion ~ color, data = anovaTest, ylab = "Banana Peel Proportion (%)", xlab = "Ripeness (Banana Color)")
}
anovaPlotText <- "The boxplots show the distribution of the data for each group. The dark line in the middle is the median, 50% of the data is inside the shaded box, the tails on the top and bottom show the min and max. If there's a lot of overlap between the boxes, it is not likely the groups are different. Although the brown peels seemed to weigh less, there's a lot of overlap with the other groups, so it can't be deemed different."

anova2Boxplot <- function () {
  par(mar = c(6, 4, 1, 1))
  boxplot(Proportion ~ Color * Season, data = data3, ylab = "Banana Peel Proportion (%)", xlab = "", las = 2, cex.axis = 0.7)
}
anova2PlotText <- "This groups every combination of Banana Color and Season. They all have a lot of overlap so nothing seems to be different. However, there does seem to be more variation in the Spring and Winter. You may want to investigate how much that will cost you!"

linPlot <- function() {
  plot(fruitWeight ~ totalWeight, data = refTest, xlab = "Whole Banana Weight (g)", ylab = "Edible Fruit Weight (g)")
  abline(lm(fruitWeight ~ totalWeight, data = refTest), col = "red")
}