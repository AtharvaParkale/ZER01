getwd()
setwd("C:/Users/athar/Desktop/ZER01")
passes <-read.csv("passes1.csv")
passes

midpts<-barplot(passes$Stat,ylab="Total passes",col=rainbow(50),main="Stats for passes",cex.names = 0.8,las = 2,ylim = c(0, 1000))

grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")

text(passes$Player,
     x = midpts,
     offset = -0.1,
     y = -20,
     cex = 0.8,
     srt = 40,
     xpd = TRUE,
     pos = 2 )

w <-read.csv("passes_wins.csv")
w
# Data generation
set.seed(1)
x <- w$Passes
y <- w$Wins

# Creating the plot
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
text(paste("Correlation:", round(cor(x, y), 2)), x = 17000, y = 25)

w <-read.csv("assists1.csv")
w
w <-read.csv("assists_wins.csv")
w
plot(w$Assists,
     w$Goals,
     main="Comparing goals and passes",
     xlab="Assists",
     ylab="Goals",
     pch=19,
     cex=w$Wins/9,
     cex.axis=1.5,
     col=rainbow(10))

color.function <- colorRampPalette( c( "#CCCCCC" , "#104E8B" ) )
pal <- colorRampPalette(colors = c("lightblue", "blue"))(3)
with(foo, barplot(mpg, 
                  names.arg = cyl, 
                  xlab = "Number of cylinders", 
                  ylab = "Mean miles per gallon", 
                  col = pal))

#goalsconceded
df<-read.csv("C:\\Users\\aayus\\Documents\\pl_20-21.csv")
library(dplyr)
defenders<-subset(df,Position=="Defender")
defenders<-defenders%>%
        arrange(desc(Appearances)) 
defenders<- head(defenders,10)
defenders["cpg"]<-defenders$Goals.Conceded/defenders$Appearances
plot(defenders$Clean.sheets,defenders$cpg,ylab='Goals conceded per game',cex=defenders$Tackles/50, main ='Goals conceded per game vs Clean sheets',xlab="Clean Sheets",col="#b77ff0",pch=20)