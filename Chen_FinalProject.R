#File   : Chen_FinalProject
#Project: Final Project
#Author : Hung Wen Chen

# Clean canvas ----
# initialization
rm(list=ls())
dev.off() 

# Libraries ----
install.packages(c("magrittr", "dplyr", "tidyr", 
                   "plyr", "tidyverse", "psych", "pacman", "ggplot2"))
pacman::p_load(magrittr, dplyr, tidyr, 
               plyr, tidyverse, psych, scales, ggplot2) #Use pacman for multiple library load

# Set working directory ----
setwd("C:/Users/User/Documents/ALY_6000/M6 Final Project")

# Dataset Preview ----
# load dataset
vgsales <- read.csv("vgsales-12-4-2019-short.csv", stringsAsFactors = FALSE)

headTail(vgsales)
str(vgsales)
summary(vgsales)

# Plot ----
## Section 1 ----
### Bar Chart: Top 10 Game Platform ----
gamePlatform <- as.data.frame(prop.table(table(vgsales$Platform)))
gamePlatform <- gamePlatform[order(-gamePlatform$Freq),]

names(gamePlatform)[1] <- "Platform"
names(gamePlatform)[2] <- "Percentage"
gamePlatform <- gamePlatform[c(1:10),] #Top 10

ggplot(gamePlatform, 
  aes(x=reorder(Platform, desc(Percentage)), y=Percentage, fill=reorder(Platform, desc(Percentage)))) +
  geom_bar(stat="identity", width=.7) +  
  geom_text(aes(label = percent(Percentage, accuracy=.1)), vjust=-0.2) +
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill="none") +
  labs(title="Top 10 Platform", x = "Platform", y = "percentage")

### Bar Chart: Top 10 Game Publisher ----
gamePublisher <- as.data.frame(prop.table(table(vgsales$Publisher)))
gamePublisher <- gamePublisher[order(-gamePublisher$Freq),]

names(gamePublisher)[1] <- "Publisher"
names(gamePublisher)[2] <- "Percentage" 
gamePublisher <- gamePublisher[!(gamePublisher$Publisher %in% c("Unknown")), ] #Exclude Unknown
gamePublisher <- gamePublisher[c(1:10),] #Top 10

ggplot(gamePublisher, 
  aes(x = reorder(Publisher, Percentage), y = Percentage, fill = reorder(Publisher, Percentage))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(Percentage, accuracy=.1)), hjust=-0.2) +
  scale_fill_brewer(palette ="Set3") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.04)) +
  guides(fill = "none") +
  labs(title ="Top 10 Publisher", x = "Publisher", y = "percentage") +
  coord_flip()
  
  
## Section 2 ----
### New attributes ----
gameSales <- data.frame(vgsales$Total_Shipped, vgsales$Global_Sales)
gameSales <- data.frame(rowSums(gameSales, na.rm=T))
names(gameSales)[1] <- "salesTotal"
vgsales <- cbind(vgsales, gameSales)

# Mean & Median
summarize_if(`[<-`(vgsales, vgsales==0, value= NA), is.numeric, mean, na.rm = TRUE)
summarize_if(`[<-`(vgsales, vgsales==0, value= NA), is.numeric, median, na.rm = TRUE)

### Bar Chart: Sales comparing Publishers, Top 10 game sales. ----
# Top 10 Sales Total by Publisher
salesPublisher <- aggregate(vgsales$salesTotal, list(vgsales$Publisher), FUN=sum, na.rm = T)
salesPublisher <- salesPublisher[order(-salesPublisher$x),]
salesPublisher <- salesPublisher[!(salesPublisher$Group.1 %in% c("Unknown")), ]
names(salesPublisher)[1] <- "Publisher"
names(salesPublisher)[2] <- "salesTotal"

topSalesPublisher <- salesPublisher[c(1:10),]
ggplot(topSalesPublisher, 
  aes(x=reorder(Publisher, salesTotal), y=salesTotal, fill=reorder(Publisher, salesTotal))) +
  geom_bar(stat="identity") +
  ylim(0, 2300) +
  geom_text(aes(label = salesTotal), hjust=-0.2) +
  scale_fill_brewer(palette="Set3") +
  guides(fill="none") +
  labs(title="Top 10 Sales by Publisher", x = "Publisher", y = "Sales(million)") +
  coord_flip()

# Top 10 game sales
topSales <- vgsales[c(1:10),]
ggplot(topSales, 
  aes(x=reorder(Name, Total_Shipped), y=Total_Shipped, fill=reorder(Name, Total_Shipped))) +
  geom_bar(stat="identity") +
  geom_text(aes(label = Publisher), hjust=1.2) +
  scale_fill_brewer(palette="Set3") +
  guides(fill="none") +
  labs(title="Top 10 Sales by Games", x = "Games", y = "Sales(million)") +
  coord_flip()

### Pie: Genre Percentage. ----
gameGenre <- as.data.frame(table(vgsales$Genre))
gameGenre <- gameGenre[order(-gameGenre$Freq),]

gameGenre1 <- gameGenre %>%
  mutate(Freq = Freq / sum(Freq))

ggplot(gameGenre1, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, colour="white") +
  geom_text(aes(x=1.7, label = scales::percent(Freq, accuracy = .1)), 
            position = position_stack(vjust = .5)) +
  coord_polar("y") +
  theme_void() +
  labs(title="Genre Percentage", fill="Genre")

### Line with Bar: Sales per year and cumulative sales from Nintendo. ----
nintendoGames <- subset(vgsales, Publisher=="Nintendo")
nintendoGames <- nintendoGames[order(nintendoGames$Year),]
nintendoGamesSales <- aggregate(nintendoGames$salesTotal, list(nintendoGames$Year), FUN=sum, na.rm = T)
names(nintendoGamesSales)[1] <- "Year"
names(nintendoGamesSales)[2] <- "Sales"
nintendoGamesSales$cumSales <- cumsum(nintendoGamesSales$Sales)

ggplot(nintendoGamesSales, aes(x=Year)) +
  geom_bar(aes(y=Sales), stat="identity", fill="cyan", color="black") +
  geom_line(aes(y=cumSales, group=1, linetype = "Cumulative sales"), color="red") +
  geom_point(aes(y=cumSales), color = "red", group=1) +
  labs(title="Sales per year in Nintendo", x="Year", y="Sales(million)")

# Clean up ----
#clears the variables
rm(list=ls())
#clears the plots
dev.off() 
