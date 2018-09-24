#loading necessary library
library(ggplot2)
library(Hmisc)
library(scales)
library(ggthemes)
library(dplyr)

#Setting values in the varaible "ideo" greater than 5 to NA
is.na(PRCPS$ideo)= PRCPS$ideo > 5

#Setting values in the varaible "income" greater than 9 to NA
is.na(PRCPS$income)= PRCPS$income > 9

#Finding the summary data of variables "income" and "ideo"
summary(PRCPS$income, na.rm=TRUE)
summary(PRCPS$ideo, na.rm=TRUE)

#Finding the Standard Deviation of variable "income"
sd(PRCPS$income, na.rm=TRUE)

#Finding the Standard Deviation of variable "ideo"
sd(PRCPS$ideo, na.rm=TRUE)

#Finding amount of numbers and amount of NA's for "income" variable
library(Hmisc)
describe(PRCPS$income)

#Finding amount of numbers and amount of NA's for "ideo" variable
describe(PRCPS$ideo)

#loading ggplot2 to being using graphs
library(ggplot2)
library(scales)

#creating a bar graph of the "ideo" varibale
bar_graph_ideo <- ggplot(PRCPS, aes(ideo)) + geom_bar() + labs(title = "Preferred Political Ideology of Americans", subtitle = "Pew Research April '17 Political Survey") + labs(x="Political Preferences", y="Survey Responses") + theme_bw() 
#creating breaks and naming ticks 
bar_graph_ideo <- bar_graph_ideo + scale_x_discrete(limits=1:5, breaks=c("1","2","3","4","5") , labels=c("1" = "Very Conservative", "2" = "Conservative", "3" = "Moderate", "4" = "Liberal", "5" = "Very Liberal"))
#changing the text of the title
bar_graph_ideo <- bar_graph_ideo + theme(plot.title=element_text(family="Times", face="bold", size=16))
#changing the rest of the text
bar_graph_ideo<- bar_graph_ideo + theme(axis.text = element_text(family="Times", size=14, colour="black"))
#calling bar_graph_ideo
bar_graph_ideo

#creating a bar graph of the "income" varibale
bar_graph_income <- ggplot(PRCPS, aes(income)) + geom_bar() + labs(title = "How Much Money Are American Families Making?", subtitle = "Pew Research April '17 Political Survey") + labs(x="Income Ranges (In Thousands of US Dollars)", y="Survey Responses") + theme_bw() 
#creating breaks and naming ticks 
bar_graph_income <- bar_graph_income + scale_x_discrete(limits=1:9, breaks=c("1","2","3","4","5","6","7","8","9") , labels=c("1" = "Below 10", "2" = "10 - 19", "3" = "20 - 29.99", "4" = "30 - 39.99", "5" = "40 - 49.99", "6" = "50 - 74.99", "7" = "75 - 99.99",  "8" = "100 - 149.99",  "9" = "150 +"))
#changing the text of the title
bar_graph_income <- bar_graph_income + theme(plot.title=element_text(family="Times", face="bold", size=16))
#changing the rest of the text
bar_graph_income<- bar_graph_income + theme(axis.text = element_text(family="Times", size=14, colour="black"))
#calling bar_graph_income
bar_graph_income


#Creating a heatmap
#loading packaing ggthemes for heatmap
library(ggthemes)
#using geom_bin2d which is basically a heatmap
heat_map_finalpaper <- ggplot(PRCPS, aes(ideo, income)) + geom_bin2d(bins=9) + scale_fill_gradient(low = "white", high = "steel blue", name ="Count") + labs(title = "Heatmap of Income and Political Ideology", subtitle = "Pew Research April 17 Political Survey") + theme_tufte() + labs(x = "Americans Political Ideology")+ labs(y = "Americans Family Income")
#changing tick names of x axis
heat_map_finalpaper <- heat_map_finalpaper + scale_x_discrete(limits=1:5, breaks=c("1","2","3","4","5") , labels=c("1" = "Very Conservative", "2" = "Conservative", "3" = "Independent", "4" = "Liberal", "5" = "Very Liberal"))
#changing tick names of y axis
heat_map_finalpaper <- heat_map_finalpaper + scale_y_discrete(limits=1:9, breaks=c("1","2","3","4","5","6","7","8","9") , labels=c("1" = "Below $10,000", "2" = "$10,000-$19,999", "3" = "$20,000-$29,999", "4" = "$30,000-$39,999", "5" = "$40,000-$49,999", "6" = "$50,000-$74,999", "7" = "$75,000-$99,999",  "8" = "$100,000-$149,999",  "9" = "$150,000 or more"))
#changing the text of the title
heat_map_finalpaper<- heat_map_finalpaper + theme(plot.title=element_text(family="Times", face="bold", size=14))
#changing the rest of the text
heat_map_finalpaper<- heat_map_finalpaper + theme(axis.text = element_text(family="Times", size=12, colour="black")) + theme(axis.title = element_text(family="Times", size=12, colour="black"))
heat_map_finalpaper

#hexagonal heatmap scatterplot with trend line 
library(hexbin)
scatterplot_finalpaper<-ggplot(PRCPS, aes(ideo , income))
#changing x axis ticks
scatterplot_finalpaper <- scatterplot_finalpaper + scale_x_discrete(limits=1:5, breaks=c("1","2","3","4","5") , labels=c("1" = "Very Conservative", "2" = "Conservative", "3" = "Independent", "4" = "Liberal", "5" = "Very Liberal"))
#changing y axis ticks
scatterplot_finalpaper <- scatterplot_finalpaper + scale_y_discrete(limits=1:9, breaks=c("1","2","3","4","5","6","7","8","9") , labels=c("1" = "Below $10,000", "2" = "$10,000-$19,999", "3" = "$20,000-$29,999", "4" = "$30,000-$39,999", "5" = "$40,000-$49,999", "6" = "$50,000-$74,999", "7" = "$75,000-$99,999",  "8" = "$100,000-$149,999",  "9" = "$150,000 or more"))
#more changes to scatterplot
scatterplot_finalpaper<-scatterplot_finalpaper + stat_bin_hex() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
scatterplot_finalpaper

#scatterplot with trend line (non-hex)
scatter1<-ggplot(PRCPS, aes(ideo ,income ))
scatter1<-scatter1 + geom_point(alpha=0.03) + geom_smooth(method = "loess") + labs(title = "Scatterplot of Income and Political Ideology", subtitle = "Pew Research April '17 Political Survey") + labs(x="Ideology", y="American Family Income") + theme_bw()
#changing x axis ticks
scatter1 <- scatter1 + scale_x_discrete(limits=1:5, breaks=c("1","2","3","4","5") , labels=c("1" = "Very Conservative", "2" = "Convervative", "3" = "Independent", "4" = "Liberal", "5"="Very Liberal"))
#changing y axis ticks
scatter1 <- scatter1 + scale_y_discrete(limits=1:9, breaks=c("1","2","3","4","5","6","7","8","9") , labels=c("1" = "Below $10,000", "2" = "$10,000-$19,999", "3" = "$20,000-$29,999", "4" = "$30,000-$39,999", "5" = "$40,000-$49,999", "6" = "$50,000-$74,999", "7" = "$75,000-$99,999",  "8" = "$100,000-$149,999",  "9" = "$150,000 or more"))
#changing the text of the title
scatter1<- scatter1 + theme(plot.title=element_text(family="Times", face="bold", size=14))
#changing the rest of the text
scatter1<- scatter1 + theme(axis.text = element_text(family="Times", size=12, colour="black")) + theme(axis.title = element_text(family="Times", size=12, colour="black"))
scatter1

