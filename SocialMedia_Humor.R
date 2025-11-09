data_1<-Assignment_3A_Data
data_1<-na.omit(data_1)
library(dplyr)
Assignment_3A_Data|> #summarize the data
  group_by(`Humor (0 = No humor, 1 = Humor)`)|>
  summarise(
    count=n(),
    mean_likes=mean(`Number of Likes Per Post`,na.rm=TRUE),
    median_likes=median(`Number of Likes Per Post`,na.rm=TRUE),
    sd_likes=sd(`Number of Likes Per Post`,na.rm=TRUE),
    var_likes=var(`Number of Likes Per Post`,na.rm=TRUE))
Assignment_3A_Data|> #summarize the data by quartiles
  group_by(`Humor (0 = No humor, 1 = Humor)`)|>
  summarise(
    min_likes=min(`Number of Likes Per Post`),
    lower_quartile=quantile(`Number of Likes Per Post`,.25,na.rm=TRUE),
    upper_quartile=quantile(`Number of Likes Per Post`,.75,na.rm=TRUE),
    max_likes=max(`Number of Likes Per Post`))
#Look at side by side histogram
library(ggplot2)
ggplot(data_1,aes(x=`Number of Likes Per Post`,fill = factor(`Humor (0 = No humor, 1 = Humor)`)))+
  geom_histogram(bins = 10,alpha=0.7,position = "identity")+
  labs(
    title = "Histogram of Likes (by Humor)",
    x="Number of likes per post",
    y="Count",
    fill="Humor (0=no, 1=yes)"
  )+
  theme_classic()
#check correlation
#show correlation of humor to likes
cor(data_1$`Humor (0 = No humor, 1 = Humor)`,data_1$`Number of Likes Per Post`)
#Pearson Correlation Coefficient is 0.8106964 Strong, Positive
#Test for statistical significance 
#Noticed these column names are messy. In the future simplify first :(
data_1<-data_1%>%
  rename(
    Likes=`Number of Likes Per Post`,
    Humor= `Humor (0 = No humor, 1 = Humor)`
  )
t.test(Likes~Humor,data = data_1)
#p-value<0.05, CI negative
#Scatterplot for correlation
data_1$Humor<-as.numeric(data_1$Humor)
correlation<-cor(data_1$Humor,data_1$Likes, use = "complete.obs")
correlation
library(ggplot2)
ggplot(data_1, aes(x=Humor, y=Likes))+
  geom_jitter(width = 0.1, alpha=0.6, color= "greenyellow")+
  geom_smooth(method = "lm", se=TRUE, color="plum2", linewidth = 1)+
  labs(
    title = "Correlation Between Humor and Likes",
    subtitle = paste("Pearson correlation =", round(correlation, 4)),
    x="Humor (0 = No, 1 = Yes)",
    y= "Number of Likes")+
  theme_classic()
#Add a Boxplot for understanding
ggplot(data_1, aes(x = factor(Humor), y = Likes, fill = factor(Humor))) +
  geom_boxplot() +
  labs(
    title = "Likes per Post by Humor Use",
    x = "Post Type (0 = No Humor, 1 = Humor)",
    y = "Number of Likes"
  ) +
  theme_dark()