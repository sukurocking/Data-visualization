#Create a plotly ID and password on the website : 
py <- plotly(username="Abi7888", key="btm8fyrsty")  # open plotly connection

# plotly_POST publishes the figure to your plotly account on the web
plotly_POST(ggiris, filename = "plot name", sharing='public')


#List of packages
library(plotly)
library(mosaic)
library(ggthemes)
library(dplyr)
library(ggplot2)

#-----------TOPIC
#-----------Airplane crashes since 1908

setwd("D:\\vis plotly\\")
ac<-read.csv("ac_since_1908.csv",h=T,stringsAsFactors=FALSE)
dim(ac)
head(ac)

# names(ac)
# [1] "Date"         "Time"         "Location"     "Operator"     "Flight.."     "Route"        "Type"        
# [8] "Registration" "cn.In"        "Aboard"       "Fatalities"   "Ground"       "Summary" 

#Data manipulation checks
summary(ac)

#Checking missing values
colSums(is.na(ac))
dim(ac)

#Remove missing values
ac<-na.omit(ac)
dim(ac)

#Create a date column
ac$date_new <- as.Date(paste(ac$month , ac$day ,ac$year, sep = "."),format = "%m.%d.%Y" )
head(ac$date_new)

ac<-ac %>% arrange(date_new)
head(ac$date_new)

#Creating buckets year decades :
#install.packages("mosaic")

library(mosaic)
ac <- mutate(ac, year_bucket = derivedFactor(
  "1910-1920" = (year > 1910 & year <= 1920),
  "1920-1930" = (year > 1920 & year <= 1930),
  "1930-1940" = (year > 1930 & year <= 1940),
  "1940-1950" = (year > 1940 & year <= 1950),
  "1950-1960" = (year > 1950 & year <= 1960),
  "1960-1970" = (year > 1960 & year <= 1970),
  "1970-1980" = (year > 1970 & year <= 1980),
  "1980-1990" = (year > 1980 & year <= 1990),
  "1990-2000" = (year > 1990 & year <= 2000),
  "2000-2010" = (year > 2000 & year <= 2010),
  .method = "first",
  .default = NA
))

table(ac$year_bucket)

#Two key numeric metrics
#1) # people aboard 
#2) # fatalities

#Two key categorical metrics
#1) Country
#2) Year

#Filter a few countries, using dplyr package
filter_countries<-ac%>%filter(Country%in%c("India","China","Germany","Russia","Brazil")) 


#-----------Bar charts

#1)Group by year_bucket find the total number of people abroad and total number of 
#Fatalities

p<-filter_countries %>% group_by(year_bucket) %>%
  summarize(no_aboard = sum(Aboard), no_fatalities=sum(Fatalities), count_crashes=n()) %>% data.frame()

k1<-p %>% ggplot(aes(x = year_bucket, y = no_aboard,fill=count_crashes)) + #Y is # people aboard                     
  geom_bar(stat = "identity")+                                      #bar chart
  ggtitle("Number of people on the flight")+                        #title
  theme(plot.background=element_rect(fill="pink"),
        plot.margin=unit(c(1,4,1,4),"cm"),                          #unit with the sizes of the top, right,bottom, and left margins
        axis.text.x =  element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1))

k2<-p %>% ggplot(aes(x = year_bucket, y = no_fatalities,fill=count_crashes)) + #Y is # no_fatalities                     
  geom_bar(stat = "identity")+                                      #bar chart
  ggtitle("#Fatalities on the flight")+                        #title
  theme(plot.background=element_rect(fill="grey"),
        plot.margin=unit(c(1,4,1,4),"cm"),                          #unit with the sizes of the top, right,bottom, and left margins
        axis.text.x =  element_text(size  = 10,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1))

ggplotly(k1)
ggplotly(k2)

#2)Group by year_bucket and Country and find the total number of people abroad and total number of 
#Fatalities

s<-filter_countries %>% group_by(year_bucket,Country) %>%
summarize(no_aboard = sum(Aboard), no_fatalities=sum(Fatalities),count_crashes=n()) %>% data.frame()

#Take the summarised dataset and create the basic layer of ggplot
#Add more layers on the g ggplot
g1<-s %>% ggplot(aes(x = year_bucket, y = no_aboard, fill=count_crashes)) + #Y is # people aboard                     
    geom_bar(stat = "identity")+                                      #bar chart
    facet_grid(~Country, scales="free_y") +                           #Split the chart
    ggtitle("Number of people on the flight")+                        #title
    theme(plot.background=element_rect(fill="pink"),
          plot.margin=unit(c(1,4,1,4),"cm"),                          #unit with the sizes of the top, right,bottom, and left margins
          axis.text.x =  element_text(size  = 10,
                         angle = 45,
                         hjust = 1,
                         vjust = 1))
                                                             
                                                            

g2<-s %>% ggplot(aes(x = year_bucket, y = no_fatalities, fill=count_crashes)) + # y = #Fatalies                       
  geom_bar(stat = "identity")+
  facet_grid(~Country, scales="free_y")+
  ggtitle("Number of Fatalities")+
  theme(plot.background =element_rect(fill="pink"),
        plot.margin=unit(c(1,4,1,4),"cm"),                          #unit with the sizes of the top, right,bottom, and left margins
        axis.text.x =  element_text(size  = 10,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1))

ggplotly(g1)
ggplotly(g2)

#-----------Point charts
#geom_point()

#1)Plotting time variable 
#Checking %Fatalities by time

#Create a new column per_fat :Percentage of fatalities
filter_countries<-mutate(filter_countries,per_fat=100*(Fatalities/Aboard))
summary(filter_countries$per_fat)

#Choose palette of colors
pal <- RColorBrewer::brewer.pal(nlevels(filter_countries$Country), "Set3")

#Notice here we are using plot_ly
#You can also use ggplot to plot this and then use 

plot_ly(data = filter_countries, 
        y = Fatalities, 
        color = Country,
        mode = "markers", 
        x=date_new,
        text = paste("Aboard:",Aboard,"Fatalities:", 
                     Fatalities,"Date:",date_new))

#Subplots :Similar to faceting using ggplots

#p<-subplot( plot_ly(first_object),plot_ly(Second_object))

p <- subplot(
  plot_ly(data = filter_countries, 
          y = Fatalities, 
          color = Country,
          mode = "markers", 
          x=date_new,
          text = paste("Fatalities:", Fatalities,"Date:",date_new,"Operator:", Operator))
  ,
  plot_ly(data = filter_countries, 
          y = Aboard, 
          color = Country,
          mode = "markers", 
          x=date_new,
          text = paste("Aboard:",Aboard,"Date:",date_new,"Operator:", Operator))
  ,
  margin = 0.05,
  nrows=2
) %>% layout(showlegend = TRUE)
p

filter_countries$id <- as.integer(as.factor(filter_countries$Country))

#Subplot using plot_ly: Similar to faceting on ggplot
k<-plot_ly(data = filter_countries, 
           x= Aboard,
           y = Fatalities,
           group = Country,
           xaxis = paste0("x", id),
           mode = "markers", 
           size = per_fat, 
           text = paste("Operator:", Operator))
k <- subplot(k)
k

#--------------------------------------OR using GGPLOT-------------------------------------------#

r<-filter_countries%>% ggplot(aes(x=date_new,y=Fatalities))+
  geom_point(aes(color=Country,size=per_fat))+
  scale_size_area()+
  labs(x = "Index")+
  facet_wrap(~Country)
ggplotly(r)

##2)Bubble charts
#Let's pick the summary function we created : s
#Now let's create a bubble chart:
dim(s)
bubble_chart<-s%>% 
  ggplot(aes(x=year_bucket,y=no_fatalities,size=count_crashes))+
  geom_point(aes(color=count_crashes))+
  labs(x = "Index")+
  facet_wrap(~Country)+
  theme(plot.margin=unit(c(1,4,1,4),"cm"),                          #unit with the sizes of the top, right,bottom, and left margins
        axis.text.x =  element_text(size  = 10,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1))
ggplotly(bubble_chart)


#-----------Tile plots

head(s)
tile<-ggplot(data = s, aes(y = Country, x = year_bucket)) + 
      geom_tile(aes(fill = no_fatalities))+
      coord_equal()+
      scale_fill_gradient(name = "# Fatalities",low = c("pink","red"), high = "Black")+
      theme_solarized_2()+
      ggtitle("#Fatalities")+ 
      xlab("Year") +
      theme(plot.margin=unit(c(1,4,1,4),"cm"),                          
            axis.text.x =  element_text(size  = 10,
                                        angle = 45,
                                        hjust = 1,
                                        vjust = 1))

s$per_fat<-100*(s$no_fatalities/s$no_aboard)

tile2<-ggplot(data = s, aes(y = Country, x = year_bucket)) + 
  geom_tile(aes(fill = per_fat))+
  coord_equal()+
  scale_fill_gradient(name = "Percentage of Fatalities",low = c("pink","red"), high = "Black")+
  theme_solarized_2()+
  ggtitle("Percentage of Fatalities")+ 
  xlab("Year") +
  theme(plot.margin=unit(c(1,4,1,4),"cm"),                          
        axis.text.x =  element_text(size  = 10,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1))

ggplotly(tile)
ggplotly(tile2)
 
#histogram 2d
#It gives the count grouped by two variables

plot_ly(x = filter_countries$year_bucket, 
        y = filter_countries$Aboard,
        z= filter_countries$Fatalities, 
        type = "histogram2d")
