library(ggmap)
x <- read.csv("vehicle_acc.csv")
str(x)
x$MV.Number = as.numeric(x$MV.Number)
x$State = as.character(x$State)

for(i in 1:nrow(x)) {
  latlon = geocode(x$State[i])
  x$lon[i] <- as.numeric(latlon[1])
  x$lat[i] <- as.numeric(latlon[2])
}
colnames(x) <- c("state",'collisions_perc','collisions_no','lon','lat')

#Using cut to transform data into bins
x$q <- with(x,cut(collisions_no,quantile(collisions_no),include.lowest = TRUE,ordered_result = TRUE))
levels(x$q) <- paste(c("1st","2nd",'3rd','4th'),"Quantile")

x$hover <- paste("Collisions:",collisions_no,"State:",state)

library(plotly)
#Storing map settings in a list
g <- list(
  scope = "usa",
  projection = list(type = "albers usa"),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
attach(x)
plot_ly(x, lon = lon, lat=lat, type = "scattergeo", color = q, mode = "markers" , text = hover,marker = list(size = sqrt(collisions_no/2)), locationmode = 'USA-states')%>%layout(title = "Vehicle accidents in USA",geo = g)
