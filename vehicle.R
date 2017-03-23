
## Plotting maps using Plotly
#List of packages:

library(plotly)
library(ggmap)

setwd("D:\\vis plotly\\")

#This public data contains vehicle accidents by US States for the year 2012
v <- read.csv("vehicle_acc.csv")
head(v)
dim(v)
str(v)

#----------------------------------------------Data preparation
v$State <- as.character(v$State)
v$MV.Number = as.numeric(v$MV.Number)

#Use geocode function to get the states
#install.packages("ggmap")
library(ggmap)

for (i in 1:nrow(v)) {
  latlon = geocode(v[i, 1])
  v$lon[i] = as.numeric(latlon[1])
  v$lat[i] = as.numeric(latlon[2])
}


#------------------------------------ Bubble maps

final_data = v
names(final_data)
#rename the names
colnames(final_data) = c('state', 'collisions_percent', 'collisions_no', 'lon', 'lat')

#Add a new column
final_data$hover <-
  paste("collisions:",
        final_data$collisions_no,
        'state:',
        final_data$state)

#Use cut function to create bins
final_data$q <-
  with(final_data,
       cut(
         collisions_no,
         quantile(collisions_no),
         include.lowest = T,
         ordered_result = TRUE
       ))
levels(final_data$q) <-
  paste(c("1st", "2nd", "3rd", "4th"), "Quantile")
#final_data$q <- as.ordered(final_data$q)

#List all the details related to nature of the map to the object g
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
attach(final_data)
plot_ly(
  final_data,
  lon = lon,
  lat = lat,
  text = hover,
  marker = list(size = sqrt(collisions_no / 2)),
  color = q,
  type = 'scattergeo',
  locationmode = 'USA-states'
) %>%
  layout(title = '2012 US city collisions', geo = g)

5#------------------------------------Choropleth Maps


# give state boundaries a black border
l <- list(color = toRGB("black"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('blue')
)

#Get the data codes:
state_code <- read.csv("state_table.csv")

head(state_code)
names(state_code)[1] <- "state"
final_data1 <- merge(final_data, state_code, by = "State", all.x = TRUE)

names(final_data1)[8] <- "code"
final_data1$code <- as.character(final_data1$code)

final_data <- final_data1

k <- brewer.pal(8, "Pastel2")
attach(final_data)
plot_ly(
  final_data,
  z = collisions_no,
  locations = code,
  text = hover,
  type = 'choropleth',
  locationmode = 'USA-states',
  color = collisions_no,
  colors = k  ,
  marker = list(line = l),
  colorbar = list(title = "Number of vehicle Collisions")
) %>%
  layout(title = 'Number of vehicle Collisions<br>(Hover for breakdown)', geo = g)
