#install.packages("DataExplorer")
library("tidyverse")
library("DataExplorer")
library("ggplot2")
library("dplyr")


airbnb<- read.csv("data/AB_NYC_2019.csv")

city_list <-airbnb %>% group_by(neighbourhood_group) %>% summarize(listing_sum = n()) %>%
  arrange(-listing_sum)
city_list

summary(airbnb)
str(airbnb)
#hiányzó adatok ellenőrzése, törlése
plot_missing(airbnb) 
airbnb[14]<- NULL
plot_missing(airbnb)

####Kiugró adatok szűrése
#kiugro árak szűrése
Q <- quantile(airbnb$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(airbnb$price, na.rm = T)
airbnb2 <- airbnb %>% filter(price > (Q[1] - 1.5*iqr) & 
                            price < (Q[2] + 1.5*iqr))

#kiugró árak nélküli adatok
par(mfrow = c(2,1))
boxplot(airbnb$price, horizontal = T)
boxplot(airbnb2$price, horizontal = T)

#kiugró minimum_nights szűrése
Q <- quantile(airbnb$minimum_nights, probs=c(.25, .75), na.rm = T)
iqr <- IQR(airbnb$minimum_nights, na.rm = T)
airbnb2 <- airbnb2 %>% filter(minimum_nights > (Q[1] - 1.5*iqr) & 
                          minimum_nights < (Q[2] + 1.5*iqr)) 

boxplot(airbnb$minimum_nights, horizontal = T)
boxplot(airbnb2$minimum_nights, horizontal = T)

#kiugró elérhetőség szűrése
Q <- quantile(airbnb$availability_365, probs=c(.25, .75), na.rm = T)
iqr <- IQR(airbnb$availability_365, na.rm = T)
airbnb2 <- airbnb2 %>% filter(availability_365 > (Q[1] - 1.5*iqr) & 
                            availability_365 < (Q[2] + 1.5*iqr))

boxplot(airbnb$availability_365, horizontal = T)
boxplot(airbnb2$availability_365, horizontal = T)


summary(airbnb2)

#városrészenként a koordináták szerinti eloszlás
ggplot(data=airbnb2)+geom_point(aes(
  x=latitude,
  y=longitude,
  color=neighbourhood_group)) 


map <- ggplot(airbnb2, mapping = aes(longitude, latitude, color = price))
map + geom_point(cex = 0.2) + labs(title = "Az ár eloszlása")

map <- ggplot(airbnb2, mapping = aes(longitude, latitude, color = room_type))
map + geom_point(cex = 0.1) + labs(title = "A szobatípusok megoszlása")


#plot_correlation(data)
#plot_histogram(data)
#plot_density(data)

#szobatípusok szerinti ár eloszlás
ggplot(data = airbnb2,
       aes(x=room_type))+geom_bar(aes(fill=price))    

#városrészenként a szobatípusok szerinti eloszlás 
ggplot(data = airbnb2,
       aes(x=neighbourhood_group))+geom_bar(aes(fill=room_type))

#városrészenként az ár alakulása a minimum éjszakák alapján
g = ggplot(data = airbnb2)
g + scale_y_log10()+geom_point(aes(x=price,
                                   y=minimum_nights,
                                   color=neighbourhood_group))

###Manhattan statisztikák
manhattan_data <- filter(airbnb, airbnb$neighbourhood_group=="Manhattan")
plot_missing(manhattan_data)
brooklyn_data[14]<- NULL
plot_missing(manhattan_data)
#Manhattani átlagár
mean(manhattan_data$price) 
#Manhattani átlag minimum éjszakák
mean(manhattan_data$minimum_nights)

#Manhattani neighbourhood vizsgálat
ggplot(data=manhattan_data)+geom_point(aes(
  x=latitude,
  y=longitude,
  color=neighbourhood))

map <- ggplot(manhattan_data, mapping = aes(longitude, latitude, color = price))
map + geom_point(cex = 0.2) + labs(title = "Az ár eloszlása Manhattanben")

map <- ggplot(manhattan_data, mapping = aes(longitude, latitude, color = room_type))
map + geom_point(cex = 0.1) + labs(title = "A szobatípusok eloszlása Manhattanben")

ggplot(data = manhattan_data,
       aes(x=room_type))+geom_bar(aes(fill=price))
#Átlag árak eltérése a Manhattani átlagártól
meandiffmanhattan=mean(airbnb2$price)-mean(manhattan_data$price)
meandiffmanhattan




###Staten Island statisztikák
sisland_data <- filter(airbnb, airbnb$neighbourhood_group=="Staten Island")
plot_missing(sisland_data)
sisland_data[14]<- NULL
plot_missing(sisland_data)
#Staten Island átlagár
mean(sisland_data$price) 
#Staten Island átlag minimum éjszakák
mean(sisland_data$minimum_nights)

#Staten Island neighbourhood vizsgálat
ggplot(data=sisland_data)+geom_point(aes(
  x=latitude,
  y=longitude,
  color=neighbourhood))

map <- ggplot(sisland_data, mapping = aes(longitude, latitude, color = price))
map + geom_point(cex = 0.2) + labs(title = "Az ár eloszlása Staten Islanden")

map <- ggplot(sisland_data, mapping = aes(longitude, latitude, color = room_type))
map + geom_point(cex = 0.1) + labs(title = "A szobatípusok eloszlása Staten Islanden")

ggplot(data = sisland_data,
       aes(x=room_type))+geom_bar(aes(fill=price))
#Átlag árak eltérése a Staten Islandi átlagártól
meandiffsisland=mean(airbnb2$price)-mean(sisland_data$price)
meandiffsisland


