#installing packages 
if(! require(ggplot2)) install.packages("ggplot2")
if(! require(tidyverse)) install.packages("tidyverse")
if(! require(tidygraph)) install.packages("tidygraph")
if(! require(visNetwork)) install.packages("visNetwork")
if(! require(igraph)) install.packages("igraph")
if(! require(wordcloud)) install.packages("wordcloud")

#Attaching packages
library(ggplot2)
library(tidyverse)
library(tidygraph)  #network analysis
library(visNetwork) #interactive network visualization 
library(igraph)
#for 4 
library('RColorBrewer')
library('wordcloud')
library('tm')

data <- read.csv("GibhliS.csv", header = TRUE, sep = ",", skip = 2, col.names = c("Year", "Title", "Directors", "Screenwriters", "Producer", "Music", "Commercial premiere", "Running Time", "Rotten Tomatoes"))
getwd()
head(data)

#summarising the data for data transformation
summary(data)
table(data$Directors) #this will show the frequency of each unique value in the specified variable 

#PARSING
#converting Commercial.premiere column into class: Date 
data$Commercial.premiere <- as.Date(data$Commercial.premiere, format = "%d-%m-%Y")
class(data$Commercial.premiere)

#converting Running.time coulmn "Xh Ym" format to  minutes 
print(data$Running.Time)
convert_to_minutes <- function(time_string) {
  # Extract hours and minutes using regular expression
  time_components <- as.numeric(regmatches(time_string, gregexpr("\\d+", time_string))[[1]])
  total_minutes <- time_components[1] * 60 + time_components[2]
  return(total_minutes)
}

data$Running.Time <- sapply(data$Running.Time,convert_to_minutes)  #applying the function to the entire column 
class(data$Running.Time)
print(data$Running.Time)

#converting column Rotten.Tomato ratings into numeric 
data$Rotten.Tomatoes<- as.numeric(gsub("%", "", data$Rotten.Tomatoes))

#1 Calculate max and min (ggplot) ---------------------------------------------------------------------
#movie ratings to check the movie with the highest and lowest rating 
#scatterplot
#aes() function in ggplot binds two variables you want to plot, geom_point used to create scatterplots 

max_rating <- max(data$Rotten.Tomatoes)
min_rating <- min(data$Rotten.Tomatoes)

#Create a new column for color based on ratings
data$Color <- ifelse(data$Rotten.Tomatoes == max_rating, "green", 
                     ifelse(data$Rotten.Tomatoes == min_rating, "red", "brown4"))

# Plotting
gg <- ggplot(data, aes(Title, Rotten.Tomatoes, color = Color)) +
  geom_point() +
  labs(x = "Movies", y = "Ratings", title = "Movie Names & Ratings ") +
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) +
  scale_color_manual(
    values = c("green", "red", "brown4"),
    labels = c( "Other Ratings","Highest Rating", "Lowest Ratings")
  )
gg

#2 Animation -----------------------------------------------------------------------
#Is there a trend in rating over the years
library(timetk)
library(gganimate)
df2 <- data_frame(
  Release.Date= data$Year,
  Rating= data$Rotten.Tomatoes
)
df2 %>%
  ggplot(aes(x= Rating,y=Release.Date)) +
  geom_line() +
  labs(x='Date',y='Rotten tomatoes Rating', title='Rating') +
  transition_reveal(Rating)

#3 TimeSeries ------------------------------------------------------------------------------
#Is there any association between the run time of a movie and its rating over the years
par(mfrow = c(2,1))
par(mar = c(4, 4, 2, 2))

plot(data$Year, data$Running.Time, type = "l",lwd=2,ylim = c(1, 140), xlim = c(1988, 2023),
     xlab = "Year", ylab = "Running time", main = "Running time Trend",
     col = "blue")  
plot(data$Year, data$Rotten.Tomatoes, type = "l",lwd=2,ylim = c(1, 120), xlim = c(1988, 2023),
     xlab = "Year", ylab = "Rotten Tomatoes", main = "Rotten Tomatoes Ratings Trend",
     col = "orange")  

# Reset plot margins to default after plotting
par(mar = c(5, 4, 4, 2) + 0.1)  # Default margins

#4 TextMining ------------------------------------------------------------------------------
# Which screenwriter, producer, composer has the highest contribution in Studio Ghibli production?
par(mfrow = c(3,1))
table(data$Screenwriters)
table(data$Producer)
table(data$Music)



data$Screenwriters <- trimws(data$Screenwriters)
# Create the frequency table and plot the word cloud
freq1 <- table(data$Screenwriters)
wordcloud(words = names(freq1), freq = freq1, min.freq = 0, scale=c(2,1), col = "#301934")

data$Producer <- trimws(data$Producer)
freq2 <- table(data$Producer)
wordcloud(words = names(freq2), freq = freq2, min.freq = 0, scale=c(2,1), col = "#AA336A")

data$Music <- trimws(data$Music)
freq3 <- table(data$Music)
wordcloud(words = names(freq3), freq = freq3, min.freq = 0, scale=c(2,1), col = "#66023C")


#5Interactive Network --------------------------------------------------------------------------
#Which Director has directed the most movies for Studio Ghibli?
data_new <- data.frame(Director = data$Directors,Title = data$Title)
data_new


#create a "tbl_graph" object
network <- data_new%>% 
  as_tbl_graph()


network %>% 
  activate(nodes) %>% 
  mutate(degree= centrality_degree()) %>%
  as_tibble() %>%
  arrange(desc(degree))

#Interactive Network 

vis_network <- network %>%
  mutate(group = if_else(condition = name %in% unique(data_new$Director),
                         true="Director",
                         false="Title")) %>% 
  toVisNetworkData() 
#interactive part

visNetwork(nodes = vis_network$nodes, edges = vis_network$edges,
           width = "100%", height = "600px", main = "The Ghibli Movie Network") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome() %>% 
  visGroups(groupname = "Title", shape = "icon", 
            icon = list(code = "f008", color = "black")) %>% 
  visGroups(groupname = "Director", shape = "icon",
            icon = list(code = "f007", color = "red")) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)
#6 --------------------------------------------------------------------------------------
data_new3 <- data.frame(Director = data$Directors,Ratingss = data$Rotten.Tomatoes)
data_new3


#create a "tbl_graph" object
network6 <- data_new3%>% 
  as_tbl_graph()


network6 %>% 
  activate(nodes) %>% 
  mutate(degree= centrality_degree()) %>%
  as_tibble() %>%
  arrange(desc(degree))

#Interactive Network 

vis_network1 <- network6 %>%
  mutate(group = if_else(condition = name %in% unique(data_new$Director),
                         true="Director",
                         false="Ratingss")) %>% 
  toVisNetworkData() 
#interactive part

visNetwork(nodes = vis_network1$nodes, edges = vis_network1$edges,
           width = "100%", height = "600px", main = "The Director-Rating Relation") %>%
  visLayout(randomSeed = 1000) %>%
  addFontAwesome() %>% 
  visGroups(groupname = "Ratingss", shape = "circle", color="lightgray") %>% 
  visGroups(groupname = "Director", shape = "icon",
            icon = list(code = "f007", color = "lightblue")) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)

install.packages("rmarkdown")
install.packages("MacTeX")

