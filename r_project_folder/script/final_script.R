#Loading the tidyverse and lubridate packages 

library(tidyverse)
library(lubridate)

#Loading data in csv format

stgo_2011 <- read.csv2("data/Stgo_2011.csv")

#Creating directories data_output and figures 

dir.create("data_output")
dir.create("figures")

#The script will now follow the chronology of the paper, producing the tables and graphs in the order that they appear in.  

### Question 1: Was there a major difference in the civil status of the suicide victims? ###

#Isolating and counting the civil status column in a smaller table

cs_count <- stgo_2011 %>% 
  count(ESTADO.CIVIL, sort = TRUE)

#Producing table with results in csv format 

write_csv2(cs_count, "data_output/cs_count.csv")

#Creating a graph from the table with ggplot2. The function "reorder" provides a sorted result

ggplot(data = cs_count, aes(x = reorder(ESTADO.CIVIL, -n) , y = n))+
  geom_point()+
  labs(title = "Civil status of suicide victims",
       subtitle = "Santiago de Chile 2011",
       x = "Civil status", y = "Freq.")

#Saving graph as pdf in figures folder 

ggsave(path = "figures", filename = "civil_status_plot.pdf")


### Question 2:	Is it possible to observe spikes in the suicide rate throughout the year of 2011? ###

#Isolating the relevant collum with the dates
dates_1 <- stgo_2011$FECHA.DEFUNCION

#Converting the data into a table and then a data frame, so it is possible to use the lubridate package on it. 
timedata_1 <- table(dates_1)

timedata_1 <- as.data.frame(timedata_1)

#Converting the newly created collum in the data.frame "dates_1" into date format using lubridate
timedata_1$dates_1 <- dmy(timedata_1$dates_1)

#Converting the "Freq" collum into a factor
timedata_1$Freq <- as.factor(timedata_1$Freq)

#Removing row 167 in the collum, because there is an error in the year stated. It says 2110 and that will mess up the graph. 
timedata_1 <- timedata_1[-c(167),]

#Creating a graph using ggplot2, that shows the suicide rate throughout the year 

ggplot(data = timedata_1, aes(x = dates_1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Suicide rate",
       subtitle = "Santiago de Chile 2011",
       x = "Time", y = "Freq.")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Saving graph as pdf in figures folder 
ggsave(path = "figures", filename = "dates_1.pdf")

### Question 3: Did being a resident of a poor commune increase an individual´s inclination towards suicide? ### 

#Isolating and sorting the relevant collum

res_count <- stgo_2011 %>% 
  count(COMUNA.RESIDENCIA, sort = TRUE)

#Extracting the first ten rows.

top_ten <- res_count[1:10,]

#Ggplot is again used to create a graph.The function "reorder" provides a sorted result. 

ggplot(data = top_ten, aes(x = reorder(COMUNA.RESIDENCIA, -n), y = n))+
  geom_point()+
  labs(title = "Residencies of suicide victims",
       subtitle = "Santiago de Chile 2011",
       x = "Comuna", y = "Freq.")

#Saving the graph as pdf in figures folder 
ggsave(path = "figures", filename = "res_topten.pdf")

#Producing table with all communes in csv format 
write.csv2(res_count, "data_output/res_count.csv")


#End of script  





