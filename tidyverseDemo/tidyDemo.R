#Author: Hassan Anees

#NOTE: to create label comments do: mac->'cmd + shift + r', pc->'ctrl + shift + r'
#To run a single line/command(which can go over multiple lines) in R do following:
#           mac -> 'cmd + enter', pc -> 'ctrl + enter'

#If you want to run a set of lines, you highlight the part you want to run, and use
#the shortcut described above to run it (or you can just click the run button too)

#Note on loading libraries: 
#If you get an error, try doing install.packages(pkg_name) in the console
library(tidyverse)
#reading in the data from csv and making it an interactable object
mydata <- read.csv("lab2data.csv", na = c("", "NA", "-999"))

# Viewing the dataset -----------------------------------------------------
#head gets us the start of the dataset
head(mydata)

#gives us the variables of interest for the dataset
glimpse(mydata)

#lets you view the dataset in another page
view(mydata)


# Partitioning out the data ----------------------------------------
print(mydata)
#we have self esteem, depression, and job satisfaction items in my data right now
#we want to make the average of all of related columns

#getting all the self esteem items
self_esteem_items <- mydata %>%
  select(SE1, SE2, SE3, SE4, SE5)

#adding a column to mydata which will have the average of each participant
mydata <- mydata %>%
  mutate(self_esteem = rowMeans(self_esteem_items, na.rm = TRUE))

#viewing the variables of interest again, now you have self_esteem as a coloumn
glimpse(mydata)

#Same process of depression items
#creating variable that holds depression items 
depression_items <- mydata %>%
  select(D1,D2,D3,D4,D5)

#each row of the depression items is associated with each participant 
#we want the average of the rows

#alternate way of getting average instead of doing it all in the mutate function
#above for self esteeem
depression_items_avg = rowMeans(depression_items, na.rm = TRUE)

#now that we have average for each participant, 
#we will add this avg as a new colomn to the existing dataset 
mydata <- mydata %>%
  mutate(depress = depression_items_avg) #adds despress as a coloumn

#yay, now we can see depress in the coloumn
glimpse(mydata)

#repeat steps but for job satisfaction 
job_items <- mydata %>% 
  select(JS1, JS2, JS3, JS4, JS5)

job_items_avg <- rowMeans(job_items, na.rm = TRUE)
mydata <- mydata %>%
  mutate(jobsat = job_items_avg)

glimpse(mydata)


# New condensed dataset ---------------------------------------------------
#Creating a new dataset that only has the essential stuff 
analytic_data <- mydata %>% 
  select(subject_num, sex, age, self_esteem, jobsat, depress)

glimpse(analytic_data)
#creating dataset for males
analytic_data_male <- analytic_data %>%
  filter(sex == "Male")

view(analytic_data_male)

#creating dataset for females
analytic_data_female <- analytic_data %>%
  filter(sex == "Female")

view(analytic_data_female)

#adding skimr lib, this is just a cool library that lets you have a nice
#overview of your dataset
library(skimr)
skim(analytic_data_male)
skim(analytic_data_female)

#Now going to ggplot2 to find relationship between job satisfaction and self esteem 
#first setting the graph, then geom layer, then adding finishing touches
mygraph <- ggplot(data = analytic_data_female, mapping = aes(x = self_esteem, y = jobsat)) +
  geom_point(shape = 5, size = 1) + #adding geom layer to the graph
  coord_cartesian(xlim = c(1,7), ylim = c(1,7)) + #how long you want your x and y axis to be
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + #how many breaks in that axis
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  xlab("Self-Esteem") +
  ylab("Job Satisfaction") +
  theme_classic() #gets you apa style

print(mygraph) #prints graph in bottom right hand side

#saving to a pdf 
ggsave(file = "Figure1.pdf", plot = mygraph, width = 6, height = 6)



