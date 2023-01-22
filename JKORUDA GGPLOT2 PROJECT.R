
### title: "Data Visualisation with ggplot2 - Project"
###output: html_notebook

  
  # AfterWork Data Science: Data Visualisation with Python Project
  
  ## 1. Defining the Question
  
  ### a) Specifying the Data Analysis Question
  ###Specify the reasearch question that you'll be answering. i.e. Provide strategy recommendations that will lead to revenue growth.

### b) Defining the Metric for Success
###The solutions to the following questions will help us answer our research question:

### When is the best time of year to book a hotel room?
### When is the optimal length of stay in order to get the best daily rate?
###How will you know if a hotel was likely to receive a disproportionately high number of special requests?

### c) Understanding the context
###Provide some background information....

### d) Recording the Experimental Design
#Describe the steps/approach that you will use to answer the given question.

### e) Data Relevance
###How relevant was the provided data?


## 2. Reading the Data
# Installing packages
# Let’s install tidyverse packages in Rstudio

install.packages("tidyverse")


###```{r}
# Load tidyverse for use in our notebook
# --- 
# Dataset url = 
# --- 
library(tidyverse)
#Read provided dataset into data frame and preview
hotel_df <- read_csv("hotel_bookings.csv")
# Checking the first 5 rows of data
# ---
head(hotel_df, 5)
###```

###```{r}
# Checking the last 5 rows of data
# ---
tail(hotel_df, 5)
# 
###```

###```{r}
# Sample 10 rows of data
# ---
sample(hotel_df, 10)
# 
###```

###```{r}
# Checking number of rows and columns
# ---
dim(hotel_df)

#  
###```

###```{r}
# Checking datatypes
# ---
str(hotel_df)

###```

###Record your general observations below:

###Observation 1
###Observation 2


## 3. External Data Source Validation
###The data is originally from the article Hotel Booking Demand Datasets, by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019.


## 4. Data Preparation
### Performing Data Cleaning
###```{r}
# Checking datatypes and missing entries of all the variables
# ---
colSums(is.na(hotel_df))

###We observe the following from our dataset

###Observation 1
###Observation 2

###```{r}
# Checking how many duplicate rows are there in the data
# ---
# YOUR CODE GOES BELOW
# 
hotel_df[duplicated(hotel_df), ]
###We choose to keep the duplicates because we don't have a unique identifier to actually proof that we have duplicates.

###```{r}
# Checking if any of the columns are all null
# ---
all(is.na(hotel_df)) == ncol(hotel_df)
# We observe the following from our dataset:
#   
#   Observation 1
# 
# ```{r}
# Checking if any of the rows are all null
#
all(is.na(hotel_df)) == ncol(hotel_df)
# 
# We observe the following from our dataset:
#   
#Observation 1
#Observation 2
# 
# {r}
# Checking the correlation of the features through the use of 
# visualizations the correlation using heatmap
# ---
# YOUR CODE GOES BELOW
# 
#We observe the following from our dataset
#   
#   Observation 1
# Observation 2
# 
#{r}
# Dropping company column because it has alot of missing values 
# and we won't need to answer any of our questions
# ---
# YOUR CODE GOES BELOW
hotel_df <- hotel_df[,!names(hotel_df) %in% c("company")]
names(hotel_df)
# From the data variable description we see that the Distribution Channel categoricy that tells us about Booking distribution.
# 
# The term “TA” means “Travel Agents”
# The term “TO” means “Tour Operators”
# This allows us to fill the missing values in the agents column with TO
# 
# ```{r}
# We replace the mising values i.e. for TO
# ---
# YOUR GOES BELOW
hotel_df$distribution_channel[is.na(hotel_df$distribution_channel)] <- "TO"
hotel_df
# We drop rows where there is no adult, baby and child as 
# these records won't help us.
# ---
hotel_df <- filter(hotel_df, adults != 0, children != 0, babies != 0)
# We replace missing children values with rounded mean value
# ---
# Hint i.e. use round()
# ---
hotel_df["children"][is.na(hotel_df["children"])] <- round(mean(hotel_df$children, na.rm=TRUE))

unique(hotel_df$children)
# # Checking for missing values in the dataframe
# # ---
colSums(is.na(hotel_df))
# # Converting the datatypes of the following columns from float to integer
# # i.e. children, company, agent
# # ---
lapply(hotel_df$children,as.numeric)
lapply(hotel_df$company,as.numeric)
lapply(hotel_df$agent,as.numeric)
glimpse(hotel_df)
# ## 5. Solution Implementation
# 
# ### 5.a) Questions
# 
# ```{r}
# # 1. How many bookings were cancelled?
cancelled_df <- hotel_df %>% group_by(is_canceled) %>% 
  summarise(count=n(),
            .groups = 'drop')

cancelled_df
# # Visualisation: Barplot
ggplot(cancelled_df, aes(x = is_canceled, y = count)) +
  geom_col(
    stat="identity", width=0.5, fill = "#0099f9"
  ) +
  labs(
    title = "Number of Bookings", 
    x = "is_canceled", 
    y = "count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# # 2. What was the booking ratio between resort hotel and city hotel?
# # ---
# # Barplot of booking ratio between resort hotel and city hotel
hoteltype_df <- hotel_df %>% group_by(hotel) %>% 
  summarise(number_of_bookings=n(),
            .groups = 'drop')

hoteltype_df

# Plotting the chart
ggplot(hoteltype_df, aes(x = hotel, y = number_of_bookings)) +
  geom_col(
    stat="identity", width=0.5, fill =# Plotting the chart
ggplot(yearlybookings_df, aes(x = arrival_date_year, y = number_of_bookings)) +
  geom_col(
    stat="identity", width=1, fill = "#F3F781"
  ) +
  labs(
    title = "Bookings per Year", 
    x = "Year", 
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
  ) +
  labs(
    title = "Bookings per Hotel Type", 
    x = "Hotel Type", 
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# # 3. What was the percentage of booking for each year?
yearlybookings_df <- hotel_df %>% group_by(arrival_date_year) %>% 
  summarise(number_of_bookings=n(),
            .groups = 'drop')

yearlybookings_df

# Plotting the chart
ggplot(yearlybookings_df, aes(x = arrival_date_year, y = number_of_bookings)) +
  geom_col(
    stat="identity", width=1, fill =  "#0099f9"
  ) +
  labs(
    title = "Bookings per Year", 
    x = "Year", 
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# # 4. Which were the most busiest months for hotels?
# # ---
# # 
monthlybookings_df <- hotel_df %>% group_by(arrival_date_month) %>% 
  summarise(number_of_bookings=n(),
            .groups = 'drop')

monthlybookings_df

# Plotting the chart
ggplot(monthlybookings_df, aes(x = arrival_date_month, y = number_of_bookings)) +
  geom_col(
    stat="identity", width=0.5, fill = "#0099f9"
  ) +
  labs(
    title = "Bookings per Month of the Year", 
    x = "Month", 
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# # 5. From which top 3 countries did most guests come from?
# # --- 
# # YOUR GOES BELOW
country_df <- hotel_df %>% group_by(country) %>% 
  summarise(number_of_bookings=n(),
            .groups = 'drop')

country_df = country_df %>% 
  arrange(desc(number_of_bookings))

top3countries = top_n(country_df, 3, number_of_bookings)

top3countries

# Plotting the chart
ggplot(top3countries, aes(x = country, y = number_of_bookings)) +
  geom_col(
    stat="identity", width=0.5, fill = "#0099f9"
  ) +
  labs(
    title = "Bookings per Country", 
    x = "Country", 
    y = "Number of Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, family="Times New Roman"), 
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# # 6.a) How long do most people stay in hotels?
hotel_df2 = hotel_df %>% 
  mutate(stays_in_nights = stays_in_weekend_nights +  stays_in_week_nights)

ggplot(hotel_df2, aes(x = stays_in_nights)) +
  geom_histogram(
    bins=20, fill = "pink", color = "white"
  ) + 
  labs(
    title = "Distribution of Hotel Stay Length (in Nights)", 
    x = "Number of Nights", 
    y ="Frequency"
  ) +
  theme(
    plot.title = element_text(color = "#0099f9", size = 15, face = "bold", hjust = 0.5), 
  )
# #   b) By city and resort? Separate the data by hotel
city_hotel = filter(hotel_df2, hotel == 'City Hotel')
resort_hotel = filter(hotel_df2, hotel == 'Resort Hotel')

ggplot(city_hotel, aes(x = stays_in_nights)) +
  geom_histogram(
    bins=20, fill = "#0099f9", color = "#FFFFFF"
  ) + 
  labs(
    title = "Distribution of Hotel Stay Length for City Hotels(in Nights)", 
    x = "Number of Nights", 
    y ="Frequency"
  ) +
  theme(
    plot.title = element_text(color = "#0099f9", size = 15, face = "bold", hjust = 0.5), 
  )
# # 7. Which was the most booked accommodation type (Single, Couple, Family)?
accommodation_df <- hotel_df %>%
  mutate(hotel_accommodation =
           case_when(
             hotel_df$adults >= 0 & (hotel_df$children >= 1 | hotel_df$babies >= 1) ~ "Family",
             hotel_df$adults == 2 & hotel_df$children == 0 & hotel_df$babies == 0 ~ "Couple",
             hotel_df$adults == 1 & hotel_df$children == 0 & hotel_df$babies == 0 ~ "Single",
             hotel_df$adults > 2 & (hotel_df$children == 0 | hotel_df$babies == 0) ~ "Family")
  )%>%
  filter(is_canceled == 0)

accommodation_df1 <- accommodation_df %>%
  group_by(hotel_accommodation) %>%
  count(hotel_accommodation)
#Plotting the bar chart
ggplot(accommodation_df1, aes(x = hotel_accommodation, y = n, fill = accommodation_df)) +
  geom_col(
    stat="identity", width=0.5, fill = "#0099f9"
  ) +
  labs(
    title = "Sum of Hotel Accommodataion by accomodation type",
    x = "Hotel Accommodation", 
    y = "Bookings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size=12, family="Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# ### 5.b) Recommendations
# From the above analysis, below are our recommendations:
# *Guests stay longer at Resort hotels as compared to city hotels. Resort hotels have lower bookings than City hotels. Therefore effort should be put to increase number of bookings in Resort hotels
# *Number of bookings from FRA and GBR much lower than that of PRT. Advertisements should be focused on these two countries and others to encourage more people to book.
# *Lowest bookings experienced from November through to February. During these months special offers should be made in order to increase bookings.
#  
## 6. Challenging your Solution
#   In this step, we review our solution and implement approaches that could potentially provide a better outcome. In our case, we could propose the following question that wasn't answered in our solution because it couldn't have greatly contributed to our recommendation.
# 
# ```{r}
# # When should hotels provide special offers?
# # ---
# # YOUR GOES BELOW
# # 
# ```
# 
# Our observations:
#   -
#   
#   How does this observation tie to our solution?
#   
#6. Follow up questions
# a). Did we have the right data?
#   
#   We had sufficient data to answer most of the questions posed.
# 
# b). Do we need other data to answer our question?
#   
#   Additional data is needed to answer the last question on the most booked accomodation type.
# 
# c). Did we have the right question?
#   
#   Yes we had the right question.
#   
#   