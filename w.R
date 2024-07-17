library(tidyverse)
library(dplyr)

#importing files
raw_file <- read.csv("C:/Users/SAIL/Downloads/Email Analysis Dataset.csv")
head(raw_file)
View(raw_file)
glimpse(raw_file)

file <- subset(raw_file, select = -Email.id)
View(file)
glimpse(file)

raw_file <- na.omit(raw_file)

# unique values
Unique_file <- sapply(file, unique)

#counting variables
file |>
  count(From.Name)

file |>
  count(To.Name)



#Distribution of email senders by their seniority
sender_level <- file |>
  count(From.seniority) |>
  mutate(percentage = (n/sum(n)) * 100)
sender_level <- sender_level %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(sender_level, aes(x = From.seniority, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Senders by their seniority",
       x = "Seniority Level",
       y = "Count") +
  theme_minimal()



#Distribution of email senders by their department
sender_dept <- file |>
  count(From.Department) |>
  mutate(percentage = (n/sum(n)) * 100)

sender_dept <- sender_dept %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))


# Create bar chart with values at the top
ggplot(sender_dept, aes(y = From.Department, x = n, , fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_fill_identity()+
  labs(title = "Distribution of Senders by their Department",
       x = "Department",
       y = "Count") +
  theme_minimal()











#Distribution of email Receivers by their seniority
receiver_level <- file |>
  count(To.seniority) |>
  mutate(percentage = (n/sum(n)) * 100)
receiver_level <- receiver_level %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(receiver_level, aes(x = To.seniority, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Receivers by their seniority",
       x = "Seniority Level",
       y = "Count") +
  theme_minimal()


#Distribution of email Receivers by their department
receive_dept <- file |>
  count(To.Department) |>
  mutate(percentage = (n/sum(n)) * 100)

receive_dept <- receive_dept %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(receive_dept, aes(y = To.Department, x = n, , fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_fill_identity()+
  labs(title = "Distribution of Receivers by their Department",
       x = "Department",
       y = "Count") +
  theme_minimal()





#Distribution of Email topics
topics <- file |>
  count(Email.topic) |>
  mutate(percentage = (n/sum(n)) * 100)

topics <- topics %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(topics, aes(y = Email.topic, x = n, , fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_fill_identity()+
  labs(title = "Distribution of Emails By Topics",
       x = "Topics",
       y = "Count") +
  theme_minimal()




##Distribution of Email sentiments
sentiment_t <- file |>
  count(Sentiment) |>
  mutate(percentage = (n/sum(n)) * 100)

sentiment_t <- sentiment %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(sentiment_t, aes(x = Sentiment, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Senders by their seniority",
       x = "Seniority Level",
       y = "Count") +
  theme_minimal()


##Distribution of Device used 
tool <- file |>
  count(Device) |>
  mutate(percentage = (n/sum(n)) * 100)

tool <- tool %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(tool, aes(x = Device, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Device used",
       x = "Device",
       y = "Count") +
  theme_minimal()


##Distribution of Opened and Unopened mail

mail <- file |>
  count(Is.opened.) |>
  mutate(percentage = (n/sum(n)) * 100)

mail <- mail %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(mail, aes(x = Is.opened., y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Opened and Unopened mail",
       x = "Device",
       y = "Count") +
  theme_minimal()



##Distribution of EMAIL SENT WITHIN THE WORKING HOUR

hour <- file |>
  count(Within.work.hours) |>
  mutate(percentage = (n/sum(n)) * 100)

hour <- hour %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(hour, aes(x = Within.work.hours, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Opened and Unopened mail",
       x = "Work Hour",
       y = "Count") +
  theme_minimal()


##Distribution of EMAIL SENT WITHIN THE WORKING Day

day <- file |>
  count(Within.workdays) |>
  mutate(percentage = (n/sum(n)) * 100)

day <- day %>%
  mutate(color = ifelse(n == max(n), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(day, aes(x = Within.workdays, y = n, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of Opened and Unopened mail",
       x = "Work Day",
       y = "Count") +
  theme_minimal()




# Communication Patterns During Work Hours
#What are the most common email topics discussed during work hours?
  
theme1_1 <- file |>
  filter(Within.work.hours == "yes")|>
  group_by(Email.topic) |>
  summarise(count = n()) |>
  arrange(desc(count))
theme1_1 <- theme1_1 %>%
  mutate(color = ifelse(count == max(count), "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(theme1_1, aes(x = count, y = Email.topic, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), 
            hjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of email topics discussed during work hours",
       x = "Topic",
       y = "Count") +
  theme_minimal()



lil_1 <- file |>
  filter(Within.work.hours == "yes")|>
  filter(Email.topic == "Sales and Marketing")
View(lil_1)

lil_1 |>
  group_by(From.Department)|>
  summarise(count = n()) |>
  arrange(desc(count))






#Executive Communication During Work Hours
#How many emails do C-level executives send and receive during work hours?

theme2_1 <- file |>
  filter(Within.work.hours == "yes")|>
  group_by(From.seniority) |>
  summarise(count = n()) |>
  arrange(desc(count))
theme2_1 <- theme2_1 %>%
  mutate(color = ifelse(count == 233, "skyblue", "gray"))

# Create bar chart with values at the top
ggplot(theme2_1, aes(x = From.seniority, y = count, fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Distribution of email topics discussed during work hours",
       x = "Seniority",
       y = "Count") +
  theme_minimal()


# Create a data frame with the values
email_data <- data.frame(
  Category = c("Received", "Sent"),
  Count = c(41, 233)
)

# Create the bar chart
ggplot(email_data, aes(x = Category, y = Count, fill = ifelse(Category == "Sent", "skyblue", "gray"))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), 
            vjust = -0.5, size = 4) +
  scale_fill_identity()+
  labs(title = "Emails C-level executives send and receive during work hours", x = "Category", y = "Count") +
  theme_minimal()




lil_1 <- file |>
  filter(Within.work.hours == "yes")|>
  filter(From.seniority == "C-level")|>
  group_by(Sentiment) |>
  summarise(count = n()) |>
  arrange(desc(count))

