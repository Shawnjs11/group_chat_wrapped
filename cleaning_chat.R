
# Code that will help clean chat data after pulling it

library(tidyverse)
library(DBI)
library(lubridate)

# Reading in the data
# Make sure that the csv is in your working directory
chat.df <- read.csv("...", header = TRUE)

# Changing the header names
names(chat.df) <- c("ROWID", "ThreadID", "IsFromMe", "FromPhoneNumber", "ToPhoneNumber", "Service", "TextDate", "MessageText", "RoomName")

# Making sure they are unique texts (in a group chat, multiple rows may convey the same info)
chat.df <- distinct(chat.df,ROWID, .keep_all= TRUE)

# # Changing date into date variable
chat.df$CalendarDate <- as.Date(chat.df$TextDate)
# Making sure that the data is for this chat and from 2022 (for my own example)
chat.df <- chat.df %>% filter(RoomName == "..." &
                                CalendarDate >= "2022-01-01")

# Optional: to change numbers to names/ or fake names, use the ifelse() function
# Otherwise, keep it as the phone numbers/emails

# Creating one without interactions

chat_wi.df <- chat.df
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 10) != "Laughed at")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 9) != "Disliked ")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 6) != "Liked ")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 6) != "Loved ")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 11) != "Emphasized ")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 8) != "Removed ")
chat_wi.df <- chat_wi.df %>% filter(substr(MessageText, 1, 11) != "Questioned ")

# create two new cleaned csv files in the same directory using write.csv()
write.csv("")
