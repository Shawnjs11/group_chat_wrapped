# group_chat_wrapped
Just a student with a fun individual project idea to share message data with friends.

Framework code to create a year in review for cleaned group chat apple data (static data)

1. pull the data from a database, or get any imessage chat data
2. clean the data (use the cleaning_chat.R)
3.  With two cleaned csv files (with and without interactions), use the shiny app folder for templates of the ui and the server

Things to note:
- Certain code was given placeholders/removed, meaning this is just a framework of a chat wrapped/dashboard
- Depending on how large the chat files are, some applications can take a minute or two to run

To pull the data, I used this as support: (https://apple.stackexchange.com/questions/421665/how-specificially-do-i-read-a-chat-db-file)


This dashboard app was initially inspired by the work of Garrick Aden Buie's Conference Tweet Dashboard (https://garrickadenbuie.com), as it helped figure out how I wanted to present my work. It also provided a real example of the mechanics of an r shiny dashboard, as I continued to learn along the way. 

Any more info on creating shiny dashboards can be found online, such as here (https://rstudio.github.io/shinydashboard/get_started.html)
