
# This is the server side of the app

# Packages used:
library(shiny)
library(shinydashboard)
library(reactable)
library(crosstalk)
library(fontawesome)
library(tidyverse)
library(shinyWidgets)
library(forcats)
library(qdap)
library(tidytext)
library(wordcloud)
library(stringr)
library(tidyquant)
library(syuzhet)

# All necessary stuff for data manipulation
# Add the pathway for the two data sets
chat.df <- read.csv("...", header = TRUE)
chat_wi.df <- read.csv("...", header = TRUE)

# Taking just the texts and counting the word totals
words_by_person <- chat_wi.df %>% 
    group_by(FromPhoneNumber) %>% 
    filter(word_count(MessageText) > 0) %>% 
    summarize(words = sum(word_count(MessageText, digit.remove = FALSE)), .groups = "drop") %>% 
    arrange(words) %>% 
    as.data.frame()

# Necessary for word cloud

# Getting the tokens
message_tokens <- chat_wi.df %>%
    unnest_tokens(word, MessageText)
# Loading in stop words, and filtering those out
data(stop_words)
message_tokens <- message_tokens %>%
    filter(!(word %in% stop_words$word))

# You can choose to stem the words and use for the wordcloud, but the
# following two lines are optional if chosen not to stem

message_tokens <- message_tokens %>%
    mutate(stem = wordStem(word))


token_summary <- message_tokens %>%
    group_by(FromPhoneNumber, word) %>%
    count() %>%
    ungroup() %>% 
    as.data.frame()

# Dataframe of just reactions for counting numbers
reactions.df <- chat.df %>% filter(substr(MessageText, 1, 10) == "Laughed at" | substr(MessageText, 1, 9) == "Disliked "
                                   | substr(MessageText, 1, 6) == "Liked " | substr(MessageText, 1, 6) == "Loved "
                                   | substr(MessageText, 1, 11) == "Emphasized " | substr(MessageText, 1, 8) == "Removed "
                                   | substr(MessageText, 1, 11) == "Questioned ") %>% 
    group_by(FromPhoneNumber) %>% 
    summarize(reactions = n(), .groups = "drop")

# Had to create temp df since I couldn't figure out how to summarize both in one step
laughs.df <- chat.df %>%
    filter(substr(MessageText, 1, 10) == "Laughed at") %>% 
    group_by(FromPhoneNumber) %>% 
    summarize(laughs = n(), .groups = "drop")

# left joining laughing totals
reactions.df <- left_join(reactions.df, laughs.df)

reactions.df$laugh_prop <- reactions.df$laughs / reactions.df$reactions

# Removing temporary laugh df
rm(laughs.df)

# Filling in na value
reactions.df$laughs <- ifelse(!is.na(reactions.df$laughs), reactions.df$laughs, 0)


# Necessary for sentiment analysis

# Getting positive and negative words out of the tokens
sentiment_tokens <- message_tokens %>% 
    inner_join(get_sentiments("bing"))

# Getting the counts of each by person
character_sentiment_summary <- sentiment_tokens %>%
    group_by(FromPhoneNumber, sentiment) %>%
    summarize(n_words = n(), .groups = "drop")

# Counting the total number of sentiment words
character_sentiment_summary <- character_sentiment_summary %>%
    group_by(FromPhoneNumber) %>%
    mutate(total_assigned_words = sum(n_words)) %>%
    ungroup()

# Filtering by just positive to get positive ratio
positive_sentiment <- character_sentiment_summary %>% 
    filter(sentiment == "positive") %>% 
    mutate(positive_ratio = round(n_words / total_assigned_words, 3))

# Left joining with original df with positive by person
character_sentiment_summary <- character_sentiment_summary %>% 
    left_join(positive_sentiment)

server = function(input, output, session) {
    
    datasetUser <- reactive({
        userData <- chat.df %>% filter(FromPhoneNumber == input$code_name)
    })
    
    datasetUser_wi <- reactive({
        userData_wi <- chat_wi.df %>% filter(FromPhoneNumber == input$code_name)
    })
    
    # These following two datasets aren't really necessary to be reactive under this framework
    # They are static generally, but left this way works as well
    datasetTotal <- reactive({
        totalData <- chat.df
    })
    
    datasetTotal_wi <- reactive({
        totalData_wi <- chat_wi.df
    })
    
    # For word cloud
    words_ind <- reactive({
        person <- token_summary %>% filter(FromPhoneNumber == input$code_name & n > 2)
    })
    
    # For text analysis
    # Randomly sampled 1/25 of each user (or 200 samples if too small) to reduce run time 
    user_sent <- reactive({
        user_word_sent <- chat_wi.df %>% 
            filter(FromPhoneNumber == input$code_name & is.na(MessageText) == FALSE)
        user_word_sent <- sample_n(user_word_sent, max(nrow(user_word_sent)/25, 200))
        user_word_sent <-  get_nrc_sentiment(user_word_sent$MessageText)
    })
    
    # Bar chart of total texts
    output$total_texts_plot <- renderPlot({
        datasetTotal_wi() %>%
            group_by(FromPhoneNumber) %>% 
            summarize(count = n(), .groups = "drop") %>%
            mutate(highlight = ifelse(FromPhoneNumber == input$code_name, "y", "n")) %>% 
            ggplot(aes(x = fct_reorder(FromPhoneNumber, count), fill = highlight)) +
            geom_bar(aes(y = count), stat = "identity") +
            scale_fill_manual(values = c("y" = "yellow", "n" = "cornflowerblue"), guide = FALSE) +
            geom_text(aes(y = count, label = count), hjust = 0) +
            coord_flip() +
            xlab("Code Names") +
            ylab("Texts Without Interactions") +
            ggtitle("Group Chat Texts by Person in 2022") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Bar chart of total words
    output$total_words_plot <- renderPlot({
        words_by_person %>% 
            mutate(highlight = ifelse(FromPhoneNumber == input$code_name, "y", "n")) %>% 
            ggplot(aes(x = fct_reorder(FromPhoneNumber, words), fill = highlight)) +
            geom_bar(aes(y = words), stat = "identity") +
            scale_fill_manual(values = c("y" = "yellow", "n" = "cornflowerblue"), guide = FALSE) +
            geom_text(aes(y = words, label = words), hjust=0) +
            coord_flip() +
            xlab("Code Names") +
            ylab("Total Words") +
            ggtitle("Group Chat Total Words in 2022") +
            theme(plot.title = element_text(hjust =0.5))
    })
    
    
    # For the individual
    
    # Number of texts by chosen person
    total_texts <- reactive({
        total_individual_texts <-
            datasetUser_wi() %>%
            count() %>% 
            pull()   
    })
    
    # Number of words by chosen person
    total_words <- reactive({
        total_individual_words <-
            datasetTotal_wi() %>% 
            group_by(FromPhoneNumber) %>% 
            filter(word_count(MessageText) > 0 & FromPhoneNumber == input$code_name) %>% 
            summarize(words = sum(word_count(MessageText, digit.remove = FALSE)), .groups = "drop") %>% 
            pull(words)
    })
    
    # Number of reactions by chosen person
    total_reactions <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 10) == "Laughed at" | substr(MessageText, 1, 9) == "Disliked "
                                 | substr(MessageText, 1, 6) == "Liked " | substr(MessageText, 1, 6) == "Loved "
                                 | substr(MessageText, 1, 11) == "Emphasized " | substr(MessageText, 1, 8) == "Removed "
                                 | substr(MessageText, 1, 11) == "Questioned ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual likes
    ind_like <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 6) == "Liked ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual love
    ind_love <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 6) == "Loved ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual laugh
    ind_laugh <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 10) == "Laughed at") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual emphasize
    ind_emphasize <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 11) == "Emphasized ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual dislike
    ind_dislike <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 9) == "Disliked ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # individual question
    ind_question <- reactive({
        datasetUser() %>% filter(substr(MessageText, 1, 11) == "Questioned ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    # To save computational time, calculated group totals locally
    # Put value of 1, so it can be changed for anyone
    # Copy individual template with datasetTotal and datasetTotal_wi to calculate automatically
    total_like <- 1
    total_love <- 1
    total_laugh <- 1
    total_emphasize <- 1
    total_dislike <- 1
    total_question <- 1
    
    
    # Number of texts in value box
    output$individual_texts <- renderValueBox({
        valueBox(
            total_texts(), "Texts Sent", icon = icon("comment")
        )
    })
    
    # Number of words
    output$individual_words <- renderValueBox({
        valueBox(
            total_words(), "Words Typed", icon = icon("pen")
        )
    })
    
    # Number of reactions
    output$individual_reactions <- renderValueBox({
        valueBox(
            total_reactions(), "Message Reactions", icon = icon("face-smile")
        )
    })
    
    # Individual word comparison
    # I added some examples that you can chose to use
    output$word_comparison <- renderUI({
        str1 <- paste("You have texted a total of ", total_words(), " words this year")
        str2 <- ""
        if (total_words() >= 100000){
            str2 <- "For comparison, To Kill a Mockingbird by Harper Lee, is 100,388 words"
        }
        else if (total_words() >= 50000){
            str2 <- "For comparison, Lord of the Flies, by William Golding, is 59,900 words"
        }
        else if(total_words() >= 30000){
            str2 <- "For comparison, The Lion, the Witch and the Wardrobe, by C.S Lewis, is 38,421 words"
        }
        else{
            str2 <- "For comparison, the average article length in The New York Times is 622 words"
        }
        HTML(paste(str1, str2, sep = '<br/>'))
    })
    
    # For the group
    
    # Number of texts
    total_group_texts <- reactive({
        total_gc_texts <-
            datasetTotal_wi() %>%
            count() %>% 
            pull()   
    })
    
    # Number of words
    total_group_words <- reactive({
        total_gc_words <-
            datasetTotal_wi() %>% 
            group_by(FromPhoneNumber) %>% 
            filter(word_count(MessageText) > 0) %>% 
            summarize(words = sum(word_count(MessageText, digit.remove = FALSE)), .groups = "drop") %>% 
            pull(words)
    })
    
    # Number of reactions
    total_group_reactions <- reactive({
        datasetTotal() %>% filter(substr(MessageText, 1, 10) == "Laughed at" | substr(MessageText, 1, 9) == "Disliked "
                                  | substr(MessageText, 1, 6) == "Liked " | substr(MessageText, 1, 6) == "Loved "
                                  | substr(MessageText, 1, 11) == "Emphasized " | substr(MessageText, 1, 8) == "Removed "
                                  | substr(MessageText, 1, 11) == "Questioned ") %>% 
            summarize(reactions = n(), .groups = "drop") %>% 
            pull(reactions)
    })
    
    
    # Number of group texts in value box
    output$group_texts <- renderValueBox({
        valueBox(
            total_group_texts(), "Texts Sent", icon = icon("comment"), color = "orange"
        )
    })
    
    # Number of words
    # 1 as a placeholder
    output$group_words <- renderValueBox({
        valueBox(
            1, "Words Typed", icon = icon("pen"), color = "orange"
        )
    })
    
    # Number of reactions
    output$group_reactions <- renderValueBox({
        valueBox(
            total_group_reactions(), "Message Reactions", icon = icon("face-smile"), color = "orange"
        )
    })
    
    
    
    # Tab 3
    
    output$individual_word_cloud <- renderPlot({
        person <- words_ind()
        wordcloud(words = person$word, freq = person$n, min.freq = median(person$n), max.words = 250, scale=c(1.5,0.5))
    })
    
    
    # Tab 3
    
    output$individual_like <- renderValueBox({
        valueBox(
            ind_like(), "Likes", icon = icon("thumbs-up")
        )
    })
    
    output$individual_love <- renderValueBox({
        valueBox(
            ind_love(), "Loves", icon = icon("heart")
        )
    })
    
    output$individual_laugh <- renderValueBox({
        valueBox(
            ind_laugh(), "Hahas", icon = icon("face-laugh")
        )
    })
    
    output$individual_emphasize <- renderValueBox({
        valueBox(
            ind_emphasize(), "Emphasizes", icon = icon("exclamation")
        )
    })
    
    output$individual_dislike <- renderValueBox({
        valueBox(
            ind_dislike(), "Dislikes", icon = icon("thumbs-down")
        )
    })
    
    output$individual_question <- renderValueBox({
        valueBox(
            ind_question(), "Questions", icon = icon("question")
        )
    })
    
    # Group statistics
    
    output$group_like <- renderValueBox({
        valueBox(
            total_like, "Likes", icon = icon("thumbs-up"), color = "orange"
        )
    })
    
    output$group_love <- renderValueBox({
        valueBox(
            total_love, "Loves", icon = icon("heart"), color = "orange"
        )
    })
    
    output$group_laugh <- renderValueBox({
        valueBox(
            total_laugh, "Hahas", icon = icon("face-laugh"), color = "orange"
        )
    })
    
    output$group_emphasize <- renderValueBox({
        valueBox(
            total_emphasize, "Emphasizes", icon = icon("exclamation"), color = "orange"
        )
    })
    
    output$group_dislike <- renderValueBox({
        valueBox(
            total_dislike, "Dislikes", icon = icon("thumbs-down"), color = "orange"
        )
    })
    
    output$group_question <- renderValueBox({
        valueBox(
            total_question, "Questions", icon = icon("question"), color = "orange"
        )
    })
    
    # Plot of total reactions
    
    output$total_reactions_plot <- renderPlot({
        reactions.df %>% 
            mutate(highlight = ifelse(FromPhoneNumber == input$code_name, "y", "n")) %>% 
            ggplot(aes(x = fct_reorder(FromPhoneNumber, reactions), fill = highlight)) +
            geom_bar(aes(y = reactions), stat = "identity") +
            scale_fill_manual(values = c("y" = "yellow", "n" = "cornflowerblue"), guide = FALSE) +
            geom_text(aes(y = reactions, label = reactions), hjust=0) +
            coord_flip() +
            xlab("Code Names") +
            ylab("Total Reactions") +
            ggtitle("Group Chat Total Reactions in 2022") +
            theme(plot.title = element_text(hjust =0.5))
    })
    
    # Plot of haha
    
    output$total_haha_plot <- renderPlot({
        reactions.df %>% 
            mutate(highlight = ifelse(FromPhoneNumber == input$code_name, "y", "n")) %>% 
            ggplot(aes(x = fct_reorder(FromPhoneNumber, laughs), fill = highlight)) +
            geom_bar(aes(y = laughs), stat = "identity") +
            scale_fill_manual(values = c("y" = "yellow", "n" = "cornflowerblue"), guide = FALSE) +
            geom_text(aes(y = laughs, label = laughs), hjust=0) +
            coord_flip() +
            xlab("Code Names") +
            ylab("Total Haha's") +
            ggtitle("Group Chat Total Laughs in 2022") +
            theme(plot.title = element_text(hjust =0.5))
    })
    
    
    # Tab 5
    
    # Plot of day of the week
    
    output$day_plot <- renderPlot({
        datasetUser() %>% 
            group_by(Day) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            ggplot(aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) +
            geom_bar(aes(y = count), stat = "identity", color = "cornflowerblue", fill = "cornflowerblue") +
            geom_text(aes(y = count, label = count), hjust=0) +
            coord_flip() +
            xlab("Day of the Week") +
            ylab("Texts and Interactions") +
            ggtitle("Texts by Day in 2022") +
            theme(plot.title = element_text(hjust=0.5))
    })
    
    # For the group
    
    output$day_plot_group <- renderPlot({
        datasetTotal() %>% 
            group_by(Day) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            ggplot(aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) +
            geom_bar(aes(y = count), stat = "identity", color = "orange", fill = "orange") +
            geom_text(aes(y = count, label = count), hjust=0) +
            coord_flip() +
            xlab("Day of the Week") +
            ylab("Texts and Interactions") +
            ggtitle("Texts by Day in 2022") +
            theme(plot.title = element_text(hjust=0.5))
    })
    
    # Moving average plot (tab 4)
    output$SMA_plot <- renderPlot({
        datasetUser() %>% 
            group_by(CalendarDate) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            select(CalendarDate, count) %>%
            mutate(CalendarDate = as.Date(CalendarDate)) %>% 
            ggplot(aes(x = CalendarDate, y = count)) +
            geom_line(color = "chocolate", show.legend = TRUE) + 
            xlab("Date") +
            ylab("Texts and Interactions") +
            ggtitle("Texts and Interactions Over Time") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_ma(aes(color = "Weekly Average"), ma_fun = SMA, n = 7, size = 1, show.legend = TRUE) + 
            geom_ma(aes(color = "30 day Average"), ma_fun = SMA, n = 30, size = 1, show.legend = TRUE) + 
            scale_colour_manual(name = 'Legend', 
                                guide = 'legend',
                                values = c('chocolate' = 'chocolate',
                                           'Weekly Average' = 'blue',
                                           '30 day Average' = 'red'), 
                                labels = c('Daily Total',
                                           '7 Day SMA',
                                           '30 Day SMA'))
        
    })
    # for the group
    
    output$SMA_plot_group <- renderPlot({
        datasetTotal() %>% 
            group_by(CalendarDate) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            select(CalendarDate, count) %>%
            mutate(CalendarDate = as.Date(CalendarDate)) %>% 
            ggplot(aes(x = CalendarDate, y = count)) +
            geom_line(color = "chocolate", show.legend = TRUE) + 
            xlab("Date") +
            ylab("Texts and Interactions") +
            ggtitle("Texts and Interactions Over Time") +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_ma(aes(color = "Weekly Average"), ma_fun = SMA, n = 7, size = 1, show.legend = TRUE) + 
            geom_ma(aes(color = "30 day Average"), ma_fun = SMA, n = 30, size = 1, show.legend = TRUE) + 
            scale_colour_manual(name = 'Legend', 
                                guide = 'legend',
                                values = c('chocolate' = 'chocolate',
                                           'Weekly Average' = 'blue',
                                           '30 day Average' = 'red'), 
                                labels = c('Daily Total',
                                           '7 Day SMA',
                                           '30 Day SMA'))
        
    })
    # Time of the day plot
    # I chose bar plots rather than lines as a preference
    
    output$hour_plot <- renderPlot({
        datasetUser() %>% 
            mutate(Hour = as.character(format(as.POSIXct(TextDate), format = "%H"))) %>% 
            group_by(Hour) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            ggplot(aes(x = Hour)) +
            geom_bar(aes(y = count / total_texts()), stat = "identity", color = "cornflowerblue", fill = "cornflowerblue") +
            xlab("Time") +
            ylab("Frequency") +
            ggtitle("Texts and Interactions over Time") +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    # for the group
    
    output$hour_plot_group <- renderPlot({
        datasetTotal() %>% 
            mutate(Hour = as.character(format(as.POSIXct(TextDate), format = "%H"))) %>% 
            group_by(Hour) %>% 
            summarize(count = n(), .groups = "drop") %>% 
            ggplot(aes(x = Hour)) +
            geom_bar(aes(y = count / nrow(chat.df)), stat = "identity", color = "orange", fill = "orange") +
            xlab("Time") +
            ylab("Frequency") +
            ggtitle("Texts and Interactions over Time") +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    
    # Tab 6
    
    output$positive_plot <- renderPlot({
        character_sentiment_summary %>% 
            ggplot(aes(x = fct_reorder(FromPhoneNumber, n_words), y = n_words, fill = sentiment)) +
            geom_bar(stat = "identity") +
            geom_text(aes(y = total_assigned_words, label = positive_ratio), hjust = 0) +
            coord_flip() +
            ylab("Words Recognized") +
            xlab("Code Names") +
            ggtitle("Positive/Negative Sentiment Score") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_fill_manual(values = c("darkred", "darkblue"))
    })
    
    output$character_plot <- renderPlot({
        barplot(colSums(user_sent()),
                las = 2,
                col = rainbow(10),
                xlab = 'Count',
                horiz = T,
                main = 'Sentiment Scores')
    })
    
    
    
    
}
