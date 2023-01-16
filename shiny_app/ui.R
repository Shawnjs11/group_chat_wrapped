
# Following packages were used:
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


# User Interface of the app
# This is a dashboard with a sidebar to access individual tabs that 
# show different information about a user and compared to the group

ui <- dashboardPage(
    dashboardHeader(
        title = "XXXX Wrapped" # XXXX for any year given

    ),
    # 5 main tabs of information, a title tab, and an author tab for a total of 7
    # You can choose to 
    dashboardSidebar(
        sidebarMenu(
            id = "selected_tab",
            menuItem("Welcome", tabName = "tab_welcome", icon = icon("hand")),
            menuItem("Message Statistics", tabName = "tab_dashboard", icon = icon("comments")),
            menuItem("Most Common", tabName = "tab_most_common", icon = icon("cloud")),
            menuItem("Over Time", tabName = "tab_time", icon = icon("clock")),
            menuItem("Your Schedule", tabName = "tab_schedule", icon = icon("calendar")),
            menuItem("Text Analysis", tabName = "tab_text_analysis", icon = icon("keyboard")),
            menuItem("More Information", tabName = "tab_more_info", icon = icon("circle-info"))
            
        )
        
    ),
    dashboardBody(
        tabItems(
            # First tab item, the welcome page
            tabItem(
                class = "text-center",
                tabName = "tab_welcome",
                h2("XXXX Group Chat Wrapped Up!"),
                HTML("<br> <br>"),
                p(class = "text-left",
                  "Hi! Welcome to your XXXX groupchat recap. In this dashboard, you will find
                     information regarding your overall message summary statistics, your habits, most common words/actions,
                     your activity and behavior over time, a bit of sentiment analysis, and much more!"),
                HTML("<br> <br>"),
                box(
                    p(class = "text-left",
                      "A few things to note:"),
                    p(class = "text-left",
                      "1. This data is static, and pulled from creator's message database"),
                    p(class = "text-left",
                      # I chose to use fake names, obviously you may chose not to
                      "2. Code names/fake names are used for security reasons"),
                    p(class = "text-left",
                      "3. Those with less than 100 messages were excluded from the data"),
                    p(class = "text-left",
                      "4. Interactions are reactions to messages, such as a like or emphasize"),
                    p(class = "text-left",
                      # A note to my friends for next year!
                      "5. More features may be added in this version, or for the next wrapped up")
                ),
                box(
                    p(class = "text-left",
                      "How to use this app/dashboard:"),
                    p(class = "text-left",
                      # This would not pertain for those that do not use code names
                      "First, request code name/fake name from owner if not yet received"),
                    p(class = "text-left",
                      "Next, select the name from the dropdown menu below"),
                    p(class = "text-left",
                      "Finally, use the menu bar on the left to explore your personalized results compared to the chat!")
                ),
                # This tab panel can just be actual names
                tabPanel("code_name", fluid = TRUE,
                         selectInput("code_name","Code Name",
                                     unique(sort(chat.df$FromPhoneNumber)), multiple = FALSE)
                )
            ),
            # Second tab, the overall texting statistics
            tabItem(
                class = "text-center",
                tabName = "tab_dashboard",
                h1("Overall Texting Statistics"),
                HTML("<br> <br>"),
                h2(class = "text-center",
                   "Your Individual Statistics"),
                fluidRow(
                    # These value boxes add summarized information in a professional way
                    valueBoxOutput("individual_texts"),
                    valueBoxOutput("individual_words"),
                    valueBoxOutput("individual_reactions")
                ),
                HTML("<br> <br>"),
                # Refers to total number of words used by an individual
                htmlOutput("word_comparison"),
                HTML("<br> <br>"),
                h2("Overall Group Chat Statistics"),
                HTML("<br> <br>"),
                fluidRow(
                    # Totals for a group
                    valueBoxOutput("group_texts"),
                    valueBoxOutput("group_words"),
                    valueBoxOutput("group_reactions")
                ),
                HTML("<br> <br>"),
                # Refers to ggplots of barplots for total texts/words by individual
                plotOutput("total_texts_plot"),
                HTML("<br> <br>"),
                plotOutput("total_words_plot")
            ),
            
            # Third Tab, most common category
            tabItem(
                class = "text-center",
                tabName = "tab_most_common",
                h1("Individual Statistics"),
                HTML("<br> <br>"),
                fluidRow(
                    valueBoxOutput("individual_like"),
                    valueBoxOutput("individual_laugh"),
                    valueBoxOutput("individual_love"),
                    valueBoxOutput("individual_emphasize"),
                    valueBoxOutput("individual_dislike"),
                    valueBoxOutput("individual_question"),
                ),
                h2(class = "text-center",
                   "Word Cloud"),
                HTML("<br> <br>"),
                plotOutput("individual_word_cloud"),
                HTML("<br> <br>"),
                h2("Group Chat Statistics"),
                HTML("<br> <br>"),
                fluidRow(
                    valueBoxOutput("group_like"),
                    valueBoxOutput("group_laugh"),
                    valueBoxOutput("group_love"),
                    valueBoxOutput("group_emphasize"),
                    valueBoxOutput("group_dislike"),
                    valueBoxOutput("group_question"),
                ),
                HTML("<br> <br>"),
                plotOutput("total_reactions_plot"),
                plotOutput("total_haha_plot")
                
                
            ),
            
            # Tab 4, time information
            tabItem(
                class = "text-center",
                tabName = "tab_time",
                h1("Over Time"),
                HTML("<br> <br>"),
                plotOutput("SMA_plot"),
                HTML("<br> <br>"),
                h2("Group Chat Statistics"),
                HTML("<br> <br>"),
                plotOutput("SMA_plot_group")
                
            ),
            
            # Tab 5, schedule information
            tabItem(
                class = "text-center",
                tabName = "tab_schedule",
                h1("When are you free?"),
                HTML("<br> <br>"),
                plotOutput("day_plot"),
                HTML("<br> <br>"),
                plotOutput("hour_plot"),
                HTML("<br> <br>"),
                h2("Group Chat Statistics"),
                HTML("<br> <br>"),
                plotOutput("day_plot_group"),
                HTML("<br> <br>"),
                plotOutput("hour_plot_group"),
            ),
            
            # Tab 6, simple text analysis
            
            tabItem(
                class = "text-center",
                tabName = "tab_text_analysis",
                h1("Text Analysis"),
                HTML("<br> <br>"),
                plotOutput("positive_plot"),
                HTML("<br> <br>"),
                plotOutput("character_plot")
            ),
            
            tabItem(
                class = "text-center",
                tabName = "tab_more_info",
                h1("More Info"),
                HTML("<br> <br>"),
                p(class = "text-left",
                  # If template is copied, a credit would be much appreciated!
                  # Otherwise, anything else can go in this tab, such as additional notes
                  "This app was created by...")
            )
        )
    )
)

