#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)    #Strings operations
library(data.table) #Transposing dataframe
library(ggplot2)    #Visualisation
library(dplyr)      #Dataframe operations
library(caret)      #Models
library(quanteda)   #Tokenizing text into words
#library(rpart)      #Decision Tree
library(janitor)    #Validating colnames of dataframe
library(partykit)   #Visualizing Decision Tree

# Define UI for application that draws a histogram
ui <- navbarPage("Book Recommendation System",
                 tabPanel("Best Rated Authors",fluidPage(
                     
                     titlePanel("Book Recommendation System"),
                     fluidRow(
                         column(8, offset = 1, 
                                sidebarPanel(
                                    sliderInput("BooksCount",
                                                "Number of Books for each author:",
                                                min = 0,
                                                max = 50,
                                                value = 5)
                                )
                         ),
                         column(3,
                                radioButtons("radio", h3("Ratings"),
                                             choices = list("Ratings more then  4" = 4,
                                                            "Ratings more then  4.2" = 4.2,
                                                            "Ratings more then  4.4" = 4.4,
                                                            "Ratings more then 4.5 " = 4.5),selected = 4.4))
                     ),
                     
                     
                     mainPanel(
                         fluidRow(
                             column(11,offset = 1,
                                    tabsetPanel(
                                        tabPanel("Plot", plotOutput("distPlot")), 
                                        tabPanel("Table", tableOutput("table"))
                                    )
                             )
                         )
                     ))),
                 tabPanel("Language based Analysis",fluidPage(
                     
                     titlePanel("Book"),
                     fluidRow(
                         column(8, offset = 1, 
                                sidebarPanel(
                                    sliderInput("BooksCount1",
                                                "Number of Books for each author:",
                                                min = 5,
                                                max = 15,
                                                value = 6)
                                )
                         ),
                         column(3,
                                radioButtons("radio1", h3("Ratings"),
                                             choices = list("Ratings more then  3 " = 3,
                                                            "Ratings more then  3.5 " = 3.5,
                                                            "Ratings more then  4 " = 4),selected = 3))
                     ),
                     
                     
                     mainPanel(
                         fluidRow(
                             column(12,offset = 0,
                                    tabsetPanel(
                                        tabPanel("Plot", plotOutput("distPlot2")), 
                                        tabPanel("Table", tableOutput("table2"))
                                    )
                             )
                         )
                     )
                     )),
                 tabPanel("Best Publishers",fluidPage(
                     
                     titlePanel("Book"),
                     fluidRow(
                         column(8, offset = 1, 
                                sidebarPanel(
                                    sliderInput("BooksCount2",
                                                "Number of Books for each author:",
                                                min = 0,
                                                max = 150,
                                                value = 15)
                                )
                         ),
                         column(3,
                                selectInput("select",  h3("Ratings"),
                                            choices = list("Ratings more then  3 " = 3,
                                                           "Ratings more then  3.5 " = 3.5,
                                                           "Ratings more then  4 " = 4,
                                                           "Ratings more then  4.5 " = 4.5),selected = 4))
                     ),
                     
                     mainPanel(
                         fluidRow(
                             column(12,offset = 0,
                                    tabsetPanel(
                                        tabPanel("Plot", plotOutput("distPlot3")), 
                                        tabPanel("Table", tableOutput("table3"))
                                    )
                             )
                         )
                     )
                 )),
                 
                 tabPanel("Best",mainPanel(
                                tabsetPanel(
                                    tabPanel("Plot", plotOutput("distPlot4")) 
                                    
                                )
                        
                 )
                 ),
                 tabPanel("Component 5")
                            
                 
    )

    



# Define server logic required to draw a histogram
server <- function(input, output) {

    
    df = read.csv('../books.csv')
    df$average_rating <- as.numeric(levels(df$average_rating))[df$average_rating]
    colnames(df)[8] <- 'Num_pages'
    df$Num_pages <- as.numeric(levels(df$Num_pages))[df$Num_pages]
    df$title <- as.character(levels(df$title))[df$title]
    
    #Ensuring that num_page is above 0
    colnames(df)[8] <- 'Num_pages'
    df <- filter(df, df$Num_pages>0)
    
    # 4 records have a value 'NOT A BOOK' in a feature author, so we'll set it to NA
    which(df$authors=='NOT A BOOK')
    df$authors <- as.character(df$authors)
    df <- dplyr:::filter(df, df$authors!='NOT A BOOK')
    
    
    #Ensuring that 'average_rating' consist of digits
    df<- dplyr:::filter(df, str_detect(df$average_rating, '^[0-9].*[0-9]$'))
    
    sapply(df, function(x) length(which(is.na(x))))
    df <- na.omit(df)
    
    output$distPlot <- renderPlot({
        
        
        d <- df %>%
            group_by(authors) %>%
            summarize(avg=mean(average_rating), books = n()) %>%
            arrange(desc(avg)) %>%
            filter(avg > input$radio , books > input$BooksCount)
        
        theme_set(theme_bw())
        
        ggplot(d, aes(x=`authors`, y=books, label=books)) + 
            geom_point(stat='identity', fill="black", size=20)  +
            geom_segment(aes(y = 0, 
                             x = `authors`, 
                             yend = books, 
                             xend = `authors`), 
                         color = "black") +
            geom_text(color="white", size=5) +
            labs(title="Find Best Rated Authors", 
                 subtitle="Average Rating more then 4.5 for  ") + 
            ylim(0,50) +
            coord_flip()+
            theme(axis.text=element_text(size=14), 
              plot.title = element_text(size = 25, face = "bold", hjust=0.5),
              axis.title=element_text(size=17),
              plot.subtitle = element_text(size = 20, face = "bold", hjust=0.48), 
              legend.title=element_text(size=17), 
              legend.text=element_text(size=13)
        )
        
        
       
    })
    output$table <- renderTable({
        m <- df %>%
            group_by(authors) %>%
            summarize(avg=mean(average_rating), books = n()) %>%
            arrange(desc(avg)) %>%
            filter(avg > input$radio , books > input$BooksCount)
        
        head(m)
        })
    
    output$table2 <- renderTable({
        n <- df %>%
            group_by(language_code) %>%
            summarize(avg=mean(average_rating), lan=n()) %>%
            arrange(desc(avg)) %>%
            filter(lan>input$BooksCount1,avg>input$radio1)
        head(n)
        })
    
    output$distPlot2 <- renderPlot({
        
        m <- df %>%
            group_by(language_code) %>%
            summarize(avg=mean(average_rating), lan=n()) %>%
            arrange(desc(avg)) %>%
            filter(lan>input$BooksCount1,avg>input$radio1)
        
        #m$language_code <- c(  'Chinese',  'Japanese','multiple languages', 'Greece','Eng-Canadian', 'French', 'German','Portuguese','Eng-other','Spanish','Eng-British', 'Eng-American')
        options(repr.plot.width = 18 , repr.plot.height = 10)
        ggplot(m)+
            theme_classic()+
            geom_bar(aes(x=factor(m$language_code, levels= as.character(m$language_code)),
                         y=lan, fill=avg), stat='identity')+
            geom_hline(yintercept=c(10, 100, 1000, 10000), linetype='dashed', alpha=0.1)+
            #coord_flip()+
            scale_y_log10()+
            scale_fill_gradient(low='gray', high='black')+
            labs(y='Number of books in a given language', 
                 x='Languages', 
                 title='Number of books in each language with averaged ratings', 
                 subtitle='Only languages with more than 5 books', fill='Averaged rating')+
            theme(axis.text=element_text(size=14), 
                  plot.title = element_text(size = 40, face = "bold", hjust=0.5),
                  axis.title=element_text(size=17),
                  plot.subtitle = element_text(size = 20, face = "bold", hjust=0.48), 
                  legend.title=element_text(size=17), legend.text=element_text(size=13))
    })
    
    
    output$distPlot3 <- renderPlot({
        
        
        m <- df %>%
            group_by(publisher) %>%
            summarize(avg=mean(average_rating), lan=n()) %>%
            arrange(desc(lan)) %>%
            #filter(language_code == 'en-US')
            filter(lan>input$BooksCount2,avg>input$select)
    
        options(repr.plot.width = 18 , repr.plot.height = 10)
        ggplot(m)+
            theme_classic()+
            geom_bar(aes(x=factor(m$publisher, levels= as.character(m$publisher)),
                         y=lan, fill=avg), stat='identity')+
            geom_hline(yintercept=c(10, 100, 1000, 10000), linetype='dashed', alpha=0.1)+
            coord_flip()+
            scale_y_log10()+
            scale_fill_gradient(low='gray', high='black')+
            labs(y='Number of books in a given language', x='Languages', title='Number of books in each language with averaged ratings', 
                 subtitle='Only languages with more than 5 books', fill='Averaged rating')+
            theme(axis.text=element_text(size=14), plot.title = element_text(size = 40, face = "bold", hjust=0.5),axis.title=element_text(size=17),
                  plot.subtitle = element_text(size = 20, face = "bold", hjust=0.48), legend.title=element_text(size=17), legend.text=element_text(size=13))
    })
    
    output$table3 <- renderTable({
        m <- df %>%
            group_by(publisher) %>%
            summarize(avg=mean(average_rating), lan=n()) %>%
            arrange(desc(avg)) %>%
            #filter(language_code == 'en-US')
            filter(lan>input$BooksCount2,avg>input$select)
        
        head(m)
    })
    
    output$distPlot4 <- renderPlot({
    
    options(repr.plot.width = 18 , repr.plot.height = 13)
    ggplot(df, aes(text_reviews_count, average_rating))+
        geom_jitter()+
        coord_cartesian(ylim=c(2, 5))+
        scale_x_sqrt()+
        geom_smooth()+
        labs(x='Count of text reviews', y='Average rating', title='Checking for correlation', 
             subtitle='between count of text reviews and average rating')+
        theme(axis.text=element_text(size=18), plot.title = element_text(size = 40, face = "bold", hjust=0.5),axis.title=element_text(size=30),
              plot.subtitle = element_text(size = 30, face = "bold", hjust=0.48))
    
    
    ggplot(df, aes(ratings_count, average_rating))+
        geom_jitter()+
        coord_cartesian(ylim=c(2, 5), xlim=c(0, 1000000))+
        scale_x_sqrt()+
        geom_smooth()+
        labs(x='Count of text reviews', y='Average rating', title='Checking for correlation', 
             subtitle='between count of ratings and average rating')+
        theme(axis.text=element_text(size=18), plot.title = element_text(size = 40, face = "bold", hjust=0.5),axis.title=element_text(size=30),
              plot.subtitle = element_text(size = 30, face = "bold", hjust=0.48))+
        geom_text(aes(label=paste('p-value: ', round(cor.test(df$ratings_count, df$average_rating, method='pearson')$p.value, 7), '\n',
                                  'correlation: ', round(cor.test(df$ratings_count, df$average_rating, method='pearson')$estimate, 3)), x=750000, y=4.75), size=10)
    
    #Books having more than 100000 ratings count
    cat('Books having more than 100000 ratings count:', round(length(which(df$ratings_count>100000))/length(df$ratings_count), 3), '\n')
    cat('They account for: ', round(sum(df[df$ratings_count>100000, 'ratings_count'])/sum(df$ratings_count), 3), 'of all ratings')
    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
