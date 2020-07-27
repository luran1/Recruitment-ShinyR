#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)

#loading and cleaning data
#------------------------------------------------------
#load in the data from github
netflix<- read.csv("C:\\Users\\Luran\\Documents\\GitHub\\Recruitment-ShinyR\\Kaggle\\netflix_titles.csv")
head(netflix)

#correct the dates from factored to date class elements.
#You can see the difference in the classes with the function class(). 

class(netflix$date_added)
netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")
class(netflix$date_added)


# this section of code is used the get the different countries.
# the code section selects the country column and seperates the lists of countries into their own row.

countries <- netflix%>%
    select(country)%>%          # selects the column.
    filter(country!="")%>%      # removes any rows that are empty by checking if there is anything writen in the row.
    separate_rows(country, sep = ",")       #seperates the countrties into their own row.

# this section of code removes the write space left over at the beinging of some counties.
# the space is caused by the seperating.

countries$country <- trimws(countries$country, which = "left")

# here we trim the countires to be only unique entries. 
# the function top_n(x) limits the number of countires in the set to the top x countries.

countries <- countries%>%
    distinct(country)%>%
    top_n(10, country)


#------------------------------------------------------
#creating the UI
# Define UI for application that draws a histogram



ui <- fluidPage(

    # Application title
    titlePanel("netflix review"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #defines the different types of inputs.
            checkboxGroupInput("movTV",
                               "Medium(TV or Movie)",
                               choices = c("Movie","TV Show"),
                               selected = "Movie"),
            
            checkboxGroupInput("actDir",
                               "Actors and Directors",
                               choices = c("cast","director"),
                               selected = "cast"),
            
            sliderInput("numDis",
                        "how many people would you like to show?",
                        min = 1,
                        max = 10,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("countries"),
           plotOutput("genre"),
           plotOutput("topActDir")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$genre <- renderPlot({
        #removes the white space btw. the listed genres.
        netflix$listed_in=trimws(netflix$listed_in,which='left')
        
        #seperate the genres and summarize their counts. 
        top_genres=netflix %>% 
            mutate(genre=strsplit(listed_in,',')) %>%   #seperate the genres in nested formate.
            unnest(genre) %>%       #unnest the new genre column, meaning that new duplciate rows are created.
            group_by(type,genre) %>%        #group by type and genre.
            filter(type %in% input$movTV) %>%       #filter to specify type, movie or show
            summarise(count=n()) %>%        #summarize the count of genre and types.
            unique() %>%        #count only unique operations since some objects might 
            arrange(desc(count)) %>%
            top_n(input$numDis,count)
        
        top_genres %>% ggplot(aes(x= fct_reorder(genre,count,.desc = T),y=count,fill=type))+
            geom_col()+
            scale_y_continuous(limits =c(0,1850),breaks =seq(0,1850,400))+
            labs(title='Top Genres - Movies Vs Tv Shows',x='Genres') +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$topActDir <- renderPlot({
        #organizing a new dataframe for actors and directors 
        Actors=netflix %>%
            select(input$actDir)%>%
            gather(key="cast_dir",value="person",input$actDir)%>%      #turning the cast and director columns into a key value system
            filter(person!="")%>%       #remove the missing values that are empty.
            separate_rows(person, sep=",")      #seperate the list of names into their own row.
        
        Actors$actor=trimws(Actors$person, which = "left")      #remove the white space from the right side of the names, a result of the seperate_rows.
        
        Actor_freq=Actors %>%
            group_by(cast_dir,person)%>%        #defining the grouping variables that will be summarized 
            summarise(count=n())%>%         #summarize the number of unique pairs 
            arrange(desc(count))%>%         #arrange the summarized table in decending order(highest to lowest)
            top_n(input$numDis,count)         #limit the number of observations to 10
        
        #creating the figure
        Actor_freq %>% ggplot(aes(x=fct_reorder(person,count,.desc = T),y=count,fill=cast_dir))+
            geom_col()+
            facet_wrap(~cast_dir,scales = 'free_x')+
            scale_x_discrete()+
            labs(title='Top Actors & Directors',x='Actors & Directors')+
            theme(axis.text.x = element_text(angle = 90))
        
    })

    output$countries <- renderPlot({
        
        # filters the larger data set to the information that is relative to the function.
        
        uniqueEntries <- netflix %>%
            select(type,country)%>%
            separate_rows(country, sep = ",")
        uniqueEntries$country <- trimws(uniqueEntries$country, which = "left")
        uniqueEntries <- uniqueEntries%>%
            filter(country!="")%>%
            filter(type %in% input$movTV)%>%
            group_by(type, country) %>%
            summarise(count=n())%>%
            arrange(desc(count))%>%
            top_n(input$numDis,count)
            

        uniqueEntries %>%
            ggplot(aes(x=fct_reorder(country,count,.desc = T), y=count,fill=type))+
            geom_col()+
            facet_wrap(~type,scales = 'free_x')+
            theme(axis.text.x = element_text(angle = 90))+
            scale_x_discrete()+
            labs(title='Top Countries for Movies Vs Tv',x='Country')+
            theme(axis.text.x = element_text(angle = 90))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
