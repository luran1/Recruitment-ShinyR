#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(keyringr)
library(redcapAPI)
library(REDCapR)
library(lubridate)
library(formattable)
library(broom)
library(DT)


#------------------------------------------------------------------------

credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)


#------------------------------------------------------------------------

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)

# events
events=c("baseline_arm_1","third_trimester_arm_1","two_week_arm_1","two_month_arm_1
","six_month_arm_1","twelve_month_arm_1")

# variables
fields=c("test_id","redcap_event_name",
         "beach_part_drop_out",
         "beachphone_hear_about_us",
         "beach_part_consent",
         "beach_study_complete",
         "beachphone_date",
                # new data pulling, pulling dates of visits and BMI
         "beachphone_age",
         "mom3t_prepreg_bmi",
         "mom2mo_todays_date",
         "mom2wk_todays_date")

records=c("BLS001A","BLS002A","BLS003A","BLS007A","BLS008A","BLS011A",
          "BLS012A","BLS013A","BLS014A","BLS016A","BLS019A","BLS020A",
          "BLS023A","BLS025A","BLS027A","BLS028A","BLS030A","BLS032A",
          "BLS033A","BLS034A","BLS035A","BLS036A","BLS038A","BLS040A",
          "BLS041A","BLS043A","BLS044A","BLS045A","BLS048A","BLS049A",
          "BLS051A","BLS052A","BLS053A","BLS055A","BLS056A","BLS059A",
          "BLS063A","BLS064A","BLS065A","BLS068A","BLS073A","BLS078A",
          "BLS079A","BLS083A","BLS006A")

# Issue with redcap_read(): 
# Error in inherits(ds, "data.frame") : object 'ds' not found
#-------------------------
# pull data
# dat<- redcap_read(
#   batch_size=300,
#   records= dropped.records.v1,
#   redcap_uri = uri, 
#   token      = beach_token, 
#   fields     = desired_fields
#   )$data

dat=redcap_read_oneshot(
    redcap_uri = uri, 
    token      = beach_token,
    fields     = fields,
    events     = events,
    records    = records)

active=dat$data

# factors
active$study_visit=as.factor(active$redcap_event_name)


#------------------------------------------------------------------------

# recode encounter type
act=active%>%
    group_by(test_id)%>%
    mutate(drop_from_study=if(any(beach_part_drop_out == 0)) 0 else NA)%>%
    mutate(consent=recode(beach_part_consent,"1"="Consented",
                          "0"="Not Consented",
                          "NA"="Not Conseted"))%>%
    mutate(Encounter_Type=recode(beachphone_hear_about_us, "1"="Flyer", 
                                 "2"="Radio", 
                                 "3"="Social Media", 
                                 "4"="Newspaper", 
                                 "5"="word-of-mouth", 
                                 "6"="other"))%>%
    mutate(Encounter_Date=beachphone_date)%>%
    mutate(study_visit = factor(study_visit, levels = 
                                    c("baseline_arm_1",
                                      "third_trimester_arm_1",
                                      "two_week_arm_1",
                                      "two_month_arm_1",
                                      "six_month_arm_1",
                                      "twelve_month_arm_1")))%>%
    select(-c(redcap_repeat_instrument,
              redcap_event_name,
              redcap_repeat_instance)) %>%
    select(test_id,study_visit, everything())%>%
    arrange(test_id, study_visit)


#------------------------------------------------------------------------
Age <- act%>%
    filter(!is.na(beachphone_age))%>%
    distinct(test_id, .keep_all = TRUE)

BMI <- act%>%
    filter(!is.na(mom3t_prepreg_bmi))

#------------------------------------------------------------------------



# Define UI for application that draws a histogram
# UI is where the visual elements of the application are placed
ui <- fluidPage(
    
    # Application title
    titlePanel("Recruitment overview"),
    
    # Sidebar with a slider input for number of bins 
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("quarter","Quarter",value = c(1,2),min=1,max=4),
            sliderInput("year","Years",value = c(2017,2018),min=2017,max=2019),
            checkboxGroupInput("encounter","Encounter Type",choices = c("Social Media","Flyer","Radio", "Newspaper","word-of-mouth","other"),selected ="Social Media"),
            sliderInput("BMI","BMI range",value = c(25,35),min=min(BMI$mom3t_prepreg_bmi),max=max(BMI$mom3t_prepreg_bmi)),
            sliderInput("Age","Age range",value = c(30,40),min=min(Age$beachphone_age),max=max(Age$beachphone_age))
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Recruitment By Time",plotOutput("distPlot"), tableOutput("recruitmentType")),
                tabPanel("Encounter Recruitment",plotOutput("encounterRecruitment")),
                tabPanel("BMI by Recruitment",plotOutput("BMIPlot")),
                tabPanel("Age by recruitment",plotOutput("AgePlot"))
            )
        )
    )
)


#------------------------------------------------------------------------


# Define server logic required to draw a histogram
# where logic of app is implemented 
server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        
        Encounter_per_time <- act%>%
            select(Encounter_Date,Encounter_Type)
        
        Encounter_per_time$Encounter_Date <- as.Date(Encounter_per_time$Encounter_Date, format= "%Y-%m-%d")
        
        my_data <- data.frame(Encounter_Date = seq(as.Date("2017-01-01"), as.Date("2019-12-31"), 
                                                       by = "1 month"), format= "%Y-%m-%d")
        
        my_complete <- full_join(my_data, Encounter_per_time, by = "Encounter_Date")
        
        Encounter_per_time <- my_complete%>%
            separate(Encounter_Date,c("y","m","d"))%>%
            mutate(Q = case_when(
                m == "01" | m == "02" | m == "03" ~ "1",
                m == "04" | m == "05" | m == "06" ~ "2",
                m == "07" | m == "08" | m == "09" ~ "3",
                m == "10" | m == "11" | m == "12" ~ "4",
                TRUE~"5"))%>%
            group_by(Encounter_Type,y,Q)%>%
            #does not work properly due to slider providing min and max values rather than all values within range
            filter(y %in% (input$year[1]:input$year[2]) & Q %in% (input$quarter[1]:input$quarter[2]))%>%
            summarize(count=n())%>%
            unite("YQ",c("y",Q),sep ="-")
        
        
        theme_set(theme_classic())
        # distribution of visits each month(histogram)
        h <- ggplot(Encounter_per_time, aes(YQ, count)) + scale_fill_brewer(palette = "Spectral")
        h + geom_histogram(aes(fill=factor(Encounter_Type)), stat = "Identity",
                           bins=24,
                           col="black", 
                           size=.1) + # change number of bins
            # geom_density() + #this function is meant to draw a trend line for the graph
            # stat_bin(aes(y=count,label=count),geom="text",vjust=-.5) +   # this will display the total count of each bin
            labs(title="How participants heard of our study monthly", 
                 subtitle="from july of 2017-January 2019",
                 x="Date(Year-Month)",
                 y="Count",
                 fill="How did you learn about the study") +
            theme(axis.text.x = element_text(angle=70, vjust =.6))
    })
    
    output$recruitmentType <- renderTable({
        recruit <- act%>%
            select(test_id, Encounter_Type, beach_part_drop_out)%>%
            distinct(test_id, .keep_all = TRUE)%>%
            group_by(Encounter_Type, beach_part_drop_out)%>%
            summarize(count=n())%>%
            rename("Encoutner method"=Encounter_Type,"Drop participant count"=beach_part_drop_out)
        
        formattable(recruit, align = c("l","c","r"))
    })
    
    output$encounterRecruitment <- renderPlot({
#------------------works for BEACH data----------------------        
        
        Encounter_per_time <- act%>%
            select(Encounter_Date,Encounter_Type)%>%
            distinct(test_id, .keep_all = TRUE)
#Should compare months and only add those that are not within the already made set.
#this can be done by using %in% with the Dates. must learn how to select and compare months.        
        my_data <- data.frame(Encounter_Date = seq(as.Date("2017-01-01"), as.Date("2019-12-31"), 
                                                       by = "1 month"), format= "%Y-%m-%d")
        
        my_complete <- full_join(my_data, Encounter_per_time, by = "Encounter_Date")
        
        Encounter_per_time <- my_complete%>%
            separate(Encounter_Date,c("y","m","d"))%>%
            mutate(Quarter = case_when(
                m == "01" | m == "02" | m == "03" ~ "Q1",
                m == "04" | m == "05" | m == "06" ~ "Q2",
                m == "07" | m == "08" | m == "09" ~ "Q3",
                m == "10" | m == "11" | m == "12" ~ "Q4",
                TRUE~"Q5"))%>%
            group_by(Encounter_Type,y,Quarter)%>%
            filter(Encounter_Type %in% input$encounter)%>%
            summarize(count=n())%>%
            unite("YQ",c("y",Quarter),sep ="-")
        
        
        theme_set(theme_classic())
        # distribution of visits each month(histogram)
        h <- ggplot(Encounter_per_time, aes(YQ, count)) + scale_fill_brewer(palette = "Spectral")
        h + geom_histogram(aes(fill=factor(Encounter_Type)), stat = "Identity",
                           bins=24,
                           col="black", 
                           size=.1) + # change number of bins
            # geom_density() + #this function is meant to draw a trend line for the graph
            # stat_bin(aes(y=count,label=count),geom="text",vjust=-.5) +   # this will display the total count of each bin
            labs(title="How participants heard of our study monthly", 
                 subtitle="from july of 2017-January 2019",
                 x="Date(Year-Month)",
                 y="Count",
                 fill="How did you learn about the study") +
            theme(axis.text.x = element_text(angle=70, vjust =.6))
    })
    
    output$BMIPlot <- renderPlot({
#==================Works with BEACH DATA=====================
        
        
        BMI <- act%>%
            select(test_id,mom3t_prepreg_bmi,Encounter_Type)%>%
            filter(!is.na(mom3t_prepreg_bmi))%>%
            filter(!is.na(Encounter_Type))
        
        
        
        #create a marginal Histogram / Boxplot for maternal ages by encounter type:
        BMI$Encounter_Type <- as.factor(BMI$Encounter_Type)
        
        library(ggplot2)
        theme_set(theme_classic())
        
        # Plot
        means <- aggregate(mom3t_prepreg_bmi ~  Encounter_Type, BMI, mean)
        
        
        g <- ggplot(BMI, aes(Encounter_Type, mom3t_prepreg_bmi))
        g + geom_boxplot(varwidth=T, fill="plum") + 
            labs(title="BMI of participants", 
                 subtitle="Divided into how they heard of the study",
                 caption="Source: BEACH Interview",
                 x="How did you hear of our study",
                 y="BMI")+
            geom_text(data = means, aes(label = mom3t_prepreg_bmi, y = mom3t_prepreg_bmi + 0.08))
    })
    
    output$AgePlot <- renderPlot({
#=====================Works with BEACH Data======================        
        
        
        Mat_age <- act%>%
            select(test_id,beachphone_age,Encounter_Type)%>%
            filter(!is.na(beachphone_age))%>%
            filter(!is.na(Encounter_Type))
        
        
        #create a marginal Histogram / Boxplot for maternal ages by encounter type:
        Mat_age$Encounter_Type <- as.factor(Mat_age$Encounter_Type)
        
        library(ggplot2)
        theme_set(theme_classic())
        
        # Plot
        means <- aggregate(beachphone_age ~  Encounter_Type, Mat_age, mean)
        
        g <- ggplot(Mat_age, aes(Encounter_Type, beachphone_age))
        g + geom_boxplot(varwidth=T, fill="plum") + 
            labs(title="Age of Participants", 
                 subtitle="Divided into how they heard of the study",
                 caption="Source: BEACH Interview",
                 x="How did you hear of our study",
                 y="Age(years)")+
            geom_text(data = means, aes(label = beachphone_age, y = beachphone_age + 6))
    })
    
    #change to DT table rather than formatabe 
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
