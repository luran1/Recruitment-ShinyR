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
          "BLS079A","BLS083A")

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
Age <- ds_some_rows_v1%>%
    filter(!is.na(analysis_mat_age))

BMI <- ds_some_rows_v1%>%
    filter(!is.na(analysis_bmi))

#------------------------------------------------------------------------



# Define UI for application that draws a histogram
# UI is where the visual elements of the application are placed
ui <- fluidPage(
    
    # Application title
    titlePanel("Recruitment overview"),
    
    # Sidebar with a slider input for number of bins 
    
    sidebarLayout(
        sidebarPanel(
            radioButtons("Rtype","Recruitment type",choices = c("Facebook","Flyer","Other")),
            sliderInput("quarter","Quarter",value = c(1,2),min=1,max=4),
            sliderInput("year","Years",value = c(2017,2018),min=2017,max=2019),
            checkboxGroupInput("encounter","Encounter Type",choices = c("Facebook","Flyer","Other"),selected ="Facebook"),
            sliderInput("BMI","BMI range",value = c(25,35),min=min(BMI$analysis_bmi),max=max(BMI$analysis_bmi)),
            sliderInput("Age","Age range",value = c(30,40),min=min(Age$analysis_mat_age),max=max(Age$analysis_mat_age))
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Recruitment By Time",plotOutput("distPlot")),
                tabPanel("Encounter Recruitment",plotOutput("encounterRecruitment")),
                tabPanel("BMI by Recruitment",plotOutput("BMIPlot"),tableOutput("BMITable")),
                tabPanel("Age by recruitment",plotOutput("AgePlot"),tableOutput("AgeTable"))
            )
        )
    )
)


#------------------------------------------------------------------------


# Define server logic required to draw a histogram
# where logic of app is implemented 
server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        
        Encounter_per_time <- ds_some_rows_v1%>%
            select(encounter_date_int,learn_about_study)
        
        Encounter_per_time$encounter_date_int <- as.Date(Encounter_per_time$encounter_date_int, format= "%Y-%m-%d")
        
        my_data <- data.frame(encounter_date_int = seq(as.Date("2017-01-01"), as.Date("2019-12-31"), 
                                                       by = "1 month"), format= "%Y-%m-%d")
        
        my_complete <- full_join(my_data, Encounter_per_time, by = "encounter_date_int")
        
        Encounter_per_time <- my_complete%>%
            separate(encounter_date_int,c("y","m","d"))%>%
            mutate(Q = case_when(
                m == "01" | m == "02" | m == "03" ~ "1",
                m == "04" | m == "05" | m == "06" ~ "2",
                m == "07" | m == "08" | m == "09" ~ "3",
                m == "10" | m == "11" | m == "12" ~ "4",
                TRUE~"5"))%>%
            group_by(learn_about_study,y,Q)%>%
            #does not work properly due to slider providing min and max values rather than all values within range
            filter(y %in% (input$year[1]:input$year[2]) & Q %in% (input$quarter[1]:input$quarter[2]))%>%
            summarize(count=n())%>%
            unite("YQ",c("y",Q),sep ="-")
        
        
        theme_set(theme_classic())
        # distribution of visits each month(histogram)
        h <- ggplot(Encounter_per_time, aes(YQ, count)) + scale_fill_brewer(palette = "Spectral")
        h + geom_histogram(aes(fill=factor(learn_about_study)), stat = "Identity",
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
    
    output$encounterRecruitment <- renderPlot({
        Encounter_per_time <- ds_some_rows_v1%>%
            select(encounter_date_int,learn_about_study)
        
        Encounter_per_time$encounter_date_int <- as.Date(Encounter_per_time$encounter_date_int, format= "%Y-%m-%d")
        
        my_data <- data.frame(encounter_date_int = seq(as.Date("2017-01-01"), as.Date("2019-12-31"), 
                                                       by = "1 month"), format= "%Y-%m-%d")
        
        my_complete <- full_join(my_data, Encounter_per_time, by = "encounter_date_int")
        
        Encounter_per_time <- my_complete%>%
            separate(encounter_date_int,c("y","m","d"))%>%
            mutate(Quarter = case_when(
                m == "01" | m == "02" | m == "03" ~ "Q1",
                m == "04" | m == "05" | m == "06" ~ "Q2",
                m == "07" | m == "08" | m == "09" ~ "Q3",
                m == "10" | m == "11" | m == "12" ~ "Q4",
                TRUE~"Q5"))%>%
            group_by(learn_about_study,y,Quarter)%>%
            filter(learn_about_study %in% input$encounter)%>%
            summarize(count=n())%>%
            unite("YQ",c("y",Quarter),sep ="-")
        
        
        theme_set(theme_classic())
        # distribution of visits each month(histogram)
        h <- ggplot(Encounter_per_time, aes(YQ, count)) + scale_fill_brewer(palette = "Spectral")
        h + geom_histogram(aes(fill=factor(learn_about_study)), stat = "Identity",
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
        BMI <- ds_some_rows_v1%>%
            select(record_id,analysis_bmi,learn_about_study)%>%
            filter(!is.na(analysis_bmi))%>%
            filter(!is.na(learn_about_study))
        
        
        
        #create a marginal Histogram / Boxplot for maternal ages by encounter type:
        BMI$learn_about_study <- as.factor(BMI$learn_about_study)
        
        library(ggplot2)
        theme_set(theme_classic())
        
        # Plot
        means <- aggregate(analysis_bmi ~  learn_about_study, BMI, mean)
        
        
        g <- ggplot(BMI, aes(learn_about_study, analysis_bmi))
        g + geom_boxplot(varwidth=T, fill="plum") + 
            labs(title="BMI of participants", 
                 subtitle="Divided into how they heard of the study",
                 caption="Source: BEACH Interview",
                 x="How did you hear of our study",
                 y="BMI")+
            geom_text(data = means, aes(label = analysis_bmi, y = analysis_bmi + 0.08))
    })
    
    output$BMITable <- renderTable({
        BMI <- ds_some_rows_v1%>%
            select(analysis_bmi,record_id,learn_about_study)%>%
            filter(between(analysis_bmi,input$BMI[1],input$BMI[2]))%>%
            group_by(learn_about_study)%>%
            summarise(count=n())%>%
            rename("encounter method"=learn_about_study,"Total"=count)
        formattable(BMI,align=c("l","r"))
        
    })
    
    output$AgePlot <- renderPlot({
        Mat_age <- ds_some_rows_v1%>%
            select(record_id,analysis_mat_age,learn_about_study)%>%
            filter(!is.na(analysis_mat_age))%>%
            filter(!is.na(learn_about_study))
        
        
        #create a marginal Histogram / Boxplot for maternal ages by encounter type:
        Mat_age$learn_about_study <- as.factor(Mat_age$learn_about_study)
        
        library(ggplot2)
        theme_set(theme_classic())
        
        # Plot
        means <- aggregate(analysis_mat_age ~  learn_about_study, Mat_age, mean)
        
        g <- ggplot(Mat_age, aes(learn_about_study, analysis_mat_age))
        g + geom_boxplot(varwidth=T, fill="plum") + 
            labs(title="Age of Participants", 
                 subtitle="Divided into how they heard of the study",
                 caption="Source: BEACH Interview",
                 x="How did you hear of our study",
                 y="Age(years)")+
            geom_text(data = means, aes(label = analysis_mat_age, y = analysis_mat_age + 6))
    })
    
    #change to DT table rather than formatabe 
    output$AgeTable <- renderTable({
        Mat_age <- ds_some_rows_v1%>%
            select(record_id,analysis_mat_age,learn_about_study)%>%
            filter(between(analysis_mat_age,input$Age[1],input$Age[2]))%>%
            filter(!is.na(analysis_mat_age))%>%
            filter(!is.na(learn_about_study))
        
        formattable(Mat_age,asign=c("l","c","r"))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
