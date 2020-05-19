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

credential_label <- "interview_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)
desired_fields <- c("record_id","int_phone_pass_fail","encounter_date_int","encounter_type_int",
                    "beach_interview_study_encounters_complete",
                    "learn_about_study_int","int_consent_complete",
                    "analysis_mat_age_cats","analysis_bmi","mom3t_education_2",
                    "analysis_mat_age","analysis_income","analysis_kids_previous")


# events to retain

events_to_retain  <- c("beach_interview_phone_screen","beach_interview_study_encounters")

# list of events

# list records
#exportRecords(rcon)  I don't know why having this in a report is an issue

# export field names

# consented records
records.v1=c("BIS001A","BIS002A","BIS003A","BIS004A","BIS005A",
             "BIS006A","BIS007A","BIS008A","BIS009A","BIS010A",
             "BIS011A","BIS013A","BIS014A","BIS018A","BIS019A",
             "BIS022A","BIS023A","BIS024A","BIS025A","BIS026A",
             "BIS027A","BIS028A","BIS029A","BIS030A","BIS031A",
             "BIS032A","BIS033A","BIS034A",
             "PRG001","PRG002","PRG003","PRG004","PRG005",
             "PRG006","PRG007","PRG008","PRG009","PRG010",
             "PRG011","PRG012","PRG013","PRG014","PRG015",
             "PRG016","PRG017","PRG018","PRG020",
             "156","157","195","196","214","216","224","225","228",
             "230","234","235","238","239",
             "240","241","242","245","246","251","255","257","258",
             "259","260","261","262","265","266","267",
             "268","269","270","271","273","274","275","276","277",
             "278","280","281","282","283","286","289",
             "290","291","293","294","295","296","297","85","88","90")

ds <- data.frame(records.v1)

# pull data
ds_some_rows_v1 <- redcap_read(
    redcap_uri = uri, 
    records= records.v1,
    token      = beach_token, 
    fields     = desired_fields
    
)$data

#------------------------------------------------------------------------

# recode encounter type
ds_some_rows_v1=ds_some_rows_v1 %>%
    #use mutate(new column = recode(current column to be recoded, "variable in column"="new variable name"))
    mutate(learn_about_study = recode(learn_about_study_int, "1"="Flyer","3"="Facebook","5"="Other"))%>%
    replace_na(list( learn_about_study = "Didn't Respond"))%>%
    mutate(education_level = recode(mom3t_education_2,"1"="8th grade or less","2"="some high school","3"="high school diploma/GED","4"="some college or community college","5"="Associates degree","6"="completed tech or vocational school","7"="college graduate","8"="some graduate or professional school","9"="Graduate or professional degree"))%>%
    mutate(previous_kids = recode(analysis_kids_previous, "1"="yes","0"="no"))%>%
    mutate(income = recode(analysis_income, "1"="$0-$15,000","2"="$15,001-$19,000","3"="$19,001-$37,000","4"="$37,001-$44,000","5"="$44,001-$52,000","6"="$52,001-$56,000","7"="$56,001-$67,000","8"="$67,001-$79,000","9"="$79,001 or more"))%>%
    mutate(phone_pass_fail = recode(int_phone_pass_fail, "1"="pass","0"="fail"))%>%
    mutate(consent_complete = recode(int_consent_complete, "1"="Consented","0"="not Consented"))


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
