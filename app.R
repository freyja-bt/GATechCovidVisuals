library(shiny)
library(tidyverse)
library(lubridate)
library(xml2)
library(rvest)
library(ggplot2)


gt_cov_url <- 'https://health.gatech.edu/coronavirus/health-alerts'

html <- read_html(gt_cov_url)
case_dates <- html_nodes(html, "#node-707 tbody td , table:nth-child(4) td")

cases <- matrix(html_text(case_dates, trim =TRUE),ncol = 4, byrow = TRUE)%>%
    as_tibble()%>%
    rename(
        c("DateReported"=V1, "Position"=V2, "DateLastOnCampus"=V3,"CampusImpact"=V4)
    )%>%
    mutate(
        DateReported = mdy(DateReported),
        Position = if_else(Position == "Staff member", "Staff", Position)
    )

cases_full <- cases

modifier = 0
for(i in seq(nrow(cases))){
    
    adtnl_cases <- if_else(str_detect(cases[i,]$Position,"([:digit:]+)")  == TRUE,
                           str_extract(
                               cases[i,]$Position,
                               "[:digit:]+")%>%as.numeric-1,
                           if_else(str_detect(cases[i,]$Position,"s$")==TRUE,
                                   str_extract(
                                       cases[i,]$CampusImpact,
                                       "[:digit:]+"
                                   )%>%as.numeric-1,0
                           ))
    
    origRow <- i+modifier
    
    
    pos <- if_else(str_detect(cases[i,]$Position,"^Student")  == TRUE,
            "Student",
            if_else(str_detect(cases[i,]$Position,"^Staff") == TRUE,
                    "Staff", "Undisclosed"
            ))
    
    cases_full[(origRow):(origRow+adtnl_cases),]$Position <- pos
    cases_full[(origRow):(origRow+adtnl_cases),]$DateReported <- cases[i,]$DateReported
    cases_full[(origRow):(origRow+adtnl_cases),]$DateLastOnCampus <- if_else(cases[i,]$DateLastOnCampus=="Various", "Undisclosed", cases[i,]$DateLastOnCampus)
    cases_full[(origRow):(origRow+adtnl_cases),]$CampusImpact <- if_else(str_length(cases[i,]$CampusImpact)>200, paste0(str_sub(cases[i,]$CampusImpact, 1,197),"..."),cases[i,]$CampusImpact)
    
    modifier <- modifier+adtnl_cases
    
}

# surveillance testing
gt_st_url <- 'https://health.gatech.edu/surveillance-testing-program-results'

html_st <- read_html(gt_st_url)
survey_test <- html_nodes(html_st, "#node-744 td")

survey_tib <- matrix(html_text(survey_test, trim =TRUE),ncol = 3, byrow = TRUE)%>%
  as_tibble()%>%
  rename(
    c("Date"=V1, "Positive"=V2, "Samples"=V3)
  )%>%
  filter(
    str_detect(Positive,"[:alpha:]")==FALSE
  )%>%
  mutate(
    
    Samples = as.numeric(str_replace_all(Samples,",","")),
    Positive = as.numeric(str_replace_all(Positive,",",""))
  )

new_rows <- tibble()

for(i in seq(nrow(survey_tib))){

  if(str_detect(survey_tib[i,]$Date, "-")  == TRUE){
    early_val <- str_extract(survey_tib[i,]$Date, "[:digit:]+(?=-)")%>%as.numeric()
    late_val <- str_extract(survey_tib[i,]$Date, "(?<=-)[:digit:]+")%>%as.numeric()
    
    new_rows <- bind_rows(new_rows,tibble(Date = seq(paste0(str_extract(survey_tib[i,]$Date, "[:alpha:]+[:blank:]"),early_val,str_extract(survey_tib[i,]$Date,"[:punct:][:blank:][:digit:]{4}"))%>%mdy(),paste0(str_extract(survey_tib[i,]$Date, "[:alpha:]+[:blank:]"),late_val,str_extract(survey_tib[i,]$Date,"[:punct:][:blank:][:digit:]{4}"))%>%mdy(), by = 'days'),
         Positive = survey_tib[i,]$Positive/length(c(early_val:late_val)),
         Samples = survey_tib[i,]$Samples/length(c(early_val:late_val))
         ))

  }
}

survey_tib <- survey_tib%>%
  filter(
    str_detect(Date, "-")==FALSE
  )%>%
  mutate(
    Date= mdy(Date)
  )%>%
  bind_rows(., new_rows)%>%
  mutate(
    Positive = round(Positive, 1)
  )%>%
  arrange(desc(Date))


ui <- fluidPage(

    # Application title
    titlePanel("Covid Cases at Georgia Tech"),

    uiOutput("about"), # "about" section

    # Sidebar with inputs/checkboxes
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "selectedDates",
                "Date Range",
                start = "2020-08-01"
                ),
            checkboxGroupInput(
                "selectedPosition",
                "Position",
                choices = list(
                    "Student",
                    "Staff",
                    "Unlisted"),
                selected = c("Student","Staff", "Unlisted")
                ),
            radioButtons(
                "tableOptions",
                "Table Options",
                choices = list(
                    "Individual Cases" = 1,
                    "Aggregated Cases - By Day" = 2,
                    "Aggregated Cases - By Month" = 3),
                selected = 2
            ),
            radioButtons(
              "posRate",
              "Surveillence Testing Display Options",
              choices = list(
                "Positivity Rate"=1,
                "Total Numbers"=2
              ),
              selected = 1
            ),
            checkboxInput(
              "graphChoice",
              "Display Total Samples Tested",
              FALSE
            )
            
        ),

        # Show a plot of the generated distribution, followed by table of cases

        mainPanel(
          
          tabsetPanel(
            type = "tabs",
            tabPanel("Reported Cases",
              plotOutput("casePlot"),
            dataTableOutput("case_visual"),
           
        h5(em("There are minor differences in monthly totals due to a discrepancy in listed cases and reported totals on the source site. The issue appears to only affect May and April.")),
        uiOutput("more")
            ),
            tabPanel("Surveillence Testing",
                     uiOutput("test_graph"),
                     uiOutput("test_table"),
                     h5(em("Data from 8/09/20 to 8/09/20 was aggregated and displayed as averages. Surveillence testing is generally not done on Sundays."))
            )
          )
        )
    ),
    h5("This program was created by Freyja Brandel-Tanis, a City Planning and Civil Engineering master's student at Georgia Tech."),
      uiOutput("credit")  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$case_visual <- renderDataTable({
        
        
        cases_full%>%
            {if(input$tableOptions == 2){mutate(., Aggregate = DateReported)}else 
                if(input$tableOptions == 3){mutate(., Aggregate = month(DateReported, label = TRUE, abbr = FALSE))}else 
                    if(input$tableOptions == 1){mutate(., Aggregate = NULL)}}%>%
        filter(
            DateReported >= input$selectedDates[1] & DateReported <= input$selectedDates[2],
            Position %in% input$selectedPosition
        )%>%
            {if(input$tableOptions %in% c(2,3)){group_by(.,
                                                         Aggregate
            )%>%
            summarise(., TotalCases = n())%>%
            arrange(.,desc(Aggregate))}else{select(.,everything())}}
    })
    
    
    
    output$casePlot <- renderPlot({
        data_cases <- cases_full%>%
            filter(
                DateReported >= input$selectedDates[1] & DateReported <= input$selectedDates[2],
                Position %in% input$selectedPosition#,
               # str_detect(CampusImpact, "Greek")
            )%>%
            group_by(DateReported)%>%
            summarise(case_no = n())


        ggplot(data = data_cases)+
          geom_line(
            aes(x = DateReported, y = case_no)
          )+
          xlab("Days")+
          ylab("Cases Reported")+
          ggtitle(paste("Cases Reported between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" "))+
          theme_classic()

            
    })
    
    output$testPlot <- renderPlot({
      test_data <- survey_tib%>%
        filter(
          Date >= input$selectedDates[1] & Date <= input$selectedDates[2]
        )
      
     if(input$graphChoice == TRUE){
       ggplot(data = test_data)+
         geom_line(aes(x = Date, y = Positive, color = 'Positive Samples'))+
         geom_line(aes(x = Date, y = Samples, color = 'Total Samples'))+
         xlab("Days")+
         ylab("Individual Samples")+
         ggtitle(paste("Surveillence Testing Results between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" "))+
         theme_classic()+
         theme(
           legend.title = element_blank()
         )
     }else if(input$graphChoice == FALSE){
       ggplot(data = test_data)+
         geom_line(aes(x = Date, y = Positive, color = 'Positive Samples'))+
         labs(
           x = "Days",
         y = "Individual Samples",
         title = paste("Surveillence Testing Results between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" ")
                 )+
         theme_classic()+
         theme(
           legend.title = element_blank()
         )
     }
     
     
    })
    
    output$ratePlot <- renderPlot({
      test_data <- survey_tib%>%
        filter(
          Date >= input$selectedDates[1] & Date <= input$selectedDates[2]
        )%>%
        mutate(
          PositivityRate = Positive/Samples
        )
      
      ggplot(data = test_data)+
        geom_line(aes(x = Date, y = PositivityRate))+
        xlab("Days")+
        ylab("Percent")+
        ggtitle(paste("Surveillence Testing Postivity Rate between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" "))+
        theme_classic()
    })
     
    output$test_visual <- renderDataTable({
      
      
      survey_tib%>%
        filter(
          Date >= input$selectedDates[1] & Date <= input$selectedDates[2]
        )
    })
    
    output$rate_visual <- renderDataTable({
      
      
      survey_tib%>%
        filter(
          Date >= input$selectedDates[1] & Date <= input$selectedDates[2]
        )%>%
        mutate(
          PositivityRate = paste0(round(Positive/Samples,4)*100, "%")
        )%>%
        select(
          Date, PositivityRate
        )
    })
    
    
    output$test_graph <- renderUI({
      if(input$posRate == 1){
        plotOutput("ratePlot")
        }else{
          plotOutput("testPlot")
        }
    })
    
    output$test_table <- renderUI({
      if(input$posRate == 1){
        dataTableOutput("rate_visual")
      }else{
        dataTableOutput("test_visual")
      }
    })
    
    
     
    url1 <- a("Covid-19 Exposure and Health Alerts", href=gt_cov_url)
    url2 <- a("Surveillance Testing Program Results.", href = gt_st_url)
    output$about <- renderUI({
        tagList(h4("Created using data from Georgia Tech's", url1, "and", url2))
    })
    url4 <- a("Covid-19 Exposure and Health Alerts.", href=gt_cov_url)
    output$more <- renderUI({
      tagList(h5(em("You can find full campus impact statements at", url4)))
    })
    
    url3 <-  a("on github.", href='https://github.com/freyja-bt')
    output$credit <- renderUI({
      
      tagList(h5("You can follow her work ",url3))
      #,h5("She did not receive input or direction from the Institute to make this program.")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
