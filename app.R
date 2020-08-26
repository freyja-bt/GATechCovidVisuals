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

cases_full <- cases#%>%
    # select(
    #     -CampusImpact
    # )

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

# # surveillance testing
# gt_st_url <- 'https://health.gatech.edu/surveillance-testing-program-results'
# 
# html_st <- read_html(gt_st_url)
# survey_test <- html_nodes(html_st, "#node-744 td")
# 
# survey_tib <- matrix(html_text(survey_test, trim =TRUE),ncol = 3, byrow = TRUE)%>%
#   as_tibble()%>%
#   rename(
#     c("Date"=V1, "Positive"=V2, "Samples"=V3)
#   )%>%
#   mutate(
#     Date = if_else(str_detect(Date, "-"),str_replace(.$Date, "([:digit:]+-)",""), Date),
#     Date= mdy(Date),
#     Samples = as.numeric(str_replace_all(Samples,",","")),
#     Positive = as.numeric(str_replace_all(Positive,",","")),
#   )




ui <- fluidPage(

    # Application title
    titlePanel("Covid Cases at Georgia Tech"),
    uiOutput("tab"),
    

    # Sidebar with date range input and position checkboxes
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
            tags$b("Graph Options"),
            checkboxInput(
              "surveyTest",
              "Display Survey Testing Data"
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
            verbatimTextOutput("value"),
            dataTableOutput("case_visual"),
        h5(em("There are minor differences in monthly totals due to a discrepancy in listed cases and reported totals on the source site. The issue appears to only affect May and April."))
        )
    ),
        h5("This program was created by Freyja Brandel-Tanis, a City Planning and Civil Engineering master's student at Georgia Tech.")#,h5("She did not receive input or direction from the Institute to make this program.")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #start_range <- as_date(as.numeric(str_extract(input$selectedDates,"[:digit:]+(?=\\s)")))
    #end_range <- as_date(as.numeric(str_extract(input$selectedDates,"(?<=\\s)[:digit:]+")))
    #dates<- mdy(input$selectedDates)
    
    # working_data <- cases_full%>%
    #         filter(
    #             DateReported >= input$selectedDates[1] & DateReported <= input$selectedDates[2],
    #             Position %in% input$selectedPosition
    #         )
    #
   
    
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
    
    # print(paste0(input$surveyTest))
    output$value <- renderText({ input$somevalue })
    output$distPlot <- renderPlot({
        data <- cases_full%>%
            filter(
                DateReported >= input$selectedDates[1] & DateReported <= input$selectedDates[2],
                Position %in% input$selectedPosition
            )%>%
            group_by(DateReported)%>%
            summarise(case_no = n())

        # test_data <- survey_tib%>%
        #   filter(
        #     Date >= input$selectedDates[1] & Date <= input$selectedDates[2]
        #   )
        # if(input$surveyTest==FALSE){
        
        ggplot(data = data)+
            geom_line(
                aes(x = DateReported, y = case_no)
            )+
            xlab("Days")+
            ylab("Cases Reported")+
            ggtitle(paste("Cases Reported between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" "))
        
        # }else{
        #   ggplot(data = data)+
        #     geom_line(
        #       aes(x = DateReported, y = case_no)
        #     )+
        #     geom_line(data = test_data, aes(x = Date, y = Positive, color = 'Red'))+
        #     geom_line(data = test_data, aes(x = Date, y = Samples, color = 'Blue'))+
        #     xlab("Days")+
        #     ylab("Cases Reported")
        #   ggtitle(paste("Cases Reported between", day(input$selectedDates[1]), month(input$selectedDates[1],label = TRUE,abbr = FALSE), year(input$selectedDates[1]),"and", day(input$selectedDates[2]), month(input$selectedDates[2],label = TRUE,abbr = FALSE), year(input$selectedDates[2]), sep=" "))
        # }
            
    })
    
    url <- a("Covid-19 Exposure and Health Alerts", href=gt_cov_url)
    output$tab <- renderUI({
        tagList(h4("Created using data from Georgia Tech's ", url, " page."))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
