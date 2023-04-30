library(shiny)
library(shinythemes)
library(digest)




ui <- fluidPage(theme = shinytheme("cerulean"),
                
                
                
                navbarPage(
                  
                  "Course Evaluation  ",id="myFrame",
                  tabPanel("Evaluation Form",
                           # sidebarPanel
                           mainPanel(
                             
                             
                             tags$head(tags$style(HTML(
                               "input[type=number] {-moz-appearance:textfield;}
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;"))),
                             
                             h1("Course Evaluation"),
                             
                             h5("Evaluation of course content & lectures"),
                             
                             
                             
                             numericInput("indexNo", "Index Number:","",1000,9999),
                             
                             
                             dateInput("date1", "Date:", value = NULL, min = NULL, max = NULL,
                                       format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                       language = "en"),
                             
                             
                             
                             actionButton("nextButton1", "Next",
                                          style="position:relative; left:calc(33%);
                                      color: black; background-color: #337ab7; 
                              border-color: #2e6da4"),
                             
                             
                             
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Section 2", " ",
                           
                           mainPanel(
                             tags$head(
                               tags$link(rel="stylesheet", type="text/css", href="https://rawcdn.githack.com/PubDe/shiny-style/be1accb7a49aecbd5584f856955c2fc449dce4f9/starstyle.css")
                             ),
                             
                             sliderInput("rating1","The teacher was punctual :",min=0,max=5,value = 0,step =1),
                             
                             sliderInput("rating2","the lectures were clear, interesting and attractive :",min=0,max=5,value = 0,step =1),
                             
                             sliderInput("rating3","The teaching aids were used effectively during the lecture :",min=0,max=5,value = 0,step =1),
                             
                             sliderInput("rating4","The lecture was conducted at all acceptable pace :",min=0,max=5,value = 0,step =1),
                             
                             sliderInput("rating5","The outeline,syllabus was covered as indicated in the timetable / time pierod :",min=0,max=5,value = 0,step =1),
                             
                             sliderInput("rating6","The lectures helped to improve knowledge about subject area:",min=0,max=5,value = 0,step =1),
                             
                             actionButton("backButton1", "Back",
                                          style="
                                      color: black; background-color: #337ab7; 
                              border-color: #2e6da4"),
                             
                             actionButton("nextButton2", "Next",
                                          style="position:relative; left:calc(3%);
                                      color: black; background-color: #337ab7; 
                              border-color: #2e6da4"),
                             
                             
                             
                             
                           ),#main panel
                  ),#tabpanel2
                  tabPanel("Section 3", "Use this page to comment about the overall progress",
                           textInput("commentIn","Your suggestions or criticisms :"),
                           tags$div(
                             actionButton("backButton2", "Back",
                                          style="
                                      color: black; background-color: #337ab7; 
                              border-color: #2e6da4"),
                             actionButton("submitButton", "submit",
                                          style="position:relative; left:calc(3%);
                                      color: black; background-color: #337ab7; 
                              border-color: #2e6da4"),
                             
                             
                           ), ),
                           tabPanel("Evaluation Form/ radio buttons",
                                    radioButtons( 
                                      "The teacher was punctual", 
                                      h3("The teacher was punctual"),
                                      choices = list("agree" = 1, 
                                                     "strongly agree" = 2, 
                                                     "disagree" = 3,
                                                     "strongly disagree"=4),
                                    ),
                                    
                                    radioButtons("the lectures were clear, interesting and attractive", 
                                                 h3("the lectures were clear, interesting and attractive"), 
                                                 choices = list("agree" = 1, 
                                                                "strongly agree" = 2, 
                                                                "disagree" = 3,
                                                                "strongly disagree"=4),
                                    ),
                                    
                                    radioButtons("the teaching aids were used effectively during the lecture", 
                                                 h3("the teaching aids were used effectively during the lecture"), 
                                                 choices = list("agree" = 1, 
                                                                "strongly agree" = 2, 
                                                                "disagree" = 3,
                                                                "strongly disagree"=4),
                                    ),
                                    
                                    radioButtons("the lecture was conducted at all acceptable pace", 
                                                 h3("the lecture was conducted at all acceptable pace"), 
                                                 choices = list("agree" = 1, 
                                                                "strongly agree" = 2, 
                                                                "disagree" = 3,
                                                                "strongly disagree"=4),
                                                 
                                                 
                                    )   ,
                                    
                                    
                                    
                                    
                                    
                           
                           tabPanel("Report"),
                  )
                  
                ) # navbarPage
) # fluidPage



server <- function(input, output, session) {
  
  
  hideTab(inputId = "myFrame", target = "Section 2")
  hideTab(inputId = "myFrame", target = "Section 3")
  
  
  
  
  observeEvent(input$nextButton1, {
    y<-(input$indexNo)
    print(y)
    
    
    if(is.na(input$indexNo)){
      
      showModal(modalDialog(
        title = "Your Index number is required",
        paste0("Please enter your index number"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else if(input$indexNo<1000 || input$indexNo>9999 ){
      showModal(modalDialog(
        title = "Please enter a valid Index number.",
        paste0("Please re-enter your index number"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      
      hideTab(inputId = "myFrame", target = "Section 1")
      showTab(inputId = "myFrame", target = "Section 2")
      updateTabsetPanel(session, "myFrame",
                        selected = "Section 2")}
  })
  
  observeEvent(input$nextButton2, {
    hideTab(inputId = "myFrame", target = "Section 2")
    showTab(inputId = "myFrame", target = "Section 3")
    updateTabsetPanel(session, "myFrame",
                      selected = "Section 3")
  })
  
  observeEvent(input$backButton1, {
    hideTab(inputId = "myFrame", target = "Section 2")
    showTab(inputId = "myFrame", target = "Section 1")
    updateTabsetPanel(session, "myFrame",
                      selected = "Section 1")
  })
  
  observeEvent(input$backButton2, {
    hideTab(inputId = "myFrame", target = "Section 3")
    showTab(inputId = "myFrame", target = "Section 2")
    updateTabsetPanel(session, "myFrame",
                      selected = "Section 2")
  })
  
  
  observeEvent(input$submitButton, {
    myhsh<-sapply(input$indexNo, digest,algo="md5")
    # read table for primary key (optional)
    row<-data.frame(myhsh,input$date1,input$rating1,input$rating2,input$rating3,input$rating4,input$commentIn)
    write.table(row,file = "data.csv",sep=",",append = TRUE,quote = FALSE,col.names = FALSE,row.names = FALSE)
    updateTextInput(session,myhsh,label=NULL,value = "")
    updateTextInput(session,"date1",label=NULL,value = "")
    updateTextInput(session,"rating1",label=NULL,value = "")
    updateTextInput(session,"rating2",label=NULL,value = "")
    updateTextInput(session,"rating3",label=NULL,value = "")
    updateTextInput(session,"rating4",label=NULL,value = "")
    updateTextInput(session,"commentIn",label=NULL,value = "")
    
    showModal(modalDialog(
      title = "Your evaluation submitted successfuly",
      paste0("Thank you for your response"),
      easyClose = TRUE,
      footer = NULL
    ))
    
    hideTab(inputId = "myFrame", target = "Section 3")
    showTab(inputId = "myFrame", target = "Section 1")
    updateTabsetPanel(session, "myFrame",
                      selected = "Section 1")
    
  })
  
} # server



shinyApp(ui = ui, server = server)

