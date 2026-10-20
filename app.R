library(shiny)
library(readxl)
library(knitr)
library(ggplot2)

# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(navbarPage("Organize & Visualize Categorical Variables",
                           tabPanel("Data",
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                      sidebarPanel(
                                        # Input: Checkbox if file has header ----
                                        checkboxInput("header", "Column names in first row", TRUE),
                                        fileInput("file1", "Choose an Excel (.XLSX), Comma-Separated Values (.CSV), or Tab-Separated (.TXT) File",
                                                  multiple = FALSE,
                                                  accept = c(".txt",
                                                             ".csv", 
                                                             ".xlsx")),
                                        # Horizontal line ----
                                        tags$hr(),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),  
                                        p("Browse for an Excel (.XLSX), comma-separated values (.CSV), tab-separated (.TXT) file to import and use to organize and visualize categorical variables. If variable names are not in the first row of your data, uncheck the header box. If importing an Excel file, the data must be in the first sheet. After your file is uploaded, navigate to either the One Variable or Two Variable tab to organize and visualize."),  
                                        p(tags$b("Note:"), "The variable(s) to organize and visualize must be categorical."),
                                        hr()
                                      ))),
                           tabPanel("One Variable",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xvar", "Variable", NULL),
                                        selectInput("plot", 
                                                    "Plot Type",
                                                    choices = c("Pie" = "pie", "Bar" = "bar"))
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),
                                        p("Choose the categorical variable that you would like to organize and visualize from the list variables in the uploaded data set. Then, choose the type of visualization to display."),  
                                        hr(),
                                        h4("Organize"),
                                        tableOutput("onewaytab"),
                                        hr(),
                                        fluidRow(column(8,
                                                        h4("Visualize"),
                                                        plotOutput("onecatplot")))))),
                           tabPanel("Two Variables",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xvar1", "Variable 1", NULL),
                                        selectInput("xvar2", "Variable 2", NULL),
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        h4("Directions"),
                                        p("Choose 2 categorical variables that you would like to organize and visualize from the list of variables in the uploaded data set."),
                                        p(tags$b("Note:"), "Variable 1 will display in the rows and Variable 2 will display in the columns of the resulting two-way table."),  
                                        hr(),
                                        h4("Organize"),
                                        br(),
                                        h5("Frequency"),
                                        tableOutput("twowaytab"),
                                        br(),
                                        h5("Percentage"),
                                        tableOutput("twowaytabpr"),
                                        hr(),
                                        fluidRow(column(10,
                                                        h4("Visualize"),
                                                        plotOutput("twocatplot"))))
                                      
                                      
                                    ))))

  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output){
    
    data <- reactive({
      req(input$file1)
      
      ext <- tools::file_ext(input$file1$name)
      switch(ext,
             csv = read.csv(input$file1$datapath, header = input$header),
             txt = read.delim(input$file1$datapath, header = input$header),
             xlsx = as.data.frame(read_xlsx(input$file1$datapath, sheet = 1, col_names = input$header, .name_repair = "universal")),
             validate("Invalid file; Please upload a .xlsx, .csv, or .txt file")
      )
    })
    
    #observeEvent(input$file1, {
    #  updateSelectInput(inputId = "xvar", choices = colnames(data()), selected=colnames(data())[2])
    #})
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar", 
                        choices = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20], 
                        selected = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20][2])
    })
    
    #output$onewaytab <- renderTable({
    #  xvarnum <- which(colnames(data()) %in% input$xvar)
    #  taby <- ftable(as.factor(data()[,xvarnum]), dnn = c(input$xvar))
    #  taby}
    #)
    
    output$onewaytab <- renderTable({
      xvarnum <- which(colnames(data()) %in% input$xvar)
      taby <- t(rbind(Frequency = addmargins(table(as.factor(data()[,xvarnum]))),
                      Proportion =  addmargins(proportions(table(as.factor(data()[,xvarnum])))),
                      Percentage = addmargins(proportions(table(as.factor(data()[,xvarnum]))))*100))
      #taby <- ftable(as.factor(data()[,xvarnum]), dnn = c(input$xvar))
      taby},
      colnames = TRUE,
      rownames = TRUE,
      digits = 2
    )
    
    output$onecatplot <- renderPlot({
      xvarnum <- which(colnames(data()) %in% input$xvar)
      xvarname <- names(data())[xvarnum]
      variable <- as.factor(data()[,xvarnum])
      tab <- table(as.factor(data()[,xvarnum]))
      if(input$plot=="pie"){
        plotout <- pie(tab, main = paste0("Pie Chart: ", input$xvar), col =rainbow(5))
      } else{
        plotout <- barplot(tab, main = paste0("Bar Plot: ", input$xvar), col = rainbow(5))
      }
      plotout
    }, res= 96)
    
    
    #observeEvent(input$file1, {
    #  updateSelectInput(inputId = "xvar1", choices = colnames(data()), selected=colnames(data())[1])
    #})
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar1", 
                        choices = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20], 
                        selected = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20][1])
    })
    
    # observeEvent(input$file1, {
    #  updateSelectInput(inputId = "xvar2", choices = colnames(data()), selected=colnames(data())[2])
    #})
    
    observeEvent(input$file1, {
      updateSelectInput(inputId = "xvar2", 
                        choices = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20], 
                        selected = colnames(data())[sapply(data(), function(x) length(unique(x))) < 20][2])
    })
    
    output$twowaytab <- renderTable({
      xvarnum1 <- which(colnames(data()) %in% input$xvar1)
      xvarnum2 <- which(colnames(data()) %in% input$xvar2)
      tab1 <- as.data.frame.matrix(addmargins(table(as.factor(data()[,xvarnum1]), as.factor(data()[,xvarnum2]))))
      tab1},
      rownames = TRUE,
      digits = 0
    )
    
    output$twowaytabpr <- renderTable({
      xvarnum1 <- which(colnames(data()) %in% input$xvar1)
      xvarnum2 <- which(colnames(data()) %in% input$xvar2)
      tab2 <- as.data.frame.matrix(addmargins(prop.table(table(as.factor(data()[,xvarnum1]), as.factor(data()[,xvarnum2]))))*100)
      tab2},
      rownames = TRUE,
      digits = 2
    )
    
    
    output$twocatplot <- renderPlot({
      xvarnum1 <- which(colnames(data()) %in% input$xvar1)
      xvarnum2 <- which(colnames(data()) %in% input$xvar2)
      tab <- table(as.factor(data()[,xvarnum1]), as.factor(data()[,xvarnum2]), 
                   dnn = c(input$xvar1, input$xvar2))
      #p <- barplot(tab, beside = TRUE, 
      #            xlab = input$xvar2, cex.names = .8, args.legend = list("topright", inset = c(-.4, 0), cex = .8, title = input$xvar1))
      #p
      tab <- data.frame(table(as.factor(data()[,xvarnum1]), as.factor(data()[,xvarnum2]), 
                              dnn = c(input$xvar1, input$xvar2)))
      p <- ggplot(tab, aes_string(input$xvar2, "Freq", fill=input$xvar1))+ geom_bar(stat = "identity", position = position_dodge())
      p}, res = 96)
    
  }
    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)
