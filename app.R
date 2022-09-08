library(shiny)
library(shinydashboard)
library(stats)


# Define UI for application 
ui <- dashboardPage(skin = "blue",
      dashboardHeader(title = "MA5761 Final Project"),
      #Define Sidebar and tabs for each distribution
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Chi Squared Distribution", tabName = "Chi"),
          menuItem("Uniform Distribution", tabName = "Uni"),
          menuItem("Exponential Distribution", tabName = "exp")
        )
      ),
      #Define App Body
      dashboardBody(
        tabItems(
          #Create page body for Chi-square distribution page
          tabItem(tabName = "Chi",
              fluidRow(
                #Create input box
                box(
                  title = "Controls",
                  background = 'navy',
                  "Please Enter Desired Parameters Perform Test to Determine if the Empirical 
                  Type I Error Rate is Equal to the Level of Significance when Testing if the 
                  Theoretical Mean is Equal to the Estimated Mean",
                  br(),
                  br(),
                  numericInput("chisample", "Sample Size:", value = 20),
                  numericInput("chisignificance", "Significance Level:", value = 0.05),
                  numericInput("chirep", "Number of Replicates:", value = 2000),
                  numericInput("chidf", "Degrees of Freedom: ", value = 1),
                  actionButton("chirun", "Run Test", class = "btn-success")
                ),
                #Create example histogram 
                box(title = "Example Histogram", background = 'navy',
                    plotOutput("chi.hist")),
              ),
              #Create location for calculation outputs
            fluidRow(
              column(width = 6, offset = 0, style='padding:0px;'),
              box(
              title = "Outputs",
              background = "navy",
              textOutput("chimean"),
              textOutput("chip.val"),
              textOutput("chise"),
              textOutput("chidiff"),
              align = 'right'
            ))
      ),
      #Create page body for uniform distirbution
      tabItem(tabName = "Uni",
              fluidRow(
                #Create input box
                box(
                  title = "Controls",
                  background = 'navy',
                  "Please Enter Desired Parameters Perform Test to Determine if the Empirical 
                  Type I Error Rate is Equal to the Level of Significance when Testing if the 
                  Theoretical Mean is Equal to the Estimated Mean",
                  br(),
                  br(),
                  numericInput("unisample", "Sample Size:", value = 20),
                  numericInput("unisignificance", "Significance Level:", value = 0.05),
                  numericInput("unirep", "Number of Replicates:", value = 2000),
                  numericInput("unia", "Input Lower Value (a): ", value = 0),
                  numericInput("unib", "Input Upper Value (b): ", value = 2),
                  actionButton("unirun", "Run Test", class = "btn-success")
                ),
                #Create histogram box
                box(title = "Example Histogram", background = "navy",
                    plotOutput("uni.hist")),
                #Create output box
                fluidRow(box(
                  title = "Outputs",
                  background = "navy",
                  textOutput("unimean"),
                  textOutput("unip.val"),
                  textOutput("unise"),
                  textOutput("unidiff")
                )),
              )),
      #Create exponential distribution page
      tabItem(tabName = "exp",
              #Create input box
              fluidRow(
                box(
                  title = "Controls",                  
                  background = 'navy',
                  "Please Enter Desired Parameters Perform Test to Determine if the Empirical 
                  Type I Error Rate is Equal to the Level of Significance when Testing if the 
                  Theoretical Mean is Equal to the Estimated Mean",
                  br(),
                  br(),
                  numericInput("expsample", "Sample Size:", value = 20),
                  numericInput("expsignificance", "Significance Level:", value = 0.05),
                  numericInput("exprep", "Number of Replicates:", value = 2000),
                  numericInput("explambda", "Value of lambda: ", value = 1),
                  actionButton("exprun", "Run Test", class = "btn-success")
                ),
                #Create histogram box
                box(title = "Example Histogram", background = 'navy',
                    plotOutput("exp.hist")),
                #Create output box
                fluidRow(box(
                  title = "Outputs",
                  background = 'navy',
                  textOutput("expmean"),
                  textOutput("expp.val"),
                  textOutput("expse"),
                  textOutput("expdiff")
                )),
              ))
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### CHI SQUARED DISTRIBUTION CALCS
  chi.p <- eventReactive(input$chirun, {
    set.seed(100)
  n <- input$chisample
  alpha <- input$chisignificance
  mu0 <- input$chidf
  m <- input$chirep
  p.val <- numeric(m)
  for (i in 1:m) {
    x <- rchisq(n, mu0)
    ttest <- t.test(x, mu = mu0)
    p.val[i] <- ttest$p.value
  }
  mean(p.val < alpha)
  })
  chi.mean <- eventReactive(input$chirun, {
    input$chidf
  })
  chi.se <- eventReactive(input$chirun, {
    round(sqrt(chi.p() * (1 - chi.p()) / input$chirep), 3)
  })
  chi.diff <- eventReactive(input$chirun, {
    round(abs(input$chisignificance - chi.p()), 3)
  })
  chi.x <-eventReactive(input$chirun, {
    rchisq(input$chisample, input$chidf)
  })
  output$chi.hist <- renderPlot({
    hist(as.numeric(chi.x()), xlab = "Value", main = "Chi-Square Distribution")
  })
  output$chimean <- renderText(paste0("The mean of the distribution is: ", chi.mean()))
  output$chip.val <- renderText(paste0("The empirical Type I error rate of the t-test is : ", chi.p()))
  output$chise <- renderText(paste0("The standard error of this estimate of Type I error rate is: ", chi.se()))
  output$chidiff <- renderText(paste0("The difference between the empirical Type I error rate of the t-test
                      and significance level is: ", chi.diff()))
  
  #### UNI DISTRIBUTION CALCS
  uni.p <- eventReactive(input$unirun, {
    set.seed(100)
    n <- input$unisample
    alpha <- input$unisignificance
    mu0 <- input$unia + input$unib / 2
    m <- input$unirep
    p.val <- numeric(m)
    for (i in 1:m) { 
      x <- runif(n, min = input$unia, max = input$unib)
      ttest <- t.test(x, mu = mu0)
      p.val[i] <- ttest$p.value
    }
    
    mean(p.val < alpha)
  })
  uni.mean <- eventReactive(input$unirun, {
    input$unia + input$unib / 2
  })
  uni.se <- eventReactive(input$unirun, {
    round(sqrt(uni.p() * (1 - uni.p()) / input$unirep), 3)
  })
  uni.diff <- eventReactive(input$unirun, {
    round(abs(input$unisignificance - uni.p()), 3)
  })
  uni.x <-eventReactive(input$unirun, {
    runif(input$unisample, input$unia, input$unib)
  })
  output$uni.hist <- renderPlot({
    hist(as.numeric(uni.x()), xlab = "Value", main = "Uniform Distribution")
  })
  output$unimean <- renderText(paste0("The mean of the distribution is: ", uni.mean()))
  output$unip.val <- renderText(paste0("The empirical Type I error rate of the t-test is : ", uni.p()))
  output$unise <- renderText(paste0("The standard error of this estimate of Type I error rate is: ", uni.se()))
  output$unidiff <- renderText(paste0("The difference between the empirical Type I error rate of the t-test
                      and significance level is: ", uni.diff()))
  
  ### EXP DISTRIBUTION CALCS
  exp.p <- eventReactive(input$exprun, {
    set.seed(100)
    n <- input$expsample
    alpha <- input$expsignificance
    mu0 <- 1 / input$explambda
    m <- input$exprep
    p.val <- numeric(m)
    for (i in 1:m) { 
      x <- rexp(n, rate = input$explambda)
      ttest <- t.test(x, mu = mu0)
      p.val[i] <- ttest$p.value
    }
    
    mean(p.val < alpha)
  })
  exp.mean <- eventReactive(input$exprun, {
    1 / input$explambda
  })
  exp.se <- eventReactive(input$exprun, {
    round(sqrt(exp.p() * (1 - exp.p()) / input$exprep), 3)
  })
  exp.diff <- eventReactive(input$exprun, {
    round(abs(input$expsignificance - exp.p()), 3)
  })
  exp.x <-eventReactive(input$exprun, {
    rexp(input$expsample, rate = input$explambda)
  })
  output$exp.hist <- renderPlot({
    hist(as.numeric(exp.x()), xlab = "Value", main = "Exponential Distribution")
  })
  output$expmean <- renderText(paste0("The mean of the distribution is: ", exp.mean()))
  output$expp.val <- renderText(paste0("The empirical Type I error rate of the t-test is : ", exp.p()))
  output$expse <- renderText(paste0("The standard error of this estimate of Type I error rate is: ", exp.se()))
  output$expdiff <- renderText(paste0("The difference between the empirical Type I error rate of the t-test
                      and significance level is: ", exp.diff()))

}

# Run the application 
shinyApp(ui = ui, server = server)
