library(shiny)
library(tidyverse)
library(rsconnect)
library(wesanderson)


#------------------------------------------------------

model = function(x, m, n, fU, fV, fW, fX, fY){
  
  y = rep(0, 5) # make a vector y of length 5 filled with 0s = [0, 0, 0, 0, 0]
  
  t = ((1/2)*fU*fX*x[1]*x[4]) +
    (((1-n)/4)*fV*fX*x[2]*x[4]) +
    (((1+n)/4)*fV*fX*x[2]*x[4]) +
    (((1+m)/2)*fU*fY*x[1]*x[5]) +
    ((((1+m)*(1-n))/4)*fV*fY*x[2]*x[5]) +
    ((1/2)*fW*fX*x[3]*x[4]) +
    ((((1+n)*(1+m))/4)*fV*fY*x[2]*x[5]) +
    (((1+m)/2)*fW*fY*x[3]*x[5]) +
    ((1/2)*fU*fX*x[1]*x[4]) +
    (((1-n)/4)*fV*fX*x[2]*x[4]) +
    (((1-m)/2)*fU*fY*x[1]*x[5]) +
    ((((1-m)*(1-n))/4)*fV*fY*x[2]*x[5]) +
    (((1+n)/4)*fV*fX*x[2]*x[4]) +
    ((((1-m)*(1+n))/4)*fV*fY*x[2]*x[5]) +
    (((1-m)/2)*fW*fY*x[3]*x[5]) +
    ((1/2)*fW*fX*x[3]*x[4])
  
  
  y[1] <- (((1/2)*fU*fX*x[1]*x[4]) +
             (((1-n)/4)*fV*fX*x[2]*x[4])) / t
  
  y[2] <- ((((1+n)/4)*fV*fX*x[2]*x[4]) +
             (((1+m)/2)*fU*fY*x[1]*x[5]) +
             ((((1+m)*(1-n))/4)*fV*fY*x[2]*x[5]) +
             ((1/2)*fW*fX*x[3]*x[4])) / t
  
  y[3] <- (((((1+n)*(1+m))/4)*fV*fY*x[2]*x[5]) +
             (((1+m)/2)*fW*fY*x[3]*x[5])) / t
  
  y[4] <- (((1/2)*fU*fX*x[1]*x[4]) +
             (((1-n)/4)*fV*fX*x[2]*x[4]) +
             (((1-m)/2)*fU*fY*x[1]*x[5]) +
             ((((1-m)*(1-n))/4)*fV*fY*x[2]*x[5])) / t
  
  y[5] <- ((((1+n)/4)*fV*fX*x[2]*x[4]) +
             ((((1-m)*(1+n))/4)*fV*fY*x[2]*x[5]) +
             (((1-m)/2)*fW*fY*x[3]*x[5]) +
             ((1/2)*fW*fX*x[3]*x[4])) / t
  
  return(y)
}


#------------------------------------------------------

model_setup <- function(generations, m, n, fU, fV, fW, fX, fY, U, V, W, X, Y) {
  
  # make an empty matrix with 5 columns
  A = matrix(data = 0, ncol = 5, nrow = generations)
  # fill the first row of the matrix
  A[1,] = c(U, V, W, X, Y)
  #test = c(U, V, W, X, Y)
  #return(test)
  
  # fill each row of the matrix after the first row with the output of the model function
  for (t in 2:generations){
    A[t,] = model(A[t-1,], m, n, fU, fV, fW, fX, fY)
  }
  
  
  
  df <- as.data.frame(A)
  
  colnames(df) <- c('XX', 'XdX', 'XdXd', 'XY', 'XdY')
  df$males <- df$XY + df$XdY
  df$females <- 1 - df$males
  
  df$Xd <- df$XdX/2 + df$XdXd + df$XdY
  df$X <-  1 - df$Xd
  
  return(df)
}


#------------------------------------------------------

ui <- pageWithSidebar(
  
  headerPanel = headerPanel(""),
  
  sidebarPanel(
    
    sliderInput(inputId = 'generations', 
                label = 'Number of generations', 
                value = 100, 
                min = 0, 
                max = 200),
    sliderInput(inputId = 'male_drive_strength', 
                label = 'Male drive strength', 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'female_drive_strength', 
                label = 'Female drive strength', 
                value = 0, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XX_fitness', 
                label = 'XX fitness', 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdX_fitness', 
                label = HTML(paste0('X',tags$sup('D'), 'X fitness')), 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdXd_fitness', 
                label = HTML(paste0('X',tags$sup('D'), 'X',tags$sup('D'), ' fitness')), 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XY_fitness', 
                label = 'XY fitness', 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdY_fitness', 
                label = HTML(paste0('X',tags$sup('D'), 'Y fitness')), 
                value = 1, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XX_starting', 
                label = 'XX starting frequency', 
                value = 0.5, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdX_starting', 
                label = HTML(paste0('X',tags$sup('D'), 'X starting frequency')), 
                value = 0, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdXd_starting', 
                label = HTML(paste0('X',tags$sup('D'), 'X',tags$sup('D'), ' starting frequency')), 
                value = 0, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XY_starting', 
                label = 'XY starting frequency', 
                value = 0.49, 
                min = 0, 
                max = 1),
    sliderInput(inputId = 'XdY_starting', 
                label = HTML(paste0('X',tags$sup('D'), 'Y starting frequency')), 
                value = 0.01, 
                min = 0, 
                max = 1),
    actionButton(inputId = "action_button", label = "Run model")
  ),
  mainPanel =  mainPanel(
    verbatimTextOutput("validation_message"),
    uiOutput("download_ui"),
    plotOutput(outputId = 'model_plot'),
    plotOutput(outputId = 'males_females_plot'),
    plotOutput(outputId = 'X_Xd_plot')
    
  )
)


#------------------------------------------------------

server <- function(input, output){
  
  # validation check
  input_validation <- reactive({
    U = input$XX_starting
    V = input$XdX_starting 
    W = input$XdXd_starting
    X = input$XY_starting
    Y = input$XdY_starting
    
    list(
      valid = abs(U + V + W + X + Y - 1) < 0.001,
      sum = U + V + W + X + Y,
      message = paste0("Starting frequencies must sum to 1.00\n",
                       "Current sum: ", round(U + V + W + X + Y, 3), "\n",
                       "Adjust the starting frequency sliders")
    )
  })
  
  
  model_output <- eventReactive(input$action_button, {
    
    # Check validation
    validation <- input_validation()
    if (!validation$valid) {
      return(NULL)
    }
    
    generations = input$generations
    m = input$male_drive_strength
    n = input$female_drive_strength
    fU = input$XX_fitness
    fV = input$XdX_fitness 
    fW = input$XdXd_fitness
    fX = input$XY_fitness
    fY = input$XdY_fitness
    U = input$XX_starting
    V = input$XdX_starting 
    W = input$XdXd_starting
    X = input$XY_starting
    Y = input$XdY_starting
    
    #print(model_setup(generations, m, n, fU, fV, fW, fX, fY, U, V, W, X, Y))
    return(model_setup(generations, m, n, fU, fV, fW, fX, fY, U, V, W, X, Y))
  })
  
  
  output$validation_message <- renderText({
    validation <- input_validation()
    if (!validation$valid) {
      validation$message
    } else {
      ""
    }
  })
  
  
  output$download_ui <- renderUI({
    df <- model_output()
    if (!is.null(df)) {
      downloadButton("download_data", "Download Data")
    }
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("meiotic_drive_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- model_output()
      if (!is.null(df)) {
        # Add a generation column
        df$generation <- 1:nrow(df)
        # Reorder columns to put generation first
        df <- df[, c("generation", names(df)[names(df) != "generation"])]
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  
  output$model_plot <- renderPlot({
    df <- model_output()
    if (is.null(df)) return(NULL)
    
    par(mfrow=c(1,2))
    plot(df$XX, col="#FF0000", lwd=3, type="l", ylim = c(0,1), xlab = "Generation", ylab = "Genotype Frequency")
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 1)      # Grid line width
    lines(df$XdX, col="#00A08A", lwd=3)
    lines(df$XdXd, col="#F2AD00", lwd=3)
    lines(df$XY, col="#F98400", lwd=3)
    lines(df$XdY, col="#5BBCD6", lwd=3)
    
    par(mar=c(5.1, 0, 4.1, 0))
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend('topleft', legend=c('XX', expression('X'^'D'*'X'), expression('X'^'D'*'X'^'D'), 'XY', expression('X'^'D'*'Y')), 
           col = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"), lty = 1, lwd = 3)
  })
  
  
  output$males_females_plot <- renderPlot({
    df <- model_output()
    if (is.null(df)) return(NULL)
    
    par(mfrow=c(1,2))
    plot(df$males, col="#FF0000", lwd=3, type="l", ylim = c(0,1), xlab = "Generation", ylab = "Frequency")
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 1)      # Grid line width
    lines(df$females, col="#00A08A", lwd=3)
    
    par(mar=c(5.1, 0, 4.1, 0))
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend('topleft', legend=c('males', 'females'), col = c("#FF0000", "#00A08A"), lty = 1, lwd = 3)
  })    
  
  
  output$X_Xd_plot <- renderPlot({
    df <- model_output()
    if (is.null(df)) return(NULL)
    
    par(mfrow=c(1,2))
    plot(df$X, col="#F98400", lwd=3, type="l", ylim = c(0,1), xlab = "Generation", ylab = "Frequency")
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 1)      # Grid line width
    lines(df$Xd, col="#5BBCD6", lwd=3)
    
    par(mar=c(5.1, 0, 4.1, 0))
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend('topleft', legend=c('X', expression('X'^'D')), col = c("#F98400", "#5BBCD6"), lty = 1, lwd = 3)
  })    
  
  
}


#------------------------------------------------------

shinyApp(ui = ui, server = server)