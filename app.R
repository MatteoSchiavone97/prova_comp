library(shiny)
library(shinythemes)
library(dplyr)
library(hash)
library(stringr)


for(file in list.files()){load(file)}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  titlePanel("Model Parameters"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "component",
                         label = "Component",
                         choices = c(
                           "gamma_X1",
                           "gamma_R",
                           "mu",
                           "Q"
                         )),
      selectInput(inputId = "copula",
                  label = "Copula:",
                  choices = c("Gumbel", "HReiss")),
      selectInput(inputId = "margin",
                  label = "Marginal Distribution:",
                  choices = c("Pareto", "Frechet", "Half_t")),
      checkboxGroupInput(inputId = "gamma",
                         label = bquote(gamma),
                         choices = round(c(1/4,1/3,1/2,2/3,3/4),2),
                         select = 0.25),
      conditionalPanel(condition = "input.copula == 'Gumbel'",
                       checkboxGroupInput(inputId = "alpha",
                                          label = expression(alpha),
                                          choices = c(0.1,0.3,0.5,0.7,0.9))),
      conditionalPanel(condition = "input.copula == 'HReiss'",
                       checkboxGroupInput(inputId = "lambda",
                                          label = expression(lambda),
                                          choices = c(0.3,0.5,0.7,1.4,1.7))),
      sliderInput(inputId = "upper",label = "plot upper limit",
                  min=0.01,max=2,value=1.5,step=0.05),
      ),
  mainPanel(
    fluidRow(
              splitLayout(cellWidths = c("33%", "33%", "33%"), plotOutput("grafico_bias", height = "400px"),
                          plotOutput("grafico_var", height = "400px"),plotOutput("grafico_mse", height = "400px"))
            )
    )
  )
)

# Define server logic to summarize and view selected dataset ----

indexes <- function(gamma,dep_par,copula){
  n_g <- which(round(c(1/4,1/3,1/2,2/3,3/4),2)%in%gamma)
  if (copula == "Gumbel"){
    n_d <- which(c(0.1,0.3,0.5,0.7,0.9)%in%dep_par)
  } else if (copula == "HReiss"){
    n_d <- which(c(0.3,0.5,0.7,1.4,1.7)%in%dep_par)
  }
  indici <- matrix(1:25,nrow=5,byrow=TRUE)
  
  indx <- c()
  for(g in n_g){
    for(a in n_d){
      indx <- append(indx,indici[g,a])
    }
  }
  return(indx)
}

server <- function(input, output) {
  output$grafico_bias <- renderPlot({
    plot(seq(0,400),ylim = c(0,input$upper),type="n",ylab="Bias",xlab="Effective Sample Fraction",xaxt="n")
    axis(1, at=seq(100,400,100), lab=paste(seq(100,400,100)/1000,"%"), las=TRUE)
    if(input$copula=="Gumbel"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$alpha,copula = input$copula)){
        lines(get(str_c("bias_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      } 
    } else if(input$copula == "HReiss"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$lambda,copula = input$copula)){
        lines(get(str_c("bias_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      }
    }
  })
  output$grafico_var <- renderPlot({
    plot(seq(0,400),ylim = c(0,input$upper),type="n",ylab="Variance",xlab="Effective Sample Fraction",xaxt="n")
    axis(1, at=seq(100,400,100), lab=paste(seq(100,400,100)/1000,"%"), las=TRUE)
    if(input$copula=="Gumbel"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$alpha,copula = input$copula)){
        lines(get(str_c("var_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      } 
    } else if(input$copula == "HReiss"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$lambda,copula = input$copula)){
        lines(get(str_c("bias_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      }
    }
  })
  output$grafico_mse <- renderPlot({
    plot(seq(0,400),ylim = c(0,input$upper),type="n",ylab="Mean Square Error",xlab="Effective Sample Fraction",xaxt="n")
    axis(1, at=seq(100,400,100), lab=paste(seq(100,400,100)/1000,"%"), las=TRUE)
    if(input$copula=="Gumbel"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$alpha,copula = input$copula)){
        lines(get(str_c("mse_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      } 
    } else if(input$copula == "HReiss"){
      t <- 2
      for(i in indexes(gamma = input$gamma, dep_par = input$lambda,copula = input$copula)){
        lines(get(str_c("bias_",input$component,"_",str_sub(input$copula,1,1),str_sub(input$margin,1,1),"_",i)),col=t,lty=t-1,lwd=2)
        t = t +1
      }
    }
  })
  output$info <- renderText({indexes(gamma = input$gamma, dep_par = input$dep_par,copula = input$copula)
    print(str_c("alpha=",input$dep_par,",gamma=",input$gamma))})
}

# Create Shiny app ----
shinyApp(ui, server)
