library(shiny)
library(psych)
library(GPArotation)
library(shinythemes)
library(shinyBS)
library(readr)

happy <- read_csv("~/Documents/HU_Berlin/CD_Bachelor/2017.csv")




#models = list() globale variablen vorherige modelle abspeichern + zugriff zum vergleich

########### USER INTERFACE ####

ui <- 
  
  fluidPage( 
  
  navbarPage("Factor Analysis Application",  theme = shinytheme("cosmo"), inverse = FALSE, fluid = TRUE, 
            
         
             
    tabPanel("Factor Analysis",
            
   ######PANEL1####
   #SIDEPANELS WITH INPUTVARIABLES ARE MADE ####
      sidebarPanel(
           
        helpText("Create a factor model:"),
          
            # Input: Selector for Faktornumber 
            sliderInput(inputId = "factorno", 
                        label = "Choose the number of factors", 
                        value = 3, min = 1, max = 9
                        ),
  
            bsTooltip(id = "factorno", title = "The number of factors can not exceed the number of variables", 
                            placement = "bottom", trigger = "hover"),
            
            #Input: Extraktionmethod is Selector
            selectInput('extraktion', 'Select the extraktion method',
                        c("Maximum Likelihood"= "ml", "Principal Axes" = "pa", "Alpha Faktoring" = "alpha",
                          "MinRes" = "minres", "ULS" = "uls"), 
                        selected = NULL, multiple = FALSE,
                        selectize = FALSE, width = NULL, size = NULL),
            
            #helpText("Note: Extraktion methods."),
          
          
            # Input: Rotation Method Selector
            selectInput("rotation", 'Select the rotation method', 
                        c( "Unrotiert" = "none", "Varimax" = "varimax","Quartimax" = "quartimax", 
                          "Promax" = "promax", "Oblimin" = "oblimin", 
                          "Varimin" = "varimin") , 
                        selected = NULL, multiple = FALSE, selectize = FALSE,
                        width = NULL, size = NULL),
            
            
            bsTooltip(id = "rotation", title = "Varimax and Quartimax are orthogonal rotation methods, 
                                 while Promax and Oblimin are oblique", placement = "bottom", trigger = "hover"),
            
            #helpText("Note: Varimax and Quartimax are orthogonal rotation methods, 
                                 #while Promax and Oblimin are oblique"),
                        
        helpText("____________________________________"),    
        
            # Input: Selector for choosing dataset 
            selectInput(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("bfi", "happy"))
            
            
      ), #sidebarpanel
   

  
  
   #MAINPANEL WITH TABS#####
  
           mainPanel(
             
              tabsetPanel( type = "pills",
                  
                  tabPanel("Dataset", 
                           
                           verbatimTextOutput(outputId = "description"),
              
                           numericInput("obs", "Number of observations to view:", 10),
                            div(style = 'overflow-x: scroll', tableOutput('view'))
                           
                          
                           
                           
                  ),
                  
                  tabPanel("Correlation Matrix", plotOutput(outputId = "corr"), verbatimTextOutput(outputId = "summary")),
                  
                  tabPanel("Number of Factors", 
                           
                           helpText("Note: The decision about the number considered in the model 
                                    is based on the Eigenvalues and can be determined by
                                    Kaiser-Criteria, Scree-Test or Parallel Analysis"),
                           plotOutput(outputId = "scree"),
                           helpText("Eigenvalues of empirical correlations matrix (PA Actual Data)"),
                           verbatimTextOutput("eigenoriginal"),
                           helpText("Eigenvalues of reduced correlations matrix (FA Actual Data)"),
                           verbatimTextOutput(outputId = "eigenreduced")
                     
                           
                          
                        
                  ),
                  
                 # tabPanel("Faktor Analysis Results", verbatimTextOutput(outputId = "factor")),
                  
                
                 
                  tabPanel("Factor Loadings", 
                           
                           fluidRow( 
                           
                           column(8,
                           verbatimTextOutput(outputId = "factor2")
                  ),
                  column(4,
                         sliderInput(inputId = "cutoff", 
                                     label = "Choose a cutoff level", 
                                     value = 0.4, min = 0, max = 0.9)
                  )
                  )
                 )
                  ,
                  
                  tabPanel("Additional Information",
                           helpText("Sum of communalities"),
                           verbatimTextOutput(outputId = "communalities"),
                           helpText("Explained variance of factor model"),
                           verbatimTextOutput(outputId = "antcom"),
                           helpText("Correlations of factors"),
                           verbatimTextOutput(outputId = "correlation")
                         
                  )
             
              ) #tabset
             
           ) #mainpanel
  
  
  ),  #navbarPage1
  
  ######PANEL2#####
  
      tabPanel("Model Comparison",
           
        
           sidebarPanel( 
           
             
             helpText("Choose two models:"),
             
             # Input: Selector for Faktornumber 
             sliderInput(inputId = "factorno1", 
                        label = "Factor Model #1", 
                         value = 3, min = 1, max = 9
             ),
             
          
             bsTooltip(id = "factorno1", title = "Choose the number of factors",
                       placement = "bottom", trigger = "hover"),
             
             
            
             #Input: Extraktionmethod is Selector
             selectInput('extraktion1', label = NULL, 
                         c("Maximum Likelihood"= "ml", "Principal Axes" = "pa", "Alpha Faktoring" = "alpha",
                           "MinRes" = "minres", "ULS" = "uls"), 
                         selected = NULL, multiple = FALSE,
                         selectize = FALSE, width = NULL, size = NULL),
             
             bsTooltip(id = "extraktion1", title = "Select extraction method",
                       placement = "bottom", trigger = "hover"),
             
             
             # Input: Rotation Method Selector
             selectInput('rotation1', label = NULL, 
                         c( "Unrotiert" = "none", "Varimax" = "varimax","Quartimax" = "quartimax", 
                           "Promax" = "promax", "Oblimin" = "oblimin", 
                           "Varimin" = "varimin") , 
                         selected = NULL, multiple = FALSE, selectize = FALSE,
                         width = NULL, size = NULL),
             
             bsTooltip(id = "rotation1", title = "Select rotation method",
                       placement = "bottom", trigger = "hover"),
         #  ),
         #  sidebarPanel(        #LASTCHANGE
             
        
         helpText("........................................................................"),
         
       
         
             
             
             
            
                      sliderInput(inputId = "factorno2", 
                                  label = "Factor Model #2", 
                                  value = 3, min = 1, max = 9),
         
                       bsTooltip(id = "factorno2", title = "Choose the number of factors",
                                 placement = "bottom", trigger = "hover"),
                      
                      selectInput('extraktion2', label = NULL,
                                  c("Maximum Likelihood"= "ml", "Principal Axes" = "pa", "Alpha Faktoring" = "alpha",
                                    "MinRes" = "minres", "ULS" = "uls"), 
                                  selected = NULL, multiple = FALSE,
                                  selectize = FALSE, width = NULL, size = NULL
                                  ),
         
                       bsTooltip(id = "extraktion2", title = "Select extraction method",
                                 placement = "bottom", trigger = "hover"),
                       
                      
                      selectInput('rotation2', label = NULL, 
                                  c("Unrotiert" = "none", "Varimax" = "varimax","Quartimax" = "quartimax", 
                                    "Promax" = "promax", "Oblimin" = "oblimin", 
                                    "Varimin" = "varimin") , 
                                  selected = NULL, multiple = FALSE, selectize = FALSE,
                                  width = NULL, size = NULL),
         
                       bsTooltip(id = "rotation2", title = "Select rotation method",
                                 placement = "bottom", trigger = "hover"),
           
        helpText("____________________________________"),
       
             
             # Input: Selector for choosing dataset 
             selectInput(inputId = "dataset",
                         label = "Choose a dataset:",
                         choices = c("bfi", "happy"))
             
           
           ),
            
         mainPanel(
           
           tabsetPanel(type = "pills",
           
                 tabPanel("Loading Plots",
                          
                        
                        
                                  fluidRow( 
                              
                                    helpText("."),
                                           sliderInput(inputId = "cutoff2", 
                                                       label = "Choose a cutoff level",
                                                       value = 0.3, min = 0, max = 0.9)
                                           
                                           ,
                                    column(5,
                                           plotOutput(outputId = "factorplot")),
                                    column(5,
                                           plotOutput(outputId = "factorplot2"))
                                  )
                  ),
                 
                 tabPanel("Factor Congruence", 
                          
                          helpText("."),
                          fluidRow('Tucker Index of Factor Congurence', 
                                   
                                   helpText("Note: The congruence coefficient describes the cosinus angle between two factors of two distinctly constructed factormodels"),
                                   
                                   verbatimTextOutput(outputId = "congruence"),
                                   plotOutput(outputId = "congruenceplot")
                           )
                 )
                 
           )
        )
     ),
#####Panel3####  
  tabPanel("About",
           helpText("This Shiny application was created by Raphael Wilhelm Hanke and is part of his Bachelorthesis created for Humboldt-University of Berlin"),
           
           helpText("Berlin, January 2018"),
           helpText("-"),
           helpText("http://worldhappiness.report/"),
           helpText("https://sapa-project.org/research/SPI/"),
           helpText("http://ipip.ori.org/")
           
  )
  
  )
)




  ######SERVER######
server <- function(input, output) {
  
  #DATA##
  v1 = rnorm(20, mean = 0, sd =1)
  v2 = jitter(v1+20, factor = 100, amount = 1)
  v3 = jitter(v1+30, factor = 100, amount = 1)
  v4 = rnorm(20, mean = 0, sd =1)
  v5 = jitter(v4+12, factor = 5, amount = 1)
  v6 = jitter(v4-20, factor = 5, amount = 1)
  v7 = rnorm(20, mean = 0, sd =1)
  v8 = jitter(v7+100, factor = 5, amount = 1)
  m1 = cbind(v1,v2,v3,v4,v5,v6,v7,v8)
   
  #BFI ANPASSUNG
  bfi = bfi[!(bfi$age == 3),]
  bfi1 = subset(bfi, select = -c(gender, education, age))
  
  harman = Harman74.cor
  
  #happy anpassung
  happy1 = subset(happy, select = -c(Country, Whisker.high, Whisker.low, Happiness.Rank, Happiness.Score))
  
  datasetInput = reactive({
    switch(input$dataset,
           "bfi" = bfi1,
           "happy" = happy1)
  }) #Datensatz input
  
  

  
  output$view = renderTable({
    head(datasetInput(), n = input$obs)
    
    
  }) #Vorschau
  
  output$description = renderPrint({
    
    
 # case_when(
  #  (datasetInput() == bfi1) ~ "This dataset deals with personality items ...",
   # (input$dataset == happy1) ~ "This dataset is part of the world happiness report ..."
 # )
    
    
  #  if (datasetInput() == bfi1)
   #         "This dataset deals with personality items ..."
  #  else (input$dataset == happy1)
   #         "This dataset is part of the world happiness report ..."
    
   # reactivePrint()
    
    
  if (length(datasetInput()) == 7)
    print("This dataset is part of the world happiness report which ranks 155 countries by their happiness levels. It conisists of seven scores for: Economy & GDP per Capita, Family, Health &Life Expectancy, Freedom, Generosity, Government Corruption and Dystopia Residual")
  else
    print("This dataset deals with personality items and is part of the SAPA, an online self evaluation project. Variables A1 to 05 relate to 25 questions about personality based on Items of the International Personality Item Pool. A detailed description of the dataset and an overview of the questions is given in Chapter 4 of the thesis.")
    
   # (input$dataset == happy1)
   #else  ({datasetInput() == m1})
    #   print("567")
    
  }) #Data Beschreibung
    
  output$corr = renderPlot({
    x = datasetInput()
    z = scale(x)
    R = cor(z, y = NULL, use = "complete.obs")
    corPlot(R, numbers= TRUE, main= "Heatmap der Korrelationen",  show.legend=TRUE
            )}) # Korrelationsplot
  
  output$scree = renderPlot({
  
  fa.parallel(datasetInput(), fm=input$extraktion, fa="both", main="Scree Plot & Parallelanalyse", SMC = TRUE, 
              ylabel = "Eigenvalue", cor="cor", use = "complete.obs", n.obs = 200)
  }) #ScreePlot
  
  output$factor2 = renderPrint({
    
    faktor = fa(datasetInput(),
             nfactors= input$factorno,
             SMC=TRUE,
             fm=input$extraktion,
             rotate= input$rotation)
    
    print(faktor$loadings, digits=3, cutoff=input$cutoff, sort=FALSE
          )
    
    
    
    #helpText("Communalities for each variable")
    
    #print(faktor$communality)
    }) # Ladungsmatrix
  
  output$correlation = renderPrint({
    
    faktor = fa(datasetInput(),
                nfactors= input$factorno,
                SMC=TRUE,
                fm=input$extraktion,
                rotate= input$rotation)
    
  s = faktor$Phi
    
    if({is.null(s)})
      print("The resulting factors are not correlated due to an orthogonal rotation")
    else
    print(faktor$Phi, digits=3, cutoff=input$cutoff, sort=FALSE
    )
  }) # korrelationen
  
  output$communalities = renderPrint({
    
    faktor = fa(datasetInput(),
                nfactors= input$factorno,
                SMC=TRUE,
                fm=input$extraktion,
                rotate= input$rotation)
    
    print(sum(faktor$communality, digits = 3)) #Kommunalit??ten
    

  
    
  }) #kommunalit??ten
  
  output$antcom = renderPrint({
    
    faktor = fa(datasetInput(),
                nfactors= input$factorno,
                SMC=TRUE,
                fm=input$extraktion,
                rotate= input$rotation)
    
    
    print(sum(faktor$communality)/sum(faktor$e.values)) # Anteil erkl??rter Varianz
    
    
    
    
  }) #erkl??rter varianzanteil
  
  output$eigenreduced = renderPrint({
    
    faktor = fa(datasetInput(),
                nfactors= input$factorno,
                SMC=TRUE,
                fm=input$extraktion,
                rotate= input$rotation)
    
    'for (x in 1:input$factorno) #Eigenvalues
      print(as.array(faktor$e.values[x]))'
    

      print(as.array(faktor$values[1:input$factorno]))
    
  }) #reduzierte eigenvalues
  
  output$eigenoriginal = renderPrint({
    
    faktor = fa(datasetInput(),
                nfactors= input$factorno,
                SMC=TRUE,
                fm=input$extraktion,
                rotate= input$rotation)
    
    print(as.array(faktor$e.values[1:input$factorno]))
    
   # for (x in 1:input$factorno) #Eigenvalues
    #print(as.array(faktor$e.values[x]))
    #print(as.array(faktor$values[1:input$factorno])/sum(faktor$e.values))
    
  }) #empirische eigenvalues
  
  
  
  #zwei

  output$factorplot = renderPlot({
    
    faktor1 = fa(datasetInput(),
                nfactors= input$factorno1,
                SMC=TRUE,
                fm=input$extraktion1,
                rotate= input$rotation1)
    
    fa.diagram(faktor1, main = "Loading diagram #1", cut = input$cutoff2)
  }) # Ladungs Plot
  
  output$factorplot2 = renderPlot({
    
    faktor2 = fa(datasetInput(),
                nfactors= input$factorno2,
                SMC=TRUE,
                fm=input$extraktion2,
                rotate= input$rotation2)
    
    fa.diagram(faktor2, main = "Loading diagram #2", cut = input$cutoff2)
  }) # Ladungs Plot
  
  output$congruence = renderPrint({
    
    faktor1 = fa(datasetInput(),
                nfactors= input$factorno1,
                SMC=TRUE,
                fm=input$extraktion1,
                rotate= input$rotation1)
    
    faktor3 = fa(datasetInput(),
                 nfactors= input$factorno2,
                 SMC=TRUE,
                 fm=input$extraktion2,
                 rotate= input$rotation2)
    
    
    print(factor.congruence(faktor1, faktor3 ,digits=2, use="complete" ,structure=TRUE))
    
  }) #kongurenzmatrix
  
  output$congruenceplot = renderPlot({
    
    faktor1 = fa(datasetInput(),
                 nfactors= input$factorno1,
                 SMC=TRUE,
                 fm=input$extraktion1,
                 rotate= input$rotation1)
    
    faktor3 = fa(datasetInput(),
                 nfactors= input$factorno2,
                 SMC=TRUE,
                 fm=input$extraktion2,
                 rotate= input$rotation2)
    
    
    C = factor.congruence(faktor1, faktor3 ,digits=2, use="complete" ,structure=FALSE)
    
    
    corPlot(C, numbers= TRUE, main= "Heatmap der Congruecematrix",  show.legend=TRUE, symmetric = FALSE)
    
  }) #heatmapcongruence
  
  
  
}


shinyApp(ui = ui, server = server)