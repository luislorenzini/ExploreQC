#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## USERS PARAMETER  ##
#Load Libraries
library(shiny)
library(ggplot2)
library(readr)
library(ggplot2)
library(gridExtra)
library(RNifti)
library(oro.nifti)
library(shinyjs)
library(wesanderson)
library(plyr)
library(openxlsx)
library(plotly)
#Read the Configuration File and set variables
configuration = openxlsx::read.xlsx("../ConfigFile.xlsx", 1)

analysisdir = configuration[which(configuration$Visualization.Module == 'AnalysisDir'), 'VIS_properties'] 
              
T1image = configuration[which(configuration$Visualization.Module == 'StructIm'), 'VIS_properties']  
FunctionalFold = configuration[which(configuration$Visualization.Module == 'FuncFold'), 'VIS_properties'] 
FunctionalImage = configuration[which(configuration$Visualization.Module == 'FuncIm'), 'VIS_properties'] 
DiffFolder = configuration[which(configuration$Visualization.Module == 'DiffFold'), 'VIS_properties'] 
DiffImage = configuration[which(configuration$Visualization.Module == 'DiffIm'), 'VIS_properties'] 



# Set directory (tmp) and read the output of xQC_Master
if (file.exists(file.path("dataframes", "QCed_data.csv"))) {
  qc_data_all = read.csv(file.path("dataframes", "QCed_data.csv"))
  
  
  print("One previous file has been found, starting from there. Delete the file to start over")
} else {
  qc_data_all = read.csv(file.path("dataframes", "QC.csv"))
  qc_data_all$Structural = rep("Passed",nrow(qc_data_all)) 
 # qc_data_all$Structural = as.character(qc_data_all$Structural)
  
  
  
  qc_data_all$Functional = rep("Passed",nrow(qc_data_all)) 
  qc_data_all$Diffusion = rep("Passed",nrow(qc_data_all)) 
  qc_data_all$ASL = rep("Passed",nrow(qc_data_all)) 
  
  #this won't be needed later
  #qc_data_all[qc_data_all == 0] <- NA
}








#Manage Site and patient name, This is temporary since is specific for EPAD study
qc_data_all$Site <- as.factor(qc_data_all$Site)


#This should already be present in the dataframe, will be left out later
qc_data_all$patient = qc_data_all$Subject




# Create variable with number of scan per site to 
for (i in 1:nrow(qc_data_all)) {
  qc_data_all$numberofscanspersite[i]=nrow(qc_data_all[which(qc_data_all$Site == qc_data_all$Site[i]),])
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  
  wellPanel(
    # Application title
    titlePanel("Explore QC visualization Tool"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      
      sidebarPanel(
        
        selectInput("modality", 
                    "Choose The MRI modality",
                    choices = c("Structural", "Functional", "Diffusion", "ASL"), 
                    selected = "Structural"),
        
        sliderInput("nscans",
                    "Exclude Sites with fewer scans than:",
                    min = 1,
                    max = 50,
                    value = 0),
        
        uiOutput("inputUI")
        
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("violinplot"),
        
        plotlyOutput("scatterplot"),#, click = "wt"), 
        
        textOutput("Text1"),
        
        #imageOutput("myImage"),
        fluidRow( 
          column(5, 
                 sliderInput("sliceX", 
                             label = "Axial",
                             min = 1,
                             max = 250,
                             value = 170),
                 
                 plotOutput("axial"),
                 includeScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-mousewheel/3.1.13/jquery.mousewheel.min.js")
          ), 
          column(5, offset = 1,
                 sliderInput("sliceY", 
                             label = "Sagittal",
                             min = 1,
                             max = 170,
                             value = 170),
                 
                 plotOutput("sagittal"),
                 includeScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-mousewheel/3.1.13/jquery.mousewheel.min.js")
          )
          
          
        ),
        
        fluidRow(
          column(5, 
                 sliderInput("sliceZ", 
                             label = "Coronal",
                             min = 1,
                             max = 250,
                             value = 250),
                 
                 plotOutput("coronal"),
                 includeScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-mousewheel/3.1.13/jquery.mousewheel.min.js")
          ), 
          column(4, offset = 1,
                 
                 uiOutput("mygui") ,
                 
                 "\n  \n  \n  Do you want to save the results of the visual QC? \n The results will be stored in a dataframen and you can continue editing later",
                 actionButton("save", "Save QCed dataframe")
                 
                 
          )
          
          
        )
      )
      
    )
    ,style = "overflow-y:scroll; max-height: 100%"
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## ADMINISTRATION  ##
  #column with include/exclude result for specific modality
  #includecolumn = reactive({qc_data_all[,]})
  
  #qc_data --> dataframe with only parameters from selected modality, and other columns(site..)
  qc_data <- reactive({
    data.frame(qc_data_all[,which(startsWith(colnames(qc_data_all), input$modality))], 
               qc_data_all$Site, 
               qc_data_all$patient,
               qc_data_all$numberofscanspersite,
               qc_data_all[,input$modality]
    )
  })
  
  #qc_data_TH -> qc_data but with just sites with more scans than threshold (input$nscans)
  qc_dataTH <- reactive({qc_data()[which(qc_data()$qc_data_all.numberofscanspersite>=input$nscans),] })
  
  # Select inputs based on modality (just parameters of one modality)
  
  output$inputUI<- renderUI({
    tagList(
      
      selectInput("QCpar",
                  label = "Select QC parameter:", 
                  choices = colnames(qc_data()),
                  selected = "Structural_Noise_SNR_GM_Ratio" ),
      
      selectInput("site",
                  label = "Select Site",
                  choices = unique(qc_dataTH()[,which(colnames(qc_data())== "qc_data_all.Site")]),
                  selected =  "040")
    )
   
  })
  
  
  # SiteData --> subset of thresholded dataframe includin only the selected site
  SiteData = reactive({qc_dataTH()[which(qc_dataTH()$qc_data_all.Site==input$site),]})
  
  
  
  
  ##   VISUALIZATION  ##
  pal <- c("red", "green")
  #Violin Plots for Between sites distributions
  output$violinplot <- renderPlotly({
    violin <- plot_ly(qc_dataTH(),
    x = ~qc_dataTH()$qc_data_all.Site,
    y = ~qc_dataTH()[,input$QCpar],
    split = ~qc_dataTH()$qc_data_all.Site,
    type = 'violin',
    box = list(
      visible = T
    ))
    violin %>% layout(title = 'Between-Site Distribution',
                      xaxis = list(title = 'Sites', zeroline = FALSE),
                      yaxis = list(title = input$QCpar, zeroline = FALSE ), showlegend = FALSE)
  })
  
  # Scatter Plot for Within site distributions 
  output$scatterplot <-  renderPlotly({
    # color is SiteData()$includecolumn
   scatter1 <- plot_ly(data = SiteData(), x = ~SiteData()$qc_data_all.patient, y = ~SiteData()[,input$QCpar], color = ~qc_data_all[which(qc_data_all$Site == input$site), input$modality], 
                       type = "scatter",
                       mode = "markers", 
                       colors = pal, 
                       marker = list(size =10, line = list(color = "black", width = 2) ))
   scatter1 %>% layout(title = paste('Within-Site Distribution Site', input$site),
                       xaxis = list(title = 'Subjects' ,
                                    zeroline = FALSE),
                       yaxis = list(title = input$QCpar, zeroline = FALSE ), showlegend = TRUE) 
                       #shapes=list(type='line', 
                                   #x0= 0, 
                                   #x1=length(qc_data_all$patient), 
                                   #y0 = mean(SiteData()[,input$QCpar]),
                                   #y1=mean(SiteData()[,input$QCpar]), 
                                   #line=list(dash='dot', width=1, color = "green")))
  })
  
  output$Text1 <- renderText({d <- event_data("plotly_click")$x
  })

  
  
  
  
  # Set image Path and Load Image based on PatientNum   #NEED TO BE DEPENDENT ON MODALITY
  
  Patientnum <- reactive({event_data("plotly_click")$x})
  
  ImagePath<- NULL
  im <- NULL
  
  makeReactiveBinding("ImagePath")
  makeReactiveBinding("im")
  observeEvent(Patientnum(), { 
      if (input$modality == 'Structural'){
        ImagePath <<- reactive({file.path(analysisdir, Patientnum(), T1image)})
        im <<- reactive({oro.nifti::readNIfTI(ImagePath())})
      } else if (input$modality == 'Functional'){
        ImagePath <<- reactive({file.path(analysisdir, Patientnum(), FunctionalFold, FunctionalImage)})
        im <<- reactive({oro.nifti::readNIfTI(ImagePath(), reorient = FALSE) })
      }
      else if (input$modality == 'Diffusion'){
        ImagePath <<- reactive({file.path(analysisdir, Patientnum(), DiffFolder, DiffImage)})
        im <<- reactive({oro.nifti::readNIfTI(ImagePath(), reorient = FALSE)})
      }
    })
  
  
  
  ## MRI IMAGE Visualization ##
  
  # Plot the three views and allow the scrolling, unfortunately the scrolling has some problems but this is the best we can do so far
  #Axial
  output$axial <- renderPlot({
    if (is.null(Patientnum)) return(NULL)
    oro.nifti::slice(im(), z=input$sliceX)
  })
  onevent("mousewheel", "axial", {
    updateSliderInput(session, "sliceX", value = (input$sliceX-10))
  })
  #Sagittal
  output$sagittal <-renderPlot({
    if (is.null(Patientnum)) return(NULL)
    oro.nifti::slice(im(), z=input$sliceY, plane = "sagittal")
    
  })
  onevent("mousewheel", "sagittal", {
    updateSliderInput(session, "sliceY", value = input$sliceY-10)
  })
  #Coronal
  output$coronal <-renderPlot({
    if (is.null(Patientnum)) return(NULL)
    oro.nifti::slice(im(), z=input$sliceZ, plane = "coronal")
    
  })
  onevent("mousewheel", "coronal", {
    updateSliderInput(session, "sliceZ", value = input$sliceZ-10)
  })
  
  
  
  
  
  
  
  ## DECISION PART ## here we set up the user interface to include exlude scans
  
  
  # This is a piece of code that respond to the clicking of CONFIRM botton and:
  observeEvent(input$update,{  
    # 1. Save the output (Pass/Fail) in the qc_data_all dataframe in the column called as the modality
    if (input$include == "Pass") {
      qc_data_all[which(qc_data_all$patient == Patientnum()),input$modality] <<-"Passed"
      #SiteData()[which(SiteData()$qc_data_all.patient == Patientnum()),input$modality]<<-"Passed"
      #SiteData()$include[which(SiteData()$patient == Patientnum()),]<-1
      
    }  else if (input$include == "Fail"){
      qc_data_all[which(qc_data_all$patient == Patientnum()),input$modality] <<- "Failed"
      #SiteData()[which(SiteData()$qc_data_all.patient == Patientnum()),input$modality]<<-"Passed"
      
      
    }
    

    # 2. Update Plot
    output$scatterplot <-  renderPlotly({
      scatter1 <- plot_ly(data = SiteData(), x = ~SiteData()$qc_data_all.patient, y = ~SiteData()[,input$QCpar], color = ~qc_data_all[which(qc_data_all$Site == input$site), input$modality], 
                          type = "scatter",
                          mode = "markers", 
                          colors = pal, 
                          marker = list(size =10, line = list(color = "black", width = 2) ))
      scatter1 %>% layout(title = paste('Within-Site Distribution Site', input$site),
                          xaxis = list(title = 'Subjects' ,
                                       zeroline = FALSE),
                          yaxis = list(title = input$QCpar, zeroline = FALSE ), showlegend = TRUE)
    })
    
    
    
  })
  
  
  # This is a gui to print the Fail/Pass choice and to select the option if the scan has already been seen, 
  #(if you already excluded the subject the gui wil appear with the Fail option selected) 
  output$mygui = renderUI({
    if (is.null(Patientnum)) return(NULL)
    if (qc_data_all[which(qc_data_all$patient == Patientnum()),input$modality] == "Failed"){
      tagList(
        (radioButtons("include", 
                      label = "Does the scan pass the QC?",
                      choices = c("Pass", "Fail"),
                      selected = "Fail",
                      inline = TRUE
        )),
        (actionButton("update", "Confirm")))
    } else if (qc_data_all[which(qc_data_all$patient == Patientnum()),input$modality] == "Passed") {
      tagList(
        (radioButtons("include", 
                      label = "Does the scan pass the QC?",
                      choices = c("Pass", "Fail"),
                      selected = "Pass",
                      inline = TRUE
        )),
        (actionButton("update", "Confirm")))
    }
  })
  
  #  Finally, here we respond to the clicking of the button SAVE and we save a CSV in the dataframe with the new columns,
  #this csv will be read autoatically in future runs. (CHANGE COLUMNS NAME? so that is not just structural...)
  observeEvent(input$save, {write.csv(qc_data_all, file = "dataframes/QCed_data.csv")})
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
