library(shiny)
library(markdown)


renderInputs <- function(prefix) {
  navlistPanel(
    "Select model parameters",
    tabPanel("Physical resources",
             h3(""),
             sliderInput(paste0(prefix, "_", "ct"), "CT:", min = 1, max = 10, value = 3, step = 1),
             sliderInput(paste0(prefix, "_", "angio_inr"), "Angio room for INR and IR procedures:", min = 1, max = 5, value = 1, step = 1),
             sliderInput(paste0(prefix, "_", "angio_ir"), "Angio room for IR procedures only:", min = 0, max = 5, value = 1, step = 1)
    ),
    tabPanel("Human resources",
             h3(""),
             sliderInput(paste0(prefix, "_", "ed_staff"), "ED staff:", min = 1, max = 20, value = 10, step = 1),
             sliderInput(paste0(prefix, "_", "stroke_staff"), "Stroke staff:", min = 1, max = 5, value = 2, step = 1),
             sliderInput(paste0(prefix, "_", "angio_staff"), "Day angio staff:", min = 3, max = 21, value = 6, step = 3),
             sliderInput(paste0(prefix, "_", "inr"), "Day interventional neuroradiologist:", min = 1, max = 10, value = 1, step = 1),
             sliderInput(paste0(prefix, "_", "ir"), "Day interventional radiologist:", min = 0, max = 10, value = 2, step = 1),

             sliderInput(paste0(prefix, "_", "angio_staff_night"), " Evening angio staff:", min = 3, max = 20, value = 3, step = 3),
             sliderInput(paste0(prefix, "_", "inr_night"), "Evening interventional neuroradiologist:", min = 1, max = 10, value = 1, step = 1),
             sliderInput(paste0(prefix, "_", "ir_night"), "Evening interventional radiologist:", min = 0, max = 10, value = 1, step = 1)
    ),
    tabPanel("Patient types",
             h3("Estimated per year"),
             sliderInput(paste0(prefix, "_", "ed_pt"), "ED patients:", min = 50000, max = 500000, value = 107700, step = 100),
             sliderInput(paste0(prefix, "_", "st_pt"), "Suspected stroke patients:", min = 300, max = 5000, value = 750, step = 50),
             sliderInput(paste0(prefix, "_", "ais_pt"), "Acute ischaemic stroke patients:", min = 50, max = 2000, value = 450, step = 50),
             sliderInput(paste0(prefix, "_", "ecr_pt"), "ECR patients:", min = 10, max = 500, value = 58, step = 1),
             sliderInput(paste0(prefix, "_", "inr_pt"), "Non acute INR patients:", min = 10, max = 500, value = 104, step = 1),
             sliderInput(paste0(prefix, "_", "eir_pt"), "Acute IR patients:", min = 1, max = 1000, value = 468, step = 1),
             sliderInput(paste0(prefix, "_", "ir_pt"), "Non acute IR patients:", min = 1000, max = 8000, value = 3805, step = 5)
    ),
    tabPanel("Capacity schedule",
             h3("Capacity schedule in 24hr time"),
             sliderInput(paste0(prefix, "_","shifts"), "Day shift:", min=1, max=24, value = c(8,17), step = 1)
    ),
    tabPanel("Simulation setup",
             h3(""),
             sliderInput(paste0(prefix, "_","nsim"), "Number of simulations:", min = 1, max = 10, value = 3, step = 1),
             sliderInput(paste0(prefix, "_","run_t"), "Simulation run time in month:", min = 1, max = 12, value = 3, step = 1)
             
    )
  )
  
}



fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 0.25;}" ),
          # Application title
          titlePanel(""),
          
          # Application title
          includeMarkdown("description.md"),
          
          hr(),
          
          renderInputs("a"),
          
          mainPanel(
            p(actionButton("goButton", "Run simulation", icon("random") )),
            
            includeMarkdown("figure_description.md"),
            
            plotOutput("a_distPlot", height = "600px") 
            #tableOutput("values")
            )
          )





