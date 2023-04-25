library(shiny)
library(shinythemes)
library(tidyverse)

# Store the needed .csv files to object by path (or by using file.choose() to select from device)
ChemicalList <- "Chemicals.csv"
ExtractionBufferMolaritiesList <- "ExtractionBuffer.csv"
EnzymeMolaritiesList <- "Enzymes.csv"

# Read in the .csv from above 
Chemicals <- read_csv(ChemicalList)
Extraction <- read_csv(ExtractionBufferMolaritiesList)
Enzyme <- read_csv(EnzymeMolaritiesList)

# In the enzyme .csv file, filter out each enzyme and store the data into its own object
Rubisco <- Enzyme %>% 
  dplyr::filter(Enzyme == "rubisco")
PhosphoglycolatePhosphatase <- Enzyme %>% 
  dplyr::filter(Enzyme == "PGP")
GlycolateOxidase <- Enzyme %>% 
  dplyr::filter(Enzyme == "glycolateOxidase")
Catalase <- Enzyme %>% 
  dplyr::filter(Enzyme == "catalase")
HydroxypyruvateReductase <- Enzyme %>% 
  dplyr::filter(Enzyme == "hydroxypyruvateReductase")
GlycerateKinase <- Enzyme %>% 
  dplyr::filter(Enzyme == "glycerateKinase")
GGAminotransferase <- Enzyme %>% 
  dplyr::filter(Enzyme == "GGAT")
AGAminotransferase <- Enzyme %>% 
  dplyr::filter(Enzyme == "AGAT")
SGAminotransferase <- Enzyme %>% 
  dplyr::filter(Enzyme == "SGAT")

# Define the user interface for the Shiny app
ui <- fluidPage(
  #Add "flatly" theme from "shinythemes" package
  theme = shinytheme("flatly"),
  #Main Title of the ShinyApp 
  navbarPage("Recipe Guide for Photorespiratory Assays", 

  #Setup Tab for Solution Prep
  tabPanel("| Solution Prep |", fluid = TRUE,
  # No title for the "Solution Prep" sub tab
  titlePanel(" "),
  # Add a sidebar layout
  sidebarLayout(
    # Define the sidebar with the input widgets
    sidebarPanel(
      p("From the dropdown menu select your chemical of interest, then enter the molarity and volume for your desired solution. Adjust the volume and mass units as needed. "),
      br(),
      # Add a dropdown menu for selecting the chemical of interest
      selectInput(inputId = "chemical", 
                  label = "Chemical:", 
                  choices = Chemicals$Name, 
                  selected = ""),
      # Add a numeric input for the molarity
      numericInput(inputId = "molarity", 
                   label = "Molarity (mol/L; M):", 
                   value = 1),
      # Add a numeric input for the volume
      numericInput(inputId = "volume", 
                   label = "Volume (select units below):", 
                   value = 1),
      # Add radio button to select units of volume measurement
      radioButtons(inputId = "volume_units",
                   label = "",
                   choices = c("L", "mL", "uL"),
                   inline = T),
      # Add radio button to select the output units 
      radioButtons(inputId = "magnitude",
                   label = "Mass Units:",
                   choices = c("g", "mg", "ug"),
                   inline = T)
    ),
    # Define the main panel for displaying the output
    mainPanel(
      # Add "Molecular Weight" header
      h3("Molecular Weight"),
      # Add a place to display the molecular weight of the chemical of interest
      verbatimTextOutput("mw"),
      # Add "Results" header
      h3("Results"),
      # Add a place to display the calculated grams
      verbatimTextOutput("grams"),
      # Add "Recipe" header
      h3("Recipe"),
      # Add a place to display the solution recipe
      verbatimTextOutput("recipe")
    )
  )),

  #Setup Tab for Extraction Buffer
  tabPanel("| Extraction Buffer |", fluid = TRUE,
         # No title for the sub tab
         titlePanel(" "),
         # Add a sidebar layout
         sidebarLayout(
           # Define the sidebar with the input widgets
           sidebarPanel(
             p("Enter the number of crude protein extractions (i.e., leaves) you want. Modify the extraction buffer volume as needed. "),
             br(),
             # Add a numeric input for the extraction number
             numericInput(inputId = "extraction", 
                         label = "How Many Extractions?", 
                         value = 1),
             # Add a numeric input for the volume of extraction buffer per reaction 
             numericInput(inputId = "EBperRXN",
                       label = "Extraction Buffer per reaction (uL)",
                       value = 1000)
           ),
           # Define the main panel for displaying the output
           mainPanel(
             # Add "Procedure" header
             h3("Procedure"),
             # Add a place to display the number of samples that can be processed 
             verbatimTextOutput("extraction"),
             # Add "Recipe" header
             h3("Recipe"),
            # Add a place to display the extraction buffer recipe
             verbatimTextOutput("EB.recipe"),
           )
         )),
  
  # Setup Tab for Enzyme Assays (add a dropdown menu)
  navbarMenu(title = "| Enzyme Assay Prep |",
  #Setup Tab for Rubisco Enzymatic Assay Prep
  tabPanel("Rubisco", fluid = TRUE,
         # Add title to sub tab
         titlePanel("Rubisco Activity Assay"),
         # Add a sidebar layout
         sidebarLayout(
           # Define the sidebar with the input widgets
           sidebarPanel(
             p("Enter the number of crude protein extractions you have (this is used for the initial activator calculation)."),
             # Add a numeric input for the sample number
             numericInput(inputId = "samples", 
                          label = "How many extractions do you have?", 
                          value = 1),
             br(),
             p("Then, input the number of reactions you are planning to do, the total reaction volume, the crude protein extract volume, and the substrate molarity below."),
             # Add a dropdown menu to input the number of "rubisco" reactions
             numericInput(inputId = "rubisco_rxn", 
                          label = "Desired number of rubisco reactions:", 
                          value = 1),
             # Add a dropdown menu to input the volume per "rubisco" reactions
             numericInput(inputId = "rubisco_vol", 
                          label = "Reaction volume (uL):", 
                          value = 200),
             # Add a dropdown menu to input the volume addition of Crude Protein Extract 
             numericInput(inputId = "cpe.rubisco", 
                          label = "Crude protein extract volume (uL):", 
                          value = 4),
             # Add a dropdown menu to input Molarity of RuBP (substrate)
             numericInput(inputId = "rubp_M", 
                          label = "RuBP (mol/L; M):", 
                          value = 0.01604)
           ),
           # Define the main panel for displaying the output
           mainPanel(
             # Add "Initial Activator" header
             h3("Initial Activator Recipe"),
             # Add a place to display the Initial Activator recipe
             verbatimTextOutput("initialActivator"),
             # Add "substrate" header
             h3("RuBP (substrate)"),
             # Add a place to display the RuBP volume requirement
             verbatimTextOutput("rubp"),
             # Add "Coupling Enzymes" header
             h3("Coupling Enzyme"),
             # Add a place to display the coupling enzyme volume requirement
             verbatimTextOutput("couplingEnzyme.rubisco"),
             # Add "reaction buffer" header
             h3("Reaction Buffer"),
             # Add a place to display the Rubisco Reaction Buffer recipe
             verbatimTextOutput("RRB")
           )
         )),
  
  tabPanel("Phosphoglycolate Phosphatase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("phosphoglycolate phosphatase assay prep"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               # Add a dropdown menu to input the number of "PGP" reactions
               numericInput(inputId = "pgp_rxn", 
                            label = "Desired number of PGP reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.pgp", 
                            label = "Volume of crude protein extract (uL):", 
                            value = 4),
               # Add a dropdown menu to input the volume per "PGP" reactions
               numericInput(inputId = "pgp_vol", 
                            label = "Volume per PGP rxn (uL):", 
                            value = 200)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Rubisco Reaction Buffer recipe
               verbatimTextOutput("PGPRB"),
             )
           )),
  
  #Setup Tab for Rubisco Enzymatic Assay Prep
  tabPanel("Glycolate Oxidase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Glycolate Oxidase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarity below."),
               br(),
               # Add a dropdown menu to input the number of "GO" reactions
               numericInput(inputId = "go_rxn", 
                            label = "Desired number of GO reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per "GO" reactions
               numericInput(inputId = "go_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.go", 
                            label = "Crude protein extract volume (uL):", 
                            value = 3),
               # Add a dropdown menu to input Molarity of substrate
               numericInput(inputId = "glycolicacid_M", 
                            label = "Glycolic Acid (mol/L; M):", 
                            value = 0.1)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Glycolic Acid (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glycolicacid"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("GORB")
             )
           )),
  
  tabPanel("Catalase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Catalase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, and the crude protein extract volume below"),
               br(),
               # Add a dropdown menu to input the number of CAT reactions
               numericInput(inputId = "cat_rxn", 
                            label = "Desired number of CAT reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per CAT reactions
               numericInput(inputId = "cat_vol", 
                            label = "Reaction volume (uL):", 
                            value = 1000),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.cat", 
                            label = "Crude protein extract volume (uL):", 
                            value = 15),
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Hydrogen Peroxide (substrate)"),
               # Add a place to display the substrate recipe
               verbatimTextOutput("h2o2"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("KPB.CATRB"),
             )
           )),
  
  tabPanel("Glutamate:Glyoxylate Aminotransferase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Glutamate:Glyoxylate Aminotransferase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarities below."),
               br(),
               # Add a dropdown menu to input the number of GGAT reactions
               numericInput(inputId = "ggat_rxn", 
                            label = "Desired number of GGAT reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per GGAT reactions
               numericInput(inputId = "ggat_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.ggat", 
                            label = "Crude protein extract volume (uL):", 
                            value = 4),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "glutamate", 
                            label = "Glutamate (mol/L; M):", 
                            value = 0.5),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "glyoxylate.ggat", 
                            label = "Glyoxylate (mol/L; M):", 
                            value = 0.1),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "glutamicDH", 
                            label = "Glutamic Dehydrogenase (units/mL):", 
                            value = 100)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Glutamate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glutamate"),
               # Add "substrate" header
               h3("Glyoxylate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glyoxylate.ggat"),
               # Add "substrate" header
               h3("Glutamic Dehydrogenase"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glutamicDH"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("KPB.GGATRB")
             )
           )),
  
  tabPanel("Alanine:Glyoxylate Aminotransferase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Alanine:Glyoxylate Aminotransferase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarities below."),
               br(),
               # Add a dropdown menu to input the number of AGAT reactions
               numericInput(inputId = "agat_rxn", 
                            label = "Desired number of AGAT reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per AGAT reactions
               numericInput(inputId = "agat_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.agat", 
                            label = "Crude protein extract volume (uL):", 
                            value = 4),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "alanine", 
                            label = "Alanine (mol/L; M):", 
                            value = 1.6),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "glyoxylate.agat", 
                            label = "Glyoxylate (mol/L; M):", 
                            value = 0.1),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "lactateDH", 
                            label = "Lactate Dehydrogenase (units/mL):", 
                            value = 3)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Alanine (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("alanine"),
               # Add "substrate" header
               h3("Glyoxylate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glyoxylate.agat"),
               # Add "substrate" header
               h3("Lactate Dehydrogenase"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("lactateDH"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("KPB.AGATRB"),
             )
           )),
  
  tabPanel("Serine:Glyoxylate Aminotransferase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Serine:Glyoxylate Aminotransferase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarities below."),
               br(),
               # Add a dropdown menu to input the number of SGAT reactions
               numericInput(inputId = "sgat_rxn", 
                            label = "Desired number of SGAT reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per SGAT reactions
               numericInput(inputId = "sgat_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.sgat", 
                            label = "Crude protein extract volume (uL):", 
                            value = 4),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "serine", 
                            label = "Serine (mol/L; M):", 
                            value = 1),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "glyoxylate.sgat", 
                            label = "Glyoxylate (mol/L; M):", 
                            value = 0.1),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "hpr1", 
                            label = "HPR1 (units/mL):", 
                            value = 1),
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Serine (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("serine"),
               # Add "substrate" header
               h3("Glyoxylate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glyoxylate.sgat"),
               # Add "substrate" header
               h3("HPR1"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("hpr1"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("KPB.SGATRB"),
             )
           )),
  
  tabPanel("Hydroxypyruvate Reductase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Hydroxypyruvate Reductase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarity below."),
               br(),
               # Add a dropdown menu to input the number of HPR reactions
               numericInput(inputId = "hpr_rxn", 
                            label = "Desired number of HPR reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per HPR reactions
               numericInput(inputId = "hpr_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.hpr", 
                            label = "Crude protein extract volume (uL):", 
                            value = 4),
               # Add a dropdown menu to input Molarity of (substrate)
               numericInput(inputId = "NaBHP_M", 
                            label = "Na-B-HP (mol/L; M):", 
                            value = 0.05)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("25 M Na-Beta-Hydroxypyruvate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("na.b.hp"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("KPB.HPRRB"),
             )
           )),
  
  #Setup Tab for Rubisco Enzymatic Assay Prep
  tabPanel("Glycerate Kinase", fluid = TRUE,
           # Add title to sub tab
           titlePanel("Glycerate Kinase Activity Assay"),
           # Add a sidebar layout
           sidebarLayout(
             # Define the sidebar with the input widgets
             sidebarPanel(
               p("Input the number of reactions you are plannig to do, the total reaction volume, the crude protein extract volume, and the substrate molarity below."),
               br(),
               # Add a dropdown menu to input the number of GLYK reactions
               numericInput(inputId = "glyk_rxn", 
                            label = "Desired number of GLYK reactions:", 
                            value = 1),
               # Add a dropdown menu to input the volume per GLYK reactions
               numericInput(inputId = "glyk_vol", 
                            label = "Reaction volume (uL):", 
                            value = 200),
               # Add a dropdown menu to input the volume addition of Crude Protein Extract 
               numericInput(inputId = "cpe.glyk", 
                            label = "Crude protein extract volume (uL):", 
                            value = 4),
               # Add a dropdown menu to input Molarity of substrate
               numericInput(inputId = "glycerate_M", 
                            label = "Glycerate (mol/L; M):", 
                            value = 0.5)
             ),
             # Define the main panel for displaying the output
             mainPanel(
               # Add "substrate" header
               h3("Glycerate (substrate)"),
               # Add a place to display the substrate volume requirement
               verbatimTextOutput("glycerate"),
               # Add "Coupling Enzyme" header
               h3("Coupling Enzyme"),
               # Add a place to display the coupling enzyme volume requirement
               verbatimTextOutput("couplingEnzyme.glyk"),
               # Add "reaction buffer" header
               h3("Reaction Buffer"),
               # Add a place to display the Reaction Buffer recipe
               verbatimTextOutput("GKRB")
             )
           ))
  )
)
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  
  # Display the molecular weight of the chemical of interest
  output$mw <- renderText({
    molecularweight <- Chemicals$MolecularWeight[Chemicals$Name == input$chemical]
    paste("The molecular weight of", input$chemical, "is", molecularweight, "(g/mol)")
  })
  
  # Display the grams needed in the solution
  output$grams <- renderText({
    # Unit conversion for volume 
    if(input$volume_units == "uL") {
      volume.answer <- input$volume / 1000000
    } else if(input$volume_units == "mL") {
      volume.answer <- input$volume / 1000
    } else {
      volume.answer <- input$volume
    }
    # Calculate grams needed with correct volume units
    grams <- input$molarity * volume.answer * Chemicals$MolecularWeight[Chemicals$Name == input$chemical] 
    # Unit conversion for grams 
    grams.answer <- if(input$magnitude == "ug") {
      grams.corr <- grams * 1000000
    } else if(input$magnitude == "mg") {
      grams.corr <- grams * 1000
    } else {
      grams.corr <- grams
    }
    # Only allow for two significant digits   
    grams.r <- round(x = grams.corr, digits = 2)
    # Paste the output 
    paste("You need", grams.r, input$magnitude, "of", input$chemical, "in your solution.")
  })
  
  # Display the Recipe
  output$recipe <- renderText({
    # Unit conversion for volume 
    if(input$volume_units == "uL") {
      volume.answer <- input$volume / 1000000
    } else if(input$volume_units == "mL") {
      volume.answer <- input$volume / 1000
    } else {
      volume.answer <- input$volume
    }
    # Calculate grams needed with correct volume units
    grams <- input$molarity * volume.answer * Chemicals$MolecularWeight[Chemicals$Name == input$chemical] 
    # Unit conversion for grams 
    grams.answer <- if(input$magnitude == "ug") {
      grams.corr <- grams * 1000000
    } else if(input$magnitude == "mg") {
      grams.corr <- grams * 1000
    } else {
      grams.corr <- grams
    }
    # Only allow for two significant digits   
    grams.r <- round(x = grams.corr, digits = 2)
    # Paste the output
    paste("Using an appropriate sized beaker:", "\n",
          "1) add ~80% of the", input$volume, input$volume_units, "H2O, and begin stirring", "\n", 
          "2) add ", grams.r, input$magnitude, "of", input$chemical, "\n",
          "3) wait until the", input$chemical, "dissolves", "\n", 
          "4) (Optional) adjust pH if needed,", "\n",
          "5) finally, bring", input$chemical, "solution to volume by adding the rest of the H2O")
  })
  
  # Display the number of sample that can be extracted  
  output$extraction <- renderText({
    # Paste the output
    paste("These recipe will allow you to extract crude protein from", input$extraction, "samples")
  })
  
  # Display the recipoe for the extraction buffer
  output$EB.recipe <- renderText({
    EB.vol <- input$extraction * input$EBperRXN
    EPPS.EB <- (EB.vol * Extraction$BufferM[Extraction$Name == "EPPS"]) / Extraction$StockM[Extraction$Name == "EPPS"]
    EDTA.EB <- (EB.vol * Extraction$BufferM[Extraction$Name == "EDTA"]) / Extraction$StockM[Extraction$Name == "EDTA"]
    TritonX.EB <- (EB.vol * Extraction$BufferM[Extraction$Name == "TritonX"]) / Extraction$StockM[Extraction$Name == "TritonX"]
    PVPP.EB <- EB.vol * 0.01 
    DTT.EB <- (EB.vol * Extraction$BufferM[Extraction$Name == "DTT"]) / Extraction$StockM[Extraction$Name == "DTT"]
    DTT.g <- round(x = (Extraction$StockM[Extraction$Name == "DTT"] * DTT.EB * Chemicals$MolecularWeight[Chemicals$Name == "DTT"]) / 1000, digits = 2)
    H2O.EB <- EB.vol - (EPPS.EB + EDTA.EB + TritonX.EB + DTT.EB) + DTT.EB
    PI.EB <- 20 # Recipe from Ludmila
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", DTT.g, "mg of DTT, and", PVPP.EB, "mg of PVPP directly into the tube,", "\n",
          "2) pour", EPPS.EB, "uL of", Extraction$StockM[Extraction$Name == "EPPS"], "M EPPS [pH 8.1],", "\n",
          "3)", EDTA.EB, "uL of", Extraction$StockM[Extraction$Name == "EDTA"], "M EDTA,", "\n",
          "4)", H2O.EB, "uL of H2O,", "\n",
          "5) next, invert/vortex until the DTT dissolves (the PVPP will not dissolve),", "\n",
          "6) finally, add", TritonX.EB, "uL of", Extraction$StockM[Extraction$Name == "TritonX"], "% TritonX,", "\n",
          "7) store on ice", "\n",
          "8) Remember to add", PI.EB,"uL of Protease Inhibitor before you start extracting!")
  })
  
  # Display the recipe for the initial activator 
  output$initialActivator <- renderText({
    CPE <- input$cpe.rubisco * input$rubisco_rxn
    MgCl2_IA <- (CPE * Rubisco$desiredM[Rubisco$Name == "MgCl2_IA"]) / Rubisco$StockM[Rubisco$Name == "MgCl2_IA"]
    NaHCO3_IA <- (CPE * Rubisco$desiredM[Rubisco$Name == "NaHCO3_IA"]) / Rubisco$StockM[Rubisco$Name == "NaHCO3_IA"]
    # Paste the output
    paste("Add the following to", input$samples, "appropriate sized tube:", "\n",
          "1) You will need", MgCl2_IA/input$samples, "uL of 1M MgCl2 per tube", "\n",
          "2) You will need", NaHCO3_IA/input$samples, "uL of 0.5M NaHCO3 per tube", "\n",
          "3) You will need", round(CPE/input$samples,2), "uL of Crude Protein Extract per tube", "\n")
  })
  
  # Calculate and display the recipe for the RuBP (substrate)
  output$rubp <- renderText({
    RuBP <- ((Rubisco$desiredM[Rubisco$Name == "RuBP"] * input$rubisco_vol) / input$rubp_M) * input$rubisco_rxn
    RuBP.r <- round(x=RuBP, digits = 2) 
    # Paste the output
    paste("You will need", RuBP.r, "uL for", input$rubisco_rxn, "RUBISCO reactions")
  })
  
  # Calculate and display and the recipe for the coupling enzyme rubisco
  output$couplingEnzyme.rubisco <- renderText({
    CE <- (0.02 * input$rubisco_vol) * input$rubisco_rxn
    # Paste the output
    paste("You will need", CE, "uL for", input$rubisco_rxn, "RUBISCO reactions")
  })
  
  # Calculate and display the recipe for the rubisco reaction buffer
  output$RRB <- renderText({
    CPE <- input$cpe.rubisco * input$rubisco_rxn
    CE <- (0.02 * input$rubisco_vol) * input$rubisco_rxn
    RuBP <- ((Rubisco$desiredM[Rubisco$Name == "RuBP"] * input$rubisco_vol) / input$rubp_M) * input$rubisco_rxn
    RuBP.r <- round(x=RuBP, digits = 2) 
    HEPES.RRB <- ((Rubisco$desiredM[Rubisco$Name == "HEPES"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "HEPES"]) * input$rubisco_rxn 
    MgCl2.RRB <- ((Rubisco$desiredM[Rubisco$Name == "MgCl2"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "MgCl2"]) * input$rubisco_rxn 
    EDTA.RRB <- ((Rubisco$desiredM[Rubisco$Name == "EDTA"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "EDTA"]) * input$rubisco_rxn 
    ATPCP.RRB <- ((Rubisco$desiredM[Rubisco$Name == "ATP/CP"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "ATP/CP"]) * input$rubisco_rxn 
    NaHCO3.RRB <- ((Rubisco$desiredM[Rubisco$Name == "NaHCO3"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "NaHCO3"]) * input$rubisco_rxn 
    NADH.RRB <- ((Rubisco$desiredM[Rubisco$Name == "NADH"] * input$rubisco_vol) / Rubisco$StockM[Rubisco$Name == "NADH"]) * input$rubisco_rxn 
    H2O.RRB <- (input$rubisco_vol * input$rubisco_rxn) - sum(HEPES.RRB, MgCl2.RRB, EDTA.RRB, ATPCP.RRB, NaHCO3.RRB, NADH.RRB) - (CPE + RuBP.r + CE)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", HEPES.RRB, "uL of 0.25 Μ HEPES [pH 7.8]", "\n",
          "2)", MgCl2.RRB, "uL of 1M MgCl2", "\n",
          "3)", EDTA.RRB, "uL of 0.5M EDTA", "\n",
          "4)", ATPCP.RRB, "uL of 0.01M ATP / 0.05M CP", "\n",
          "5)", NaHCO3.RRB, "uL of 0.5M NaHCO3", "\n",
          "6)", NADH.RRB, "uL of 0.04M NADH", "\n",
          "7)", H2O.RRB, "uL of H2O", "\n",
          "Invert a few times to mix, and store at the assay temperature")
  })
  output$PGPRB <- renderText({
    CPE <- input$cpe.pgp * input$pgp_rxn
    HEPES.PGPRB <- ((PhosphoglycolatePhosphatase$desiredM[PhosphoglycolatePhosphatase$Name == "HEPES"] * input$pgp_vol) / PhosphoglycolatePhosphatase$StockM[PhosphoglycolatePhosphatase$Name == "HEPES"]) * input$pgp_rxn 
    MgCl2.PGPRB <- ((PhosphoglycolatePhosphatase$desiredM[PhosphoglycolatePhosphatase$Name == "MgCl2"] * input$pgp_vol) / PhosphoglycolatePhosphatase$StockM[PhosphoglycolatePhosphatase$Name == "MgCl2"]) * input$pgp_rxn 
    EDTA.PGPRB <- ((PhosphoglycolatePhosphatase$desiredM[PhosphoglycolatePhosphatase$Name == "EDTA"] * input$pgp_vol) / PhosphoglycolatePhosphatase$StockM[PhosphoglycolatePhosphatase$Name == "EDTA"]) * input$pgp_rxn 
    
    H2O.PGPRB <- (input$pgp_vol * input$pgp_rxn) - sum(HEPES.PGPRB, MgCl2.PGPRB, EDTA.PGPRB) - (CPE)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", HEPES.PGPRB, "uL of 0.25 Μ HEPES [pH 7.5]", "\n",
          "2)", MgCl2.PGPRB, "uL of 1M MgCl2", "\n",
          "3)", EDTA.PGPRB, "uL of 0.5M EDTA", "\n",
          "4)", H2O.PGPRB, "uL of H2O", "\n",
          "Invert a few times to mix, and store at the assay temperature")
  })
  
  output$glycolicacid <- renderText({
    GlycolicAcid <- ((GlycolateOxidase$desiredM[GlycolateOxidase$Name == "GlycolicAcid"] * input$go_vol) / input$glycolicacid_M) * input$go_rxn
    GlycolicAcid.r <- round(x=GlycolicAcid, digits = 2) 
    # Paste the output
    paste("You will need", GlycolicAcid.r, "uL for", input$go_rxn, "GO reactions")
  })
  
  output$GORB <- renderText({
    CPE <- input$cpe.go * input$go_rxn
    GlycolicAcid <- ((GlycolateOxidase$desiredM[GlycolateOxidase$Name == "GlycolicAcid"] * input$go_vol) / input$glycolicacid_M) * input$go_rxn
    GlycolicAcid.r <- round(x=GlycolicAcid, digits = 2) 
    
    Phenylhydrazine.GORB <- ((GlycolateOxidase$desiredM[GlycolateOxidase$Name == "Phenylhydrazine"] * input$go_vol) / GlycolateOxidase$StockM[GlycolateOxidase$Name == "Phenylhydrazine"]) * input$go_rxn 
    Riboflavin.GORB <- ((GlycolateOxidase$desiredM[GlycolateOxidase$Name == "Riboflavin"] * input$go_vol) / GlycolateOxidase$StockM[GlycolateOxidase$Name == "Riboflavin"]) * input$go_rxn 
    KPhosphateBuffer.GORB <- ((GlycolateOxidase$desiredM[GlycolateOxidase$Name == "K-PhosphateBuffer"] * input$go_vol) / GlycolateOxidase$StockM[GlycolateOxidase$Name == "K-PhosphateBuffer"]) * input$go_rxn 
    H2O.GORB <- (input$go_vol * input$go_rxn) - sum(Phenylhydrazine.GORB, Riboflavin.GORB, KPhosphateBuffer.GORB) - (CPE + GlycolicAcid.r)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", KPhosphateBuffer.GORB, "uL of 0.5 Μ K-Phosphate Buffer [pH 8.1]", "\n",
          "2)", H2O.GORB, "uL of H2O", "\n",
          "3)", Phenylhydrazine.GORB, "uL of 0.110M Phenylhydrazine", "\n",
          "4)", Riboflavin.GORB, "uL of 0.0013M Riboflavin", "\n",
          "Invert a few times to mix, and store at the assay temperature")
  })
  
  # Calculate and display the recipe for the coupling enzyme glyK 
  output$h2o2 <- renderText({
    # Paste the output
    paste("To make 10 mL stock of 30 mM H2O2, add:", "\n",
          "1) 9.966 mL of 0.05M K-Phosphate Buffer [pH 8.1]", "\n",
          "2) 34 uL of 30% H2O2 stock")
  })
  
  output$KPB.CATRB <- renderText({
    KPhosphateBuffer.CATRB <- ((Catalase$desiredM[Catalase$Name == "K-PhosphateBuffer"] * input$cat_vol) / Catalase$StockM[Catalase$Name == "K-PhosphateBuffer"]) * input$cat_rxn 
    CPE <- input$cpe.cat * input$cat_rxn
    H2O2 <- 34 * input$cat_rxn
    H2O.CATRB <- (input$cat_vol*input$cat_rxn) - (KPhosphateBuffer.CATRB + H2O2 + CPE)
    # Paste the output
    paste("To create 0.05M K-Phosphate Buffer, add:", "\n",
          "1)", KPhosphateBuffer.CATRB, "uL of 0.5M K-Phosphate Buffer [pH 8.1]", "\n",
          "2)", H2O.CATRB, "uL of H2O")
  })
  
  output$glutamate <- renderText({
    Glutamate <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "Glutamate"] * input$ggat_vol) / input$glutamate) * input$ggat_rxn 
    Glutamate.r <- round(x=Glutamate, digits = 2) 
    # Paste the output
    paste("You will need", Glutamate.r, "uL for", input$ggat_rxn, "GGAT reactions")
  })
  
  output$glyoxylate.ggat <- renderText({
    Glyoxylate <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "Glyoxylate"] * input$ggat_vol) / input$glyoxylate.ggat) * input$ggat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    # Paste the output
    paste("You will need", Glyoxylate.r, "uL for", input$ggat_rxn, "GGAT reactions")
  })
  
  output$glutamicDH <- renderText({
    GlutamicDH <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "GlutamicDH"] * input$ggat_vol) / input$glutamicDH) * input$ggat_rxn
    GlutamicDH.r <- round(x=GlutamicDH, digits = 2)
    # Paste the output
    paste("You will need", GlutamicDH.r, "uL for", input$ggat_rxn, "GGAT reactions")
  })
  
  output$KPB.GGATRB <- renderText({
    CPE <- input$cpe.ggat * input$ggat_rxn
    
    Glutamate <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "Glutamate"] * input$ggat_vol) / input$glutamate) * input$ggat_rxn 
    Glutamate.r <- round(x=Glutamate, digits = 2) 
    Glyoxylate <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "Glyoxylate"] * input$ggat_vol) / input$glyoxylate.ggat) * input$ggat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    GlutamicDH <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "GlutamicDH"] * input$ggat_vol) / input$glutamicDH) * input$ggat_rxn
    GlutamicDH.r <- round(x=GlutamicDH, digits = 2)
    
    KPhosphateBuffer.GGATRB <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "K-PhosphateBuffer"] * input$ggat_vol) / GGAminotransferase$StockM[GGAminotransferase$Name == "K-PhosphateBuffer"]) * input$ggat_rxn 
    NADH.GGATRB <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "NADH"] * input$ggat_vol) / GGAminotransferase$StockM[GGAminotransferase$Name == "NADH"]) * input$ggat_rxn 
    P5P.GGATRB <- ((GGAminotransferase$desiredM[GGAminotransferase$Name == "Pyridoxal5Phosphate"] * input$ggat_vol) / GGAminotransferase$StockM[GGAminotransferase$Name == "Pyridoxal5Phosphate"]) * input$ggat_rxn 
    H2O.GGATRB <- (input$ggat_vol * input$ggat_rxn) - sum(KPhosphateBuffer.GGATRB, NADH.GGATRB, P5P.GGATRB) - (CPE+Glutamate.r+Glyoxylate.r+GlutamicDH.r)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", KPhosphateBuffer.GGATRB, "uL of 0.5M K-Phosphate Buffer", "\n",
          "5)", NADH.GGATRB, "uL of 0.04M NADH", "\n",
          "5)", P5P.GGATRB, "uL of 0.02M P5P", "\n",
          "6)", H2O.GGATRB, "uL of H2O")
  })
  
  output$alanine <- renderText({
    Alanine <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "Alanine"] * input$agat_vol) / input$alanine) * input$agat_rxn 
    Alanine.r <- round(x=Alanine, digits = 2) 
    # Paste the output
    paste("You will need", Alanine.r, "uL for", input$agat_rxn, "AGAT reactions")
  })
  
  output$glyoxylate.agat <- renderText({
    Glyoxylate <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "Glyoxylate"] * input$agat_vol) / input$glyoxylate.agat) * input$agat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    # Paste the output
    paste("You will need", Glyoxylate.r, "uL for", input$agat_rxn, "AGAT reactions")
  })
  
  output$lactateDH <- renderText({
    LactateDH <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "LactateDH"] * input$agat_vol) / input$lactateDH) * input$agat_rxn
    LactateDH.r <- round(x=LactateDH, digits = 2)
    # Paste the output
    paste("You will need", LactateDH.r, "uL for", input$agat_rxn, "AGAT reactions")
  })
  
  output$KPB.AGATRB <- renderText({
    CPE <- input$cpe.agat * input$agat_rxn

    Alanine <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "Alanine"] * input$agat_vol) / input$alanine) * input$agat_rxn 
    Alanine.r <- round(x=Alanine, digits = 2)
    Glyoxylate <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "Glyoxylate"] * input$agat_vol) / input$glyoxylate.agat) * input$agat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    LactateDH <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "LactateDH"] * input$agat_vol) / input$lactateDH) * input$agat_rxn
    LactateDH.r <- round(x=LactateDH, digits = 2)

    KPhosphateBuffer.AGATRB <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "K-PhosphateBuffer"] * input$agat_vol) / AGAminotransferase$StockM[AGAminotransferase$Name == "K-PhosphateBuffer"]) * input$agat_rxn
    NADH.AGATRB <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "NADH"] * input$agat_vol) / AGAminotransferase$StockM[AGAminotransferase$Name == "NADH"]) * input$agat_rxn
    P5P.AGATRB <- ((AGAminotransferase$desiredM[AGAminotransferase$Name == "Pyridoxal5Phosphate"] * input$agat_vol) / AGAminotransferase$StockM[AGAminotransferase$Name == "Pyridoxal5Phosphate"]) * input$agat_rxn
    H2O.AGATRB <- (input$agat_vol * input$agat_rxn) - sum(KPhosphateBuffer.AGATRB, NADH.AGATRB, P5P.AGATRB) - (CPE+Alanine.r+Glyoxylate.r+LactateDH.r)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", KPhosphateBuffer.AGATRB, "uL of 0.5M K-Phosphate Buffer", "\n",
          "5)", NADH.AGATRB, "uL of 0.04M NADH", "\n",
          "5)", P5P.AGATRB, "uL of 0.02M P5P", "\n",
          "6)", H2O.AGATRB, "uL of H2O")
  })

  output$serine <- renderText({
    Serine <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "Serine"] * input$sgat_vol) / input$serine) * input$sgat_rxn 
    Serine.r <- round(x=Serine, digits = 2) 
    # Paste the output
    paste("You will need", Serine.r, "uL for", input$sgat_rxn, "SGAT reactions")
  })
  
  output$glyoxylate.sgat <- renderText({
    Glyoxylate <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "Glyoxylate"] * input$sgat_vol) / input$glyoxylate.sgat) * input$sgat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    # Paste the output
    paste("You will need", Glyoxylate.r, "uL for", input$sgat_rxn, "SGAT reactions")
  })
  
  output$KPB.SGATRB <- renderText({
    CPE <- input$cpe.sgat * input$sgat_rxn
    
    Serine <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "Serine"] * input$sgat_vol) / input$serine) * input$sgat_rxn 
    Serine.r <- round(x=Serine, digits = 2) 
    Glyoxylate <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "Glyoxylate"] * input$sgat_vol) / input$glyoxylate.sgat) * input$sgat_rxn 
    Glyoxylate.r <- round(x=Glyoxylate, digits = 2) 
    
    #### ADD HPR1 to the equations here and below for H2O ####
    
    
    KPhosphateBuffer.SGATRB <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "K-PhosphateBuffer"] * input$sgat_vol) / SGAminotransferase$StockM[SGAminotransferase$Name == "K-PhosphateBuffer"]) * input$sgat_rxn
    NADH.SGATRB <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "NADH"] * input$sgat_vol) / SGAminotransferase$StockM[SGAminotransferase$Name == "NADH"]) * input$sgat_rxn
    P5P.SGATRB <- ((SGAminotransferase$desiredM[SGAminotransferase$Name == "Pyridoxal5Phosphate"] * input$sgat_vol) / SGAminotransferase$StockM[SGAminotransferase$Name == "Pyridoxal5Phosphate"]) * input$agat_rxn
    H2O.SGATRB <- (input$sgat_vol * input$sgat_rxn) - sum(KPhosphateBuffer.SGATRB, NADH.SGATRB, P5P.SGATRB) - (CPE+Serine.r+Glyoxylate.r+1) 
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", KPhosphateBuffer.SGATRB, "uL of 0.5M K-Phosphate Buffer", "\n",
          "5)", NADH.SGATRB, "uL of 0.04M NADH", "\n",
          "5)", P5P.SGATRB, "uL of 0.02M P5P", "\n",
          "6)", H2O.SGATRB, "uL of H2O")
  })
  
  output$na.b.hp <- renderText({
    NaBHP <- ((HydroxypyruvateReductase$desiredM[HydroxypyruvateReductase$Name == "Na-B-Hydroxypyruvate"] * input$hpr_vol) / input$NaBHP_M) * input$hpr_rxn
    NaBHP.r <- round(x=NaBHP, digits = 2) 
    H2O.NaBHP <- NaBHP.r
    # Paste the output
    paste("Using an appropriate sized Eppendorf tube, add:", "\n",
          "1)", NaBHP.r, "uL of 0.025M Na-b-HP", "\n",
          "2)", H2O.NaBHP, "uL of H2O", "\n",
          "This will give you", NaBHP.r + H2O.NaBHP, "uL of 0.25M of Na-B-HP")
  })
  
  output$KPB.HPRRB <- renderText({
    CPE <- input$cpe.hpr * input$hpr_rxn
    NaBHP <- ((HydroxypyruvateReductase$desiredM[HydroxypyruvateReductase$Name == "Na-B-Hydroxypyruvate"] * input$hpr_vol) / input$NaBHP_M) * input$hpr_rxn
    NaBHP.r <- round(x=NaBHP, digits = 2) 
    H2O.NaBHP <- NaBHP.r
    KPhosphateBuffer.HPRRB <- ((HydroxypyruvateReductase$desiredM[HydroxypyruvateReductase$Name == "K-PhosphateBuffer"] * input$hpr_vol) / HydroxypyruvateReductase$StockM[HydroxypyruvateReductase$Name == "K-PhosphateBuffer"]) * input$hpr_rxn 
    KPhosMONO <- KPhosphateBuffer.HPRRB * 0.5
    KPhosDI <- KPhosphateBuffer.HPRRB * 0.5
    NADH.HPRRB <- ((HydroxypyruvateReductase$desiredM[HydroxypyruvateReductase$Name == "NADH"] * input$hpr_vol) / HydroxypyruvateReductase$StockM[HydroxypyruvateReductase$Name == "NADH"]) * input$hpr_rxn 
    H2O.HPRRB <- (input$hpr_vol * input$hpr_rxn) - sum(KPhosphateBuffer.HPRRB, NADH.HPRRB) - (CPE + NaBHP.r + H2O.NaBHP)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", KPhosphateBuffer.HPRRB, "uL of 0.5M K-Phosphate Buffer", "\n",
          "5)", NADH.HPRRB, "uL of 0.04M NADH", "\n",
          "6)", H2O.HPRRB, "uL of H2O")
  })
  
  output$glycerate <- renderText({
    Glycerate <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "Glycerate"] * input$glyk_vol) / input$glycerate_M) * input$glyk_rxn
    Glycerate.r <- round(x=Glycerate, digits = 2) 
    # Paste the output
    paste("You will need", Glycerate.r, "uL for", input$go_rxn, "GLYK reactions")
  })
  
  # Calculate and display the recipe for the coupling enzyme glyK 
  output$couplingEnzyme.glyk <- renderText({
    CE <- (0.02 * input$glyk_vol) * input$glyk_rxn
    # Paste the output
    paste("You will need", CE, "uL for", input$glyk_rxn, "GLYCERATE KINASE reactions")
  })
  
  # Calculate and display the recipe for the glycerate kinase reaction buffer
  output$GKRB <- renderText({
    CPE <- input$cpe.glyk * input$glyk_rxn
    CE <- (0.02 * input$glyk_vol) * input$glyk_rxn
    Glycerate <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "Glycerate"] * input$glyk_vol) / input$glycerate_M) * input$glyk_rxn
    Glycerate.r <- round(x=Glycerate, digits = 2) 
    HEPES.GKRB <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "HEPES"] * input$glyk_vol) / GlycerateKinase$StockM[GlycerateKinase$Name == "HEPES"]) * input$glyk_rxn 
    MgCl2.GKRB <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "MgCl2"] * input$glyk_vol) / GlycerateKinase$StockM[GlycerateKinase$Name == "MgCl2"]) * input$glyk_rxn 
    KCl.GKRB <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "KCl"] * input$glyk_vol) / GlycerateKinase$StockM[GlycerateKinase$Name == "KCl"]) * input$glyk_rxn 
    ATPCP.GKRB <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "ATP/CP"] * input$glyk_vol) / GlycerateKinase$StockM[GlycerateKinase$Name == "ATP/CP"]) * input$glyk_rxn 
    NADH.GKRB <- ((GlycerateKinase$desiredM[GlycerateKinase$Name == "NADH"] * input$glyk_vol) / GlycerateKinase$StockM[GlycerateKinase$Name == "NADH"]) * input$glyk_rxn 
    H2O.GKRB <- (input$glyk_vol * input$glyk_rxn) - sum(HEPES.GKRB, MgCl2.GKRB, KCl.GKRB, ATPCP.GKRB, NADH.GKRB) -(CPE + Glycerate.r + CE)
    # Paste the output
    paste("Using an appropriate sized falcon tube, add:", "\n",
          "1)", HEPES.GKRB, "uL of 0.25 Μ HEPES [pH 7.8]", "\n",
          "2)", MgCl2.GKRB, "uL of 1M MgCl2", "\n",
          "3)", KCl.GKRB, "uL of 4M KCl", "\n",
          "4)", ATPCP.GKRB, "uL of 0.01M ATP / 0.05M CP", "\n",
          "5)", NADH.GKRB, "uL of 0.04M NADH", "\n",
          "6)", H2O.GKRB, "uL of H2O", "\n",
          "Invert a few times to mix, and store at the assay temperature")
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


