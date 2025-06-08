# PhotorespiratoryAssay_Recipes
This app is a web-based tool that allows researchers to automate the solution, extraction buffer, and enzyme activity assay preparation. The app is built on the R programming language and the Shiny web framework.

The app is hosted at [https://l-gregory.shinyapps.io/photorespiratoryassay_recipes](https://l-gregory.shinyapps.io/photorespiratoryassay_recipes-main/) with 25 active hours per month.

## Getting Started

### Prerequisite
To run the app locally, you need to have R (version 4.0.0 or higher) and RStudio installed on your computer. You also need to install the following packages:

tidyverse


You can install these packages by running the following command in R:

```{r]
install.packages(c("tidyverse"))
```

### Installation 
To install the app, you can download the code from the GitHub repository (do the following command in the terminal, after setting the desired directory):

```{r}
git clone https://github.com/L-gregory/PhotorespiratporyAssay_Recipes.git
```

If the script has run successfully, a new folder titled "PhotorespiratporyAssay_Recipes" should be located in the desired directory

### Running the App
Open the folder and open the ShinyApp.R file in RStudio. Click the **"Run App"** button in the top right corner of the script editor window. This will launch the app in a new window.

## Using the App
Navigate the various functions on the App:  
- Solution Prep tab: From the dropdown menu on the side panel select your chemical of interest. Enter the molarity and the volume for your desired solution. Adjust the volume and mass units as needed. 
- Extraction Buffer tab: Enter the number of crude protein extractions (i.e., samples) you want, and adjust thew total volume of Extraction Buffer per reaction if needed (default = 1 mL). 
- Enzyme Assay Prep tab: From the dropdown menu find Phosphoglycolate Phosphatase. 


## Credit
The App was developed by Luke Gregory under the supervision of Berkley Walker at Michigan State University.
