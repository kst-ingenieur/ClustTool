if (!("shiny" %in% rownames(installed.packages()))){install.packages("shiny", repos="http://cran.rstudio.com/")}
library(shiny)
ui <- fluidPage()
server <- function(input, output){}
runApp(launch.browser = T)
