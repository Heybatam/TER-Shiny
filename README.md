# Consensus clustering Shiny app

This Shiny application, based on the diceR package, provides an user interface to perform clustering algorithms such as : Kmeans, GMM, Spectral clustering and Affinity propagation on a given dataset. 

And also perform consensus clustering will be performed depend on different input.


## Installation

Use the IDE [Rstudio](https://rstudio.com/) to install the required packages.
The following command will check, if is not present install it, and load the package.


```r
list.of.packages <- c("shiny",
                      "ggplot2",
                      "DT",
                      "GGally",
                      "psych",
                      "Hmisc",
                      "mclust",
                      "kernlab",
                      "MASS",
                      "factoextra",
                      "apcluster",
                      "knitr",
                      "kableExtra",
                      "diceR")

# Load and install packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

## Usage
When you download the repository set the path as the working directory.
```r
setwd()
```
Afterward compile the 'app.r' file :

```r
library(shiny)
source('ui.R', local = TRUE)
source('server.R')


shinyApp(
  ui = ui,
  server = server
)


```

With the interfac you can:
  - Upload data, use PCA and visualize data
  - Check different clustering algorithms (Kmeans, GMM, SC, AP)
  - Perform consensus clustering with different input (nk, reps, p.item, consensus functions)

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
