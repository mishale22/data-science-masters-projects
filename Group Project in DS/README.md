# Group Project in Data Science - CSC8633

Student Name              
--------------------------------------
Sandra M Nino             
Alexantros Tamboutsiaris  
Lihao Du                  
Jianfei Xu                
Feijing Chen              
George Snape              
Rongwei Zhang             
Jones Samuel Kirubakaran  

# Guide to Project Execution and Deliverable Locations

## Instructions how to run the Project

Before executing the project's code, it's essential to ensure that the ProjectTemplate package is installed. For installation, execute the following in R:

```r
install.packages(ProjectTemplate)
```

Once installed, incorporate the ProjectTemplate library into your session and initiate your R project by employing the load.project() function. This action will seamlessly integrate your project's data, functions, and necessary scripts:

```r
library(ProjectTemplate)
migrate.project()
load.project()
```

Please make sure the requisite R packages for data handling and visualization (`knitr`, `dplyr`, `ggplot2`, and others) are present. Should they be missing, install them with the commands listed below:

```r
install.packages("knitr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("zoo")
install.packages("plotly")
install.packages("gridExtra")
install.packages("readxl")
install.packages("scales")
install.packages("readr")
```

## Deliverable Files and Their Locations

Access project deliverables in the following directories:

- Report (Pdf file): `WaterUsageProject/reports/report.pdf`

- Report (Rmd file): `WaterUsageProject/reports/report.Rmd`

- Data Preprocessing: `WaterUsageProject/munge/03-prep_aggregated_house.R`
                      `WaterUsageProject/munge/03-prep_bidet.R`
                      `WaterUsageProject/munge/03-prep_kitchen_faucet.R`
                      `WaterUsageProject/munge/03-prep_pred.R`
                      `WaterUsageProject/munge/03-prep_shower.R`
                      `WaterUsageProject/munge/03-prep_sink.R`
                      `WaterUsageProject/munge/03-prep_washing.R`

- Modeling: `WaterUsageProject/munge/04-model_aggregated_house.R`
            `WaterUsageProject/munge/04-model_bidet.R`
            `WaterUsageProject/munge/04-model_kitchen_faucet.R`
            `WaterUsageProject/munge/04-model_shower.R`
            `WaterUsageProject/munge/04-model_sink.R`
            `WaterUsageProject/munge/04-model_washing.R`

- Libraries: `WaterUsageProject/config/global.dcf`





