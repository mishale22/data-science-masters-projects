# Cyber Security Course In FutureLearn Provider Analysis

### Libraries

For this project, we are using the following libraries: dplyr for data
preprocessing, ggplot2 and ggpubr for creating the plots.

You have to make sure that these libraries (reshape2, plyr, tidyverse,
stringr, lubridate, ggplot2, ggpubr) are installed. If any library is
missing, you can install it with the following command:

```         
install.packages("{library_name}")
```

### Requirements

-   IDE to run the analysis. Ideally, RStudio is recommended.

### Folder Structure Of The Project

The folders used in the project and what they include are listed below:

-   **config:** In this folder we have the configuration of
    ProjectTemplate to run the analysis. The libraries mentioned
    previously are listed in this file.
-   **data:** Here is the raw data from FutureLearn. Including CSV and
    PDF files.
-   **munge:** In this folder there is the data preprocessing file
    `01-Enrolments.R` in which both cycles were managed.
-   **reports:** This folder has the R Markdown file
    (`AnalysisReport.Rmd`) which generates the PDF file
    (`AnalysisReport.pdf`) with the analysis report. Also, a slides presentation with the findings (`FutureLearn Presentation EDA.pdf`) 
-   **src:** In this folder we have a script that generates the plots to
    be used in the report `AnalysisReport.Rmd`.

### Steps To Run The Analysis

In this project we are using R Markdown to knit the analysis report,
thus, you need to follow the following steps in RStudio to run it in
your machine.

1.  Install the package below in order to generate the PDF file with the
    analysis:

```         
install.packages("tinytex")
```

2.  Open the `AnalysisReport.Rmd` to knit it. This file is located under
    the `reports` folder.

3.  Click `Knit` button.
