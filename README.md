# shinysurvival

This shiny app is aimed to perform survival analysis using Kaplan-Meier and COX regression models. 

The app requires a .xlsx or .csv file with the following specifications.

1. Each row is one patient/sample.
2. One column specifying numerical survival. This can be in days/months/years. Please note that this should be a numerical value, not a date!
3. One column specifying if the event (in many cases death) has occoured or not as a binary 0 or 1 value. For example, Censored = 0, Death = 1.
4. Atleast one column specifying some numerical biomarker measurement. Make sure that this measurement is correctly normalised before analysing it in this app.

Optional requirements: 
5. One or more clinical columns. This can be things as Age (numerical), Smoking status (Nominal), Tumor stage (Categorical) etc. The app is built to recognize numerical, nominal and categorical values.

# How to launch the application 

1. Open a new script in Rstudio.
2. Paste and run the following code:

```r
library(shiny)

runGitHub(repo = "shinysurvival", user = "robban13", ref = "main")
```
3. The code for the application will be downloaded and the app will be initialized. Happy analysis! 


# Usage 

1. Launch application.
2. Upload your .xlsx or .csv file that fulfills the specifications above, or click the "Load Mock Patient Data" button to load the included `mock_patient_data.csv` example dataset. If you upload an Excel file with multiple sheets, select the sheet in the dropdown menu.
3. Select which column contains the event column, numerical survival data.
4. Select your main variable to analyse:
   4.1 If it is a numerical biomarker value, make sure to tick the "Divide Patients into Groups Based on Biomarker Expression" box. This will stratify the patients into two groups, "High and Low".
   4.2 If your main variable is a clinical parameter such as "Tumor stage" with the levels c(I, II, III, IVA, IVB, IVC), disable the "Divide Patients into Groups Based on Biomarker Expression".
You can now inspect a univariate cox regression model and a Kaplan-Meier plot.

5. (Optional): To create multivariate COX models, you need to specify atleast one co-variable. These can be both numerical, nominal or categorical variables.
   NOTE! Kaplan-Meier plots can not do multivariate analysis per definition. If you need to account for important clinical parameters in your survival analysis, COX models are your choice.
6. (Optional): The app includes three tabs, "Data inspection", "Grouping verification" and "Variable distribution" for quick QC check of your data to make sure that they are correctly distributed. For convinence, a couple of common outlier removal and variable transformation options are available in the side menu if needed.
7. (Optional): There are global settings to change the look of the plots in both the side menu and plot specific settings under each plot.

# Please cite correctly if you used this application for any publication! 
This application only provides a user interface for easier survival analysis by combining existing libraries. Please make sure to cite them correctly. 
