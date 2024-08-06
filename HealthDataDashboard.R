# Load required libraries
#install.packages("shiny")
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(DT)
library(knitr)
library(kableExtra)
library(summarytools)
library(maps)
# DUMMY DATA 
vitals <- read.csv("vitals.csv")
vitals$Name[85979]<- "No Known Vitals to report"

vitals<-as.data.frame(vitals)
#vitals<-vitals[-c(1:2, 4, 12:13)]

vitals <- vitals %>% mutate_all(~ na_if(., ""))

vitals <- vitals %>%
  mutate_at(vars(Name, Normalized.Name), tolower)

vitals <- vitals %>%
  group_by(Name) %>%
  mutate(
    Code = ifelse(is.na(Code), unique(Code[!is.na(Code)]), Code),
    Code.Set = ifelse(is.na(Code.Set), unique(Code.Set[!is.na(Code.Set)]), Code.Set),
    Normalized.Name = ifelse(is.na(Normalized.Name), unique(Normalized.Name[!is.na(Normalized.Name)]), Normalized.Name),
    Unit = ifelse(is.na(Unit), unique(Unit[!is.na(Unit)]), Unit)
    
  ) %>%
  ungroup()


vitals <- vitals %>%
  group_by(Code) %>%
  mutate(
    Name = ifelse(is.na(Name), unique(Code[!is.na(Name)]), Name),
    Unit = ifelse(is.na(Unit), unique(Unit[!is.na(Unit)]), Unit)
  ) %>%
  ungroup()


#vitals$Normalized.Name <- ifelse(is.na(vitals$Normalized.Name), vitals$Name, vitals$Normalized.Name)
vitals$Code[is.na(vitals$Code)] <- "Unknown"
vitals$Normalized.Name[is.na(vitals$Normalized.Name)] <- "Unknown"
vitals$Code.Set[is.na(vitals$Code.Set)] <- "Unknown"

vitals<-na.omit(vitals)
vitals$Date <- as.Date(substring(vitals$Date, 1, 10), format="%Y-%m-%d")
vitals$Value <- as.numeric(vitals$Value)
vitals<-na.omit(vitals)



# UNMAPPED VITALS
# DUMMY DATA
unmappedVitals <- read.csv("vitals_unmapped.csv")
unmappedVitals$Name[85979]<- "No Known Vitals to report"

unmappedVitals<-as.data.frame(unmappedVitals)
#unmappedVitals<-unmappedVitals[-c(1:2, 4, 12:13)]

unmappedVitals <- unmappedVitals %>% mutate_all(~ na_if(., ""))

unmappedVitals <- unmappedVitals %>%
  mutate_at(vars(Name, Normalized.Name), tolower)

unmappedVitals <- unmappedVitals %>%
  group_by(Name) %>%
  mutate(
    Code = ifelse(is.na(Code), unique(Code[!is.na(Code)]), Code),
    Code.Set = ifelse(is.na(Code.Set), unique(Code.Set[!is.na(Code.Set)]), Code.Set),
    Normalized.Name = ifelse(is.na(Normalized.Name), unique(Normalized.Name[!is.na(Normalized.Name)]), Normalized.Name),
    Unit = ifelse(is.na(Unit), unique(Unit[!is.na(Unit)]), Unit)
    
  ) %>%
  ungroup()


unmappedVitals <- unmappedVitals %>%
  group_by(Code) %>%
  mutate(
    Name = ifelse(is.na(Name), unique(Code[!is.na(Name)]), Name),
    Unit = ifelse(is.na(Unit), unique(Unit[!is.na(Unit)]), Unit)
  ) %>%
  ungroup()


#unmappedVitals$Normalized.Name <- ifelse(is.na(unmappedVitals$Normalized.Name), unmappedVitals$Name, unmappedVitals$Normalized.Name)
unmappedVitals$Code[is.na(unmappedVitals$Code)] <- "Unknown"
unmappedVitals$Normalized.Name[is.na(unmappedVitals$Normalized.Name)] <- "Unknown"
unmappedVitals$Code.Set[is.na(unmappedVitals$Code.Set)] <- "Unknown"

unmappedVitals<-na.omit(unmappedVitals)
unmappedVitals$Date <- as.Date(substring(unmappedVitals$Date, 1, 10), format="%Y-%m-%d")
unmappedVitals$Value <- as.numeric(unmappedVitals$Value)
unmappedVitals<-na.omit(unmappedVitals)

## DEMO
# DUMMY DATA
demo <-read.csv("demo.csv")

demo<-as.data.frame(demo)
#demo <- demo[, -c(1, 2, 4, 16)]
demo$Country.Address<- "USA"
demo$Religious.Affiliation[demo$Religious.Affiliation == ""] <- "Unknown"
demo$DOB <- as.Date(demo$DOB, format = "%Y-%m-%d")

#SOCIAL
# DUMMY DATA
social <- read.csv("socialHistory.csv")
#social <- social[, -c(1, 2, 4, 13, 14)]
social <- social %>% mutate_all(~na_if(., ""))
social$Code[is.na(social$Code)] <- "Unknown"
social$Start.Date <- as.Date(substring(social$Start.Date, 1, 10))
social$End.Date <- as.Date(substring(social$End.Date, 1, 10))

# MEDS
#DUMMY DATA
meds <- read.csv("meds.csv")

meds <- subset(meds, Resource.Type == "Medication")
#meds <- meds[, -c(1, 2, 4, 12, 13)]

meds <- meds %>%
  mutate_if(is.numeric, as.character)

# Replace all "" with NA in all columns
meds <- meds %>% mutate_all(~ na_if(., ""))

# Convert Dosage.Value back to numeric
meds$Dosage.Value <- as.numeric(meds$Dosage.Value)

# Convert Start.Date back to Date
meds$Start.Date <- as.Date(substring(meds$Start.Date, 1, 10))

meds <- meds %>%
  group_by(Name) %>%
  mutate(
    Code = ifelse(is.na(Code), unique(Code[!is.na(Code)]), Code),
    Code.Set = ifelse(is.na(Code.Set), unique(Code.Set[!is.na(Code.Set)]), Code.Set),
    Normalized.Name = ifelse(is.na(Normalized.Name), unique(Normalized.Name[!is.na(Normalized.Name)]), Normalized.Name),
    
  ) %>%
  ungroup()


meds <- meds %>%
  group_by(Code) %>%
  mutate(
    Name = ifelse(is.na(Name), unique(Code[!is.na(Name)]), Name),
  ) %>%
  ungroup()

meds$Code[is.na(meds$Code)] <- "Unknown"
meds$Name[is.na(meds$Name)] <- "Unknown"
meds$Normalized.Name[is.na(meds$Normalized.Name)] <- "Unknown"
meds$Code.Set[is.na(meds$Code.Set)] <- "Unknown"
meds$Instructions[is.na(meds$Instructions)] <- "Unknown"
meds$Indication.Description[is.na(meds$Indication.Description)] <- "Unknown"


#LABS
#DUMMY DATA
labs <- read.csv("labs.csv")

# Remove empty columns from dataframe
labs <- labs %>% select_if(~sum(!is.na(.)) > 0)

labs <- labs %>% mutate_all(~na_if(., ""))
#labs <- labs[, -c(1, 2, 4, 14)]

# Identify columns with encoding issues
bad_columns <- sapply(labs, function(x) if (is.character(x)) any(is.na(iconv(x, from = "", to = "UTF-8"))) else FALSE)

# Fix encoding issues only in problematic columns
labs_fixed <- labs %>%
  mutate(across(where(~ is.character(.x) && any(is.na(iconv(.x, from = "", to = "UTF-8")))), ~ iconv(.x, from = "", to = "UTF-8")))

# Apply the desired transformation to convert all character columns to lowercase
labs <- labs_fixed %>% mutate_if(is.character, tolower)



labs <- labs %>%
  group_by(Name) %>%
  mutate(
    Code = ifelse(is.na(Code), first(na.omit(Code)), Code),
    Code.Set = ifelse(is.na(Code.Set), first(na.omit(Code.Set)), Code.Set),
    Normalized.Name = ifelse(is.na(Normalized.Name), first(na.omit(Normalized.Name)), Normalized.Name),
    Unit = ifelse(is.na(Unit), first(na.omit(Unit)), Unit)
  ) %>%
  ungroup()

labs <- labs %>%
  group_by(Code) %>%
  mutate(
    Name = ifelse(is.na(Name), first(na.omit(Name)), Name),
    Unit = ifelse(is.na(Unit), first(na.omit(Unit)), Unit)
  ) %>%
  ungroup()

labs <- labs %>%
  group_by(Interpretation.Code) %>%
  mutate(
    Interpretation.Name = ifelse(is.na(Interpretation.Name), first(na.omit(Interpretation.Name)), Interpretation.Name)
  ) %>%
  ungroup()

labs <- labs %>%
  group_by(Interpretation.Name) %>%
  mutate(
    Interpretation.Code = ifelse(is.na(Interpretation.Code), first(na.omit(Interpretation.Code)), Interpretation.Code)
  ) %>%
  ungroup()


labs <- labs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "h", "high", Interpretation.Name))

labs <- labs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "n", "normal", Interpretation.Name))

labs <- labs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "l", "low", Interpretation.Name))

labs <- labs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "a", "average", Interpretation.Name))

labs$Interpretation.Name <- ifelse(labs$Interpretation.Name %in% c("high", "hi"), "critical high",
                                   ifelse(labs$Interpretation.Name %in% c("critical low", "crit", "low"), "critical low",
                                          ifelse(labs$Interpretation.Name %in% c("normal", "average"), "normal",
                                                 ifelse(labs$Interpretation.Name == "c", "unknown", 
                                                        labs$Interpretation.Name))))


labs$Normalized.Name <- ifelse(is.na(labs$Normalized.Name), labs$Name, labs$Normalized.Name)

labs$Code[is.na(labs$Code)] <- "Unknown"
labs$Code.Set[is.na(labs$Code.Set)] <- "Unknown"


labs$Value <-as.numeric(labs$Value)
labs$Date <- as.Date(substring(labs$Date, 1, 10))
labs<-na.omit(labs)

## UNMAPPED LABS
## DUMMY DATA
unmappedLabs <- read.csv("labs_unmapped.csv")

# Remove empty columns from dataframe
unmappedLabs <- unmappedLabs %>% select_if(~sum(!is.na(.)) > 0)

unmappedLabs <- unmappedLabs %>% mutate_all(~na_if(., ""))
#unmappedLabs <- unmappedLabs[, -c(1, 2, 4, 14)]

# Identify columns with encoding issues
bad_columns <- sapply(unmappedLabs, function(x) if (is.character(x)) any(is.na(iconv(x, from = "", to = "UTF-8"))) else FALSE)

# Fix encoding issues only in problematic columns
unmappedLabs_fixed <- unmappedLabs %>%
  mutate(across(where(~ is.character(.x) && any(is.na(iconv(.x, from = "", to = "UTF-8")))), ~ iconv(.x, from = "", to = "UTF-8")))

# Apply the desired transformation to convert all character columns to lowercase
unmappedLabs <- unmappedLabs_fixed %>% mutate_if(is.character, tolower)



unmappedLabs <- unmappedLabs %>%
  group_by(Name) %>%
  mutate(
    Code = ifelse(is.na(Code), first(na.omit(Code)), Code),
    Code.Set = ifelse(is.na(Code.Set), first(na.omit(Code.Set)), Code.Set),
    Normalized.Name = ifelse(is.na(Normalized.Name), first(na.omit(Normalized.Name)), Normalized.Name),
    Unit = ifelse(is.na(Unit), first(na.omit(Unit)), Unit)
  ) %>%
  ungroup()

unmappedLabs <- unmappedLabs %>%
  group_by(Code) %>%
  mutate(
    Name = ifelse(is.na(Name), first(na.omit(Name)), Name),
    Unit = ifelse(is.na(Unit), first(na.omit(Unit)), Unit)
  ) %>%
  ungroup()

unmappedLabs <- unmappedLabs %>%
  group_by(Interpretation.Code) %>%
  mutate(
    Interpretation.Name = ifelse(is.na(Interpretation.Name), first(na.omit(Interpretation.Name)), Interpretation.Name)
  ) %>%
  ungroup()

unmappedLabs <- unmappedLabs %>%
  group_by(Interpretation.Name) %>%
  mutate(
    Interpretation.Code = ifelse(is.na(Interpretation.Code), first(na.omit(Interpretation.Code)), Interpretation.Code)
  ) %>%
  ungroup()


unmappedLabs <- unmappedLabs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "h", "high", Interpretation.Name))

unmappedLabs <- unmappedLabs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "n", "normal", Interpretation.Name))

unmappedLabs <- unmappedLabs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "l", "low", Interpretation.Name))

unmappedLabs <- unmappedLabs %>%
  mutate(Interpretation.Name = if_else(Interpretation.Code == "a", "average", Interpretation.Name))

unmappedLabs$Interpretation.Name <- ifelse(unmappedLabs$Interpretation.Name %in% c("high", "hi"), "critical high",
                                           ifelse(unmappedLabs$Interpretation.Name %in% c("critical low", "crit", "low"), "critical low",
                                                  ifelse(unmappedLabs$Interpretation.Name %in% c("normal", "average"), "normal",
                                                         ifelse(unmappedLabs$Interpretation.Name == "c", "unknown", 
                                                                unmappedLabs$Interpretation.Name))))


#unmappedLabs$Normalized.Name <- ifelse(is.na(unmappedLabs$Normalized.Name), unmappedLabs$Name, unmappedLabs$Normalized.Name)

unmappedLabs$Code[is.na(unmappedLabs$Code)] <- "Unknown"
unmappedLabs$Code.Set[is.na(unmappedLabs$Code.Set)] <- "Unknown"


unmappedLabs$Value <-as.numeric(unmappedLabs$Value)
unmappedLabs$Date <- as.Date(substring(unmappedLabs$Date, 1, 10))
unmappedLabs<-na.omit(unmappedLabs)


#PROBLEMS
# DUMMY DATA
problems <- read.csv("problems.csv")
#problems <- problems[, -c(1, 2, 4, 11, 12)]
problems <- problems %>% mutate_all(~na_if(., ""))
problems <- problems %>% mutate_if(is.character, tolower)
problems$Code[is.na(problems$Code)] <- "Unknown"
problems$Start.Date <- as.Date(substring(problems$Start.Date, 1, 10))
problems$End.Date <- as.Date(substring(problems$End.Date, 1, 10))
problems<-na.omit(problems)


# Define UI
ui <- fluidPage(
  #shinythemes::themeSelector(),
  titlePanel("CCD Dashboard"),
  tabsetPanel(
    tabPanel("Vitals",
             sidebarLayout(
               sidebarPanel(
                 selectInput("PatientID", "Select Patient ID:", choices = c("All", unique(vitals$PatientID))),
                 dateRangeInput("dateRange", "Select Date Range:",
                                start = min(vitals$Date),
                                end = max(vitals$Date),
                                min = min(vitals$Date),
                                max = max(vitals$Date)),
                 
                 selectInput("nameType", "Select Name Type:", choices = c("Name", "Normalized Name")),
                 
                 uiOutput("codeMenu"),
                 uiOutput("nameMenu"),
                 uiOutput("sliderUI"),
                 actionButton("unkownCodeToggleVitals", "Show/Hide 'Unknown' Codes"),
                 radioButtons("vitals_choice", "Choose Vitals Dataset:",
                              choices = list("Mapped Vitals" = "vitals", "Unmapped Vitals" = "unmappedVitals"),
                              selected = "vitals"),
                 downloadButton("download_vitals", "Download Filtered Data")
                 
               ),
               mainPanel(
                 plotOutput("DatePlot"),
                 DTOutput("fulltable"), 
                 DTOutput("rowDetailsVitals")
               )
             )
    ),
    tabPanel("Demographics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("DemoPatientID", "Select Patient", choices = c("All" = "All", unique(demo$PatientID))),
                 selectInput("DemoState", "Select State", choices = c("All", unique(demo$State.Address))),
                 selectInput("DemoGender", "Select Gender", choices = c("All", unique(demo$Gender))),
                 selectInput("DemoReligion", "Select Religious Affiliation", choices = c("All", unique(demo$Religious.Affiliation))),
                 downloadButton("download_demo", "Download Filtered Data")
                 
               ), 
               mainPanel(
                 plotOutput("mapPlot"),
                 DTOutput("fulltableDemo"),
                 
               )
             )
    ),
    tabPanel("Social History",
             sidebarLayout(
               sidebarPanel(
                 selectInput("SocialPatientID", "Select Patient", choices = c("All" = "All", unique(social$PatientID))),
                 radioButtons("filterOptionSocial", "Filter By", choices = c("Name", "Normalized Name")),
                 uiOutput("codeMenuSocialHistory"),
                 uiOutput("nameMenuSocialHistory"),
                 actionButton("unkownCodeToggleSocial", "Show/Hide 'Unknown' Codes"),
                 downloadButton("download_social", "Download Filtered Data")
               ), 
               mainPanel(
                 plotOutput("DatePlotSocial"),
                 DTOutput("fulltableSocial"),
                 DTOutput("rowDetailsSocial")
               )
             )
    ),
    tabPanel("Medication",
             sidebarLayout(
               sidebarPanel(
                 selectInput("PatientID_Meds", "Patient ID", choices = c("All", unique(meds$PatientID))),
                 radioButtons("filterChoice", "Filter By", choices = c("Name", "Normalized Name")),
                 uiOutput("codeMenuMeds"),
                 uiOutput("nameMenuMeds"),
                 dateRangeInput("DateRange_Meds", "Date Range", start = min(meds$Start.Date),
                                end = max(meds$Start.Date),
                                min = min(meds$Start.Date),
                                max = max(meds$Start.Date)),
                 actionButton("unkownCodeToggleMeds", "Show/Hide 'Unknown' Codes"),
                 downloadButton("download_meds", "Download Filtered Data")
               ), 
               mainPanel(
                 plotOutput("CodeFreq"),
                 plotOutput("NameFreq"),
                 DTOutput("fulltableMeds"), 
                 DTOutput("rowDetailsMeds")
               )
             )
    ),
    tabPanel("Labs",
             sidebarLayout(
               sidebarPanel(
                 selectInput("PatientID_Labs", "Select Patient ID:", choices = NULL),
                 uiOutput("codeMenuLabs"),
                 uiOutput("dateMenuLabs"),
                 radioButtons("nameOption", "Select Name Type:",
                              choices = c("Name" = "Name", "Normalized Name" = "Normalized.Name"),
                              selected = "Name"),
                 uiOutput("nameMenuLabs"),  # Dynamic UI for Name
                 sliderInput("ValueRange_Labs", "Select Value Range:",
                             min = min(labs$Value), max = max(labs$Value), value = c(min(labs$Value), max(labs$Value))),  # Initial range; to be updated dynamically
                 
                 checkboxGroupInput("interpretation", "Select Interpretation:", 
                                    choices = c("critical high", "critical low", "normal", "unknown"), 
                                    selected = c("critical high", "critical low", "normal", "unknown")),
                 radioButtons("labs_choice", "Choose Labs Dataset:",
                              choices = list("Mapped Labs" = "labs", "Unmapped Labs" = "unmappedLabs"),
                              selected = "labs"),
                 actionButton("toggleUnknown", "Show/Hide Unknown Codes"),
                 downloadButton("download_labs", "Download Filtered Data")
               ), 
               mainPanel(
                 plotOutput("codeHistogram"), 
                 plotOutput("lineChartLabs"),
                 DTOutput("fulltableLabs"), 
                 DTOutput("rowDetailsLabs"),
                 
               )
             )
    ),
    tabPanel("Problems",
             sidebarLayout(
               sidebarPanel(
                 selectInput("probPatientID", "Select Patient ID:",
                             choices = c("All", unique(problems$PatientID))),
                 selectInput("probCode", "Select Code:",
                             choices = c("All", unique(problems$Code))),
                 
                 # Radio buttons for choosing between Name and Normalized Name
                 radioButtons("nameFilterType", "Select Name Type:",
                              choices = c("Name", "Normalized.Name"),
                              selected = "Name"),
                 
                 # UI output for dynamic name or normalized name selection
                 uiOutput("nameFilterUI"),
                 
                 selectInput("probStatus", "Select Status:",
                             choices = c("All", unique(problems$Status))),
                 
                 # Dynamic date range input
                 uiOutput("dateRangeProbs"),
                 actionButton("toggleUnknownProb", "Show/Hide Unknown Codes"),
                 # Add download button
                 downloadButton("download_problems", "Download Filtered Data")
               ), 
               mainPanel(
                 plotOutput("LabHist"),
                 DTOutput("fulltableProb"), 
                 DTOutput("rowDetailsProb"),
                 
               )
             )
    ),
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # OVERALL FUNCTIONS
  
  download_data <- function(data, filename) {
    downloadHandler(
      filename = function() {
        paste0(filename, ".csv")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
  }
  
  # Function to determine appropriate binwidth and date breaks
  get_binwidth_and_breaks <- function(date_range) {
    n_days <- as.numeric(diff(date_range))
    
    if (n_days <= 30) {
      binwidth <- 1  # Daily bins
      date_breaks <- "1 week"
    } else if (n_days <= 180) {
      binwidth <- 7  # Weekly bins
      date_breaks <- "1 month"
    } else if (n_days <= 365) {
      binwidth <- 30  # Monthly bins
      date_breaks <- "2 months"
    } else {
      binwidth <- 90  # Quarterly bins
      date_breaks <- "3 months"
    }
    
    return(list(binwidth = binwidth, date_breaks = date_breaks))
  }
  
  # Define a reactive value to track show/hide "Unknown" codes
  showUnknown <- reactiveVal(TRUE)
  
  observeEvent(input$unkownCodeToggleVitals, {
    showUnknown(!showUnknown())
  })
  
  # Reactive expression for the selected vitals dataset
  selected_vitals <- reactive({
    if (input$vitals_choice == "vitals") {
      vitals
    } else {
      unmappedVitals
    }
  })
  
  filtered_vitals <- reactive({
    req(input$PatientID)  # Ensure patientID input is available
    data <- selected_vitals()
    
    if (input$PatientID != "All") {
      data <- data %>% filter(PatientID == input$PatientID)
    }
    
    if (!is.null(input$Code) && input$Code != "All") {
      data <- data %>% filter(Code == input$Code)
    }
    
    if (!is.null(input$VitalName) && input$VitalName != "All") {
      if (input$nameType == "Normalized Name") {
        data <- data %>% filter(Normalized.Name == input$VitalName)
      } else {
        data <- data %>% filter(Name == input$VitalName)
      }
    }
    
    data <- data %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    
    if (!showUnknown()) data <- filter(data, Code != "Unknown")
    
    return(data)
  })
  
  # Update the date range input based on the filtered data
  observe({
    data <- filtered_vitals()
    
    if (nrow(data) > 0) {
      min_date <- min(data$Date, na.rm = TRUE)
      max_date <- max(data$Date, na.rm = TRUE)
      
      updateDateRangeInput(session, "dateRange",
                           start = as.Date(min_date),
                           end = as.Date(max_date),
                           min = as.Date(min_date),
                           max = as.Date(max_date))
    }
  })
  
  # Code options based on PatientID
  output$codeMenu <- renderUI({
    data <- vitals
    if (input$PatientID != "All") {
      data <- data %>% filter(PatientID == input$PatientID)
    }
    
    if (input$nameType == "Normalized Name" && input$VitalName != "All") {
      data <- data %>% filter(Normalized.Name == input$VitalName)
    } else if (input$VitalName != "All") {
      data <- data %>% filter(Name == input$VitalName)
    }
    
    selectInput("Code", "Select Code:",
                choices = c("All", unique(data$Code)),
                selected = input$Code)
  })
  
  # Render the Name menu based on the selected Code
  output$nameMenu <- renderUI({
    req(input$PatientID)
    
    if (input$nameType == "Normalized Name") {
      if (is.null(input$Code) || input$Code == "All") {
        selectInput("VitalName", "Select Name:",
                    choices = c("All" = "All", unique(vitals$Normalized.Name)),
                    selected = input$VitalName)
      } else {
        df_subset <- subset(vitals, PatientID == input$PatientID & Code == input$Code)
        selectInput("VitalName", "Select Name:",
                    choices = c("All" = "All", unique(df_subset$Normalized.Name)),
                    selected = input$VitalName)
      }
    } else {
      if (is.null(input$Code) || input$Code == "All") {
        selectInput("VitalName", "Select Name:",
                    choices = c("All" = "All", unique(vitals$Name)),
                    selected = input$VitalName)
      } else {
        df_subset <- subset(vitals, PatientID == input$PatientID & Code == input$Code)
        selectInput("VitalName", "Select Name:",
                    choices = c("All" = "All", unique(df_subset$Name)),
                    selected = input$VitalName)
      }
    }
  })
  
  observeEvent(input$PatientID, {
    data <- vitals
    if (input$PatientID != "All") {
      data <- data %>% filter(PatientID == input$PatientID)
    }
    
    updateSelectInput(session, "Code", choices = c("All", unique(data$Code)), selected = input$Code)
    updateSelectInput(session, "VitalName", choices = c("All", unique(data$Normalized.Name)), selected = input$VitalName)
  })
  
  observeEvent(input$Code, {
    data <- vitals
    if (input$PatientID != "All") {
      data <- data %>% filter(PatientID == input$PatientID)
    }
    if (input$Code != "All") {
      data <- data %>% filter(Code == input$Code)
    }
    
    if (input$nameType == "Normalized Name") {
      updateSelectInput(session, "VitalName", choices = c("All", unique(data$Normalized.Name)), selected = input$VitalName)
    } else {
      updateSelectInput(session, "VitalName", choices = c("All", unique(data$Name)), selected = input$VitalName)
    }
  })
  
  observeEvent(input$VitalName, {
    data <- vitals
    if (input$PatientID != "All") {
      data <- data %>% filter(PatientID == input$PatientID)
    }
    if (input$VitalName != "All") {
      if (input$nameType == "Normalized Name") {
        data <- data %>% filter(Normalized.Name == input$VitalName)
      } else {
        data <- data %>% filter(Name == input$VitalName)
      }
    }
    updateSelectInput(session, "Code", choices = c("All", unique(data$Code)), selected = input$Code)
  })
  
  name_range <- reactive({
    req(input$PatientID, input$dateRange, input$Code)
    
    filtered_data <- vitals %>%
      filter((input$PatientID == "All" | PatientID == input$PatientID) &
               (Date >= input$dateRange[1] & Date <= input$dateRange[2]))
    
    if (input$Code != "All") {
      filtered_data <- filtered_data %>% filter(Code == input$Code)
    }
    
    if (input$VitalName != "All") {
      if (input$nameType == "Normalized Name") {
        filtered_data <- filtered_data %>% filter(Normalized.Name == input$VitalName)
      } else {
        filtered_data <- filtered_data %>% filter(Name == input$VitalName)
      }
    }
    
    name_values <- filtered_data %>%
      group_by(if(input$nameType == "Normalized Name") Normalized.Name else Name) %>%
      summarise(min_value = min(Value, na.rm = TRUE), max_value = max(Value, na.rm = TRUE))
    
    return(name_values)
  })
  
  output$DatePlot <- renderPlot({
    req(input$PatientID, input$dateRange, input$valueRange)
    selectPatient <- filtered_vitals()
    selectPatient <- selectPatient %>% 
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) 
    selectPatient <- selectPatient %>%
      filter(Value >= input$valueRange[1], Value <= input$valueRange[2])
    
    date_range <- range(selectPatient$Date)
    bin_info <- get_binwidth_and_breaks(date_range)
    
    ggplot(selectPatient, aes(x = Date)) +
      geom_histogram(binwidth = bin_info$binwidth, fill = "steelblue", color = "black") +
      geom_density(aes(y = ..count.. * bin_info$binwidth), color = "red", size = 1) +
      labs(title = "Histogram of Dates", x = "Date", y = "Frequency") +
      scale_x_date(date_breaks = bin_info$date_breaks, date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      stat_bin(binwidth = bin_info$binwidth, geom = "text", aes(label = ..count..), 
               vjust = -0.5, color = "black")
  })
  
  # Dynamic UI for slider input
  output$sliderUI <- renderUI({
    req(input$Code)
    name_values <- name_range()
    
    # Only show slider if there are values
    if (nrow(name_values) > 0) {
      sliderInput("valueRange", "Select Value Range:",
                  min = min(name_values$min_value),
                  max = max(name_values$max_value),
                  value = c(min(name_values$min_value), max(name_values$max_value)))
    }
  })
  
  # Full table
  output$fulltable <- renderDT({
    req(filtered_vitals())  # Ensure filtered data is available
    
    selected_data <- filtered_vitals()
    
    DT::datatable(selected_data[, c("Code", "Name", "Normalized.Name", "Code.Set", "Date")],
                  caption = "Filtered Table: Code, Name, Normalized Name, Code Set, and Date",
                  selection = 'single')
  })
  
  # Reactive value to store row indices
  selected_rows <- reactiveVal(NULL)
  
  observeEvent(input$fulltable_rows_selected, {
    selected_rows(input$fulltable_rows_selected)
  })
  
  # Render details table based on selected row
  output$rowDetailsVitals <- renderDT({
    selected_row <- selected_rows()
    
    if (!is.null(selected_row) && length(selected_row) > 0) {
      filtered_data <- filtered_vitals()
      selected_data <- filtered_data[selected_row, , drop = FALSE]
      datatable(selected_data, options = list(dom = 't', autoWidth = TRUE))
    } else {
      datatable(data.frame(), options = list(dom = 't'))
    }
  })
  
  output$download_vitals <- download_data(filtered_vitals, "filtered_vitals")
  
  
  
  
  
  
  #DEMO
  
  filtered_demo <- reactive({
    filtered <- demo
    
    # Filter by State.Address if DemoState is not "All"
    if (input$DemoState != "All" && !is.null(input$DemoState)) {
      filtered <- filtered %>% filter(State.Address == input$DemoState)
    }
    
    # Filter by Gender if DemoGender is not "All"
    if (input$DemoGender != "All" && !is.null(input$DemoGender)) {
      filtered <- filtered %>% filter(Gender == input$DemoGender)
    }
    
    # Filter by Religious.Affiliation if DemoReligion is not "All"
    if (input$DemoReligion != "All" && !is.null(input$DemoReligion)) {
      filtered <- filtered %>% filter(Religious.Affiliation == input$DemoReligion)
    }
    
    # Filter by PatientID if DemoPatientID is not "All"
    if (input$DemoPatientID != "All" && !is.null(input$DemoPatientID) && input$DemoPatientID != "") {
      filtered <- filtered %>% filter(PatientID == input$DemoPatientID)
    }
    
    filtered
  })
  
  output$fulltableDemo <- renderDT({
    filtered_demo()
  })
  
  output$mapPlot <- renderPlot({
    filtered_data <- filtered_demo()
    filtered_data <- filtered_data[!is.na(filtered_data$State.Address) & filtered_data$State.Address != "", ]
    
    state_abbr <- data.frame(
      region = tolower(c(
        "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida",
        "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine",
        "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska",
        "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio",
        "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas",
        "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")),
      abbreviation = c(
        "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA",
        "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
        "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
    )
    
    filtered_data <- filtered_data %>%
      left_join(state_abbr, by = c("State.Address" = "abbreviation")) %>%
      distinct(region)
    
    filtered_data$in_list <- TRUE
    
    all_states <- map_data("state") %>%
      distinct(region) %>%
      left_join(filtered_data, by = "region")
    
    all_states$in_list[is.na(all_states$in_list)] <- FALSE
    
    map_us <- map_data("state")
    
    map_data_merged <- left_join(map_us, all_states, by = "region")
    
    map_data_merged$in_list[is.na(map_data_merged$in_list)] <- FALSE
    
    ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = in_list)) +
      geom_polygon(color = "black") +
      scale_fill_manual(values = c("FALSE" = "grey", "TRUE" = "blue"),
                        labels = c("Not Listed", "Listed"),
                        guide = guide_legend(title = "State Status")) +
      labs(title = "State Listing Map",
           caption = "Blue indicates states listed in 'state_data$in_list'. Grey indicates others.") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$stateMenu <- renderUI({
    req(input$DemoPatientID)
    
    if (input$DemoPatientID == "All") {
      selectInput("DemoState", "Select State:",
                  choices = c("All", unique(demo$State.Address)),
                  selected = input$DemoState)
    } else {
      df_subset <- demo %>% filter(PatientID == input$DemoPatientID)
      if (input$DemoGender != "All" && !is.null(input$DemoGender)) {
        df_subset <- df_subset %>% filter(Gender == input$DemoGender)
      }
      selectInput("DemoState", "Select State:",
                  choices = c("All", unique(df_subset$State.Address)),
                  selected = input$DemoState)
    }
  })
  
  output$genderMenu <- renderUI({
    req(input$DemoPatientID)
    
    if (input$DemoPatientID == "All") {
      selectInput("DemoGender", "Select Gender:",
                  choices = c("All", unique(demo$Gender)),
                  selected = input$DemoGender)
    } else {
      df_subset <- demo %>% filter(PatientID == input$DemoPatientID)
      if (input$DemoState != "All" && !is.null(input$DemoState)) {
        df_subset <- df_subset %>% filter(State.Address == input$DemoState)
      }
      selectInput("DemoGender", "Select Gender:",
                  choices = c("All", unique(df_subset$Gender)),
                  selected = input$DemoGender)
    }
  })
  
  output$religionMenu <- renderUI({
    req(input$DemoPatientID)
    
    if (input$DemoPatientID == "All") {
      selectInput("DemoReligion", "Select Religion:",
                  choices = c("All", unique(demo$Religious.Affiliation)),
                  selected = input$DemoReligion)
    } else {
      df_subset <- demo %>% filter(PatientID == input$DemoPatientID)
      if (input$DemoState != "All" && !is.null(input$DemoState)) {
        df_subset <- df_subset %>% filter(State.Address == input$DemoState)
      }
      selectInput("DemoReligion", "Select Religion:",
                  choices = c("All", unique(df_subset$Religious.Affiliation)),
                  selected = input$DemoReligion)
    }
  })
  
  # Initialize select inputs and update choices based on PatientID
  observe({
    if (input$DemoPatientID == "All") {
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID)), selected = input$DemoPatientID)
    } else {
      filtered <- demo
      
      if (input$DemoState != "All" && !is.null(input$DemoState)) {
        filtered <- filtered %>% filter(State.Address == input$DemoState)
      }
      
      if (input$DemoGender != "All" && !is.null(input$DemoGender)) {
        filtered <- filtered %>% filter(Gender == input$DemoGender)
      }
      
      if (input$DemoReligion != "All" && !is.null(input$DemoReligion)) {
        filtered <- filtered %>% filter(Religious.Affiliation == input$DemoReligion)
      }
      
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(filtered$PatientID)), selected = input$DemoPatientID)
    }
  })
  
  
  # Reactive updates based on selections
  # Reactive updates based on selections
  observeEvent(input$DemoPatientID, {
    if (input$DemoPatientID == "All") {
      updateSelectInput(session, "DemoState", choices = c("All", unique(filtered_demo()$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoGender", choices = c("All", unique(filtered_demo()$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(filtered_demo()$Religious.Affiliation)), selected = input$DemoReligion)
    } else {
      df_subset <- demo[demo$PatientID == input$DemoPatientID, ]
      updateSelectInput(session, "DemoState", choices = c("All", unique(df_subset$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoGender", choices = c("All", unique(df_subset$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(df_subset$Religious.Affiliation)), selected = input$DemoReligion)
    }
  })
  
  observeEvent(input$DemoState, {
    if (input$DemoState == "All") {
      updateSelectInput(session, "DemoGender", choices = c("All", unique(filtered_demo()$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(filtered_demo()$Religious.Affiliation)), selected = input$DemoReligion)
    } else {
      df_subset <- demo[demo$State.Address == input$DemoState, ]
      updateSelectInput(session, "DemoGender", choices = c("All", unique(df_subset$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(df_subset$Religious.Affiliation)), selected = input$DemoReligion)
    }
    
    # Update DemoPatientID choices based on DemoState selection
    if (input$DemoState != "All" && !is.null(input$DemoState)) {
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID[demo$State.Address == input$DemoState])), selected = input$DemoPatientID)
    } else {
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID)), selected = input$DemoPatientID)
    }
  })
  
  observeEvent(input$DemoGender, {
    if (input$DemoGender == "All") {
      updateSelectInput(session, "DemoState", choices = c("All", unique(filtered_demo()$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(filtered_demo()$Religious.Affiliation)), selected = input$DemoReligion)
    } else {
      df_subset <- demo[demo$Gender == input$DemoGender, ]
      updateSelectInput(session, "DemoState", choices = c("All", unique(df_subset$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoReligion", choices = c("All", unique(df_subset$Religious.Affiliation)), selected = input$DemoReligion)
    }
    
    # Update DemoPatientID choices based on DemoGender selection
    if (input$DemoGender != "All" && !is.null(input$DemoGender)) {
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID[demo$Gender == input$DemoGender])), selected = input$DemoPatientID)
    } else {
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID)), selected = input$DemoPatientID)
    }
  })
  
  observeEvent(input$DemoReligion, {
    if (input$DemoReligion != "All" && !is.null(input$DemoReligion)) {
      df_subset <- demo[demo$Religious.Affiliation == input$DemoReligion, ]
      updateSelectInput(session, "DemoState", choices = c("All", unique(df_subset$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoGender", choices = c("All", unique(df_subset$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(df_subset$PatientID)), selected = input$DemoPatientID)
    } else {
      updateSelectInput(session, "DemoState", choices = c("All", unique(filtered_demo()$State.Address)), selected = input$DemoState)
      updateSelectInput(session, "DemoGender", choices = c("All", unique(filtered_demo()$Gender)), selected = input$DemoGender)
      updateSelectInput(session, "DemoPatientID", choices = c("All", unique(demo$PatientID)), selected = input$DemoPatientID)
    }
  })
  
  output$download_demo <- download_data(filtered_demo, "filtered_demo")
  
  
  
  
  
  
  #Social History
  
  # Define a reactive value to track show/hide "Unknown" codes
  showUnknown <- reactiveVal(TRUE)
  
  observeEvent(input$unkownCodeToggleSocial, {
    showUnknown(!showUnknown())
  })
  
  # Reactive filtering of social data
  filtered_social <- reactive({
    filtered <- social
    
    if (!is.null(input$SocialCode) && input$SocialCode != "All") {
      filtered <- filtered %>% filter(Code == input$SocialCode)
    }
    
    if (input$filterOptionSocial == "Name") {
      if (!is.null(input$SocialName) && input$SocialName != "All") {
        filtered <- filtered %>% filter(Name == input$SocialName)
      }
    } else if (input$filterOptionSocial == "Normalized Name") {
      if (!is.null(input$SocialNormalizedName) && input$SocialNormalizedName != "All") {
        filtered <- filtered %>% filter(Normalized.Name == input$SocialNormalizedName)
      }
    }
    
    if (!is.null(input$SocialPatientID) && input$SocialPatientID != "All") {
      filtered <- filtered %>% filter(PatientID == input$SocialPatientID)
    }
    
    if (!showUnknown()) filtered <- filtered %>% filter(Code != "Unknown")
    
    filtered
  })
  
  # Render Date Plot
  output$DatePlotSocial <- renderPlot({
    req(filtered_social())
    
    filtered_data <- filtered_social() %>%
      dplyr::mutate(Start.Date = as.Date(Start.Date)) %>%
      dplyr::filter(!is.na(Start.Date))
    
    if (nrow(filtered_data) == 0) {
      ggplot() +
        labs(title = "No Data Available", x = NULL, y = NULL) +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 5, hjust = 0.5)
    } else {
      date_range <- range(filtered_data$Start.Date, na.rm = TRUE)
      bin_info <- get_binwidth_and_breaks(date_range)
      
      ggplot(filtered_data, aes(x = Start.Date)) +
        geom_histogram(binwidth = bin_info$binwidth, fill = "steelblue", color = "black") +
        geom_density(aes(y = ..count.. * bin_info$binwidth), color = "red", size = 1) +
        labs(title = "Histogram of Dates by Month", x = "Date", y = "Frequency") +
        scale_x_date(date_breaks = bin_info$date_breaks, date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        stat_bin(binwidth = bin_info$binwidth, geom = "text", aes(label = ..count..), vjust = -0.5, color = "black")
    }
  })
  
  # Render Data Table
  output$fulltableSocial <- renderDT({
    req(filtered_social())
    
    DT::datatable(filtered_social()[, c("Code", "Name", "Normalized.Name", "Code.Set", "Start.Date")],
                  caption = "Filtered Table: Code, Name, Normalized Name, Code Set, and Date", selection = 'single')
  })
  
  # Reactive value to store row indices
  selected_rows_social <- reactiveVal(NULL)
  
  observeEvent(input$fulltableSocial_rows_selected, {
    selected_rows_social(input$fulltableSocial_rows_selected)
  })
  
  # Render Row Details
  output$rowDetailsSocial <- renderDT({
    selected_row <- selected_rows_social()
    
    if (!is.null(selected_row) && length(selected_row) > 0) {
      filtered_data <- filtered_social()
      selected_data <- filtered_data[selected_row, , drop = FALSE]
      DT::datatable(selected_data, options = list(dom = 't', autoWidth = TRUE))
    } else {
      DT::datatable(data.frame(), options = list(dom = 't'))
    }
  })
  
  # Dynamic update of Code selections
  output$codeMenuSocialHistory <- renderUI({
    req(input$SocialPatientID)
    
    if (is.null(input$SocialPatientID)) return(NULL)
    
    if (input$SocialPatientID == "All") {
      selectInput("SocialCode", "Select Code:",
                  choices = c("All", unique(social$Code)),
                  selected = input$SocialCode)
    } else {
      df_subset <- social %>% filter(PatientID == input$SocialPatientID)
      selectInput("SocialCode", "Select Code:",
                  choices = c("All", unique(df_subset$Code)),
                  selected = input$SocialCode)
    }
  })
  
  # Dynamic update of Name and Normalized Name selections
  output$nameMenuSocialHistory <- renderUI({
    req(input$SocialPatientID, input$filterOptionSocial)
    
    if (is.null(input$filterOptionSocial)) return(NULL)
    if (is.null(input$SocialPatientID)) return(NULL)
    
    df_subset <- filtered_social()  # Use filtered data to ensure correct choices
    
    if (input$filterOptionSocial == "Name") {
      selectInput("SocialName", "Select Name:",
                  choices = c("All", unique(df_subset$Name)),
                  selected = input$SocialName)
    } else if (input$filterOptionSocial == "Normalized Name") {
      selectInput("SocialNormalizedName", "Select Normalized Name:",
                  choices = c("All", unique(df_subset$Normalized.Name)),
                  selected = input$SocialNormalizedName)
    }
  })
  
  # Observe selections and update choices dynamically
  observe({
    selected_data <- filtered_social()
    
    # Update choices for each filter dynamically based on selected data
    updateSelectInput(session, "SocialPatientID", 
                      choices = c("All", unique(selected_data$PatientID)),
                      selected = input$SocialPatientID)
    
    updateSelectInput(session, "SocialCode", 
                      choices = c("All", unique(selected_data$Code)),
                      selected = input$SocialCode)
    
    # Depending on the name filter type, update the appropriate input
    if (input$filterOptionSocial == "Name") {
      updateSelectInput(session, "SocialName", 
                        choices = c("All", unique(selected_data$Name)),
                        selected = input$SocialName)
    } else {
      updateSelectInput(session, "SocialName", 
                        choices = c("All", unique(selected_data$Normalized.Name)),
                        selected = input$SocialName)
    }
  })
  
  
  output$download_social <- download_data(filtered_social, "filtered_social")
  
  
  
  
  # Meds
  
  
  # Reactive value to toggle the visibility of 'Unknown' codes
  showUnknown <- reactiveVal(TRUE)
  
  observeEvent(input$unkownCodeToggleMeds, {
    showUnknown(!showUnknown())
  })
  
  observe({
    updateSelectInput(session, "PatientID_Meds", choices = c("All", unique(meds$PatientID)), selected = input$PatientID_Meds)
  })
  
  filtered_meds <- reactive({
    req(input$PatientID_Meds, input$filterChoice)  # Ensure inputs are available
    
    filtered <- meds
    
    if (!is.null(input$Code_Meds) && input$Code_Meds != "All") {
      filtered <- filtered %>% filter(Code == input$Code_Meds)
    }
    
    if (input$filterChoice == "Name") {
      if (!is.null(input$Name_Meds) && input$Name_Meds != "All") {
        filtered <- filtered %>% filter(Name == input$Name_Meds)
      }
    } else if (input$filterChoice == "Normalized Name") {
      if (!is.null(input$NormalizedName_Meds) && input$NormalizedName_Meds != "All") {
        filtered <- filtered %>% filter(Normalized.Name == input$NormalizedName_Meds)
      }
    }
    
    if (!is.null(input$PatientID_Meds) && input$PatientID_Meds != "All") {
      filtered <- filtered %>% filter(PatientID == input$PatientID_Meds)
    }
    
    if (!showUnknown()) {
      filtered <- filtered %>% filter(Code != "Unknown")
    }
    
    filtered
  })
  
  output$codeMenuMeds <- renderUI({
    req(input$PatientID_Meds)
    
    if (input$PatientID_Meds == "All") {
      selectInput("Code_Meds", "Select Code:",
                  choices = c("All", unique(meds$Code)),
                  selected = input$Code_Meds)
    } else {
      df_subset <- meds %>% filter(PatientID == input$PatientID_Meds)
      selectInput("Code_Meds", "Select Code:",
                  choices = c("All", unique(df_subset$Code)),
                  selected = input$Code_Meds)
    }
  })
  
  output$nameMenuMeds <- renderUI({
    req(input$PatientID_Meds, input$filterChoice)
    
    df_subset <- filtered_meds() 
    
    if (input$filterChoice == "Name") {
      selectInput("Name_Meds", "Select Name:",
                  choices = c("All", unique(df_subset$Name)),
                  selected = input$Name_Meds)
    } else if (input$filterChoice == "Normalized Name") {
      selectInput("NormalizedName_Meds", "Select Normalized Name:",
                  choices = c("All", unique(df_subset$Normalized.Name)),
                  selected = input$NormalizedName_Meds)
    }
  })
  
  observe({
    selected_data <- filtered_meds()
    
    updateSelectInput(session, "PatientID_Meds", 
                      choices = c("All", unique(selected_data$PatientID)),
                      selected = input$PatientID_Meds)
    
    updateSelectInput(session, "Code_Meds", 
                      choices = c("All", unique(selected_data$Code)),
                      selected = input$Code_Meds)
    
    if (input$filterChoice == "Name") {
      updateSelectInput(session, "Name_Meds", 
                        choices = c("All", unique(selected_data$Name)),
                        selected = input$Name_Meds)
    } else {
      updateSelectInput(session, "Name_Meds", 
                        choices = c("All", unique(selected_data$Normalized.Name)),
                        selected = input$Name_Meds)
    }
  })
  
  output$fulltableMeds <- renderDT({
    req(filtered_meds())
    
    selected_data <- filtered_meds()
    
    DT::datatable(selected_data[, c("Code", "Name", "Normalized.Name", "Code.Set", "Start.Date")],
                  caption = "Filtered Table: Code, Name, Normalized Name, and Date", selection = 'single')
  })
  
  output$rowDetailsMeds <- renderDT({
    selected_row <- input$fulltableMeds_rows_selected
    
    if (length(selected_row)) {
      selected_data <- filtered_meds()[selected_row, ]
      DT::datatable(selected_data, options = list(dom = 't', autoWidth = TRUE))
    } else {
      DT::datatable(data.frame(), options = list(dom = 't'))
    }
  })
  
  # Render Code Frequency Plot
  output$CodeFreq <- renderPlot({
    tryCatch({
      df <- filtered_meds()
      
      if (nrow(df) > 0) {
        top_codes <- table(df$Code) %>% sort(decreasing = TRUE) %>% head(20)
        ggplot(data = as.data.frame(top_codes), aes(x = reorder(Var1, Freq), y = Freq)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          geom_text(aes(label = Freq), vjust = -0.5, color = "black") +
          labs(x = "Code", y = "Frequency", title = "Top 20 Codes by Frequency")
      } else {
        ggplot() +
          labs(title = "No Data Available", x = NULL, y = NULL) +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 5, hjust = 0.5)
      }
    }, error = function(e) {
      # Display a meaningful error message if an error occurs
      ggplot() +
        labs(title = "Error", x = NULL, y = NULL) +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = paste("An error occurred:", e$message), size = 5, hjust = 0.5)
    })
  })
  
  # Render Name Frequency Plot
  output$NameFreq <- renderPlot({
    tryCatch({
      df <- filtered_meds()
      
      if (nrow(df) > 0) {
        top_names <- table(df$Name) %>% sort(decreasing = TRUE) %>% head(20)
        ggplot(data = as.data.frame(top_names), aes(x = reorder(Var1, Freq), y = Freq)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          geom_text(aes(label = Freq), vjust = -0.5, color = "black") +
          coord_flip() +
          labs(x = "Name", y = "Frequency", title = "Top 20 Names by Frequency")
      } else {
        ggplot() +
          labs(title = "No Data Available", x = NULL, y = NULL) +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters", size = 5, hjust = 0.5)
      }
    }, error = function(e) {
      # Display a meaningful error message if an error occurs
      ggplot() +
        labs(title = "Error", x = NULL, y = NULL) +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = paste("An error occurred:", e$message), size = 5, hjust = 0.5)
    })
  })
  
  # Provide download functionality for filtered data
  output$download_meds <- download_data(filtered_meds, "filtered_meds")
  
  
  
  
  # LABS
  
  
  # Reactive value to track whether "Unknown" codes are shown
  showUnknown <- reactiveVal(TRUE)
  
  observeEvent(input$toggleUnknown, {
    showUnknown(!showUnknown())
  })
  
  selected_labs <- reactive({
    if (input$labs_choice == "labs") {
      labs
    } else {
      unmappedLabs
    }
  })
  
  # Dynamic UI for Code
  output$codeMenuLabs <- renderUI({
    req(input$PatientID_Labs)
    
    df_subset <- if (input$PatientID_Labs == "All") labs else filter(labs, PatientID == input$PatientID_Labs)
    
    selectInput("Code_Labs", "Select Code:",
                choices = c("All", unique(df_subset$Code)),
                selected = "All")
  })
  
  # Dynamic UI for Date Range
  output$dateMenuLabs <- renderUI({
    req(input$PatientID_Labs, input$Code_Labs)
    
    df_subset <- if (input$PatientID_Labs == "All") labs else filter(labs, PatientID == input$PatientID_Labs)
    
    if (input$Code_Labs != "All") {
      df_subset <- filter(df_subset, Code == input$Code_Labs)
    }
    
    dates <- pull(df_subset, Date)
    
    dateRangeInput("DateRange_Labs", "Select Date Range:",
                   start = min(dates, na.rm = TRUE),
                   end = max(dates, na.rm = TRUE))
  })
  
  # Dynamic UI for Name or Normalized Name
  output$nameMenuLabs <- renderUI({
    req(input$PatientID_Meds, input$nameOption)
    
    df_subset <- filtered_labs() 
    
    if (input$nameOption == "Name") {
      selectInput("Name_Labs", "Select Name:",
                  choices = c("All", unique(df_subset$Name)),
                  selected = input$Name_Labs)
    } else if (input$nameOption == "Normalized Name") {
      selectInput("NormalizedName_Labs", "Select Normalized Name:",
                  choices = c("All", unique(df_subset$Normalized.Name)),
                  selected = input$NormalizedName_Labs)
    }
  })
  
  # Initialize PatientID choices
  observe({
    updateSelectInput(session, "PatientID_Labs", choices = c("All", unique(labs$PatientID)), selected = "All")
  })
  
  # Dynamically update Value Range Slider based on filtered data
  observe({
    df_subset <- filtered_labs()
    
    if (nrow(df_subset) > 0) {
      min_value <- min(df_subset$Value, na.rm = TRUE)
      max_value <- max(df_subset$Value, na.rm = TRUE)
      
      # Update slider only if range values have changed
      if (input$ValueRange_Labs[1] != min_value || input$ValueRange_Labs[2] != max_value) {
        updateSliderInput(session, "ValueRange_Labs",
                          min = min_value,
                          max = max_value,
                          value = c(min_value, max_value))
      }
    } else {
      # Handle case when there is no data
      updateSliderInput(session, "ValueRange_Labs",
                        min = 0,
                        max = 100,
                        value = c(0, 100))
    }
  })
  
  # Reactive expression to filter data based on user inputs
  filtered_labs <- reactive({
    req(input$PatientID_Labs, input$nameOption)  # Ensure inputs are available
    
    data <- selected_labs()
    
    if (!is.null(input$Code_Labs) && input$Code_Labs != "All") {
      data <- data %>% filter(Code == input$Code_Labs)
    }
    
    if (input$nameOption == "Name") {
      if (!is.null(input$Name_Labs) && input$Name_Labs != "All") {
        data <- data %>% filter(Name == input$Name_Labs)
      }
    } else if (input$nameOption == "Normalized Name") {
      if (!is.null(input$NormalizedName_Labs) && input$NormalizedName_Labs != "All") {
        data <- data %>% filter(Normalized.Name == input$NormalizedName_Labs)
      }
    }
    
    if (!is.null(input$PatientID_Labs) && input$PatientID_Labs != "All") {
      data <- data %>% filter(PatientID == input$PatientID_Labs)
    }
    
    if (!showUnknown()) {
      data <- data %>% filter(Code != "Unknown")
    }
    
    
    # Filter based on Value Range
    data <- filter(data, Value >= input$ValueRange_Labs[1] & Value <= input$ValueRange_Labs[2])
    
    data
  })
  
  
  observe({
    selected_data <- filtered_labs()
    
    updateSelectInput(session, "PatientID_Labs", 
                      choices = c("All", unique(selected_data$PatientID)),
                      selected = input$PatientID_Labs)
    
    updateSelectInput(session, "Code_Labs", 
                      choices = c("All", unique(selected_data$Code)),
                      selected = input$Code_Labs)
    
    if (input$nameOption == "Name") {
      updateSelectInput(session, "Name_Labs", 
                        choices = c("All", unique(selected_data$Name)),
                        selected = input$Name_Labs)
    } else {
      updateSelectInput(session, "Name_Labs", 
                        choices = c("All", unique(selected_data$Normalized.Name)),
                        selected = input$Name_Labs)
    }
  })
  
  output$codeHistogram <- renderPlot({
    # Ensure data is available
    req(filtered_labs())
    
    # Extract and clean the filtered data
    filtered_data <- filtered_labs() %>%
      dplyr::mutate(Date = as.Date(Date)) %>%
      dplyr::filter(!is.na(Date))
    
    # Check if there's data to plot
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Exit if no data to plot
    }
    
    # Calculate the date range from the filtered data
    date_range <- range(filtered_data$Date, na.rm = TRUE)
    
    # Get appropriate binwidth and date breaks based on the date range
    bin_info <- get_binwidth_and_breaks(date_range)
    
    # Define color mapping
    color_mapping <- c(
      "critical high" = "red",
      "critical low" = "yellow",
      "normal" = "green",
      "unknown" = "steelblue"
    )
    
    # Create the plot with dynamic binwidth and date breaks
    ggplot(filtered_data, aes(x = Date, fill = Interpretation.Name)) +
      geom_histogram(binwidth = bin_info$binwidth, position = "identity", alpha = 1.0) +  # Histogram with fill color
      # Optionally include a density plot
      # geom_density(aes(y = ..count.. * bin_info$binwidth, color = "Density"), size = 1) +  # Density plot
      scale_fill_manual(values = color_mapping) +  # Apply custom colors
      scale_color_manual(values = c("Density" = "red")) +  # Color for density line
      labs(title = "Histogram of Dates by Month", x = "Date", y = "Frequency") +
      scale_x_date(date_breaks = bin_info$date_breaks, date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      stat_bin(binwidth = bin_info$binwidth, geom = "text", aes(label = ..count..), position = position_identity(), vjust = -0.5, color = "black")
  })
  
  output$lineChartLabs <- renderPlot({
    color_mapping <- c(
      "critical high" = "red",
      "critical low" = "yellow",
      "normal" = "green",
      "unknown" = "steelblue"
    )
    
    filtered_data <- filtered_labs()  # Use filtered data to ensure it respects the date range
    req(input$Code_Labs != "All")  # Ensure a specific code is selected
    
    ggplot(filtered_data, aes(x = Date, y = Value, color = Interpretation.Name)) +
      geom_line() +
      scale_color_manual(values = color_mapping) +
      labs(title = paste("Line Chart of Values for Code:", input$Code_Labs), x = "Date", y = "Value") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Add table output referencing filtered data
  output$fulltableLabs <- renderDT({
    req(filtered_labs())  # Ensure filtered data is available
    
    selected_data <- filtered_labs()
    
    DT::datatable(selected_data[, c("Code", input$nameOption, "Date", "Code.Set", "Interpretation.Name")],
                  caption = "Filtered Table: Code, Name/Normalized Name, Date, Code Set, and Interpretation",
                  selection = 'single')
  })
  
  # Render the full row details when a row is clicked
  output$rowDetailsLabs <- renderDT({
    # Get the index of the selected row
    selected_row <- input$fulltableLabs_rows_selected
    
    # If a row is selected, show its details
    if (length(selected_row)) {
      selected_data <- filtered_labs()[selected_row, ]
      DT::datatable(selected_data, options = list(dom = 't', autoWidth = TRUE)) # Hide search and paging
    } else {
      # If no row is selected, show an empty table
      DT::datatable(data.frame(), options = list(dom = 't'))
    }
  })
  
  output$download_labs <- download_data(filtered_labs, "filtered_labs")
  
  
  
  
  # PROBLEMS 
  
  # Define a reactive value to track show/hide "Unknown" codes
  showUnknown <- reactiveVal(TRUE)
  
  # Observe button click to toggle showing "Unknown" codes
  observeEvent(input$unkownCodeToggleProb, {
    showUnknown(!showUnknown())
  })
  
  # Dynamic UI for Name or Normalized Name based on user selection
  output$nameFilterUI <- renderUI({
    choices <- if (input$nameFilterType == "Name") {
      unique(problems$Name)
    } else {
      unique(problems$Normalized.Name)
    }
    
    # Ensure "All" is always an option
    choices <- c("All", choices)
    
    # Set default to "All" to avoid empty value
    selectInput(
      "probName", 
      "Select Name or Normalized Name:",
      choices = choices,
      selected = "All"
    )
  })
  
  # Reactive expression for filtering the data
  filtered_problems <- reactive({
    # Ensure probName has a valid value
    probName <- ifelse(is.null(input$probName) || input$probName == "", "All", input$probName)
    
    # Initialize the filtered dataset
    filtered_data <- problems %>%
      filter(
        (input$probPatientID == "All" | PatientID == input$probPatientID) &
          (input$probCode == "All" | Code == input$probCode) &
          (input$probStatus == "All" | Status == input$probStatus)
      )
    
    # Further filter based on Name or Normalized Name selection
    if (input$nameFilterType == "Name") {
      filtered_data <- filtered_data %>%
        filter(probName == "All" | Name == probName)
    } else {
      filtered_data <- filtered_data %>%
        filter(probName == "All" | Normalized.Name == probName)
    }
    
    # Exclude "Unknown" codes if toggle is off
    if (!showUnknown()) {
      filtered_data <- filter(filtered_data, Code != "Unknown")
    }
    
    # Remove NA values
    filtered_data <- na.omit(filtered_data)
    
    return(filtered_data)
  })
  
  # Update the date range input dynamically based on filtered data
  observeEvent(filtered_problems(), {
    data <- filtered_problems()
    
    # Check if there's data after filtering
    if (nrow(data) > 0) {
      # Update the date range input with the minimum and maximum Start.Date from the filtered data
      updateDateRangeInput(
        session, 
        "probDateRange", 
        start = min(data$Start.Date, na.rm = TRUE),
        end = max(data$Start.Date, na.rm = TRUE),
        min = min(problems$Start.Date, na.rm = TRUE),
        max = max(problems$Start.Date, na.rm = TRUE)
      )
    }
  }, ignoreInit = TRUE)  # Ignore the initial call to prevent unnecessary update
  
  # Dynamic UI for the date range input
  output$dateRangeProbs <- renderUI({
    # Use the initial unfiltered data for setting the default date range
    dateRangeInput(
      "probDateRange", 
      "Select Start Date Range:",
      start = min(problems$Start.Date, na.rm = TRUE),
      end = max(problems$Start.Date, na.rm = TRUE),
      min = min(problems$Start.Date, na.rm = TRUE),
      max = max(problems$Start.Date, na.rm = TRUE)
    )
  })
  
  # Observe selections and update choices dynamically
  observe({
    selected_data <- filtered_problems()
    
    # Update choices for each filter dynamically based on selected data
    updateSelectInput(session, "probPatientID", 
                      choices = c("All", unique(selected_data$PatientID)),
                      selected = input$probPatientID)
    
    updateSelectInput(session, "probCode", 
                      choices = c("All", unique(selected_data$Code)),
                      selected = input$probCode)
    
    # Depending on the name filter type, update the appropriate input
    if (input$nameFilterType == "Name") {
      updateSelectInput(session, "probName", 
                        choices = c("All", unique(selected_data$Name)),
                        selected = input$probName)
    } else {
      updateSelectInput(session, "probName", 
                        choices = c("All", unique(selected_data$Normalized.Name)),
                        selected = input$probName)
    }
    
    updateSelectInput(session, "probStatus", 
                      choices = c("All", unique(selected_data$Status)),
                      selected = input$probStatus)
  })
  
  # Render the table based on the filtered data
  output$fulltableProb <- renderDT({
    # Apply date range filter only if it's valid
    data <- filtered_problems()
    if (!is.null(input$probDateRange) && length(input$probDateRange) == 2) {
      data <- data %>%
        filter(Start.Date >= input$probDateRange[1] & Start.Date <= input$probDateRange[2])
    }
    
    DT::datatable(data[, c("Code", "Name", "Normalized.Name", "Code.Set", "Start.Date")],
                  caption = "Filtered Table: Code, Name, Normalized Name, Code Set, and Date", selection = 'single')
  })
  
  # Render row details based on selection
  output$rowDetailsProb <- renderDT({
    # Get the index of the selected row
    selected_row <- input$fulltableProb_rows_selected
    
    # If a row is selected, show its details
    if (length(selected_row)) {
      selected_data <- filtered_problems()[selected_row, ]
      DT::datatable(selected_data, options = list(dom = 't', autoWidth = TRUE)) # Hide search and paging
    } else {
      # If no row is selected, show an empty table
      DT::datatable(data.frame(), options = list(dom = 't'))
    }
  })
  
  # Render histogram for Start.Date
  output$LabHist <- renderPlot({
    data <- filtered_problems()
    
    # Apply date range filter to data
    if (!is.null(input$probDateRange) && length(input$probDateRange) == 2) {
      data <- data %>%
        filter(Start.Date >= input$probDateRange[1] & Start.Date <= input$probDateRange[2])
    }
    
    # Convert Start.Date to Date object and remove NA values
    data <- data %>%
      dplyr::mutate(Start.Date = as.Date(Start.Date)) %>%
      dplyr::filter(!is.na(Start.Date))
    
    # Check if there's data to plot
    if (nrow(data) == 0) {
      return(NULL)  # Exit if no data to plot
    }
    
    # Calculate the date range from the filtered data
    date_range <- range(data$Start.Date, na.rm = TRUE)
    
    # Get appropriate binwidth and date breaks based on the date range
    bin_info <- get_binwidth_and_breaks(date_range)
    
    # Create the plot with dynamic binwidth and date breaks
    ggplot(data, aes(x = Start.Date)) +
      geom_histogram(binwidth = bin_info$binwidth, fill = "steelblue", color = "black") +
      # Optionally include a density plot
      # geom_density(aes(y = ..count.. * bin_info$binwidth), color = "red", size = 1) +  # Density plot
      labs(title = "Histogram of Dates by Month", x = "Date", y = "Frequency") +
      scale_x_date(date_breaks = bin_info$date_breaks, date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      stat_bin(binwidth = bin_info$binwidth, geom = "text", aes(label = ..count..), vjust = -0.5, color = "black")
  })
  
  output$download_problems <- download_data(filtered_problems, "filtered_problems")
  
}

# Run the application
shinyApp(ui = ui, server = server)