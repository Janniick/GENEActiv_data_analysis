#The main goal of this code is to make the Analysis of GENEActiv raw data (.bin files) more userfriendly using a GUI. 
#For the first part of the analysis, I've used the open source code called GGIR and adjusted it to my needs.
#You can find detailed information about GGIR on their website: https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html
#The second part of the code was written by myself and tries to gather and display interesting data about the persons
#day- and especially their nighttime activity.

#The only thing that has to be adjusted by the user is the starting directory called baseDir which you'll find at the beginning of the server function
#baseDir <- 
#the expected folder structure at the chosen baseDir has to look as follows: 
#baseDir\GENEActiv\Data\TX(timepoint)\participantnumber\Night/Week

#I added many comments throughout the code to make it more understandable and user-friendly

#If you've never used the code before, you'll have to install the GGIR & shiny packages by uncommenting and running the following lines.
#install.packages("GGIR", dependencies = TRUE)
#install.packages("shiny", dependencies = TRUE)
#install.packages("shinyFiles", dependencies = TRUE)
#install.packages("shinythemes", dependencies = TRUE)
#install.packages("shinyWidgets", dependencies = TRUE)
#install.packages("shinyjs", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
#install.packages("zoo", dependencies = TRUE)
#install.packages("remotes", dependencies = TRUE)
#remotes::install_github("wadpac/GGIR") #updates the GGIR package if needed
library(GGIR)
library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(zoo)
library(remotes)

#clear environment
rm(list=ls())

# Define a function to process files using the GGIR code. It takes paths to the files, their original directories, 
# UI element IDs for status and error messages, and session and output objects from Shiny.
processFiles <- function(filePaths, originalDirPaths, statusIconIds, errorMsgIds, session, output) {
  for (i in seq_along(filePaths)) {
    local({
      print(paste("processFiles starts now"))
      file <- filePaths[i]
      print(paste("File:", file))
      statusIconId <- statusIconIds[i]
      errorMsgId <- errorMsgIds[i]
      
      # Immediately display rotating gear emoji to indicate processing, doesn't work for some reason
      output[[statusIconId]] <- renderUI({
        tags$span(class = "processing-icon", "⚙️")  # Rotating gear emoji
      })
    
      # Step 1: Define the temporary output directory path
      # Since the GGIR function doesn't allow the raw data file path and the output file path to be similar, 
      # we need to add this step of a temporary output file path
      # The timestamp is needed to make unique outputs, so if you run the analysis multiple times,
      # you don't lose the output od a previous analysis
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")
      original_dir_path <- originalDirPaths[i]
      # print(paste("original_dir_path:", original_dir_path)) #print statements for debugging
      path_parts <- strsplit(original_dir_path, .Platform$file.sep)[[1]]
      timepoint <- path_parts[length(path_parts)-2]
      participant_number <- path_parts[length(path_parts)-1]
      night_or_week <- ifelse(grepl("Night", file, ignore.case = TRUE), "Night", "Week")
      folder_prefix <- paste(timepoint, participant_number, night_or_week, sep = "_")
      #print(paste("folder_prefix:", folder_prefix)) #print statements for debugging
      temp_output_dir_path <- file.path("D:/Sleep & CoPe", paste(folder_prefix, "Processed", timestamp, sep = "_"))
      #print(paste("temp_output_dir_path:", temp_output_dir_path)) #print statements for debugging
      
      # Check and create the temp output directory if it doesn't exist
      if (!dir.exists(temp_output_dir_path)) {
        dir.create(temp_output_dir_path, recursive = TRUE)
      }
    
      # Step 2: Perform the GGIR analysis
      # Call the GGIR function to process the .bin file, this might take up to 15min for one file
      GGIR(
        #GGIR is constructed in 5 parts
        mode=c(1,2,3,4,5), #mode defines which parts to run
        datadir=original_dir_path,
        outputdir=temp_output_dir_path,
        overwrite = TRUE,
        do.report=c(2,4,5),
        #=====================
        # Part 2 
        #=====================
        strategy = 1,
        hrs.del.start = 0,
        hrs.del.end = 0,
        maxdur = 10, #maximum number of days you expect in your data file
        includedaycrit = 1, #how many hours of valid data per day (midnight to midnight) is acceptable
        qwindow=c(0,24), #how to segment the day for day-segment specific analysis 
        mvpathreshold =c(100), #moderate to vigorous physical activity threshold (acceleration value)
        bout.metric = 6, #(bout=period of activity) defines the length of a bout, default = 6
        excludefirstlast = FALSE, #if TRUE, then the first and last night of the measurements get excluded
        includenightcrit = 1, #how many hours of a valid night (noon to noon) is acceptable
        #=====================
        # Part 3 + 4
        #=====================
        def.noc.sleep = 1, #specifies how the sleep period time window should be estimated if no sleeplog is used, default = 1
        outliers.only = TRUE, #default = TRUE
        criterror = 4, #default = 4
        do.visual = TRUE, #default = TRUE
        #=====================
        # Part 5
        #=====================
        threshold.lig = c(30), #threshold for light acceleration (activity) levels, default = 30
        threshold.mod = c(100),  #threshold for moderate acceleration (activity) levels, default = 100
        threshold.vig = c(400), #threshold for vigorous acceleration (activity) levels, default = 400
        boutcriter = 0.8, #what fraction of a bout needs to meet the threshold to be categorized as such, default = 0.8
        boutcriter.in = 0.9, #default = 0.9
        boutcriter.lig = 0.8, #default = 0.8
        boutcriter.mvpa = 0.8, #default = 0.8
        boutdur.in = c(1,10,30), #Configure duration of bouts. Note that this can be a vector of multiple values indicating the minimum and maximum duration of subsequent bout types, e.g. 1-5 minutes MVPA, 5-10 minutes MVPA, and longer than 10 minutes MVPA, default = c(1,10,30)
        boutdur.lig = c(1,10), #default = c(1,10)
        boutdur.mvpa = c(1), #default = c(1)
        save_ms5rawlevels = TRUE, #saves processed data to have access to specific timestamps and categorization of bouts
        part5_agg2_60seconds = TRUE, #smoothes the data over 1 min
        includedaycrit.part5 = 2/3, #default = 2/3
        #=====================
        # Visual report
        #=====================
        timewindow = c("WW"), #specify whether days should be defined from midnight to midnight "MM", from waking-up to waking-up "WW", or both c("MM","WW")
        visualreport=TRUE) #to create a visual report, this only works when all five parts of GGIR have successfully run
      
      # Step 3: Transfer the output folder to the original_dir_path
      # Attempt to move the temporary folder to the original directory after processing is complete
      copy_success <- file.copy(temp_output_dir_path, original_dir_path, recursive = TRUE)
      
      # If the copy was successful, delete the temp directory
      if (copy_success) {
        unlink(temp_output_dir_path, recursive = TRUE)
        # Display success message/icon, doesn't work either
        session$sendCustomMessage(type = "updateIcon", message = list(id = statusIconId, icon = "✅"))
        
        # Step 4: Verify the _Processed_ folder with the correct timestamp exists
        processedFolderPath <- file.path(original_dir_path, paste(folder_prefix, "Processed", timestamp, sep = "_"))
        # print(paste("processedFolderPath:", processedFolderPath)) #print statements for debugging
        
        if (dir.exists(processedFolderPath)) {
          
          # Step 5: Check if the output CSV file (used for further analysis) exists in the _Processed_ folder
          # Determine whether it's a Week or Night based on processedFolderPath
          if (grepl("Week", processedFolderPath)) {
            periodType <- "Week"
          } else if (grepl("Night", processedFolderPath)) {
            periodType <- "Night"
          } else {
            stop("The processed folder path does not specify Week or Night.")
          }
          # Construct csvDirPath based on periodType
          csvDirPath <- file.path(processedFolderPath, paste0("output_", periodType, "/meta/ms5.outraw/30_100_400"))
          # print(paste("csvDirPath:", csvDirPath)) #print statements for debugging
          # List CSV files in the directory
          csvFiles <- list.files(csvDirPath, pattern = "\\.csv$", full.names = TRUE)
          # print(paste("csvFiles:", csvFiles)) #print statements for debugging
          # Assuming there is exactly one CSV file
          if (length(csvFiles) == 1) {
            csvFilePath <- csvFiles[1]
            # print(paste("csvFilePath:", csvFilePath)) #print statements for debugging
            # Step 6: Proceed with further analysis using the found CSV file using the nextStepAnalysis function
            nextStepAnalysis(csvFilePath, processedFolderPath)
            
          } else {
            output[[errorMsgId]] <- renderUI({
              tags$div(class = "alert alert-danger", role = "alert", 
                       "No or multiple .csv files were detected.")
            })
          }
        } else {
          output[[errorMsgId]] <- renderUI({
            tags$div(class = "alert alert-danger", role = "alert", 
                     "The processed output folder doesn't exist.")
          })
        }
      } else {
        # Copy failed, display error message
        output[[errorMsgId]] <- renderUI({
          tags$div(class = "alert alert-danger", role = "alert", 
                   "Failed to transfer files to the original directory.")
        })
      }
    }) # Closing local
    }  # Closing for loop
}  # Closing function

# This function calculates sleep disturbance metrics from GENEActiv data.
# The data is categorized (the variable is called class_id) in different stages of activity (0-14), where 0 indicates sleep
# and 1-14 different kinds of physical activity (interpreted as wakefulness)
calculateDisturbanceMetrics <- function(startIndex, endIndex, GENEActiv_data) {
  total_sleep_duration_minutes <- as.numeric(difftime(GENEActiv_data$real_time[endIndex], GENEActiv_data$real_time[startIndex], units = "mins"))
  
  disturbances <- GENEActiv_data$class_id[startIndex:endIndex] > 0.5
  transitions <- diff(c(FALSE, disturbances, FALSE))
  starts <- which(transitions == 1)
  ends <- which(transitions == -1) - 1
  disturbance_durations <- if(length(starts) > 0) ends - starts + 1 else numeric(0)
  
  total_disturbance_duration_minutes <- sum(disturbance_durations)
  mean_disturbance_duration_minutes <- if(length(disturbance_durations) > 0) mean(disturbance_durations) else NA
  disturbance_count <- length(disturbance_durations)
  effective_sleep_duration_minutes <- total_sleep_duration_minutes - total_disturbance_duration_minutes
  
  return(list(
    mean_disturbance_duration = round(mean_disturbance_duration_minutes, 2),
    total_disturbance_duration = round(total_disturbance_duration_minutes, 2),
    disturbance_count = disturbance_count,
    total_sleep_duration_minutes = round(total_sleep_duration_minutes, 2),
    effective_sleep_duration_minutes = round(effective_sleep_duration_minutes, 2)
  ))
}

# Adds a new period (e.g., Night, Daytime Nap, Day) to the periodsData (output file) 
addPeriod <- function(periodsData, GENEActiv_data, periodType, startTimeIndex, endTimeIndex) {
  metrics <- if (periodType == "Night" || periodType == "Daytime Nap") calculateDisturbanceMetrics(startTimeIndex, endTimeIndex, GENEActiv_data)
  else list(
    mean_disturbance_duration = NA,
    total_disturbance_duration = NA,
    disturbance_count = NA,
    total_sleep_duration_minutes = NA,
    effective_sleep_duration_minutes = NA
  )
  
  # Round values
  total_sleep_duration_hours <- round(metrics$total_sleep_duration_minutes / 60, 2)
  effective_sleep_duration_hours <- round(metrics$effective_sleep_duration_minutes / 60, 2)
  sleep_efficiency <- if(!is.na(metrics$effective_sleep_duration_minutes) && metrics$total_sleep_duration_minutes > 0) {
    round((metrics$effective_sleep_duration_minutes / metrics$total_sleep_duration_minutes) * 100, 2)
  } else NA
  
  period <- list(
    period = periodType,
    start_time = GENEActiv_data$real_time[startTimeIndex],
    end_time = GENEActiv_data$real_time[endTimeIndex],
    total_sleep_duration_minutes = metrics$total_sleep_duration_minutes,
    total_sleep_duration_hours = total_sleep_duration_hours,
    disturbance_count = metrics$disturbance_count,
    mean_disturbance_duration_minutes = metrics$mean_disturbance_duration,
    total_disturbance_duration_minutes = metrics$total_disturbance_duration,
    effective_sleep_duration_minutes = metrics$effective_sleep_duration_minutes,
    effective_sleep_duration_hours = effective_sleep_duration_hours,
    sleep_efficiency = sleep_efficiency
  )
  
  return(c(periodsData, list(period)))
}

# Perform further analysis using the output CSV file generated by GGIR. 
# It aims to give a summary about the different day- and nighttime periods found in the GENEActiv data. 
nextStepAnalysis <- function(csvFilePath, processedFolderPath) {
  GENEActiv_data <- read.csv(csvFilePath)
  GENEActiv_data <- GENEActiv_data[order(GENEActiv_data$timenum), ]
  # The time has to be converted to be interpretable and usable for the following analysis
  GENEActiv_data$real_time <- as.POSIXct(GENEActiv_data$timenum, origin = "1970-01-01", tz = "GMT") + 3600
  
  # Intermediate file for debugging
  # write.csv(GENEActiv_data[c("real_time", "class_id")], "processed_data.csv", row.names = FALSE)
  
  # periodsData is the list where all the day- and nighttime periods are added after processing
  periodsData <- list()
  # in_sleep_phase is a boolean variable that indicates whether we're currently in a sleep phase or not
  in_sleep_phase <- FALSE
  # sleep_phase_start is used as a variable in which the night starts and the day ends
  sleep_phase_start <- NA
  # last_sleep_end is used as a variable in which the night ends and the day starts
  # It's initialized with the first timestamp found in the data since the recording always starts at daytime and marks
  # the beginning of the first "day"
  last_sleep_end <- 1
  
  for (i in 2:nrow(GENEActiv_data)) {
    # When we're not in a sleep phase and the class_id drops below 0.5 (0 being sleep), then the sleep phase starts and the day ends
    if (!in_sleep_phase && GENEActiv_data$class_id[i] < 0.5) {
      if (!is.null(sleep_phase_start)) {
        # Since the day ended, the data is stored in the periodsData list
        periodsData <- addPeriod(periodsData, GENEActiv_data, "Day", last_sleep_end - 1, i - 1)
      }
      # Sleep phase starts at timepoint with index i
      sleep_phase_start <- i
      in_sleep_phase <- TRUE
    } 
    # If already in a sleep phase it checks for conditions to determine if the current sleep phase has ended or needs to be recorded as a sleep or nap period.
    else if (in_sleep_phase) {
      endTime <- ifelse(i == nrow(GENEActiv_data), i, i - 1)
      startTimeHour <- as.integer(format(GENEActiv_data$real_time[sleep_phase_start], "%H"))
      periodDurationHours <- as.numeric(difftime(GENEActiv_data$real_time[endTime], GENEActiv_data$real_time[sleep_phase_start], units = "hours"))
      
      # Determine period type based on time and duration
      # If sleep started between 10 am and 5 pm AND duration is shorter than 4h, it's categorised as Daytime Nap
      periodType <- if (periodDurationHours < 4 && startTimeHour >= 10 && startTimeHour <= 17) "Daytime Nap" else "Night"
      
      # If at the end of the dataset OR (||) class_id > 0.5 (indicating wakefulness) and no sleep (!any class_id < 0.5) detected 
      # in the following hour (i+60), then we have a wakeup
      if ((i == nrow(GENEActiv_data)) || (GENEActiv_data$class_id[i] > 0.5 && !any(GENEActiv_data$class_id[i:min(i+60, nrow(GENEActiv_data))] < 0.5))) {
        # Night or Daytime Nap period being added
        periodsData <- addPeriod(periodsData, GENEActiv_data, periodType, sleep_phase_start, endTime)
        sleep_phase_start <- NA
        in_sleep_phase <- FALSE
        last_sleep_end <- i + 1
      }
    }
  }
  
  # This is how the output file is generated and properly named. 
  P <- dplyr::bind_rows(periodsData)
  newFileName <- sub("T5A5\\.csv$", "Processed.csv", basename(csvFilePath))
  #print(paste("newFileName:", newFileName)) #print statements for debugging
  newCsvFilePath <- file.path(processedFolderPath, newFileName)
  #print(paste("newCsvFilePath:", newCsvFilePath)) #print statements for debugging
  write.csv(P, newCsvFilePath, row.names = FALSE)
  print(paste("Done"))
  }
  
# Define the appearance and content of the UI (User Interface)
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateIcon', function(message) {
        $('#' + message.id).html(message.icon);
      });
    "))
  ),
  theme = shinytheme("flatly"),  # Using shinythemes for a better look
  titlePanel("Data Processing with GGIR"),
  
  navbarPage("Navigation", id="nav",
             tabPanel("Process Files",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("timepointFilter", "Select Timepoint", choices = NULL, selected = "All", options = list(`actions-box` = TRUE), multiple = TRUE),
                          pickerInput("participantFilter", "Select Participant", choices = NULL, selected = "All", options = list(`actions-box` = TRUE), multiple = TRUE),
                          selectInput("periodFilter", "Select Period", choices = c("All", "Night", "Week"), selected = "All"),
                          actionButton("processButton", "Process Selected Files")
                        ),
                        mainPanel(
                          textOutput("selectionMessage"),  # Placeholder for the selection message
                          uiOutput("fileCheckboxes"),  # File checkboxes and status icons will be here
                          textOutput("fileProcessingStatus")  # Status for file processing
                      )
                      )
             ),
             tabPanel("About",
                      "This app processes .bin files using the GGIR package. Select criteria and click 'Process Selected Files' to start. The processing status will be displayed here."
             ),
             tabPanel("Output First Part (GGIR)",
                      h2("Output Files Information"),
                      p("There are many output files generated by the GGIR function. Here, I try to explain and highlight the most important ones so far, respectively the ones I think I understand. The files are part of a folder (Night/Week_Processed_YYYY_MM_DD_HH_MM) which is generated in the same directory as the respective .bin file originates from.  If you want to know more, please consider the GGIR-website: "), a(href = "https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html#1_Introduction", "GGIR Package Introduction"),
                      p("These are some of the generated files:"),
                      tags$ul(
                        tags$li("Night/Week_Processed_YYYY_MM_DD_HH_MM\\output_XXX\\meta\\ms5.outraw\\30_100_400\\XXXXX_GAW_TX_T5A5.csv"),
                        tags$ul(
                          tags$li("This .csv-file contains the whole recorded timeframe subdivided in smoothed 1min-epochs (column timenum in the timeformat Unix time). The other important column is the class_id (0-14) which corresponds to a certain type of physical activity. This is more clearly explained in the corresponding csv-file (...ms5.outraw\\behavioralcodesXXXX-XX-XX.csv)."),
                        ),
                        tags$li("...meta\\sleep.qc\\graphperday_id_not stored in header_71001_GAW_T0.bin.pdf"),
                        tags$ul(
                          tags$li("Visualization of the angles that the watch had during the whole recording period"),
                        ),
                        tags$li("...results\\file summary reports\\Report_XXXXX_GAW_TX.bin.pdf"),
                        tags$ul(
                          tags$li("Visualization of the most important data already colored according to the activity type. Gives a very nice Impression of the recorded data."),
                        ),
                        tags$li("...results\\QC"),
                        tags$ul(
                          tags$li("QC stands for Quality Control. Therefore these files can be considered if you want to check the quality of your data."),
                        ),
                        tags$li("...results\\partX_day/nightsummary.csv"),
                        tags$ul(
                          tags$li("summarizes important parameters about the day/night of the specific days and nights of the recording"),
                        ),
                        tags$li("...results\\visualisation_sleep.pdf"),
                        tags$ul(
                          tags$li("I don't know why this doesn't work but in my experience, I never got an usable plot out of this."),
                        ),
                      
                        ),
                    
  ),
  tabPanel("Output Second Part",
           h2("Output Files Information"),
           p("There is only one file generated by the sencond part of the code. The file is stored here: ...Data\TX\participantnumber\Week/Night\Tx_participantnumber_Week/Night_Processed_YYYY_MM_DD_HH_MM",
           p("This file is called 'participantnumber_GAW/GAN_TX_Processed.csv'"),
           p("This file contains the following information:"),
           tags$ul(
             tags$li("period"),
             tags$ul(
               tags$li("Either Day, Night or Daytime Nap."),
             ),
             tags$li("start_time"),
             tags$ul(
               tags$li("Starting timepoint of respective period."),
             ),
             tags$li("end_time"),
             tags$ul(
               tags$li("Endpoint of respective period."),
             ),
             tags$li("total_sleep_duration_minutes/hours"),
             tags$ul(
               tags$li("end_time - start_time"),
             ),
             tags$li("disturbance_count"),
             tags$ul(
               tags$li("How many times was a switch from class_id < 0.5 (sleep) to class_id > 0.5 (non-sleep) detected during a specific night."),
             ),
             tags$li("mean/total_disturbance_duration_minutes/hours"),
             tags$ul(
               tags$li("self-explanatory"),
             ),
             tags$li("effective_sleep_duration_minutes/hours"),
             tags$ul(
               tags$li("total_sleep_duration_minutes/hours - total_disturbance_duration_minutes/hours"),
             ),
             tags$li("sleep_efficiency"),
             tags$ul(
               tags$li("100/total_sleep_duration*effective_sleep_duration"),
             ),
           ),
           
  )
)
)
)

# Defines the server logic
server <- function(input, output, session) {
  volumes <- getVolumes()
  baseDir <- "D:/Sleep & CoPe/GENEActiv/Data"
  
  # Set the text message for the selection page
  output$selectionMessage <- renderText({
    "Select the .bin files you wish to analyze from the list below. The filters on the left can be used to simplify the selection process. 
    Files displayed in gray indicate they have already been analyzed. However, you can still select these files for a new analysis if required.
    One file might take around 15 minutes to be analysed."
  })
  
  # Function to list all timepoint directories
  listTimepoints <- reactive({
    # List all directories in the base directory
    dirs <- list.dirs(baseDir, full.names = FALSE, recursive = FALSE)
    
    # Filter directories based on a pattern (e.g., directories starting with 'T' followed by digits)
    timepoint_dirs <- dirs[grepl("^T\\d+$", dirs)]
    
    # Sort timepoint directories
    sorted_timepoint_dirs <- sort(timepoint_dirs)
    
    if (length(sorted_timepoint_dirs) == 0) {
      return("All")
    } else {
      return(c("All", sorted_timepoint_dirs))
    }
  })
  
  # Update timepoint choices based on available timepoint directories
  observe({
    updatePickerInput(session, "timepointFilter", choices = listTimepoints(), selected = "All")
  })
  
  # Function to list all participant numbers
  listParticipants <- reactive({
    # List all .bin files
    files <- list.files(baseDir, pattern = "\\.bin$", full.names = TRUE, recursive = TRUE)
    
    # Extract participant numbers (5-digit numbers) from file names
    participant_numbers <- regmatches(files, regexpr("\\b\\d{5}\\b", files))
    
    # Deduplicate and sort participant numbers
    participant_numbers <- unique(participant_numbers)
    sorted_participant_numbers <- sort(participant_numbers)
    
    if (length(sorted_participant_numbers) == 0) {
      return("All")
    } else {
      return(c("All", sorted_participant_numbers))
    }
  })
  
  # Update participant choices based on available participant numbers
  observe({
    updatePickerInput(session, "participantFilter", choices = listParticipants(), selected = "All")
  })
  
  # Function to list all .bin files
  listBinFiles <- reactive({
    # List all .bin files recursively under the selected timepoints
    files <- lapply(input$timepointFilter, function(tp) {
      if (tp == "All") {
        return(list.files(baseDir, pattern = "\\.bin$", full.names = TRUE, recursive = TRUE))
      } else {
        return(list.files(file.path(baseDir, tp), pattern = "\\.bin$", full.names = TRUE, recursive = TRUE))
      }
    })
    files <- unlist(files)
    
    # Filter files based on input
    if (!"All" %in% input$participantFilter) {
      files <- files[vapply(files, function(f) {
        any(vapply(input$participantFilter, function(pf) {
          grepl(pf, f)
        }, logical(1)))
      }, logical(1))]
    }
    if (input$periodFilter != "All") {
      files <- files[grepl(input$periodFilter, files)]
    }
    
    files
  })
  
  # UI for file selection, "Process Selected Files" button
  output$fileCheckboxes <- renderUI({
    files <- listBinFiles()
    div(
      lapply(seq_along(files), function(i) {
        # Check if any folder with '_Processed_' in its name exists
        processed_folders <- list.files(dirname(files[i]), full.names = FALSE, recursive = FALSE, pattern = "_Processed_")
        processed_folder_exists <- length(processed_folders) > 0
        
        # Determine the color based on the existence of the processed folder
        color <- ifelse(processed_folder_exists, "color: gray;", "color: black;")
        
        div(
          style = color,
          checkboxInput(inputId = paste0("file", i), label = basename(files[i]), value = FALSE)
        )
      })
    )
  })
  
  # Define what happens if the processButton is pressed
  observeEvent(input$processButton, {
    # Disable the button and file selection UI if the code is running
    shinyjs::disable("processButton")
    shinyjs::disable("timepointFilter")
    shinyjs::disable("participantFilter")
    shinyjs::disable("periodFilter")
    shinyjs::disable("fileCheckboxes")
    
    # Get a logical vector indicating which files were selected
    selectedIndices <- sapply(seq_along(listBinFiles()), function(i) {
      input[[paste0("file", i)]]
    })
    
    # Filter filesToProcess based on selectedIndices
    filesToProcess <- listBinFiles()[selectedIndices]
    print(paste("Files to process:", filesToProcess))
    originalDirPaths <- sapply(filesToProcess, dirname)
    print(paste("originalDirPaths:", originalDirPaths))
    statusIconIds <- paste0("status_icon_", which(selectedIndices))
    errorMsgIds <- paste0("error_msg_", which(selectedIndices))
    
    # Right after updating UI with gear icons and before starting a long process
    Sys.sleep(0.1)  # Gives Shiny a chance to flush the message loop
    
    if (length(filesToProcess) > 0) {
      tryCatch({
        # Call the processFiles function to process the selected files
        processFiles(filesToProcess, originalDirPaths, statusIconIds, errorMsgIds, session, output)
        nextStepAnalysis(csvFilePath, processedFolderPath)
        
      }, error = function(e) {
        # Handle any errors that occur during the processing
        for (errorMsgId in errorMsgIds) {
          output[[errorMsgId]] <- renderUI({
            tags$div(class = "alert alert-danger", role = "alert",
                     "An error occurred during file processing: ", e$message)
          })
        }
      })
    }

    # After processing, re-enable the UI elements
    shinyjs::enable("processButton")
    shinyjs::enable("timepointFilter")
    shinyjs::enable("participantFilter")
    shinyjs::enable("periodFilter")
    shinyjs::enable("fileCheckboxes")
  }) 
}

# Calls the final GUI
shinyApp(ui = ui, server = server)
