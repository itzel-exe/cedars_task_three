
# Load the packages needed for the analysis
library(readxl)      # Used for working with Excel files
library(dplyr)       # Used for filtering, grouping, sorting, and modifying data
library(lubridate)   # Used for working with dates and times
library(writexl)     # Used to export the final results to an Excel file


# Reads CSV file
data <- read.csv(
  "C:/Users/Itzel/Desktop/tasks/assignment_three/202210 - EDW - CCU_assessment.csv"
)


# This prints all of the column names
#print(names(data))


# Group the data by patient and encounter.
# MRN_hidden identifies the patient.
# ENC_NO_hidden identifies the hospital encounter.

# Convert the date/time column from text into a date/time format
# This allows us to calculate how many hours passed between ICU admission,
# ICU discharge, and later ICU admissions.
data <- data %>%
  mutate(
    ADT_DTTM_hidden = mdy_hm(ADT_DTTM_hidden)
  )
# Display the three assignment example encounters
# results %>%
#   filter(
#     (MRN_hidden == 203470 & ENC_NO_hidden == 29204845127) |
#       (MRN_hidden == 51455895 & ENC_NO_hidden == 29188820095) |
#       (MRN_hidden == 51455895 & ENC_NO_hidden == 29192933522)
#   )

# Within each patient/encounter, sort the events by date and time so that
# we can examine the patient's events in chronological order.
data <- data %>%
  group_by(MRN_hidden, ENC_NO_hidden) %>%
  arrange(ADT_DTTM_hidden, .by_group = TRUE)




# Create two new TRUE/FALSE columns to identify ICU movements.
data <- data %>%
  mutate(
    # TRUE when the patient is being transferred TO the CICU.
    # This identifies an ICU entry.
    ICU_ENTRY = TO_DEPT == "4N-CICU",
    
    # TRUE when the patient is leaving the CICU.
    # The patient must be coming FROM the CICU and the event must be
    # either a TRANSFER OUT or a DISCHARGE.
    ICU_EXIT = FROM_DEPT == "4N-CICU" &
      ADT_EVNT_NM %in% c("TRANSFER OUT", "DISCHARGE")
  )


# Create a smaller table containing only events that are relevant
# to entering or leaving the ICU.
#
# This is for checking/debugging the ICU logic.
# icu_events <- data %>%
#   filter(ICU_ENTRY | ICU_EXIT) %>%
#   select(
#     MRN_hidden,
#     ENC_NO_hidden,
#     ADT_EVNT_NM,
#     FROM_DEPT,
#     ADT_DTTM_hidden,
#     TO_DEPT,
#     ICU_ENTRY,
#     ICU_EXIT
#   )

# Determines the first ICU admission, first ICU discharge, and first 
# ICU length of stay for one encounter.
find_first_icu <- function(df) {
  
  # Sorts the events chronologically within this encounter
  # Makes sure we look at the patient's events in the correct
  # order based on the date and time.
  df <- df %>%
    arrange(ADT_DTTM_hidden)
  
  # Finds the row numbers where the patient entered the ICU.
  entry_rows <- which(df$ICU_ENTRY)
  

  # If the patient never entered the ICU during this encounter,
  # return missing values for all three ICU fields.
  if (length(entry_rows) == 0) {
    return(tibble(
      FIRST_ICU_ADMISSION = as.POSIXct(NA),
      FIRST_ICU_DISCHARGE = as.POSIXct(NA),
      FIRST_ICU_LOS = NA_real_
    ))
  }
  
  
  # Uses the first ICU entry as the patient's first ICU admission.
  entry_index <- entry_rows[1]
  admission <- df$ADT_DTTM_hidden[entry_index]
  
  # Keeps track of the current ICU entry in case the patient leaves
  # and returns to the ICU within one hour.
  current_entry_index <- entry_index
  
  
  # Continues checking ICU exits and possible ICU re-entries.
  while (TRUE) {
    
    
    # Finds the next ICU exit that happens after the current ICU entry.
    exit_rows <- which(
      df$ICU_EXIT &
        seq_len(nrow(df)) > current_entry_index
    )
    
    
    # If there is no ICU exit after the entry, the patient is considered
    # to still be in the ICU.
    # The assessment says to use October 3, 2022 as the discharge date
    # when the patient is still in the ICU.
    if (length(exit_rows) == 0) {
      
      discharge <- as.POSIXct("2022-10-03 00:00:00")
      
      break
    }
    
    
    # Uses the first ICU exit after the current ICU entry.
    exit_index <- exit_rows[1]
    exit_time <- df$ADT_DTTM_hidden[exit_index]
    
    
    # Looks for another ICU entry after this ICU exit.
    next_entry_rows <- which(
      df$ICU_ENTRY &
        seq_len(nrow(df)) > exit_index
    )
    
    
    # If there is no later ICU entry, the ICU exit we found
    # is the end of the first ICU visit.
    if (length(next_entry_rows) == 0) {
      
      discharge <- exit_time
      
      break
    }
    
    
    # Gets the date and time of the next ICU entry.
    next_entry_index <- next_entry_rows[1]
    next_entry_time <- df$ADT_DTTM_hidden[next_entry_index]
    
    
    # Calculates the number of hours between leaving the ICU
    # and returning to the ICU.
    gap_hours <- as.numeric(
      difftime(next_entry_time, exit_time, units = "hours")
    )
    
    
    # If the patient was outside the ICU for MORE than one hour,
    # the first ICU visit is considered finished at the original exit.
    if (gap_hours > 1) {
      
      discharge <- exit_time
      
      break
      
    } else {
      
      # If the patient returned to the ICU within one hour,
      # treat the ICU stay as continuous.
      # Updates the current ICU entry and continue checking.
      current_entry_index <- next_entry_index
    }
  }
  
  
  # Calculates the ICU length of stay in hours.
  los <- as.numeric(
    difftime(discharge, admission, units = "hours")
  )
  
  
  # Returns the three requested results for this encounter.
  # Whole number of hours, as required by the assessment.
  tibble(
    FIRST_ICU_ADMISSION = admission,
    FIRST_ICU_DISCHARGE = discharge,
    FIRST_ICU_LOS = floor(los)
  )
}

# Applies the function to every patient/encounter combination

# Each patient and encounter is analyzed separately
# The function returns the first ICU admission, first ICU discharge,
# and first ICU length of stay for each encounter
results <- data %>%
  group_by(MRN_hidden, ENC_NO_hidden) %>%
  group_modify(~ find_first_icu(.x)) %>%
  ungroup()

# Print the final results
print(results)

# ---------------------------------------------------------
# TESTING
# ---------------------------------------------------------

# Prints the ICU-related events to check whether ICU entries
# and exits were identified correctly.
#print(icu_events)

# Checks how many missing date/time values exist.
#print(sum(is.na(data$ADT_DTTM_hidden)))

# ---------------------------------------------------------
# EXPORT RESULTS
# ---------------------------------------------------------

# Save the final results as an Excel file.
#
# The Excel file will be saved in the assignment_three folder
# with the name "ICU_results.xlsx".
# write_xlsx(
#   results,
#   "C:/Users/Itzel/Desktop/tasks/assignment_three/ICU_results.xlsx"
# )
write.csv(results, "C:/Users/Itzel/Desktop/tasks/assignment_three/ICU_results.csv", row.names = FALSE)
