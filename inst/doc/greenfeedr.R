## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(greenfeedr)

## ----download data------------------------------------------------------------
USER <- "your_username"
PASS <- "your_password"

studies <- list(
  list(
    Experiment = "Experiment_01",
    Unit = c(2, 3),
    StartDate = "2024-01-20",
    EndDate = Sys.Date(),
    save_dir = "/tempdir()/Experiment_01/"
  ),
  list(
    Experiment = "Experiment_02",
    Unit = c(212),
    StartDate = "2024-02-01",
    EndDate = Sys.Date(),
    save_dir = "/tempdir()/Experiment_02/"
  )
)

# Here you loop (using 'for') over all your studies applying get_gfdata() function
# for (element in studies) {
#  get_gfdata(USER, PASS, element$Experiment, element$Unit, element$StartDate, element$EndDate, element$save_dir)
# }

## ----data---------------------------------------------------------------------
# Open the daily data downloaded from C-Lock Inc. server
daily_data <- readr::read_csv(system.file("extdata", "StudyName_GFdata.csv", package = "greenfeedr"), show_col_types = FALSE)

# View the structure of the daily data
str(daily_data)

# View the first few rows of the daily data
head(daily_data)

# Open the finalized data received from C-Lock Inc.
final_data <- readxl::read_excel(system.file("extdata", "StudyName_FinalReport.xlsx", package = "greenfeedr"),
  col_types = c("text", "text", "numeric", rep("date", 3), rep("numeric", 12), "text", rep("numeric", 6))
)

# View the structure of the daily data
str(final_data)

# View the first few rows of the daily data
head(final_data)

## ----report data, message = FALSE, echo = FALSE, warning = FALSE--------------
library(dplyr)
library(ggplot2)

file <- system.file("extdata", "StudyName_FinalReport.xlsx", package = "greenfeedr")

start_date <- "2024-05-13"
end_date <- "2024-05-25"
input_type <- "final"
plot_opt <- "All"
rfid_file <- NULL

df <- readxl::read_excel(file, col_types = c("text", "text", "numeric", rep("date", 3), rep("numeric", 12), "text", rep("numeric", 6)))
names(df)[1:14] <- c(
  "RFID",
  "AnimalName",
  "FeederID",
  "StartTime",
  "EndTime",
  "GoodDataDuration",
  "HourOfDay",
  "CO2GramsPerDay",
  "CH4GramsPerDay",
  "O2GramsPerDay",
  "H2GramsPerDay",
  "H2SGramsPerDay",
  "AirflowLitersPerSec",
  "AirflowCf"
)

# df contains finalized GreenFeed data
df <- df %>%
  ## Remove leading zeros from RFID col to match with IDs
  dplyr::mutate(
    RFID = gsub("^0+", "", RFID),
    ## Extract hours, minutes, and seconds from GoodDataDuration
    GoodDataDuration = round(
      as.numeric(substr(GoodDataDuration, 12, 13)) * 60 + # Hours to minutes
        # as.numeric(substr(GoodDataDuration, 1, 2)) * 60 +  # Hours to minutes
        as.numeric(substr(GoodDataDuration, 15, 16)) + # Minutes
        # as.numeric(substr(GoodDataDuration, 4, 5)) +
        as.numeric(substr(GoodDataDuration, 18, 19)) / 60, # Seconds to minutes
      # as.numeric(substr(GoodDataDuration, 7, 8)) / 60,
      2
    )
  ) %>%
  ## Remove data with Airflow below the threshold (25 l/s) and data in the time range selected
  dplyr::filter(
    AirflowLitersPerSec >= 25,
    as.Date(StartTime) >= as.Date(start_date) & as.Date(StartTime) <= as.Date(end_date)
  )


cols_to_convert <- c("CH4GramsPerDay", "CO2GramsPerDay", "O2GramsPerDay", "H2GramsPerDay")
df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)

# Plot 1: Total number of production records per day
plot1 <- ggplot(as.data.frame(table(as.Date(df$StartTime))), aes(x = Var1, y = Freq)) +
  geom_col(color = "black") +
  labs(
    title = "Total Records Per Day",
    x = "",
    y = "Total Records"
  ) +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 2.2, position = position_dodge(width = 0.9)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1.05, size = 8),
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

plot1

# Assuming RFIDfile is provided or not, set the grouping variable
group_var <- if (!is.null(rfid_file) && is.data.frame(rfid_file) && nrow(rfid_file) > 0) "FarmName" else "RFID"

farmname_order <- df %>%
  dplyr::mutate(day = as.Date(EndTime)) %>%
  dplyr::group_by(!!sym(group_var), day) %>%
  dplyr::summarise(
    n = n(),
    daily_CH4 = weighted.mean(CH4GramsPerDay, GoodDataDuration, na.rm = TRUE)
  ) %>%
  dplyr::group_by(!!sym(group_var)) %>%
  dplyr::summarise(
    n = sum(n),
    daily_CH4 = mean(daily_CH4, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(daily_CH4)) %>%
  dplyr::pull(!!sym(group_var))


# Plot 1: Total number of records per animal
plot1 <- df %>%
  dplyr::mutate(day = as.Date(EndTime)) %>%
  dplyr::group_by(!!sym(group_var), day) %>%
  dplyr::summarise(
    n = n(),
    daily_CH4 = weighted.mean(CH4GramsPerDay, GoodDataDuration, na.rm = TRUE)
  ) %>%
  dplyr::group_by(!!sym(group_var)) %>%
  dplyr::summarise(
    n = sum(n),
    daily_CH4 = mean(daily_CH4, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = factor(!!sym(group_var), levels = farmname_order), y = n)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Total Records Per Animal",
    x = "",
    y = "Total Records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1.05, size = 8),
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  geom_text(aes(label = n), vjust = -1, color = "black", position = position_dodge(width = 0.9), size = 2.2)


# Plot distribution of records throughout the day
plot2 <- df %>%
  dplyr::mutate(AMPM = case_when(
    HourOfDay >= 22 ~ "10PM-4AM",
    HourOfDay < 4 ~ "10PM-4AM",
    HourOfDay >= 4 & HourOfDay < 10 ~ "4AM-10AM",
    HourOfDay >= 10 & HourOfDay < 16 ~ "10AM-4PM",
    HourOfDay >= 16 & HourOfDay < 22 ~ "4PM-10PM",
    TRUE ~ NA_character_
  )) %>%
  dplyr::group_by(!!sym(group_var), AMPM) %>%
  dplyr::summarise(n = n()) %>%
  ggplot(aes(
    x = factor(!!sym(group_var), levels = farmname_order), y = n,
    fill = factor(AMPM, levels = c("10PM-4AM", "4AM-10AM", "10AM-4PM", "4PM-10PM"))
  )) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Daily Records Distribution",
    x = "",
    y = "Percentage of total records",
    fill = "Time-Windows (24h)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1.05, size = 8),
    legend.position = "bottom",
    axis.title.y = element_text(size = 10, face = "bold")
  ) +
  scale_fill_brewer(palette = "BrBG") +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"), expand = c(0, 0))


# Create the plots
plot1
plot2

## ----pellin and viseat--------------------------------------------------------
file <- list(system.file("extdata", "feedtimes.csv", package = "greenfeedr"))

result <- pellin(
  file_path = file,
  unit = 1,
  gcup = 34,
  start_date = "2024-05-13",
  end_date = "2024-05-25",
  save_dir = tempdir()
)

head(result)

