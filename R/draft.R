##2

#downloads the dataset if it dosen't exists yet
if (!fs::file_exists("data.zip")) {
curl::curl_download(
"https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
"data.zip",
quiet = FALSE
)
}


library(tidyverse)
library(data.table)

#the codes reads the file directly from the ZIP, so it does not unzip it manually
patients <-
readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
setDT() |>
setkey(id) #this defines the index column

#we remove useless columns or rows
#here we remove columns and rows with NA
patients <- janitor::remove_empty(patients, quiet = FALSE)
# A column with only one constant value is also not very interesting
patients <- janitor::remove_constant(patients, quiet = FALSE)


##3
library(pointblank)

checks <-
patients |>
create_agent(label = "A very simple example.") |>
col_vals_between(
where(is.Date),
as.Date("1900-01-01"),
as.Date(Sys.Date()),
na_pass = TRUE,
label = "All variables must be between 1900 - today's date"
) |>
col_vals_gte(
deathdate,
vars(birthdate),
na_pass = TRUE,
label = "The death date must happen after the birth date"
) |>
col_vals_regex(
ssn, # This variable was described in ECS1!
#the format xxx-xx-xxxx, since it it 3 repetitions, then 2 repetitions and then 4 repetitions, with any numbers between 0-9
"[0-9]{3}-[0-9]{2}-[0-9]{4}$",
label = "SSN must follow the format xxx-xx-xxxx"
) |>
col_is_integer(
id,
label = "The patient ID becomes an integer value"
) |>

#Additional checks from me: (I also did summary(patients) in the console to look at the data)
col_vals_in_set(
  marital,
  c("single", "married", "divorced", "widowed"),
  label = "Marital status must be either single, married, divorced, widowed"
) |>
  col_vals_in_set(
  gender,
  c("M", "F"),
  label = "Gender must be either male (M) or female (F)"
) |>
  col_vals_between(
  birthdate,
  as.Date("1900-01-01"),
  as.Date(Sys.Date()),
  label = "Birthdate must be between 1900 and today, not resonable for someone being born before"
) |>
interrogate()

checks

#creates an report with the data quality problems, so what checks passed and failed
export_report(checks, "patient_validation.html")


##3.1

#counts how many observations there is en each martial category
patients[, .N, marital] # Run it!

#it puts labels on the letters so they become more readable 
#so for example S becomes Single
patients[,
marital := factor(
marital,
levels = c("S", "M", "D", "W"),
labels = c("Single", "Married", "Divorced", "Widowed")
)
]

#doing the same for gender
patients[, .N, gender]
patients[,
  gender := factor(
    gender,
    levels = c("M", "F"),
    labels = c("Male", "Female")
  )
]

#we find character columns with fewer than 10 unique values
#this is becuase thet are more likely to be variables that could be converted into factors
fctr_candidates <-
patients[, which(lapply(.SD, uniqueN) < 10), .SDcols = is.character] |>
names()
#this shows the uniques values for each of those variables
#this makes it easy to see and decude if the variable should be a factor
patients[,
lapply(.SD, \(x) paste(unique(x), collapse = ", ")),
.SDcols = fctr_candidates
] |>
glimpse()

# This converts categorical variables with a small number of unique values to factors
# makes them easier to interpret 
patients[,
names(.SD) := lapply(.SD, as.factor),
.SDcols = c("prefix", "suffix", "race", "ethnicity", "state")
]


##3.2

#counts how many individuals exists for each combination of gender, race and state
patients[, .N, .(gender, race, state)][order(N)]

#combines rare race catgories into other to reduce the very small groups
#this is becuase small groups could potentially identify bias
#0.05 means those groups representing less then 5% of the data will be combined
patients[, race := forcats::fct_lump_prop(race, prop = 0.05)]


##4 

# Calculates each patient's age in years based on todays date
patients[, age := as.integer((as.IDate(Sys.Date()) - birthdate)) %/% 365.241]
# a histogram for the age distribution
patients[, hist(age)]

#filters out the people who died, histogram plot
patients[is.na(deathdate), hist(age)]

library(duckplyr)

# unzip 
unzip("data.zip", files = "data-fixed/payer_transitions.csv")

# finding the latest start_date to estimate the dataset snapshot
lastdate <- readr::read_csv("data-fixed/payer_transitions.csv") |>
  summarise(lastdate = max(start_date)) |>
  pull(lastdate)

lastdate

# calculate age at time of dataset snapshot
patients[, age_extract := as.integer((as.IDate(lastdate) - birthdate)) %/% 365.241]

# histogram of living patients' age at snapshot time
patients[is.na(deathdate), hist(age_extract)]


##5
# Replace missing values in prefix and middle name with empty strings
# so they do not appear as "NA" when constructing full names
patients[,
names(.SD) := lapply(.SD, \(x) replace_na(x, "")),
.SDcols = c("prefix", "middle")
]
# Combine name components into a single full name variable
patients[,
full_name := paste(
prefix,
first,
middle,
last,
fifelse(!suffix %in% c("", NA), paste0(", ", suffix), "") ## Adds suffix only when it exists
)
]
patients[, full_name]

#removes preciding and following whitespaces
patients[, names(.SD) := lapply(.SD, trimws), .SDcols = is.character]

#takes away double spaces
patients[, full_name := stringr::str_replace(full_name, " ", " ")]

#removes the original name columns since we don't need them anymore
patients[, c("prefix", "first", "middle", "last", "suffix", "maiden") := NULL]
