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


##6

#creates a new variable that gives true or false wheter the person have a drivers licence or not
# patients[, driver := !is.na(drivers)][, drivers := NULL]
#however the dataset already had a driver variable so I got an error message so I did not use this code line

library(leaflet)

#Visualizes the patient locations on a map
leaflet::leaflet(data = patients) |>
  leaflet::addTiles() |>
  leaflet::addMarkers(~lon, ~lat, label = ~full_name)

#Would it be reasonable to make statistical inference for the whole of USA? For the whole world?
#Answer: no becuase the locations seems mostly to be at the same place
#therefore it is not representative to the entire USA and not the entire world
#the statistical inference should only be used to the sampled population


##7
# Load procedures dataset directly from the zip 
procedures <- readr::read_csv(
  unz("data.zip", "data-fixed/procedures.csv")
)

# Keep relevant columns and remove observations without procedure reason
procedures <- procedures |>
  dplyr::select(patient, reasoncode_icd10, start) |>
  dplyr::filter(!is.na(reasoncode_icd10))

# Convert to data table and set patient as key
setDT(procedures, key = "patient")

# Extract year of the procedure and remove the exact date
procedures[, year := lubridate::year(start)][, start := NULL]

# Joins patient birthdates and calculates age at procedure
# keep adults (>=18 years) a
proc_n_adults <-
procedures[
  patients[, .(id, birthdate = as.IDate(birthdate))],
  on = c(patient = "id")
] |>
_[year - lubridate::year(birthdate) >= 18L, .N, .(reasoncode_icd10, year)]

library(decoder)

# Add descriptive labels for ICD-10 codes
cond_by_year <- setDT(decoder::icd10se)[
  proc_n_adults,
  on = c(key = "reasoncode_icd10")
]

# Find the five most common procedure conditions
top5 <- cond_by_year[, .(N = sum(N)), .(value)][order(-N)][1:5, value]

# Plot procedure trends over time for the five most common conditions
ggplot(cond_by_year[.(top5), on = "value"], aes(year, N, color = value)) +
  geom_line() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_discrete(
    labels = function(x) stringr::str_wrap(x, width = 40)
  )

#Wath happens in the end? Is it really reasonable to include tha last year (we were looking at the assumed data extraction date earlier)
#Answer: The last year shows a drop in procedure counts, maybe becuase of extraction before the year was over
#so including the last year would give misleading results

#What happened early in history? Do we actually have all relevant data already from the start
#or should we focus on years which are more acurately recorded?
#Answer: # Early years contain very few observations, might reflect incomplete
#historical records. we should be cautious interpretating early trends


#Does this visualisation tell us anything? Are certain conditions more common today or do we
#need to standardize the numbers with account to population size or health seeking behaviour
#etc.
#Answer: Increasing counts over time may reflect population growth, improved
# diagnostics, or increased healthcare utilization rather than true
# increases in disease prevalence.


#Are patients sicker today or do they get more treatment for conditions which might have been
#undertreated in the past
#Answer: not necessarily, it may reflect improved diagnosis, expanded treatment options,
# or greater access to healthcare services