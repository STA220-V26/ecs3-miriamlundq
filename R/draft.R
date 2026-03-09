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
