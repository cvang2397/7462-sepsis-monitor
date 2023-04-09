#task #1: setup ----
library(tidyverse)
library(data.table) ## For the fread function
library(lubridate)

source("sepsis_monitor_functions.R")

#task #2: speed reading ----
library(tictoc)

#fread test ----
#50 patients (0.003 sec) - fread 
tic()

makeSepsisDataset <- function(n = 50, read_fn = "fread") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()

#100 patients (0.003 sec) - fread
tic()

makeSepsisDataset <- function(n = 100, read_fn = "fread") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()

#500 patients (0.006 sec) - fread
tic()

makeSepsisDataset <- function(n = 500, read_fn = "fread") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()


#read_delim test ----
#50 patients (0.003 sec) - read_delim
tic()

makeSepsisDataset <- function(n = 50, read_fn = "read_delim") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()

#100 patients (0.004 sec) - read_delim
tic()

makeSepsisDataset <- function(n = 100, read_fn = "read_delim") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()

#500 patients (0.004 sec) - read_delim
tic()

makeSepsisDataset <- function(n = 500, read_fn = "read_delim") {
  ptids <- sample(1:10000, n, replace = FALSE)
  
  sepsis_data <- map_dfr(ptids, getPatient, read_function = read_fn) %>%
    filter(ICULOS > 1) %>%
    group_by(PatientID) %>%
    mutate(ICULOS = ICULOS - min(ICULOS)) %>%
    mutate(obsTime = Sys.time() + c(0, sort(runif(n()-1, min(ICULOS), max(ICULOS))*3600))) %>%
    arrange(PatientID)
  
  return(sepsis_data)
}

toc()

#task 3: upload to google dirve ----
library(googledrive)

df <- makeSepsisDataset()

# We have to write the file to disk first, then upload it
df %>% write_csv("sepsis_data_temp.csv")

url <- "https://drive.google.com/drive/folders/1UgJXz01f1dDP0Hhrf0lMZ0qxVQVoRfUQ"

# Uploading happens here
sepsis_file <- drive_put(media = "sepsis_data_temp.csv", 
                         path = url,
                         name = "sepsis_data.csv")

# Set the file permissions so anyone can download this file.
sepsis_file %>% drive_share_anyone()