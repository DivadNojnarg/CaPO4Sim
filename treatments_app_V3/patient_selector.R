patient_selector <- function() {
  # read the datas folder and 
  # find the number of patients
  data_folder <- paste0(getwd(), "/www/datas")
  file_list <- list.files(data_folder)
  n_patients <- length(file_list)
  
  # generate a random number
  random_patient <- sample(1:n_patients, 1)
  readRDS(file = paste0(data_folder, "/", "patient_", random_patient, ".rds"))
}