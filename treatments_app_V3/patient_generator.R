# Function needed to generate patients in the /www/datas folder


# id: patient id, must be unique
# name: patient name
# picture: path to image if any
# age: patient age
# height: patient height in cm
# weight: patient weight in kg
# gender: whether the patient is a male or female. Use to generate rendom avatar images
# medical_history -> list containing the following fields pathologies,
# examination_dates, doctors, doctors_gender, disease_description, disease_image that are also lists
# disease_id <- php1, hypopara, hypoD3, ... To set up the initial conditions

patient_generator <- function(id, name, picture = NULL, age, height, weight, gender,
                              pathologies = list() , examination_dates = list(), 
                              doctors = list(), doctors_gender = list(), 
                              disease_description = list(), disease_image = list(), 
                              disease_id) {
  
  # check if the provided id is already used
  data_folder <- paste0(getwd(), "/treatments_app_V3/www/datas")
  file_list <- list.files(data_folder)
  n_patients <- length(file_list)
  
  if (n_patients > 0) {
    test <- lapply(1:n_patients, FUN = function(i) {
      temp <- readRDS(file = paste0(data_folder, "/", "patient_", i, ".rds"))
      if (id == temp$id) stop("You must choose a unique id number")
    })
  } 
  
  # setup initial conditions depending on the disease_id
  state_folder <- paste0(getwd(), "/treatments_app_V3/www")
  if (disease_id == "php1") {
    state <- read.csv(paste0(state_folder, "/init_php1.csv"),
                      stringsAsFactors = FALSE)
    state <- unlist(state[,-1])
  } else if (disease_id == "hypopara") {
    state <- read.csv(paste0(state_folder, "/init_hypopara.csv"),
                      stringsAsFactors = FALSE)
    state <- unlist(state[,-1])
  } else if (disease_id == "hypoD3") {
    state <- read.csv(paste0(state_folder, "/init_hypoD3.csv"),
                      stringsAsFactors = FALSE)
    state <- unlist(state[,-1])
  }
  
  # set up random patient image
  patient_images_folder <- paste0(state_folder, "/patients_img")
  if (gender == "male") {
    patient_images_folder <- paste0(patient_images_folder, "/male")
  } else {
    patient_images_folder <- paste0(patient_images_folder, "/female")
  }
  random_image_number <- sample(1:12, 1)
  patient_avatar <- paste0(
    patient_images_folder, 
    "/", 
    list.files(patient_images_folder)[[random_image_number]]
  )
  patient_avatar <- unlist(str_split(string = patient_avatar, pattern = "www/"))[2]
  
  # set up a rendom doctor image
  doctor_images_folder <- paste0(state_folder, "/doctors_img")
  doctors_avatars <- lapply(1:length(doctors), FUN = function(i){
    if (doctors_gender[[i]] == "male") {
      doctor_images_folder <- paste0(doctor_images_folder, "/male")
    } else {
      doctor_images_folder <- paste0(doctor_images_folder, "/female")
    }
    random_image_number <- sample(1:7, 1)
    doctor_avatar <- paste0(
      doctor_images_folder, 
      "/", 
      list.files(doctor_images_folder)[[random_image_number]]
    )
    doctor_avatar <- unlist(str_split(string = doctor_avatar, pattern = "www/"))[2]
  })
  
  
  # if the previous id test id passed, generated the patient
  patient_data <- list(
    id = id,
    name = name,
    picture = patient_avatar,
    age = age,
    height = paste0(height, " cm"),
    weight = paste0(weight, " kg"),
    medical_history = list(
      pathologies = pathologies,
      examination_dates = examination_dates,
      doctors = doctors,
      doctors_avatars = doctors_avatars,
      disease_description = disease_description,
      disease_image = disease_image
    ),
    disease_id = disease_id,
    initial_conditions = state
  )
  saveRDS(object = patient_data, file = paste0(getwd(), "/treatments_app_V3/www/datas/patient_", id, ".rds"))
}

# patient_generator(
#   id = 1,
#   name = "David",
#   age = 27,
#   height = 164,
#   weight = 70,
#   gender = "male",
#   disease_id = "php1",
#   pathologies = list("calcium stones", "nausea", "hypertension") ,
#   examination_dates = list("17-02-16", "24-08-17", "26-04-18"),
#   doctors = list("Prof. Adam Jones", "Dr.med. Ulla Turman", "Dr. John Smith"),
#   doctors_gender = list("male", "female", "male"),
#   disease_description = list(
#     "Calcium oxalate stones are the most common type of kidney stone.
#     Kidney stones are solid masses that form in the kidney when
#     there are high levels of calcium, oxalate, cystine, or phosphate
#     and too little liquid. There are different types of kidney stones.
#     Your healthcare provider can test your stones to find what type you have.
#     Calcium oxalate stones are caused by too much oxalate in the urine",
#     "Nausea is pronounced stomach discomfort and the sensation of wanting to vomit.
#     Nausea can be a precursor to vomiting the contents of the stomach.
#     The condition has many causes and can often be prevented",
#     "Traditionnellement, l'hypertension est définie comme une tension artérielle
#     supérieure à 140/90, et est considérée comme grave si elle est supérieure à 180/120"
#     ),
#   disease_image = list(
#     "https://steemitimages.com/0x0/https://img.esteem.ws/4dssd1ydr0.jpg",
#     NULL,
#     "https://image.shutterstock.com/image-photo/blood-pressure-gauge-show-hypertension-260nw-611687891.jpg"
#   )
# )