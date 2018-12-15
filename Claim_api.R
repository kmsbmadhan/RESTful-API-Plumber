library(plumber)
model <- readRDS("model.Rds")

MODEL_VERSION <- "0.0.1"
VARIABLES <- list(
  AccidentArea = "AccidentArea = Urban/Rural)",
  Sex = "Sex = Male/Female",
  Age = "Age = # in years",
  Fault = "Fault = ThirdParty/PolicyHolder",
  VehicleCategory = "VehicleCategory = Sedan/Sport/Utility",
 # VehiclePrice = "VehiclePrice = ",
#  FraudFound_P = "Successful submission will results in a prediction of 0(no) or 1(yes)",
  Deductible = "Deductible = 300 to 700",
 # Days_Policy_Accident = "Days after accident",
  #PastNumberOfClaims = "Past Number of claims",
  #AgeOfVehicle = "Age = #Age of vehicle",
  #AgeOfPolicyHolder = "AgeOfPolicyHolder = #",
  AgentType = "AgentType = External/Internal",
  #NumberOfSuppliments = "NumberOfSuppliments = ",
  AddressChange_Claim = "AddressChange_Claim = 1 year/2 to 3 years/4 to 8 years/no change/under 6 months",
  BasePolicy = "BasePolicy = All Perils/Collision/Liability",
  MaritalStatus = "MaritalStatus = Single/Married/Divorced/Widow")

transform_titantic_data <- function(input_claim_data) {
  ouput_claim_data <- data.frame(
    AccidentArea = character(input_claim_data$AccidentArea, levels = c("Urban","Rural")),
    Sex = character(input_claim_data$Sex, levels = c("Male","Female")),
    Age = integer(input_claim_data$Age),
    Fault = character(input_claim_data$Fault, levels = c("Third Party","Policy Holder")),
    VehicleCategory = character(input_claim_data$VehicleCategory, levels = c("Sedan","Sport","Utility")),
    Deductible = integer(input_claim_data$Deductible),
    AgentType = character(input_claim_data$AgentType, levels = c("External","Internal")),
    AddressChange_Claim = character(input_claim_data$AddressChange_Claim, levels = c("1 year","2 to 3 years","4 to 8 years","no change","under 6 months")),
    BasePolicy = character(input_claim_data$BasePolicy, levels = c("All Perils","Collision","Liability")),
    MaritalStatus = character(input_claim_data$MaritalStatus, levels = c("Single","Married","Divorced","Widow"))
  )
}

validate_feature_inputs <- function(AccidentArea, Sex, Age,Fault,VehicleCategory,Deductible,AgentType,BasePolicy,MaritalStatus,AddressChange_Claim) {
  AccidentArea_valid <- (AccidentArea %in% c("Urban","Rural"))
  Age_valid <- (Age >= 0 & Age < 150 | is.na(Age))
  Sex_valid <- (Sex %in% c("Male", "Female"))
  Fault_valid <- (Fault %in% c("Policy Holder","Third Party"))
  VehicleCategory_valid <- (VehicleCategory %in% c("Sport","Utility","Sedan"))
  Deductible_valid <- (Deductible %in% c(300,400,500,700))
  AgentType_valid <- (AccidentArea %in% c("External","Internal"))
  BasePolicy_valid <- (BasePolicy %in% c("All Perils","Collision","Liability"))
  MaritalStatus_valid <- (MaritalStatus %in% c("Single", "Married","Divorced","Widow"))
  AddressChange_Claim_valid <- (AddressChange_Claim %in% c("1 year", "2 to 3 years","4 to 8 years","no change","under 6 months"))
  
  
  tests <- c("Age must be between 0 and 150 or NA", 
             "AccidentArea must be Urban/Rural", 
             "Sex must be either Male/Female",
             "Fault must be Policy Holder/Third Party",
             "Vehicle Category must be Sport/Utility/Sedan",
             "Deductible must be 300/400/500/700",
             "Agenttype must be External/Internal",
             "BasePolicy must be All Perils/Collision/Liability",
             "MaritalStatus must be Single/Married/Divorced/Widow",
             "AddressChange_Claim must be 1year/2-3years/4-8years/under 6 months/no change")
  test_results <- c(AccidentArea_valid, Age_valid, Sex_valid, Fault_valid, VehicleCategory_valid,Deductible_valid,AgentType_valid,
                    BasePolicy_valid,MaritalStatus_valid, AddressChange_Claim_valid)
  if(!all(test_results)) {
    failed <- which(!test_results)
    return(tests[failed])
  } else {
    return("OK")
  }
}


#* @get /claimcheck
claimcheck <- function() {
  result <- data.frame(
    "input" = "",
    "status" = 200,
    "model_version" = MODEL_VERSION
  )
  
  return(result)
}



#* @get /
#* @html
home <- function() {
  title <- "Insurance Claim Prediction API"
  body_intro <-  "Welcome to the Insurance Claim Prediction API!"
  body_model <- paste("We are currently serving model version:", MODEL_VERSION)
  body_msg <- paste("To receive a prediction on insurance claims,", 
                    "submit the following variables to the <b>/claimcheck</b> endpoint:",
                    sep = "\n")
  body_reqs <- paste(VARIABLES, collapse = "<br>")
  
  result <- paste(
    "<html>",
    "<h1>", title, "</h1>", "<br>",
    "<body>", 
    "<p>", body_intro, "</p>",
    "<p>", body_model, "</p>",
    "<p>", body_msg, "</p>",
    "<p>", body_reqs, "</p>",
    "</body>",
    "</html>",
    collapse = "\n"
  )
  
  return(result)
}





#* @post /prediction
#* @get /prediction
predict_claim <- function(Age=NA, AccidentArea=NULL, Sex=NULL, Fault=NULL,VehicleCategory=NULL,Deductible=NULL,
                             Agenttype=NULL,BasePolicy=NULL,MaritalStatus=NULL,AddressChange_Claim=NULL) {
  age = as.integer(Age)
  AccidentArea = as.character(AccidentArea)
  Sex = as.character(Sex)
  Fault = as.character(Fault)
  VehicleCategory = as.character(VehicleCategory)
  Deductible = as.integer(Deductible)
  Agenttype = as.character(Agenttype)
  BasePolicy = as.character(BasePolicy)
  MaritalStatus = as.character(MaritalStatus)
  AddressChange_Claim = as.character(AddressChange_Claim)
  
  
  valid_input <- validate_feature_inputs(AddressChange_Claim, Sex, MaritalStatus, Age, Fault, VehicleCategory, Deductible, Agenttype,
                                         AddressChange_Claim,BasePolicy)
  if (valid_input[1] == "OK") {
    payload <- data.frame(Age=Age, AccidentArea=AccidentArea, Sex=Sex, Fault=Fault,VehicleCategory=VehicleCategory,
                          Deductible=Deductible,Agenttype=Agenttype,BasePolicy=BasePolicy,MaritalStatus=MaritalStatus,
                          AddressChange_Claim=AddressChange_Claim)
    clean_data <- transform_titantic_data(payload)
    prediction <- predict(model, clean_data, type = "response")
    result <- list(
      input = list(payload),
      reposnse = list("FraudFound_P" = prediction,
                      "FraudFound_P" = (prediction >= 0.5)
      ),
      status = 200,
      model_version = MODEL_VERSION)
  } else {
    result <- list(
      input = list(Age=Age, AccidentArea=AccidentArea, Sex=Sex, Fault=Fault,VehicleCategory=VehicleCategory,
                   Deductible=Deductible,Agenttype=Agenttype,BasePolicy=BasePolicy,MaritalStatus=MaritalStatus,
                   AddressChange_Claim=AddressChange_Claim),
      response = list(input_error = valid_input),
      status = 400,
      model_version = MODEL_VERSION)
  }
  
  return(result)
}

predict_claim(Age=50, AccidentArea="Urban", Sex="Male", Fault="Third Party",VehicleCategory="Sedan",Deductible=500,
              Agenttype="External",BasePolicy="Collision",MaritalStatus="Married",AddressChange_Claim="1 year")
