rm(list = ls())
Sys.setlocale(category = "LC_ALL", locale = "english")
options("scipen" = 100)

library(PatientLevelPrediction)
library(FeatureExtraction)
library(DatabaseConnector)

source("./config/connection_details.R")

nlp_categorical_concept_id <- unique(read.csv('./covariates/categorical_covariates.csv')$concept_id)
nlp_numeric_concept_id <- unique(read.csv('./covariates/numeric_covariates.csv')$concept_id)

baseline_cov <- FeatureExtraction::createCovariateSettings(
  useDemographicsAgeGroup = TRUE,
  useCharlsonIndex = TRUE
)

nlp_covariate_cat_cov <- FeatureExtraction::createCovariateSettings(
  #useMeasurementShortTerm = TRUE,
  useMeasurementValueAsConceptShortTerm = TRUE,
  shortTermStartDays = -7,
  endDays = 7,
  includedCovariateConceptIds = nlp_categorical_concept_id,
  excludedCovariateConceptIds = c(45878583, 45884086)
)
nlp_covariate_numeric_cov <- FeatureExtraction::createCovariateSettings(
  useMeasurementValueShortTerm = TRUE,
  shortTermStartDays = -7,
  endDays = 7,
  includedCovariateConceptIds = nlp_numeric_concept_id
)


connection_details <- DatabaseConnector::createConnectionDetails(dbms=dbms,
                                         user=user,
                                         password=password,
                                         server=server,
)
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connection_details,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 cdmDatabaseName = cdmDatabaseName,
                                                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                                                 tempEmulationSchema = tempEmulationSchema,
                                                                 cohortTable = cohortTable, 
                                                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                                 outcomeTable = outcomeTable, 
                                                                 targetId = targetId,
                                                                 outcomeIds = outcomeIds,
                                                                 cdmVersion = cdmVersion)

covariateSettingsList <- list(baseline_cov, nlp_covariate_cat_cov, nlp_covariate_numeric_cov)


baseline_plpData <- getPlpData(databaseDetails = databaseDetails,
                      covariateSettings = baseline_cov,
                      restrictPlpDataSettings = createRestrictPlpDataSettings())


plpData <- getPlpData(databaseDetails = databaseDetails,
                      covariateSettings = covariateSettingsList,
                      restrictPlpDataSettings = createRestrictPlpDataSettings())
					  

### Define the study population
populationSettings <- createStudyPopulationSettings(
  binary = TRUE,
  includeAllOutcomes = TRUE,
  firstExposureOnly = TRUE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = TRUE,
  minTimeAtRisk = 0,
  riskWindowStart = 0,
  startAnchor = 'cohort start',
  riskWindowEnd = as.integer(365.24*5),
  endAnchor = 'cohort start',
  restrictTarToCohortEnd = FALSE
)
a <- as.data.frame(plpData$covariateData$covariateRef)
absent_cov_ids = a[a$valueAsConceptId %in% c(45878583, 45884086),]$covariateId
plpData$covariateData$covariates <- plpData$covariateData$covariates %>% dplyr::filter(!(.data$covariateId %in% absent_cov_ids))
plpData$covariateData$covariateRef <- plpData$covariateData$covariateRef %>% dplyr::filter(!(.data$covariateId %in% absent_cov_ids))
set.seed(seed)

split_type <- 'stratified'
nfold <- 5
testFraction <- 0.25
trainFraction <- 0.75

### Regularised logistic regression
model <- setLassoLogisticRegression(seed = seed)
sampling_type <- 'overSample'
model_name <- paste0('LR(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
                                          runSampleData = TRUE,
                                          runfeatureEngineering = FALSE,
                                          runPreprocessData = TRUE,
                                          runModelDevelopment = TRUE,
                                          runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setLassoLogisticRegression(seed = seed)
sampling_type <- 'underSample'
model_name <- paste0('LR(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
                                          runSampleData = TRUE,
                                          runfeatureEngineering = FALSE,
                                          runPreprocessData = TRUE,
                                          runModelDevelopment = TRUE,
                                          runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setLassoLogisticRegression(seed = seed)
sampling_type <- 'none'
model_name <- paste0('LR(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE),
  saveDirectory = paste0("./result/", model_name)
)


### Random Forest
model <- setRandomForest(classWeight=list(NULL, 'balanced', 'balanced_subsample'), seed = seed)
orig_model_name = 'RF'
sampling_type <- 'overSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setRandomForest(classWeight=list(NULL, 'balanced', 'balanced_subsample'), seed = seed)
sampling_type <- 'underSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name, sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setRandomForest(classWeight=list(NULL, 'balanced', 'balanced_subsample'),seed = seed)
sampling_type <- 'none'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name ,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


### GBM
model <- setGradientBoostingMachine(seed = seed)
orig_model_name = 'GBM'
sampling_type <- 'overSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setGradientBoostingMachine(seed = seed)
sampling_type <- 'underSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name, sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setGradientBoostingMachine(seed = seed)
sampling_type <- 'none'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name ,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


### ADA
model <- setAdaBoost(seed = seed)
orig_model_name = 'ADA'
sampling_type <- 'overSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setAdaBoost(seed = seed)
sampling_type <- 'underSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name, sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setAdaBoost(seed = seed)
sampling_type <- 'none'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name ,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


### LightGBM
model <- setLightGBM(seed = seed)
orig_model_name = 'LightGBM'
sampling_type <- 'overSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setLightGBM(seed = seed)
sampling_type <- 'underSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name, sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setLightGBM(seed = seed)
sampling_type <- 'none'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name ,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


### DT
model <- setDecisionTree(seed = seed)
orig_model_name = 'DT'
sampling_type <- 'overSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)

model <- setDecisionTree(seed = seed)
sampling_type <- 'underSample'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name, sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)


model <- setDecisionTree(seed = seed)
sampling_type <- 'none'
model_name <- paste0(orig_model_name, '(', sampling_type,')')
analysisId <- 1
analysisName <- paste(analysisId, model_name ,sep='_')
model_result <- runPlp( 
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = analysisId, 
  analysisName = analysisName, 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = split_type,
    testFraction = testFraction,
    trainFraction = trainFraction,
    nfold = nfold,
    splitSeed = seed
  ), 
  sampleSettings = createSampleSettings(type=sampling_type,sampleSeed=seed),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = model, 
  executeSettings = createExecuteSettings(runSplitData = TRUE,
										  runSampleData = TRUE,
										  runfeatureEngineering = FALSE,
										  runPreprocessData = TRUE,
										  runModelDevelopment = TRUE,
										  runCovariateSummary = TRUE), 
  saveDirectory = paste0("./result/", model_name)
)