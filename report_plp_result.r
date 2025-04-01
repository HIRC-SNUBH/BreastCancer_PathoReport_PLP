rm(list = ls())
Sys.setlocale(category = "LC_ALL", locale = "english")
options("scipen" = 100)

library(PatientLevelPrediction)
library(FeatureExtraction)
library(DatabaseConnector)

library(ggplot2)
wd <- paste0(getwd(),'/result')
report_dir <- paste0(getwd(), '/report_result')
eval_list = c('Train','Test','CV')
dir.create(report_dir, showWarnings = FALSE)
result_dirs <- list.dirs(path=wd, full.name=FALSE, recursive=FALSE)
model_result_list = list()
model_result_list$model <- c()
model_result_list$train_populationSize <- c()
model_result_list$train_outcomeCount <- c()
model_result_list$train_AUROC <- c()
model_result_list$train_AUROC_ci05 <- c()
model_result_list$train_AUROC_ci95 <- c()
model_result_list$train_AUPRC <- c()
model_result_list$train_BrierScore <- c()

model_result_list$test_populationSize <- c()
model_result_list$test_outcomeCount <- c()
model_result_list$test_AUROC <- c()
model_result_list$test_AUROC_ci05 <- c()
model_result_list$test_AUROC_ci95 <- c()
model_result_list$test_AUPRC <- c()
model_result_list$test_BrierScore <- c()

model_result_list$cv_populationSize <- c()
model_result_list$cv_outcomeCount <- c()
model_result_list$cv_AUROC <- c()
model_result_list$cv_AUROC_ci05 <- c()
model_result_list$cv_AUROC_ci95 <- c()
model_result_list$cv_AUPRC <- c()
model_result_list$cv_BrierScore <- c()
temp_insertRunPlpToSqlite <- get("insertRunPlpToSqlite", envir=asNamespace("PatientLevelPrediction"))
for(idx in 1:length(result_dirs)){
	model_name <- result_dirs[idx]
	current_dir <- paste0(wd, '/', model_name)

	model_report_dir <- paste0(report_dir,'/', model_name)
	dir.create(model_report_dir, showWarnings = FALSE)

	model_result_dir <- paste0(current_dir, '/1/plpResult')
	plp_result <- PatientLevelPrediction::loadPlpResult(model_result_dir)

	server <- temp_insertRunPlpToSqlite(runPlp = plp_result, externalValidatePlp = NULL, 
								 diagnosePlp = NULL)
	eval_stats <- plp_result$performanceEvaluation$evaluationStatistics

	model_result_list$model <- c(model_result_list$model, model_name)
	c_eval <- eval_stats[(eval_stats$evaluation == 'Test'),]
	model_result_list$test_populationSize <- c(model_result_list$test_populationSize, c_eval[(c_eval$metric == 'populationSize'),'value'][[1]])
	model_result_list$test_outcomeCount <- c(model_result_list$test_outcomeCount, c_eval[(c_eval$metric == 'outcomeCount'),'value'][[1]])
	model_result_list$test_AUROC <- c(model_result_list$test_AUROC, c_eval[(c_eval$metric == 'AUROC'),'value'][[1]])
	model_result_list$test_AUROC_ci05 <- c(model_result_list$test_AUROC_ci05, c_eval[(c_eval$metric == '95% lower AUROC'),'value'][[1]])
	model_result_list$test_AUROC_ci95 <- c(model_result_list$test_AUROC_ci95, c_eval[(c_eval$metric == '95% upper AUROC'),'value'][[1]])
	model_result_list$test_AUPRC <- c(model_result_list$test_AUPRC, c_eval[(c_eval$metric == 'AUPRC'),'value'][[1]])
	model_result_list$test_BrierScore <- c(model_result_list$test_BrierScore, c_eval[(c_eval$metric == 'brier score'),'value'][[1]])

	c_eval <- eval_stats[(eval_stats$evaluation == 'Train'),]
	model_result_list$train_populationSize <- c(model_result_list$train_populationSize, c_eval[(c_eval$metric == 'populationSize'),'value'][[1]])
	model_result_list$train_outcomeCount <- c(model_result_list$train_outcomeCount, c_eval[(c_eval$metric == 'outcomeCount'),'value'][[1]])
	model_result_list$train_AUROC <- c(model_result_list$train_AUROC, c_eval[(c_eval$metric == 'AUROC'),'value'][[1]])
	model_result_list$train_AUROC_ci05 <- c(model_result_list$train_AUROC_ci05, c_eval[(c_eval$metric == '95% lower AUROC'),'value'][[1]])
	model_result_list$train_AUROC_ci95 <- c(model_result_list$train_AUROC_ci95, c_eval[(c_eval$metric == '95% upper AUROC'),'value'][[1]])
	model_result_list$train_AUPRC <- c(model_result_list$train_AUPRC, c_eval[(c_eval$metric == 'AUPRC'),'value'][[1]])
	model_result_list$train_BrierScore <- c(model_result_list$train_BrierScore, c_eval[(c_eval$metric == 'brier score'),'value'][[1]])

	c_eval <- eval_stats[(eval_stats$evaluation == 'CV'),]
	model_result_list$cv_populationSize <- c(model_result_list$cv_populationSize, c_eval[(c_eval$metric == 'populationSize'),'value'][[1]])
	model_result_list$cv_outcomeCount <- c(model_result_list$cv_outcomeCount, c_eval[(c_eval$metric == 'outcomeCount'),'value'][[1]])
	model_result_list$cv_AUROC <- c(model_result_list$cv_AUROC, c_eval[(c_eval$metric == 'AUROC'),'value'][[1]])
	model_result_list$cv_AUROC_ci05 <- c(model_result_list$cv_AUROC_ci05, c_eval[(c_eval$metric == '95% lower AUROC'),'value'][[1]])
	model_result_list$cv_AUROC_ci95 <- c(model_result_list$cv_AUROC_ci95, c_eval[(c_eval$metric == '95% upper AUROC'),'value'][[1]])
	model_result_list$cv_AUPRC <- c(model_result_list$cv_AUPRC, c_eval[(c_eval$metric == 'AUPRC'),'value'][[1]])
	model_result_list$cv_BrierScore <- c(model_result_list$cv_BrierScore, c_eval[(c_eval$metric == 'brier score'),'value'][[1]])


	write.csv(as.data.frame(plp_result$covariateSummary), file = file.path(model_report_dir,'covariateSummary.csv'))
	write.csv(plp_result$model$covariateImportance, file = file.path(model_report_dir, 'covariateImportance.csv'))
  
	file_name <- 'demographic_summary.png'  
	plot_obj <- PatientLevelPrediction::plotDemographicSummary(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 20)

	file_name <- 'AUROC.png'  
	plot_obj <- PatientLevelPrediction::plotSparseRoc(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 16)


	file_name <- 'Generalizability.png'  
	plot_obj <- PatientLevelPrediction::plotGeneralizability(covariateSummary = plp_result$covariateSummary)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 16, height = 5)


	file_name <- 'F1.png'  
	plot_obj <- PatientLevelPrediction::plotF1Measure(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 12)


	file_name <- 'SmoothCalibration.png'  
	plot_obj <- PatientLevelPrediction::plotSmoothCalibration(plpResult = plp_result,scatter = TRUE)
	ggsave(file.path(model_report_dir, 'Calibration_Smooth(Test).png'), plot_obj$test$smoothPlot,width = 5, height = 5)
	ggsave(file.path(model_report_dir, 'HistCalibration_Smooth(Test).png'), plot_obj$test$histPlot,width = 5, height = 5)
	ggsave(file.path(model_report_dir, 'Calibration_Smooth(Train).png'), plot_obj$train$smoothPlot,width = 5, height = 5)
	ggsave(file.path(model_report_dir, 'HistCalibration_Smooth(Train).png'), plot_obj$train$histPlot,width = 5, height = 5)
	ggsave(file.path(model_report_dir, 'Calibration_Smooth(CV).png'), plot_obj$cv$smoothPlot,width = 5, height = 5)
	ggsave(file.path(model_report_dir, 'HistCalibration_Smooth(CV).png'), plot_obj$cv$histPlot,width = 5, height = 5)


	file_name <- 'SparseCalibration.png'  
	plot_obj <- PatientLevelPrediction::plotSparseCalibration(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 12)


	file_name <- 'SparseCalibration.png'  
	plot_obj <- PatientLevelPrediction::plotVariableScatterplot(covariateSummary = plp_result$covariateSummary)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 12)


	file_name <- 'PR.png'  
	plot_obj <- PatientLevelPrediction::plotPrecisionRecall(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 12)



	file_name <- 'SparseCalibration.png'  
	plot_obj <- PatientLevelPrediction::plotSparseCalibration(plpResult = plp_result)
	ggsave(file.path(model_report_dir, file_name), plot_obj,width = 5, height = 12)
  
}

write.csv(model_result_list, file = file.path(report_dir, 'model_performance_summary.csv'))