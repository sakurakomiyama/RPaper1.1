

library(SandeelDepthDistribution)

sandeelFilePath <- '~/Projects/UiB/PhD/Sakura/sandeel(80%18varSetseed).Rdata' 
sandeelFilePath <- 'Data/LDAresult_80%TrainData_18var_set.seed/sandeel.Rdata'

#### Fit the model with 20 day and 4 hour interals: ####
# First read the data and add the intervals:
sandeel.dt <- readSandeelData(sandeelFilePath)
# The fit the model:
fit <- estimateDepthDistribution(
	data = sandeel.dt, 
	variable = "distanceToBottom", 
	dateVariable <- "Date", 
	timeVariable = "Hour", 
	dateIntervalLength = 20, 
	timeIntervalLength = 4, 
	p0 = 0.5, 
	lower = 0.01, 
	upper = 0.99, 
	minLength = 20
)
# Present the parameters in separate tables:
dcastParTable(fit$parTable, "DateIntervalString", "HourIntervalString")

# Plot the fits:
plots <- plotModelFit(
	fit, 
	variable = "distanceToBottom", 
	showAllFits = FALSE, 
	dateIntevalVariable = "DateIntervalString", 
	timeIntevalVariable = "HourIntervalString", 
	arrange = TRUE, 
	crop = TRUE, 
	adds = ggplot2::theme(plot.title = ggplot2::element_text(size = 5))
)

plots


# Use altitude instead of hours:
fit_altitude <- estimateDepthDistribution(
	data = sandeel.dt, 
	variable = "distanceToBottom", 
	dateVariable <- "Date", 
	timeVariable = "altitude", 
	dateIntervalLength = 60, 
	timeIntervalLength = 0.2, 
	timeTruncate = c(0.2,0.8), 
	p0 = 0.5, 
	lower = 0.01, 
	upper = 0.99, 
	minLength = 20
)

dcastParTable(fit_altitude$parTable, "DateIntervalString", "altitudeIntervalString")

plotModelFit(
	fit_altitude, 
	variable = "distanceToBottom", 
	showAllFits = FALSE, 
	dateIntevalVariable = "DateIntervalString", 
	timeIntevalVariable = "altitudeIntervalString", 
	arrange = TRUE, 
	crop = TRUE, 
	adds = ggplot2::theme(plot.title = ggplot2::element_text(size = 5))
)




# Use altitude instead of hours:
fit_relative_time <- estimateDepthDistribution(
  data = data, 
  variable = "distanceToBottom", 
  dateVariable <- "Date", 
  timeVariable = "relative_time", 
  dateIntervalLength = 20, 
  timeIntervalLength = 0.2, 
  timeTruncate = c(0.2,0.8), 
  p0 = 0.5, 
  lower = 0.01, 
  upper = 0.99, 
  minLength = 20
)

dcastParTable(fit_altitude$parTable, "DateIntervalString", "altitudeIntervalString")

plotModelFit(
  fit_altitude, 
  variable = "distanceToBottom", 
  showAllFits = FALSE, 
  dateIntevalVariable = "DateIntervalString", 
  timeIntevalVariable = "altitudeIntervalString", 
  arrange = TRUE, 
  crop = TRUE, 
  adds = ggplot2::theme(plot.title = ggplot2::element_text(size = 5))
)

