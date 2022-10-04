
library(data.table)

library(ggplot2)

zeropad<-function(x, n = NULL){
	
	numberOfCharactersX <- nchar(x)
	if(!length(n)) {
		n <- max(numberOfCharactersX)
	}
	
	
	if(!all(numberOfCharactersX == numberOfCharactersX[1])){
		formatC(x, width = n, format = "d", flag = "0")
	}
	else {
		x
	}
}
	
addIntervals <- function(x, variable, length = 1, start = NULL, unit = NULL) {
	# Generate day intervals:
	if(!length(start)) {
		start <- x[, min(get(variable))]
	}
	last <- x[, max(get(variable))]
	totalLength <- last - start
	steps <- ceiling(totalLength / length)
	intervalSeq <- seq(start, length.out = steps + 1, by = if(length(unit)) paste(length, unit) else length)
	
	intervalSeqString <- paste(zeropad(intervalSeq[- length(intervalSeq)]), zeropad(intervalSeq[-1]), sep = " - ")
	
	intervalName <- paste0(variable, "Interval")
	intervalStringName <- paste0(variable, "IntervalString")
	intervalLengthName <- paste0(variable, "IntervalLength")
	
	x[, eval(intervalName) := findInterval(get(variable), intervalSeq, rightmost.closed = TRUE)]
	x[, eval(intervalStringName) := eval(intervalSeqString)[get(intervalName)]]
	x[, eval(intervalLengthName) := ..length]
}

estimateGammaAndNormalGivenP <- function(data, variable, p) {
	# First divide into feeding and resting:
	x <- data[[variable]]
	numberOfResting <- ceiling(length(x) * p)
	orderInd <- order(x)
	xresting <- x[orderInd[seq_len(numberOfResting)]]	
	xfeeding <- x[orderInd[seq(numberOfResting + 1, length(x))]]
	
	# Fit the Gamma distribution to the resting data:
	par = MASS::fitdistr(xresting/mean(xresting), densfun = dgamma, list(shape = 1, rate = 1))$est
	par[2] = par[2]/mean(xresting)
	
	# Fit the normal distribution to the feeding data:
	mu = mean(xfeeding, na.rm = TRUE)
	sigma = sd(xfeeding, na.rm = TRUE)
	
	out <- list(
		shape = par[1], 
		rate = par[2], 
		mu = mu, 
		sigma = sigma
	)
}

estimateDepthDistribution <- function(data, variable, p0, interval = c(0, 1), ggplotObject = NULL, plotNr = 1, lower = 0.05, upper = 0.95) {
	
	par <- estimateGammaAndNormalGivenP(data, variable, p0)
	
	# Estimate the p by Nelder–Mead:
	verticalPDF <- function(x, p, par) {
		p * dgamma(x, shape = par$shape, rate = par$rate) + (1 - p) * dnorm(x, mean = par$mu, sd = par$sigma)
	}
	
	
	l <- function(p) {
		# The minus sign is becuse optim performs minimization:
		-sum(log(verticalPDF(data[[variable]], p, par)))
	}
	
	pnew <- optim(p0, fn = l, method = "Brent", lower = lower, upper = upper)
	
	parNew <- estimateGammaAndNormalGivenP(data, variable, p0)
	
	
	parNew$p <- pnew$par
	
	parNew$MeanMaxLogLikelihood <- -pnew$value / nrow(data)
	
	if(length(ggplotObject)) {
		#x_seq <- seq(0, max(data[[variable]]), by = max(data[[variable]]) * 0.001)
		#y <- parNew$p * dgamma(x_seq, shape =  parNew$shape, rate =  parNew$rate) + (1 - parNew$p) * dnorm(x_seq, mean = parNew$mu, sd = parNew$sigma)
		#thisData <- data.table(
		#	x = x_seq, 
		#	y = y
		#)
		#ggplotObject <- ggplotObject + 
		#	geom_path(data = thisData, aes_string(x = "x", y = "y", color = plotNr))
		
		ggplotObject <- addLine(
			ggplotObject = ggplotObject, 
			data = data, 
			variable = variable, 
			par = parNew, 
			plotNr = plotNr
		)
			
		parNew$ggplotObject <- ggplotObject
	}
	
	return(parNew)
}

estimateDepthDistributionIteratively <- function(data, variable, p0, minLength = 20, interval = c(0, 1), plot = FALSE, lower = 0.05, upper = 0.95) {
	
	if(nrow(data) > minLength) {
		
		if(plot) {
			main <- paste0("Date ", data$DateISOIntervalString[1], ", Hour ", data$hourOfDayIntervalString[1])
			
			ggplotObject <- ggplot(data = data, aes_string(x = variable)) + 
			geom_histogram(aes(y=..density..), colour = "black", fill = "pink") + 
			ggtitle(main) + 
			xlim(0, NA)
		}
		
		diff <- Inf
		oldp <- p0
		
		
		n <- 0
		while(diff > 0.001 && n < 100) {
			parNew <- estimateDepthDistribution(data, variable, oldp, interval = interval, ggplotObject = if(plot) ggplotObject, plotNr = n + 1, lower = lower, upper = upper)
			
			ggplotObject <- parNew$ggplotObject
			
			diff <- abs(oldp - parNew$p)
			n <- n + 1
			
			oldp <- parNew$p
			
		}
		
		#out <- parNew$p$par
		#parNew$MeanMaxLogLikelihood <- -parNew$p$value / numberOfObservations
		#parNew$p <- parNew$p$par
		out <- parNew
		
		if(plot) {
			#x_seq <- seq(0, max(data[[variable]]), by = max(data[[variable]]) * 0.001)
			#y <- parNew$p * dgamma(x_seq, shape =  parNew$shape, rate =  parNew$rate) + (1 - parNew$p) * dnorm(x_seq, mean = parNew$mu, sd = parNew$sigma)
			#thisData <- data.table(
			#	x = x_seq, 
			#	y = y
			#)
			#ggplotObject <- ggplotObject + 
			#	geom_path(data = thisData, aes_string(x = "x", y = "y", color = plotNr))
			#	
				
				
			
			#x_seq <- seq(0, max(data[[variable]]), by = max(data[[variable]]) * 0.001)
			#y <- parNew$p$par * dgamma(x_seq, shape =  parNew$shape, rate = parNew$rate) + (1 - parNew$p$par) * dnorm(x_seq, mean = parNew$mu, sd = parNew$sigma)
			#ylim = c(0, max(y))
			#lines(x_seq, y, lwd = 2)
			
			valuesToPrint <- c("shape", "rate", "mu", "sigma", "p")
			toPrint <- paste0(
				valuesToPrint, 
				" = ", 
				round(unlist(parNew[valuesToPrint]), 3), 
				collapse = "\n"
			)
			
			
			
			ggplotObject <- addLine(
				ggplotObject = ggplotObject, 
				data = data, 
				variable = variable, 
				par = parNew, 
				plotNr = 1, 
				size = 1
			) + 
			theme(legend.position = "none") + 
			annotate("text",  x = Inf, y = Inf, label = toPrint, vjust = 1, hjust = 1, size = 3)
			
			out$ggplotObject <- list(ggplotObject)
		}
		
		
		
	}
	else {
		out <- structure(as.list(rep(NA_real_, 6)), names = c("shape", "rate", "mu", "sigma", "p", "MaxLogLikelihood"))
		
		if(plot) {
			main <- paste0("Date ", data$DateISOIntervalString[1], ", Hour ", data$hourOfDayIntervalString[1])
			
			out$ggplotObject <- ggplot() + theme_void() + ggtitle(main)
			# We need to wrap in a list to add the plot the cells of the data.table:
			out$ggplotObject <- list(out$ggplotObject)
		}
	}
	
	return(out)
	
}

addLine <- function(ggplotObject, data, variable, par, plotNr, ...) {
	x_seq <- seq(0, max(data[[variable]]), by = max(data[[variable]]) * 0.001)
	y <- par$p * dgamma(x_seq, shape =  par$shape, rate =  par$rate) + (1 - par$p) * dnorm(x_seq, mean = par$mu, sd = par$sigma)
	thisData <- data.table(
		x = x_seq, 
		y = y
	)
	ggplotObject <- ggplotObject + 
		geom_path(data = thisData, aes_string(x = "x", y = "y", color = plotNr), ...)
	
	return(ggplotObject)
}

expandDateAndHourIntervalGrid <- function(x) {
	#merge(
	#	x, 
	#	CJ(
	#		DateISOInterval = sort(unique(x$DateISOInterval)), 
	#		hourOfDayInterval = sort(unique(x$hourOfDayInterval))#, 
	#		#DateISOIntervalString = sort(unique(x$DateISOIntervalString)), 
	#		#hourOfDayIntervalString = sort(unique(x$hourOfDayIntervalString))
	#	), 
	#by = c("DateISOInterval", "hourOfDayInterval"), 
	#all.y = TRUE)
	
	x2 <- x[
		CJ(
			DateISOIntervalString = sort(unique(x$DateISOIntervalString)), 
			hourOfDayIntervalString = sort(unique(x$hourOfDayIntervalString)), unique = TRUE
		),
		on = .(DateISOIntervalString, hourOfDayIntervalString), nomatch = NA]
}

getCroppedAxes <- function(x) {
	atCol <- which(colSums(!is.na(x[, -1])) > 0)
	atCol <- do.call(seq, as.list(range(atCol)))
	
	atRow <- which(rowSums(!is.na(x[, -1])) > 0)
	atRow <- do.call(seq, as.list(range(atRow)))
	
	axisCol <- colnames(parMatrix_MeanMaxLogLikelihood)[-1][atCol]
	axisRow <- x[[1]][atRow]
	
	list(
		axisCol = axisCol,
		axisRow = axisRow
	)
}

readSandeelDataAddDateAndHourInterval <- function(filePath, numberOfDays = 10, numberOfHours = 2) {
	
	# Read the sandeel data to a data.table:
	d <- load(filePath)
	sandeel.dt <- data.table::setDT(sandeel.dt)

	# Add BottomDepth and distance from the bottom i meters:
	sandeel.dt[, BottomDepth := weighted_meanDepth / nor_Depth]
	sandeel.dt[, distanceToBottom_weighted := BottomDepth - weighted_meanDepth]
	sandeel.dt[, distanceToBottom := BottomDepth - meanDepth]

	# Convert to proper Date and Time:
	sandeel.dt[, DateTime := as.POSIXct(YMD_time, tz = "UTC")]
	sandeel.dt[, DateISO := as.Date(DateTime)]


	sandeel.dt[, hourOfDay := as.POSIXlt(DateTime)$hour]



	addIntervals(sandeel.dt, "DateISO", length = numberOfDays, unit = "days")
	addIntervals(sandeel.dt, "hourOfDay", length = numberOfHours)
	# expand to full grid of date and hour interval:
	sandeel.dt <- expandDateAndHourIntervalGrid(sandeel.dt)
	
	return(sandeel.dt)
}


#sandeelFilePath <- '~/Projects/UiB/PhD/Sakura/sandeel(80%18varSetseed).Rdata' 
sandeelFilePath <- 'Data/LDAresult_80%TrainData_18var_set.seed/sandeel.Rdata'




#### Fit the model with 20 day and 4 hour interals: ####

# First read the data and add the intervals:
sandeel.dt <- readSandeelDataAddDateAndHourInterval(sandeelFilePath, numberOfDays = 20, numberOfHours = 4)
# Subset by area:
#sandeel.dt <- subset(sandeel.dt, area == "Engelsk_Klondyke")

# Fit the model for each interval:
sandeel.dt[, c("shape", "rate", "mu", "sigma", "p", "MeanMaxLogLikelihood", "ggplotObject") := estimateDepthDistributionIteratively(.SD, "distanceToBottom", p0 = 0.5, plot = TRUE, lower = 0.01, upper = 0.99), by = c("DateISOInterval", "hourOfDayInterval")]
# This table holds the parameters and the plots:
par <- unique(sandeel.dt, by = c("DateISOIntervalString", "hourOfDayIntervalString"))

# Print the fitted parameters:
parMatrix_shape <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "shape", sum, fill =  NA)
parMatrix_rate <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "rate", sum, fill =  NA)
parMatrix_mu <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "mu", sum, fill =  NA)
parMatrix_sigma <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "sigma", sum, fill =  NA)
parMatrix_p <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "p", sum, fill =  NA)
parMatrix_MeanMaxLogLikelihood <- dcast(par, DateISOIntervalString ~ hourOfDayIntervalString, value.var = "MeanMaxLogLikelihood", sum, fill =  NA)

parMatrix_shape
parMatrix_rate
parMatrix_mu
parMatrix_sigma
parMatrix_p
parMatrix_MeanMaxLogLikelihood


# Crop to the intervals which have estimates and arrange the plots:
croppedAxes <- getCroppedAxes(parMatrix_p)
parCropped <- subset(par, DateISOIntervalString %in% croppedAxes$axisRow & hourOfDayIntervalString %in% croppedAxes$axisCol)
ggpubr::ggarrange(
	plotlist  = lapply(parCropped$ggplotObject, "+", theme(plot.title = element_text(size = 5))), 
	nrow = length(croppedAxes$axisRow), 
	ncol = length(croppedAxes$axisCol)
)


