rm(list=ls())
library(dplyr)
library(data.table)
library(ggplot2)
my_theme <- function() theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


#================================#
####         Load data        ####
#================================#


#setwd("C:/Users/a37907/Desktop/KnowSandeel15781/Data")
#setwd("E:/KnowSandeel15781/Data")


#====   school, trawl, bio data   ====#
lapply(c("Data/School_EROS_bio.df", "Data/spdf.Rdata", "Data/School_SD.Rdata", "gps.Rdata"),load,.GlobalEnv)

load("Data/School_EROS.Rdata")
School_EROS.dt <- School.dt
rm(School.dt)






#=======================================#
#===       find Sandeel school       ===#
#### Discriminant analyses (LDA/DFA) ####
#=======================================#

#library("tidyverse")
library("caret")
#== pre-processing ==#
cols <- c("Date","Latitude", "Longitude", "PingNo", "pixelNo", 
          "SampleCount", "vessel", "area", "YMD_time", "altitude", 
          "meanDepth", "DepthStart", "DepthStop", "sV_var", "nor_DepthStart", 
          "nor_DepthStop", "nor_Depth",  "altitude_degree")

data <- subset(School_EROS.dt, Frequency %in% c(200) 
               & !category %in% "PSAND"
               # & school_area >= median(School_EROS.dt$school_area)
               )
data[,cols]=NULL
data[,Frequency:=NULL]
# remove negative value of depth from bottom
for (i in 1:nrow(data)) {
  if (data$DepthfromBottom[i] < 0)
    data$DepthfromBottom[i] <- 0
  else
    data$DepthfromBottom[i] <- data$DepthfromBottom[i]
}
# log transform #
data[,sV_mean:=log(sV_mean)][,sV_max:=log(sV_max)][,sV_min:=log(sV_min)][,sV_stdv:=log(sV_stdv)]
data[,rf:=log(rf)][,SE:=log(SE)][,sA:=log(sA)][,school_area:=log(school_area)][,school_length:=log(school_length)]
data[,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1)][,school_height:=log(school_height)]
data[,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)]

#== plot (*take long time) ==#
#library("psych")
#pairs.panels(data[, -c("id","category")],bg=c("pink","blue", "green")[as.factor(data$category)], pch=21, gap=0)

#== Split the data into training and test set ==#
#training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
#train.data <- data[training.samples, ]
#test.data <- data[-training.samples, ]
#train.data <- data # when use all available data to tune a model
#== Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
#preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
#data <- predict(preproc.param, data)
#train.data <- predict(preproc.param, train.data)
#test.data <- predict(preproc.param, test.data)
#train.data$category <- as.factor(train.data$category)
#test.data$category <- as.factor(test.data$category)
ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free")
featurePlot(data[, 3:ncol(data)],as.factor(data$category), plot="density", auto.key = list(columns = 2))


#== saildrone data ==#
test_SD.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% c("KORONA", "manual") & !area %in% "outside")
test_SD.data[,cols]=NULL
test_SD.data$original_id <- NULL
test_SD.data[, Frequency:=NULL]
# remove negative value of depth from bottom
for (i in 1:nrow(test_SD.data)) {
  if (test_SD.data$DepthfromBottom[i] < 0)
    test_SD.data$DepthfromBottom[i] <- 0
  else
    test_SD.data$DepthfromBottom[i] <- test_SD.data$DepthfromBottom[i]
}
# log transformation #
test_SD.data[,sV_mean:=log(sV_mean)][,sV_max:=log(sV_max)][,sV_min:=log(sV_min)][,sV_stdv:=log(sV_stdv)]
test_SD.data[,rf:=log(rf)][,SE:=log(SE)][,sA:=log(sA)][,school_area:=log(school_area)][,school_length:=log(school_length)]
test_SD.data[,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1)][,school_height:=log(school_height)]
test_SD.data[,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)]

# Normarize
preproc.param <- preProcess(test_SD.data[,-c("id")], method = c("center", "scale"))
test_SD.data <- predict(preproc.param, test_SD.data)
test_SD.data$category <- as.factor(test_SD.data$category)
ggplot(melt(test_SD.data[,-c("id", "category")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free")


#== step-wise LDA * 10,  Saildrone data ==#
#library(ROCR)
library(klaR) #uses stepclass in the klaR
library(MASS) 
maxvar <-(ncol(data))-2
direction <-"backward"
test_SD.data <- data.frame(test_SD.data)
score <- 0
score.df <- data.frame(matrix(ncol = 4, nrow = 0))
coef.df <- data.frame(matrix(ncol = 3, nrow = 0))
confusion.matrix <- list()
slda.lst <- list()
for_lift <- data.frame()
data.lst <- list()
rm(train.data, test.data)

#==============================#
####   80% train 20% test   ####
#==============================#
for(i in 1:2) {
  score <- 0
  # 80% : training data, 20% : test data
  set.seed(123)
  training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) 
  train.data <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  # Normalize the data (train data)
  preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
  train.data <- predict(preproc.param, train.data)
  train.data$category <- as.factor(train.data$category)
  # Normalize the data (test data)
  preproc.param <- preProcess(test.data[,-c("id")], method = c("center", "scale"))
  test.data <- predict(preproc.param, test.data)
  test.data$category <- as.factor(test.data$category)
  # tune a model
  set.seed(123)
  #slda <- train(category ~ ., data = train.data[, -c("id")],
  #              method = "stepLDA", importance = TRUE,metric = "ROC", tuneLength = 10,
  #              trControl = trainControl(method = "repeatedcv", #"repeatedcv" / "cv"
  #                                       number = 10, repeats = 3, 
  #                                       savePredictions = "all", #"all" / "final"
  #                                       classProbs = TRUE), 
  #              tuneGrid = data.frame(maxvar,direction),
  #)
  slda <- train(category ~ ., data = train.data[, -c("id")],
                method = "stepLDA", importance = TRUE,metric = "ROC", tuneLength = 10,
                trControl = trainControl(method = "cv", #"repeatedcv" / "cv"
                                         number = 10, 
                                         savePredictions = "all", #"all" / "final"
                                         classProbs = TRUE), 
                tuneGrid = data.frame(maxvar,direction),
  )
  # store the results
  temp <- data.frame(id = test_SD.data$id)
  temp2 <- data.table(slda$finalModel$fit$scaling, keep.rownames = TRUE)
  temp2$attempt <- i
  coef.df <- rbind(coef.df, temp2)
  predictions <- predict(slda, train.data)
  temp3 <- confusionMatrix(reference=as.factor(train.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"train")]] <- temp3
  predictions <- predict(slda, test.data)
  temp4 <- confusionMatrix(reference=as.factor(test.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"test")]] <- temp4
  slda.lst[[i]] <- slda
  # plot ROC curve
  #predictions <- predict(slda, train.data, type="prob")
  #pred <- prediction(predictions[2], train.data$category)
  #png(paste0("ROC",i,"_train.png") , width = 800, height = 600)
  #plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate
  #dev.off()
  # apply to SD data
  for(j in 1:nrow(slda$finalModel$fit$scaling)) {
    score <- score + (slda$finalModel$fit$scaling[j] * test_SD.data[, row.names(slda$finalModel$fit$scaling)[j]])
  }
  temp$score <- score
  temp$pred <- predict(slda, test_SD.data)
  temp$attempt <- i
  score.df <- rbind(score.df, temp)
  #== for ROC curve (1 curve per 1 crossvalidation) ==#
  #temp5 <- data.frame(Class = test.data$category, 
  #                       lda = predict(slda, test.data, type = "prob")[,"SAND"],
  #                       resample = paste0("trial", i))
  #== for ROC curve (1 curve per k)   ==#
  temp5 <- data.frame(Class = slda$pred$obs, 
                      lda = slda$pred$SAND, 
                      resample = slda$pred$Resample)
  #=================================#
  for_lift <- rbind(for_lift, temp5)
  # store train and test data
  data.lst[[paste0(i,"train")]] <- train.data
  data.lst[[paste0(i,"test")]] <- test.data
}

rm(temp, temp2, temp3, temp4, temp5, slda, training.samples, preproc.param)
save(score.df, file="score.Rdata")
save(coef.df, file="coef.Rdata")
save(confusion.matrix, file="confusion.matrix.Rdata")
save(slda.lst, file="slda.lst.Rdata")
save(for_lift, file = "for_lift.Rdata")
save(data.lst, file = "data.lst.Rdata")
#

#==  ROC curve  ==#
# by each trial
#for_lift <- data.frame(Class = slda.lst[[trial]]$pred$obs, lda = slda$pred$SAND, resample = slda$pred$Resample)
#png(paste0("ROC",trial,".png") , width = 800, height = 600)

lift_df <-  data.frame()
for (fold in unique(for_lift$resample)) {
  fold_df <- dplyr::filter(for_lift, resample == fold)
  lift_obj_data <- lift(Class ~ lda, data = fold_df, class = "SAND")$data
  lift_obj_data$fold = fold
  lift_df = rbind(lift_df, lift_obj_data)
  rm(lift_obj_data)
}

detach(package:plyr) # if "group_by" is not working


lift_df %>% 
  rename(group = n) %>%
  mutate(False_positive = 1-Sp) %>% 
  group_by(group) %>% 
  summarize(mean_Sn = mean(Sn), mean_Sp = mean(False_positive), 
            sd_Sn = sd(Sn), sd_Sp = sd(False_positive),
            n_Sn = length(Sn), n_Sp = length(False_positive), 
            se_Sn = sd_Sn / sqrt(n_Sn), se_Sp = sd_Sp / sqrt(n_Sp), 
            lower_Sn = mean_Sn - qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            upper_Sn = mean_Sn + qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            lower_Sp = mean_Sp - qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,
            upper_Sp = mean_Sp + qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,) %>% 
  ggplot(aes(x = mean_Sp, y = mean_Sn)) + 
  my_theme() + 
  geom_line(data = lift_df, aes(x = 1-Sp, y = Sn, group = fold), alpha = .1, lwd = .1) +
  #geom_ribbon(aes(ymin = lower_Sn, ymax = upper_Sn,
  #                xmin = lower_Sp, xmax = upper_Sp),
  #            fill = "red", alpha = .3) + 
  geom_line(colour = "red", lwd = 1.0) + 
  labs(x = "False positive", y = "True positive")

#==========================#


#======================#
####   100% train   ####
#======================#
for(i in 1:10) {
  score <- 0
  # 80% : training data, 20% : test data
  training.samples <- createDataPartition(data$category, p = 1, list = FALSE) 
  train.data <- data[training.samples, ]
  #test.data <- data[-training.samples, ]
  # Normalize the data (train data)
  preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
  train.data <- predict(preproc.param, train.data)
  train.data$category <- as.factor(train.data$category)
  # Normalize the data (test data)
  #preproc.param <- preProcess(test.data[,-c("id")], method = c("center", "scale"))
  #test.data <- predict(preproc.param, test.data)
  #test.data$category <- as.factor(test.data$category)
  # tune a model
  slda <- train(category ~ ., data = train.data[, -c("id")],
                method = "stepLDA", importance = TRUE,metric = "ROC", tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", #"repeatedcv" / "cv"
                                         number = 10, repeats = 3, 
                                         savePredictions = "all", #"all" / "final"
                                         classProbs = TRUE), 
                tuneGrid = data.frame(maxvar,direction),
  )
  # store the results
  temp <- data.frame(id = test_SD.data$id)
  temp2 <- data.table(slda$finalModel$fit$scaling, keep.rownames = TRUE)
  temp2$attempt <- i
  coef.df <- rbind(coef.df, temp2)
  predictions <- predict(slda, train.data)
  temp3 <- confusionMatrix(reference=as.factor(train.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"train")]] <- temp3
  #predictions <- predict(slda, test.data)
  #temp4 <- confusionMatrix(reference=as.factor(test.data$category) , data= predictions, mode = "everything", positive = "SAND")
  #confusion.matrix[[paste0(i,"test")]] <- temp4
  slda.lst[[i]] <- slda
  # apply to SD data
  for(j in 1:nrow(slda$finalModel$fit$scaling)) {
    score <- score + (slda$finalModel$fit$scaling[j] * test_SD.data[, row.names(slda$finalModel$fit$scaling)[j]])
  }
  temp$score <- score
  temp$pred <- predict(slda, test_SD.data)
  temp$attempt <- i
  score.df <- rbind(score.df, temp)
  # data for ROC curve
  temp5 <- data.frame(Class = train.data$category, 
                      lda = predict(slda, train.data, type = "prob")[,"SAND"],
                      resample = paste0("trial", i))
  for_lift <- rbind(for_lift, temp5)
  # store train and test data
  data.lst[[paste0(i,"train")]] <- train.data
  #data.lst[[paste0(i,"test")]] <- test.data
}

rm(temp, temp2, temp3, temp4, temp5, slda, training.samples, preproc.param)
save(score.df, file="score.Rdata")
save(coef.df, file="coef.Rdata")
save(confusion.matrix, file="confusion.matrix.Rdata")
save(slda.lst, file="slda.lst.Rdata")
save(for_lift, file = "for_lift.Rdata")
save(data.lst, file = "data.lst.Rdata")
#

#==  ROC curve  ==#
# by each trial
#for_lift <- data.frame(Class = slda.lst[[trial]]$pred$obs, lda = slda$pred$SAND, resample = slda$pred$Resample)
#png(paste0("ROC",trial,".png") , width = 800, height = 600)

lift_df <-  data.frame()
for (fold in unique(for_lift$resample)) {
  fold_df <- dplyr::filter(for_lift, resample == fold)
  lift_obj_data <- lift(Class ~ lda, data = fold_df, class = "SAND")$data
  lift_obj_data$fold = fold
  lift_df = rbind(lift_df, lift_obj_data)
  rm(lift_obj_data)
}

detach(package:plyr) # if "group_by" is not working


lift_df %>% 
  rename(group = n) %>%
  mutate(False_positive = 1-Sp) %>% 
  group_by(group) %>% 
  summarize(mean_Sn = mean(Sn), mean_Sp = mean(False_positive), 
            sd_Sn = sd(Sn), sd_Sp = sd(False_positive),
            n_Sn = length(Sn), n_Sp = length(False_positive), 
            se_Sn = sd_Sn / sqrt(n_Sn), se_Sp = sd_Sp / sqrt(n_Sp), 
            lower_Sn = mean_Sn - qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            upper_Sn = mean_Sn + qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            lower_Sp = mean_Sp - qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,
            upper_Sp = mean_Sp + qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,) %>% 
  ggplot(aes(x = mean_Sp, y = mean_Sn)) + 
  my_theme() + 
  geom_line(data = lift_df, aes(x = 1-Sp, y = Sn, group = fold), alpha = .1, lwd = .1) +
  #geom_ribbon(aes(ymin = lower_Sn, ymax = upper_Sn,
  #                xmin = lower_Sp, xmax = upper_Sp),
  #            fill = "red", alpha = .3) + 
  geom_line(colour = "red", lwd = 1.0) + 
  labs(x = "False positive", y = "True positive")

#==========================#




#==========================#
#== determine ids "SAND" ==#
#==========================#

ids <- score.df
ids$pred <- ifelse(score.df$pred=="SAND", 1, 0)
ids <- with(ids, aggregate(ids[,c("score", "pred")], list(id), mean)) # sandeel 10 times -> mean 1, other 10 times -> mean 0
ids <- data.table(ids)
ids_nonsandeel <- ids[pred %in% 0]$Group.1 # extract ids only the mean = 0 (non-sandeel)
ids <- ids[pred %in% 1]$Group.1 # extract ids only the mean = 1 (sandeel 10 times)
sandeel.dt <-  School_SD.dt[id %in% ids & Frequency %in% 200]
nonsandeel.dt <- School_SD.dt[id %in% ids_nonsandeel & Frequency %in% 200]
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
colnames(nonsandeel.dt) <- make.unique(names(nonsandeel.dt))

test_SD.data$predict <- predict(slda.lst[[10]], test_SD.data)
featurePlot(test_SD.data[, 3:(ncol(test_SD.data)-1)],as.factor(test_SD.data$predict), plot="density", auto.key = list(columns = 2))

#== add coverage_name ==#
label <- unique(data.table(vessel = gps.dt$vessel, coverage_name = gps.dt$coverage_name, 
                           StartTime = gps.dt$StartTime, StopTime = gps.dt$StopTime,
                           distance_sum = gps.dt$distance_sum, area = gps.dt$area))
sandeel.dt <- data.table(sandeel.dt)
nonsandeel.dt <- data.table(nonsandeel.dt)
setDT(sandeel.dt)[setDT(label), on =. (YMD_time >= StartTime, 
                                       YMD_time <= StopTime,
                                       area == area, vessel == vessel), 
                  coverage_name := coverage_name] 
setDT(nonsandeel.dt)[setDT(label), on =. (YMD_time >= StartTime, 
                                       YMD_time <= StopTime,
                                       area == area, vessel == vessel), 
                  coverage_name := coverage_name]

save(sandeel.dt, file="sandeel.Rdata")
save(nonsandeel.dt, file="nonsandeel.Rdata")

#=========================#
#==   biological data   ==#
#=========================#
x <- data.table(matrix(ncol = 5, nrow = 0))

#==  *use only test.data*   ==#
for(i in 1:length(slda.lst)) {
  temp <- data.table(id = data.lst[[paste0(i,"test")]]$id, 
                     pred = predict(slda.lst[[i]], data.lst[[paste0(i,"test")]]), 
                     obs = data.lst[[paste0(i,"test")]]$category,
                     data = "test", attempt = i)
  x <- rbind(x, temp, use.names=F)
  colnames(x) <- c("id", "pred", "obs", "data", "attempt")
}

#==  *use both test and train data*   ==#
for(i in 1:length(slda.lst)) {
  temp <- data.table(id = School_EROS.dt[Frequency %in% 200 & !category %in% "PSAND"]$id)
  # use 100% data
  if (length(slda.lst[[i]][["pred"]][["pred"]]) == nrow(School_EROS.dt[Frequency %in% 200 & !category %in% "PSAND"]))
    pred = data.table(predict(slda.lst[[i]], data.lst[[paste0(i,"train")]]),
                      id = data.lst[[paste0(i,"train")]][["id"]],
                      data = "100%")
  # use 80% data
  else 
    pred = rbind(data.table(pred = predict(slda.lst[[i]], data.lst[[paste0(i,"test")]]),
                            id = data.lst[[paste0(i,"test")]][["id"]],
                            data = "test"),
                 data.table(pred = predict(slda.lst[[i]], data.lst[[paste0(i,"train")]]),
                            id = data.lst[[paste0(i,"train")]][["id"]],
                            data = "train"))
  
  pred <- pred[order(id),]
  temp$pred <- pred$pred
  temp$obs = data$category
  temp$data = pred$data
  temp$attempt = i
  x <- rbind(x, temp, use.names=F)
  colnames(x) <- c("id", "pred", "obs", "data", "attempt")
}

### method A, B or C ###
### so far C is best ###

#===  (method A) correctly classified all 10 times   ===#
# *since ids of test data are different at each training,       #
#  it's rare to have an ideal id which is sandeel for 10 times. #
#  Thus, this method is not optimal.*                           #
x$pred_num <- ifelse(x$pred=="SAND", 1, 0)
x <- x[obs == "SAND"]
x <- data.table(with(x, aggregate(x[,c("pred_num")], list(id), sum)))
colnames(x) <- c("id", "pred_num")
x$pred <- ifelse(x$pred_num == length(slda.lst), "SAND", "OTHER")

#===  (method B) use the best accuracy model    ===#
#  *This method also leaves too few ids since      # 
#   maximum N of ids is 1530 and only schools that #
#   are observed in biological data can be used.*  #
x$correct <- ifelse(x$pred == x$obs, 1, 0)
setDT(x)[, accuracy := sum(correct)/.N, by = c("attempt")]
i <- unique(x[x$accuracy==max(x$accuracy),]$attempt)
x <- subset(x, attempt == i)

#===  (method C) use all 10 attempt but only consistent result  ===#
x$correct <- ifelse(x$pred == x$obs, 1, 0)
setDT(x)[, accuracy := sum(correct)/.N, by = c("attempt")]
setDT(x)[, duplicate_id := .N, by = c("id")][, discrepancy := mean(correct), by = c("id")]
x <- subset(x, discrepancy %in% c(1, 0))
x <- x[!duplicated(x[,c('id')]),]

#====================================================#

bio_id <- School_EROS_bio.df[, c("id", "meanLength", "weighted_meanLength")]
bio_id <- unique(bio_id)
bio_id <- x[bio_id, on = "id", roll = FALSE]
bio_id <- bio_id[complete.cases(bio_id), ]
bio_id$pred <- gsub('OTHER', 'Incorrect', bio_id$pred)
bio_id$pred <- gsub('SAND', 'Correct', bio_id$pred)


load("Data/LDAresult_80%TrainData_18var_set.seed/bio_id.Rdata")

ggplot(bio_id) + my_theme() + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15)) + 
  geom_boxplot(aes(x=pred, y = meanLength), fill = "grey") + 
  labs(y = "mean body length (cm)")
ggplot(bio_id) + my_theme() + theme(legend.title = element_blank())+
  geom_histogram(aes(x=meanLength, y = stat(density), fill=pred), bins=30, colour="grey50", alpha=0.5, position="dodge") +
  geom_density(aes(x=meanLength, fill=pred), colour="grey50", alpha=0.5) +
  labs(x="mean body length of a trawl catch (cm)", y="Density")
nrow(bio_id[pred=='Correct'])/nrow(bio_id)


#=================================================================#
####  t-test mean length correctly vs incorrectly classified   ####
#=================================================================#
# Student's two sample t-test (t-test for equal variance two samples)
t.test(meanLength ~ pred, data=bio_id, var.equal = TRUE)
# F test (test if the two samples have the same variance?)
var.test(meanLength ~ pred, data=bio_id)
# equal variance -> Student's / unequal variance -> Welch's


#==  calculate accuracy ==# (schools larger than 500m2)
eros.lda <- data.table(id=train.data$id, pred=predict(slda.lst[[1]], train.data))
eros.lda <- merge(x = School_EROS.dt, y = eros.lda, by = "id", all.x = TRUE)
eros.lda <- eros.lda[Frequency %in% 200 & !category%in%"PSAND" & !pred%in%NA]
eros.lda <- eros.lda[, c("id", "category","pred","YMD_time", "PingNo","school_area", "DepthStart", "DepthStop", "meanDepth", "area")]
nrow(eros.lda[category == pred & school_area >=500])/nrow(eros.lda[school_area >=500])

#== variable importance ==# (use different algorithm with stepwise)
varImp(slda.lst[[i]], scale = FALSE)
plot(varImp(slda.lst[[i]], scale = FALSE))


#== sandeel vs nonsandeel ==#
sandeel.dt$category_2 <- "SAND"
nonsandeel.dt$category_2 <- "OTHER"
saildrone <- data.table(rbind(sandeel.dt, nonsandeel.dt))



#==================================================#
####  Discriminant analyses 1 time (for test)   ####
#==================================================#
maxvar <-(ncol(train.data))-2
direction <-"backward"
slda1 <- train(category ~ ., data = train.data[, -c("id")],
               method = "stepLDA", importance = TRUE,metric="ROC", tuneLength=10,
               trControl = trainControl(method = "repeatedcv",number=10, repeats = 3, savePredictions = "final", classProbs = TRUE), #"repeatedcv" / "cv"
               tuneGrid=data.frame(maxvar,direction),
)
slda1$finalModel
varImp(slda1)
slda1
slda1$finalModel$fit
coef <- data.table(slda1$finalModel$fit$scaling, keep.rownames = TRUE)

predictions.test <- predict(slda1, test.data) #, type="prob"
mean(predictions.test==test.data$category)
confusionMatrix(reference=as.factor(test.data$category) , data= predictions.test, mode = "everything", positive = "SAND")

library(ROCR)
predictions.test <- predict(slda, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate



#==========================================#
#===         find Sandeel school        ===#
#### Discriminant analyses (with 18kHz) ####
#==========================================#

#== pre-processing ==#

#library("tidyverse")
library("caret")
#== pre-processing ==#
cols <- c("Date","Latitude", "Longitude", "PingNo", "pixelNo", 
          "SampleCount", "vessel", "area", "YMD_time", "altitude", 
          "meanDepth", "DepthStart", "DepthStop", "sV_var", "nor_DepthStart", 
          "nor_DepthStop", "nor_Depth",  "altitude_degree")

data <- subset(School_EROS.dt, Frequency %in% c(200, 18) 
               & !category %in% "PSAND"
               # & school_area >= median(School_EROS.dt$school_area)
)

data[,cols]=NULL
rf_18 <- data[Frequency == 18]$rf
data <- subset(data, Frequency %in% c(200)) 
data[, rf_18 := rf_18]
data[,Frequency:=NULL]
# remove negative value of depth from bottom
for (i in 1:nrow(data)) {
  if (data$DepthfromBottom[i] < 0)
    data$DepthfromBottom[i] <- 0
  else
    data$DepthfromBottom[i] <- data$DepthfromBottom[i]
}
# log transform #
data[,sV_mean:=log(sV_mean)][,sV_max:=log(sV_max)][,sV_min:=log(sV_min)][,sV_stdv:=log(sV_stdv)]
data[,rf:=log(rf)][,SE:=log(SE)][,sA:=log(sA)][,school_area:=log(school_area)][,school_length:=log(school_length)]
data[,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1)][,school_height:=log(school_height)]
data[,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)]
data[,rf_18:=log(rf_18)]


ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free")
featurePlot(data[, 3:ncol(data)],as.factor(data$category), plot="density", auto.key = list(columns = 2),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))



#== step-wise LDA * 10,  Saildrone data ==#
#library(ROCR)
library(klaR) #uses stepclass in the klaR
library(MASS) 
maxvar <-(ncol(data))-2
direction <-"backward"
#test_SD.data <- data.frame(test_SD.data)
#score <- 0
#score.df <- data.frame(matrix(ncol = 4, nrow = 0))
coef.df <- data.frame(matrix(ncol = 3, nrow = 0))
confusion.matrix <- list()
slda.lst <- list()
for_lift <- data.frame()
data.lst <- list()
rm(train.data, test.data)

#==============================#
####   80% train 20% test   ####
#==============================#
for(i in 1:2) {
  score <- 0
  # 80% : training data, 20% : test data
  set.seed(123)
  training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) 
  train.data <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  # Normalize the data (train data)
  preproc.param <- preProcess(train.data[,-c("id")], method = c("center", "scale"))
  train.data <- predict(preproc.param, train.data)
  train.data$category <- as.factor(train.data$category)
  # Normalize the data (test data)
  preproc.param <- preProcess(test.data[,-c("id")], method = c("center", "scale"))
  test.data <- predict(preproc.param, test.data)
  test.data$category <- as.factor(test.data$category)
  # tune a model
  set.seed(123)
  slda <- train(category ~ ., data = train.data[, -c("id")],
                method = "stepLDA", importance = TRUE,metric = "ROC", tuneLength = 10,
                trControl = trainControl(method = "cv", #"repeatedcv" / "cv"
                                         number = 10, 
                                         savePredictions = "all", #"all" / "final"
                                         classProbs = TRUE), 
                tuneGrid = data.frame(maxvar,direction),
  )
  # store the results
  temp2 <- data.table(slda$finalModel$fit$scaling, keep.rownames = TRUE)
  temp2$attempt <- i
  coef.df <- rbind(coef.df, temp2)
  predictions <- predict(slda, train.data)
  temp3 <- confusionMatrix(reference=as.factor(train.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"train")]] <- temp3
  predictions <- predict(slda, test.data)
  temp4 <- confusionMatrix(reference=as.factor(test.data$category) , data= predictions, mode = "everything", positive = "SAND")
  confusion.matrix[[paste0(i,"test")]] <- temp4
  slda.lst[[i]] <- slda
  #== for ROC curve (1 curve per k)   ==#
  temp5 <- data.frame(Class = slda$pred$obs, 
                      lda = slda$pred$SAND, 
                      resample = slda$pred$Resample)
  #=================================#
  for_lift <- rbind(for_lift, temp5)
  # store train and test data
  data.lst[[paste0(i,"train")]] <- train.data
  data.lst[[paste0(i,"test")]] <- test.data
}


rm(temp, temp2, temp3, temp4, temp5, slda, training.samples, preproc.param)

save(coef, file="Data/coef.Rdata")
save(confusion.matrix, file="Data/confusion.matrix.Rdata")
save(slda.lst, file="Data/slda.lst.Rdata")
save(for_lift, file = "Data/for_lift.Rdata")
save(data.lst, file = "Data/data.lst.Rdata")
#








#==  ROC curve  ==#
# by each trial
#for_lift <- data.frame(Class = slda.lst[[trial]]$pred$obs, lda = slda$pred$SAND, resample = slda$pred$Resample)
#png(paste0("ROC",trial,".png") , width = 800, height = 600)

lift_df <-  data.frame()
for (fold in unique(for_lift$resample)) {
  fold_df <- dplyr::filter(for_lift, resample == fold)
  lift_obj_data <- lift(Class ~ lda, data = fold_df, class = "SAND")$data
  lift_obj_data$fold = fold
  lift_df = rbind(lift_df, lift_obj_data)
  rm(lift_obj_data)
}

detach(package:plyr) # if "group_by" is not working


lift_df %>% 
  rename(group = n) %>%
  mutate(False_positive = 1-Sp) %>% 
  group_by(group) %>% 
  summarize(mean_Sn = mean(Sn), mean_Sp = mean(False_positive), 
            sd_Sn = sd(Sn), sd_Sp = sd(False_positive),
            n_Sn = length(Sn), n_Sp = length(False_positive), 
            se_Sn = sd_Sn / sqrt(n_Sn), se_Sp = sd_Sp / sqrt(n_Sp), 
            lower_Sn = mean_Sn - qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            upper_Sn = mean_Sn + qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            lower_Sp = mean_Sp - qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,
            upper_Sp = mean_Sp + qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,) %>% 
  ggplot(aes(x = mean_Sp, y = mean_Sn)) + 
  my_theme() + 
  geom_line(data = lift_df, aes(x = 1-Sp, y = Sn, group = fold), alpha = .1, lwd = .1) +
  #geom_ribbon(aes(ymin = lower_Sn, ymax = upper_Sn,
  #                xmin = lower_Sp, xmax = upper_Sp),
  #            fill = "red", alpha = .3) + 
  geom_line(colour = "red", lwd = 1.0) + 
  labs(x = "False positive", y = "True positive")


#=========================#
#==   biological data   ==#
#=========================#
x <- data.table(matrix(ncol = 5, nrow = 0))

#==  *use only test.data*   ==#
for(i in 1:length(slda.lst)) {
  temp <- data.table(id = data.lst[[paste0(i,"test")]]$id, 
                     pred = predict(slda.lst[[i]], data.lst[[paste0(i,"test")]]), 
                     obs = data.lst[[paste0(i,"test")]]$category,
                     data = "test", attempt = i)
  x <- rbind(x, temp, use.names=F)
  colnames(x) <- c("id", "pred", "obs", "data", "attempt")
}

#==  *use both test and train data*   ==#
for(i in 1:length(slda.lst)) {
  temp <- data.table(id = School_EROS.dt[Frequency %in% 200 & !category %in% "PSAND"]$id)
  # use 100% data
  if (length(slda.lst[[i]][["pred"]][["pred"]]) == nrow(School_EROS.dt[Frequency %in% 200 & !category %in% "PSAND"]))
    pred = data.table(predict(slda.lst[[i]], data.lst[[paste0(i,"train")]]),
                      id = data.lst[[paste0(i,"train")]][["id"]],
                      data = "100%")
  # use 80% data
  else 
    pred = rbind(data.table(pred = predict(slda.lst[[i]], data.lst[[paste0(i,"test")]]),
                            id = data.lst[[paste0(i,"test")]][["id"]],
                            data = "test"),
                 data.table(pred = predict(slda.lst[[i]], data.lst[[paste0(i,"train")]]),
                            id = data.lst[[paste0(i,"train")]][["id"]],
                            data = "train"))
  
  pred <- pred[order(id),]
  temp$pred <- pred$pred
  temp$obs = data$category
  temp$data = pred$data
  temp$attempt = i
  x <- rbind(x, temp, use.names=F)
  colnames(x) <- c("id", "pred", "obs", "data", "attempt")
}

### method A, B or C ###
### so far C is best ###

#===  (method A) correctly classified all 10 times   ===#
# *since ids of test data are different at each training,       #
#  it's rare to have an ideal id which is sandeel for 10 times. #
#  Thus, this method is not optimal.*                           #
x$pred_num <- ifelse(x$pred=="SAND", 1, 0)
x <- x[obs == "SAND"]
x <- data.table(with(x, aggregate(x[,c("pred_num")], list(id), sum)))
colnames(x) <- c("id", "pred_num")
x$pred <- ifelse(x$pred_num == length(slda.lst), "SAND", "OTHER")

#===  (method B) use the best accuracy model    ===#
#  *This method also leaves too few ids since      # 
#   maximum N of ids is 1530 and only schools that #
#   are observed in biological data can be used.*  #
x$correct <- ifelse(x$pred == x$obs, 1, 0)
setDT(x)[, accuracy := sum(correct)/.N, by = c("attempt")]
i <- unique(x[x$accuracy==max(x$accuracy),]$attempt)
x <- subset(x, attempt == i)

#===  (method C) use all 10 attempt but only consistent result  ===#
x$correct <- ifelse(x$pred == x$obs, 1, 0)
setDT(x)[, accuracy := sum(correct)/.N, by = c("attempt")]
setDT(x)[, duplicate_id := .N, by = c("id")][, discrepancy := mean(correct), by = c("id")]
x <- subset(x, discrepancy %in% c(1, 0))
x <- x[!duplicated(x[,c('id')]),]

#====================================================#

bio_id <- School_EROS_bio.df[, c("id", "meanLength", "weighted_meanLength")]
bio_id <- unique(bio_id)
bio_id <- x[bio_id, on = "id", roll = FALSE]
bio_id <- bio_id[complete.cases(bio_id), ]
bio_id$pred <- gsub('OTHER', 'Incorrect', bio_id$pred)
bio_id$pred <- gsub('SAND', 'Correct', bio_id$pred)

ggplot(bio_id) + my_theme() + theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 15)) + 
  geom_boxplot(aes(x=pred, y = meanLength), fill = "grey") + 
  labs(y = "mean body length (cm)")
ggplot(bio_id) + my_theme() + theme(legend.title = element_blank())+
  geom_histogram(aes(x=meanLength, y = stat(density), fill=pred), bins=30, colour="grey50", alpha=0.5, position="dodge") +
  geom_density(aes(x=meanLength, fill=pred), colour="grey50", alpha=0.5) +
  labs(x="mean body length of a trawl catch (cm)", y="Density")
nrow(bio_id[pred=='Correct'])/nrow(bio_id)




#===========================#
#==========================#

























































































































































































































































































































































































#========================================================================================================================#
#### test code                                                                                                        ####
#========================================================================================================================#


#replace NaN by interpolating previous and next non-NaN observation values
#library("zoo")
#bottom.dt$BottomDepth <- zoo::na.approx(bottom.dt$BottomDepth) 



#=======================================#
####      find Sandeel school        ####
#=======================================#

#== frequency response by category / area (use DT "joined") ==#
data = School_EROS.dt[category %in% c("SAND", "OTHER")] # rbind(School_EROS.dt, School_SD.dt)
frequency = c(18,38,70,120,200)
category = c("SAND" , "OTHER") #"SAND" / "PSAND" / "OTHER" /"KORONA" / "manual" 
aggregate(data$rf, list(data$Frequency, data$category),  sd)

ggplot(data=subset(data, Frequency %in% frequency & category %in% category & rf < 20 ), aes(x=Frequency, y = rf, colour=vessel)) + # & rf < 20
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  stat_summary(fun = mean, geom="point") + stat_summary(fun = mean, geom = "line") + #, linetype="dashed"
  #stat_summary(fun = mean, 
  #             fun.min = function(x) mean(x) - sd(x), 
  #             fun.max = function(x) mean(x) + sd(x), 
  #             geom = "errorbar", width = 7) +
  #stat_summary(fun = mean, geom="point") + stat_summary(data = subset (data, Frequency %in% c(38,200) & category %in% category & rf < 20) , fun = mean, geom = "line", size=1) + #  
  scale_x_continuous(breaks = c(38, 200)) + scale_y_continuous(breaks=c(-1, 1, 3, 5, 10)) + #, breaks=c(0.5, 1.0, 1.5) +
  facet_wrap(~category)+ #, scale="free_y"
  #  facet_wrap(~area, scale="free_y")+
  labs(x="Frequency", y="Frequency response")
#

#== r(f) for one school (specify "id") ==#
data = School_1032.dt
frequency = c(38,200)

ggplot(data=subset(data, id %in% 3487 & Frequency %in% frequency), aes(x=Frequency, y = rf )) + #colour=id, group=id
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  # geom_jitter(col="gray", size=0.5, shape=1,stroke = 1 ) + 
  geom_point() + geom_line() + geom_line(aes(y=rf+SE), linetype = "dashed") + geom_line(aes(y=rf-SE), linetype = "dashed") + #+ geom_errorbar(aes(ymin = rf-SE, ymax = rf+SE))
  scale_x_continuous(breaks = frequency) + # scale_y_continuous(limits = c(0.5, 1.5), breaks=c(0.5, 1.0, 1.5)) +
  labs(x="Frequency", y="Frequency response")
#

#== histogram of r(f) ==#
data = School_1032.dt
frequency = c(200)

ggplot(data=subset(data, Frequency %in% frequency ), aes(x = rf, colour=category, fill=category)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=0.1) + scale_x_continuous(limits = c(0, 7.5)) + geom_vline(xintercept = 1, linetype="dashed", alpha=.5) +
  #facet_wrap(~area, scales = "free_y")+
  #facet_grid(category~.)+
  #facet_wrap(~area, scale="free_y")+
  #annotate("text", label =paste("n="), size=4, x = Inf, y = Inf, hjust = 3, vjust = 7.5) + 
  #annotate("text", label =paste("n=", unique(joined$id.1)), size=4, x = Inf, y = Inf, hjust = 3, vjust = 7.5) + 
  labs(x="200kHz Frequency response", y="Number of school")
#

#!! mada !!#
School_EROS.dt %>% group_by(category, area) %>% mutate(N=n()) %>%
  mutate(N=ifelse(rf==max(rf,na.rm=T),paste0('n=',N),NA)) %>%
  ggplot(aes(x=rf, y=N, fill=category, label=N)) + scale_x_continuous(limits = c(0, 7.5)) + 
  geom_point()+
  #geom_histogram(bins = 1, binwidth=0.1) +
  geom_text(fontface='bold')+
  facet_wrap(~ area, nrow=3, scales = "free")
#xlab("")+
#scale_fill_manual(values = c("coral1", "lightcyan1", "olivedrab1"))+
#theme(legend.position="none")
#!! mada !!#


#== sV distribution (All) ==#
data <- rbind(School_EROS.dt, School_1032.dt, School_1031.dt)
frequency = c(200)

ggplot(data=subset(data, Frequency %in% frequency), aes(x=category, y=sV_mean)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(sV_mean), colour=vessel), alpha=.9) + #geom_point(aes(y=mean(sV_mean)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10() + #limits=c(1e-10,1e-03)
  labs(y="200 kHz sV mean")
#== sV distribution (above -82dB) ==#
sV_threshold <-10^(-82/10)
ggplot(data=data[Frequency==frequency & sV_mean > sV_threshold], aes(x=category, y=sV_mean)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + #geom_boxplot(aes(middle = mean(sV_mean)), alpha=.9) +geom_point(aes(y=mean(sV_mean)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10(limits=c(1e-10,1e-03))+ 
  #  facet_grid(.~category)+
  labs(y="sV mean")
#


#== Aspect ratio of school morphology ==#
data <- rbind(School_EROS.dt, School_1032.dt)
data$ratio <- data$PingNo/data$SampleCount
ggplot(data=subset(data, Frequency %in% c(200)), aes(x=category, y=ratio)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(ratio)), alpha=.9) +
  geom_point(aes(y=mean(ratio)), shape=1, size=3, col="red",stroke = 2 ) +
  #  facet_grid(.~category)+
  labs(y="aspect ration of school morphology")
#== r(f) vs school morphology aspect ratio (ratio) ==#
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  geom_point(aes(x=rf, y=ratio, colour=category), size=1, shape=1) + 
  scale_x_continuous(limits = c(0, 4)) + 
  #facet_grid(.~category) +
  labs(y="ping number/sample count ratio", x="frequency response")
#

#== pixel number by category ==#
data <- rbind(School_EROS.dt)#School_1032.dt
ggplot(data=subset(data, Frequency %in% c(200)), aes(x=category, y=pixelNo)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank())+
  geom_jitter(size=.5) + geom_boxplot(aes(middle = mean(pixelNo)), alpha=.9) +
  #geom_point(aes(y=mean(pixelNo)), shape=1, size=3, col="red",stroke = 2 ) +
  scale_y_log10()+
  #  facet_grid(.~category)+
  labs(y="pixel number")

#== r(f) vs pixel number (or aspect ratio) ==#
ggplot(data=subset(data, Frequency %in% c(200) ), aes(x=rf, y=pixelNo, col=category, fill=category)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point( size=1) + 
  scale_x_continuous(limits = c(0, 5)) + scale_y_continuous(limits = c(0,1000))+
  #facet_wrap(~area)+
  labs(x="200kHz Frequency response", y="pixel number")
#

#== school size by vehicles ==#
data <- rbind(School_EROS.dt,School_1032.dt, School_1031.dt)# School_1032.dt
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=school_area, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="school size (m2)")
#


#== school length by vehicles ==#
data <- rbind(School_EROS.dt, School_1032.dt)
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=school_length, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="school distance (m)")
#

#== sA by vehicles ==#
data <- rbind(School_EROS.dt)
ggplot(data=subset(data, Frequency %in% c(200))) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_boxplot(aes(x=category, y=sA, fill=vessel)) + scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() + 
  labs(x="", y="NASC")
#

#== school size vs frequency response ==#
data <- rbind(School_EROS.dt, School_1032.dt)
summary(data$school_area)
data$school_area_range <- cut(data$school_area, breaks=seq(0, 30870, 10), labels=as.character(seq(10, 30870, by=10)))
setDT(data)[, mean_rf := mean(rf), by = c("school_area_range", "Frequency", "category")]

category = c("SAND" ,"PSAND") #"SAND" / "PSAND" / "OTHER" /"KORONA" / "manual" 
ggplot(data=subset(data, Frequency %in% c(333) & category %in% category & mean_rf < 20)) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point(aes(x=school_area_range, y=mean_rf)) +# scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() +
  facet_wrap(~category) + #, scale="free_y"
  labs(x="school size", y="frequency response")
#

#== route ==#
data <- rbind(sandeel.dt, School_EROS.dt[category%in%"SAND"]) #rbind(School_EROS.dt, School_SD.dt)
ggplot(data=subset(data, Frequency %in% 200 & !area %in% "outside")) + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(),  axis.title = element_blank(), panel.spacing=unit(0, "lines"))+
  geom_point(aes(x=YMD_time, y=area, col=vessel)) + #facet_grid(area~.) + 
  labs(x="time")
#

#== chronological change ==#
data <- rbind(School_EROS.dt, School_SD.dt)
ggplot() + 
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point(aes(y=sA, x=YMD_time, col=category),data=subset(data, Frequency %in% c(200) & category %in% c("SAND", "KORONA", "manual")), shape=1) + scale_y_log10() + 
  #geom_point(aes(y=sA, x=YMD_time), data=subset(data,  Frequency %in% c(200) & category %in% c("SAND","KORONA", "manual")
  #& school_area >=100
  #& 10*log10(sV_mean) >= -55 & 10*log10(sV_mean) <= -40 
  #& rf >= 0.8 & rf<= 3.5)
  #), col="red")  +
  scale_y_log10() + 
  #geom_histogram(aes(x=school_area), bins = 30) + facet_grid(category~area, scale="free_y") + scale_x_log10() +
  #facet_wrap(~category) + #, scale="free_y"
  labs(y="school NASC", x="")
#


#
#




#== proportion of each categories ==#
data = data.table(School_EROS.dt)
max(data$rf)
#EROS : 662739.7 -> 662740
#1032 : 65.0 -> 65.0
#
data$range <- cut(data$rf, breaks=seq(0, 662740, 0.1), labels=as.character(seq(0.1, 662740, by=0.1)))
setDT(data)[, sum := .N, by = c("range", "Frequency")][, prop := .N, by = c("range", "Frequency", "category")][, prop := prop/sum][, sum := NULL]
max(data$school_area)
#EROS : 9107.837
#1032 : 110120 max(110117)
#
data$range_size <- cut(data$school_area, breaks=seq(0, 9110, 10), labels=as.character(seq(10, 9110, by=10)))
setDT(data)[, sum := .N, by = c("range_size", "Frequency")][, prop_size := .N, by = c("range_size", "Frequency", "category")][, prop_size := prop_size/sum][, sum := NULL]
summary(10*log10(data$sV_mean))
#EROS : min(-102.71)   max(-12.68)
#1032 : 
#
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-103, -12, 1), labels=as.character(seq(-102, -12, by=1)))]
setDT(data)[, sum := .N, by = c("range_SV", "Frequency")][, prop_SV := .N, by = c("range_SV", "Frequency", "category")][, prop_SV := prop_SV/sum][, sum := NULL]
max(data$sA)
#EROS : 13027787
#1032 : 
#
data$range_sA <- cut(data$sA, breaks=seq(0, 13027787, 10), labels=as.character(seq(10, 13027787, by=10)))
setDT(data)[, sum := .N, by = c("range_sA", "Frequency")][, prop_sA := .N, by = c("range_sA", "Frequency", "category")][, prop_sA := prop_sA/sum][, sum := NULL]



x <- data.table(category=data$category, Frequency=data$Frequency, area=data$area, 
                range = data$range, prop=data$prop, range_size = data$range_size, prop_size=data$prop_size,
                range_SV = data$range_SV, prop_SV=data$prop_SV, range_sA=data$range_sA, prop_sA=data$prop_sA)
#x <- unique(x)
x$range <- as.numeric(as.character(x$range))
x$range_size <- as.numeric(as.character(x$range_size))
x$range_SV <- as.numeric(as.character(x$range_SV))
x$range_sA <- as.numeric(as.character(x$range_sA))



#== frequency response (rf) ==#
ggplot(data=subset(x, Frequency %in% c(200) & !category %in% "PSAND"), aes(x=range, y=prop, col=category)) + geom_vline(xintercept = 1, linetype="dashed", alpha=.5) +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 7.5))+
  #facet_wrap(~area)+
  labs(x="200kHz Frequency response", y="Proportion")
#


#== School size ==#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = pixelNo, colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=10) + scale_x_continuous(limits = c(0, 1000))+
  labs(x="School size", y="Number of school")
#
ggplot(data=subset(x, Frequency %in% c(200) & category %in% c("OTHER","SAND","PSAND")), aes(x=range_size, y=prop_size, col=category)) +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 1000))+
  #facet_wrap(~area)+
  labs(x="School size", y="Proportion")
#

#== SV_mean ==#
ggplot(data=subset(x, Frequency %in% c(38) ), aes(x=range_SV, y=prop_SV, col=category))  +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + #scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(-80, -35))+
  #facet_wrap(~area)+
  labs(x="38kHz mean SV", y="Proportion")
#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = 10*log10(sV_mean), colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=1) + scale_x_continuous(limits = c(-80, -35))+
  labs(x="mean SV", y="Number of school")
#

#== sA ==#
ggplot(data=subset(School_EROS.dt, Frequency %in% c(200) ), aes(x = sA, colour=category, fill=category)) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_histogram(bins = 1, binwidth=10) + scale_x_continuous(limits = c(0, 1000))+
  labs(x="school sA", y="Number of school")
#
ggplot(data=subset(x, Frequency %in% c(200) ), aes(x=range_sA, y=prop_sA, col=category))  +
  theme_bw(base_size=20) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())+
  geom_point() + geom_line() + scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0, 1000))+
  #facet_wrap(~area)+
  labs(x="200kHz sA", y="Proportion")
#



#=====================   test code   =========================#
data <- subset(School_EROS.dt, Frequency %in% 200)
summary(10*log10(data$sV_mean))
#EROS : -83.98(Min). -17.60(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-84, -17, 1), labels=as.character(seq(-83, -17, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
table(data$category)


data_1032 <- subset(School_1032.dt, Frequency %in% 200)
summary(10*log10(data_1032$sV_mean))
#1032 : -75.02(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-76, -20, 1), labels=as.character(seq(-75, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
table(data_1032$category)


#== Is the school size reasonable? ==#
# EROS #
nrow(data[data$category=="SAND"])/length(unique(School_EROS.dt[category=="SAND"]$id)) # detection rate
# saildrone #
sum(data_1032$sA)/sum(School_1032.dt[Frequency==200]$sA)


#== frequency response(rf) ==#
# EROS #
#test <- data[rf>= 1]
test <- data[rf>= 0.8 & rf<= 3.5] #rf>= 0.85 & rf<= 1.6
nrow(test[category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[rf>= 0.8 & rf<= 1.6])/nrow(data_1032) # % of sandeel
table(data_1032[rf>= 0.8 & rf<= 1.6]$category)


#== mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -54 & SV_mean<= -43])/nrow(data_1032)
table(data_1032[SV_mean >= -54 & SV_mean<= -43]$category)


#== frequency response(rf) + mean_SV ==#
# EROS #
test <- data[SV_mean >= -55 & SV_mean<= -40 &  rf >= 0.8 & rf<= 3.5]
nrow(test[test$category=="SAND"])/nrow(test) # accuracy
table(test$category)
nrow(test[category=="SAND"])/length(unique(data[category=="SAND"]$id)) # detection rate
sum(test[category=="SAND"]$sA)/sum((data[Frequency==200 & category=="SAND"]$sA)) # sA
# SAILDRONE #
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6])/nrow(data_1032)
table(data_1032[SV_mean >= -55 & SV_mean<= -45 &  rf >= 0.8 & rf<= 1.6]$category)

data <- data.table(School_EROS.dt)
summary(10*log10(data$sV_mean))
#EROS : -102.71(Min). -12.68(Max)
setDT(data)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data$SV_mean, breaks=seq(-102, -12, 1), labels=as.character(seq(-101, -12, by=1)))]
aggregate(data$SV_mean, list(data$category,data$Frequency),  mean)
#data <- subset(data, data$school_area>=100)
data <- subset(data, Frequency %in% 200)

data_1032 <- data.table(School_1032.dt)
summary(10*log10(data_1032$sV_mean))
#1032 : -82.78(Min). -20.56(Max). 
setDT(data_1032)[, SV_mean := 10*log10(sV_mean)][, range_SV := cut(data_1032$SV_mean, breaks=seq(-82, -20, 1), labels=as.character(seq(-81, -20, by=1)))]
#data_1032 <- subset(data_1032, data_1032$school_area>=100)
data_1032 <- subset(data_1032, Frequency %in% 200)

#== frequency response(rf) + school size ==#
#= EROS =#
test <- data[school_area > 100 & rf >= 0.8  & rf<= 1.7]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND" ])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= saildrone =#
nrow(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7])/nrow(data_1032)
sum(data_1032[data_1032$Frequency==200 & school_area > 100 & rf >= 0.8  & rf<= 1.7]$sA)/sum(data_1032$sA) # sum(sA)/sum(sA from all sandeel schools)

#== frequency response(rf) + school size + mean_SV ==#
#= EROS =#
test <- data[SV_mean >= -55 & SV_mean<= -40  &  rf >= 0.7 & rf<= 1.8]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
table(test$category)
nrow(test[test$category=="SAND"])/length(unique(data$id[data$category=="SAND"])) # exclusion % of sandeel schools
sum(test[test$category=="SAND"]$sA)/sum((data[category=="SAND"]$sA)) # sum(sA)/sum(sA from all sandeel schools)
#= SAILDRONE =#
nrow(data_1032[SV_mean >= -55 & SV_mean<= -45 &  pixelNo > 324 &  rf >= 0.9 & rf<= 1.6])/nrow(data_1032)

#== mean_SV + school size ==#
test <- data[Frequency==200 & SV_mean >= -55 & SV_mean<= -45 &  school_area > 100 ]
nrow(test[test$category=="SAND"])/nrow(test) # % of sandeel
#table(test$category)
nrow(test[test$category=="SAND"])/length(unique(School_EROS.dt$id[School_EROS.dt$category=="SAND"])) # exclusion % of sandeel schools
#= SAILDRONE =#


test <- subset(School_EROS.dt[,-c("id","category","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "sV_stdv","area", "SE", "YMD_time", "altitude")])
test2 <-  melt(test)
ggplot(test2, aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")



cols <- as.character(slda$finalModel$model$name)
cols <- append(cols,c("id", "category"))
data <- subset(School_EROS.dt, Frequency %in% c(200)& !category %in% "PSAND")#& !category %in% "PSAND"
data[, setdiff(names(data), cols) := NULL][]
cols_log <- cols[!cols %in% c("DepthStop","meanDepth","DepthfromBottom", "altitude_degree", "id", "category")]
data[,(cols_log):=lapply(.SD, log),.SDcols=cols_log]
#Split the data into training and test set:
training.samples <- createDataPartition(data$category, p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. variance=1, mean=varied. Categorical variables are automatically ignored.
#Estimate pre-processing parameters
preproc.param <- train.data[,-c("id")] %>% preProcess(method = c("center", "scale"))
#Transform the data using the estimated parameters
train.data <- predict(preproc.param, train.data)
test.data <- predict(preproc.param, test.data)



#============================#
#=  discriminant analysis  ==#
#============================#

#== Fit the model ==#
formula <- as.character(slda$finalModel$formula[3])
formula <- paste0("category~", formula)
model <- lda(as.formula(formula), data = train.data[,-c("id")]) #qda, mda, fda
plot(model, dimen=1, type="both") #plot of dimension 1
#library(devtools)
# Make predictions
pred.train <- predict(model, train.data)
pred.test <- predict(model, test.data)
# Model accuracy
mean(pred.train$class==train.data$category)
table(train.data$category, pred.train$class)
#mean(pred.test$class==test.data$category)
#table(test.data$category, pred.test$class)
library(ROCR)
pred <- prediction(pred.test$posterior[,2], test.data$category) # 2 class->[,2], 3 class->[,3]
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate

#library(ggord)
#ggord(model, train.data$category, xlim=c(-7, 7), ylim=c(-7.5,5))

#== determine ids "SAND" ==#
apply_SD <-  predict(slda, test_SD.data)
predict(slda, test.data, type="prob")
test_SD.data$predict <- apply_SD
nrow(test_SD.data[predict=="SAND"])/nrow(test_SD.data)
table(test_SD.data$predict)
ids <- test_SD.data[predict %in% c("SAND")]$id
#write.csv(ids, "ids_9.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#featurePlot(test_SD.data[, 3:19],as.factor(test_SD.data$predict), plot="density", auto.key = list(columns = 2))

#== "KORONA" + "manual" ==#
test_SD_KM.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% c("KORONA","manual") & !area %in% "outside")
test_SD_KM.data[,cols]=NULL
test_SD_KM.data[, Frequency:=NULL]
test_SD_KM.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+2)][,school_height:=log(school_height)]
test_SD_KM.data <- predict(preproc.param, test_SD_KM.data)
test_SD_KM.data$predictions <- predict(slda.lst[[1]], test_SD_KM.data)
table(test_SD_KM.data$predictions, test_SD_KM.data$category)
ids <- test_SD_KM.data[predictions%in%"SAND"]$id
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)



#==   average ROC   ==#
#predictions <- slda.lst[[1]][["pred"]][["SAND"]]
#predictions <- predict(slda, train.data, type="prob")
#pred <- prediction(predictions, rep(train.data$category,times=3))
#pred <- prediction(predictions[2], train.data$category)
perf <- performance(pred, "tpr", "fpr")
plot(unlist(perf@y.values) ~ unlist(perf@x.values))



for_lift <- data.frame(Class = slda$pred$obs, lda = slda$pred$SAND, resample = slda$pred$Resample)

lift_df <-  data.frame()
for (fold in unique(for_lift$resample)) {
  fold_df <- dplyr::filter(for_lift, resample == fold)
  lift_obj_data <- lift(Class ~ lda, data = fold_df, class = "SAND")$data
  lift_obj_data$fold = fold
  lift_df = rbind(lift_df, lift_obj_data)
  rm(lift_obj_data)
}


detach(package:plyr) # if "group_by" is not working

lift_df %>% 
  rename(group = n) %>%
  mutate(False_positive = 1-Sp) %>% 
  group_by(group) %>% 
  summarize(mean_Sn = mean(Sn), mean_Sp = mean(False_positive), 
            sd_Sn = sd(Sn), sd_Sp = sd(False_positive),
            n_Sn = length(Sn), n_Sp = length(False_positive), 
            se_Sn = sd_Sn / sqrt(n_Sn), se_Sp = sd_Sp / sqrt(n_Sp), 
            lower_Sn = mean_Sn - qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            upper_Sn = mean_Sn + qt(1 - (0.05 / 2), n_Sn - 1) * se_Sn,
            lower_Sp = mean_Sp - qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,
            upper_Sp = mean_Sp + qt(1 - (0.05 / 2), n_Sp - 1) * se_Sp,) %>% 
  ggplot(aes(x = mean_Sp, y = mean_Sn)) + 
  my_theme() + 
  geom_line(data = lift_df, aes(x = 1-Sp, y = Sn, group = fold), alpha = .1, lwd = .1) +
  #geom_ribbon(aes(ymin = lower_Sn, ymax = upper_Sn,
  #                xmin = lower_Sp, xmax = upper_Sp),
  #            fill = "red", alpha = .3) + 
  geom_line(colour = "red", lwd = .5) + 
  labs(x = "False positive", y = "True positive")



  
  
#==  calculate AUC   ==#
library(plyr)
library(MLmetrics)
ddply(slda$pred, "Resample", summarise,
      accuracy = Accuracy(pred, obs))





#== biological data ==#
#===  (method B) use the best accuracy model    ===#
result <- data.frame()
for(i in 1:length(slda.lst)) {
  temp <- c(i, slda.lst[[i]][["results"]][["Accuracy"]],
            slda.lst[[i]][["results"]][["Kappa"]])
  result <- rbind(result, temp)
}
colnames(result) <- c("attempt","Accuracy","Kappa")
i <- result[result$Accuracy==max(result$Accuracy),]$attempt
x <- data.table(id = data.lst[[paste0(i,"test")]]$id, 
                pred = predict(slda.lst[[i]], data.lst[[paste0(i,"test")]]), 
                data = "test", attempt = i)






#=========================#
#### Logistic analysis ####
#=========================#
cols <- c("id","Frequency", "Date","Latitude", "Longitude", "PingNo", "pixelNo", "SampleCount", "vessel", "area", "YMD_time", "altitude", "nor_Depth", 
          "weighted_meanDepth")
#"SE","sV_stdv","school_length","school_height", "DepthStart", "DepthStop", "meanDepth" ,"sA", "altitude_degree", "Perimeter", "DepthfromBottom", "Elongation", "school_rect", "school_circ",
data <- subset(School_EROS.dt, Frequency %in% 200 & !category %in% "PSAND" & school_area >= median(School_EROS.dt$school_area))#& !category %in% "PSAND"
data[,cols]=NULL
data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
#Split the data into training and test set:
training.samples <- data$category %>%
  createDataPartition(p = 0.8, list = FALSE) #80% : training data, 20% : test data
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]
#Normalize the data. Categorical variables are automatically ignored.
# SNAD -> 1, OTHER -> 0 
train.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
test.data[,category:= ifelse(category=="SAND", 1, 0)]#[,category:= as.factor(category)]
ggplot(melt(data[,-c("id")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")


#model fit
#train.data$category <- as.factor(train.data$category)
lr <- train(as.double(category)~., data = train.data, family=binomial(), importance = TRUE,
            trControl = trainControl(method = "repeatedcv",number=10, repeats = 5, savePredictions = "final", classProbs = TRUE),
            method = "glmStepAIC")
lr$results
lr$finalModel
varImp(lr$finalModel)
plot(lr, type=c("g", "o"))

predictions.test <- ifelse(predict(lr, test.data)> 0.5, 1, 0)
mean(predictions.test==test.data$category)
confusionMatrix(reference=as.factor(test.data$category) , data= as.factor(predictions.test), mode = "everything", positive = as.character("1"))

library(ROCR)
predictions.test <- predict(slda, test.data, type="prob")
pred <- prediction(predictions.test[2], test.data$category)
plot(performance(pred, "tpr", "fpr"), colorize=TRUE)#tpr:true prediction rate, fpr:false prediction rate



#model <- glm(as.numeric(category)~., data=train.data, family = binomial())
train.data[, predict := predict(model, type="response")][,predict := ifelse(predict> 0.5, 1, 0)]
sum(train.data$category == train.data$predict)/nrow(train.data)
table(train.data$category, train.data$predict)
#test data
test.data[, predict := stats::predict(model, type="response", newdata = test.data)][,predict := ifelse(predict> 0.5, 1, 0)]
sum(test.data$category == test.data$predict)/nrow(test.data)
table(test.data$category, test.data$predict)
#find cutoff value using ROC



#apply to saildron data
test_SD.data <- subset(School_SD.dt, Frequency %in% 200 & category %in% "KORONA" & !area %in% "outside")
test_SD.data[,cols]=NULL
test_SD.data[, Frequency:=NULL]
test_SD.data[,sV_mean:=log(sV_mean)][,rf:=log(rf)][,school_area:=log(school_area)][,Elongation:=log(Elongation)][,school_rect:=log(school_rect)][,school_circ:=log(school_circ)][,sV_stdv:=log(sV_stdv)][,SE:=log(SE)][,sA:=log(sA)][,school_length:=log(school_length)][,Perimeter:=log(Perimeter)][,DepthfromBottom:=log(DepthfromBottom+1.7)][,school_height:=log(school_height)]
ggplot(melt(test_SD.data[,-c("id", "category")]), aes(value))+geom_histogram(bins=30)+facet_wrap(~variable, scales="free_x")
test_SD.data[, predict := ifelse(predict(lr, test_SD.data)> 0.5, "SAND", "OTHER")]
table(test_SD.data$predict)

ids <- test_SD.data[predict %in% "SAND"]$id
write.csv(ids, "ids_LR_1.csv")
sandeel.dt <-  subset(School_SD.dt, id %in% ids & Frequency %in% 200)
colnames(sandeel.dt) <- make.unique(names(sandeel.dt)) #for ggplot error
#




ids <- School_EROS.dt[category %in% c("SAND") & Frequency %in% 200 & !area %in% "outside"
                      #& school_area >=100
                      #& 10*log10(sV_mean) >= -55 & 10*log10(sV_mean) <= -40 
                      #& rf >= 0.8 & rf<= 3.5
                      #& area %in% c("Engelsk_Klondyke") # "Engelsk_Klondyke" / "Inner_Shoal_East_2016","Inner_Shoal_North", "Inner_Shoal_West_2018", "Inner_Shoal_test"
]$id
sandeel.dt <-  subset(School_EROS.dt, id %in% ids & Frequency %in% 200)











#==========================================#
####   compare different LDA results    ####
#==========================================#

load('sandeel_diffLDAresults.Rdata')
# add column "sandeel" or "non_sandeel" 
sandeel_diffLDAresults.dt[, pred:= ifelse((LDA %like% "nonsandeel"), "non-sandeel", "sandeel")]
sandeel_diffLDAresults.dt$LDA <- gsub("_nonsandeel","",as.character(sandeel_diffLDAresults.dt$LDA))
# re-order the results for boxplot
table(sandeel_diffLDAresults.dt$LDA)
sandeel_diffLDAresults.dt$LDA <- factor(sandeel_diffLDAresults.dt$LDA,
                       levels = c('3000', '1000', '2000',
                                  '80train', '80train18var'), 
                       ordered = TRUE)
ggplot(sandeel_diffLDAresults.dt
       [!LDA %in% c("2000")]) + 
  geom_boxplot(aes(y = log(sA), x = LDA, fill = pred)) + 
  my_theme() + theme(axis.title.x = element_blank(), legend.title = element_blank()) +
  scale_x_discrete(labels = c('100% 14var\n(3019)', 
                              '100% 14var\n(1085)',
                              '80% 14var\n(958)',
                              '80% 18var\n(1072)'))

#===========================================#



### horizontal distribution plot ###
data <- rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #rbind(sandeel.dt, School_EROS.dt[Frequency%in%200 & category%in%"SAND" & !area %in% "outside" ]) #sandeel.dt #& school_area >= median(School_EROS.dt$school_area)
data$month <- strftime(data$YMD_time, "%m", tz="UTC")
data <- mutate (data, month = case_when (month=="04"~"April", month=="05"~"May", month=="06"~"June", month=="07"~"July~", TRUE ~"July~"))
data <- mutate (data, area = case_when (area=="AlbjoernLing" ~ "AlbjoernLing", area=="VestbankenSouthEast" ~ "Vestbanken",area=="VestbankenSouthWest" ~ "Vestbanken", area=="Vestbanken_North" ~ "Vestbanken",area=="Vikingbanken" ~ "Vikingbanken", area=="Engelsk_Klondyke" ~ "Engelish Klondyke",area=="Inner_Shoal_East_2016" ~ "Inner Shoal", area=="Inner_Shoal_North" ~ "Inner Shoal",area=="Inner_Shoal_West_2018" ~ "Inner Shoal", area=="Inner_Shoal_test" ~ "Inner Shoal",area=="Nordgyden" ~ "Nordgyden", area=="Ostbanken" ~ "Ostbanken", area=="Outer_Shoal" ~ "Outer Shoal",TRUE ~ "NA"))
setDT(data)[, scale_lat:= scale(Latitude)[,1], by=area][, scale_lon:=scale(Longitude)[,1], by = area ]

ggplot(data=data) + 
  theme_bw(base_size=15) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  geom_point(aes(x=scale_lat, y=log(sA)),size=1, shape=1) + # + scale_y_continuous(trans = log_trans(), breaks = trans_breaks("log", function(x) exp(x)),labels = trans_format("log", math_format(e^.x))) + 
  geom_smooth(method="lm", formula=log(y) ~ x, se=F, aes(x=scale_lat, y=sA)) + #,colour="grey", linetype = "dashed"
  #facet_grid(area~factor(month, levels=c("April", "May", "June", "July~"))) + #, ncol=1, scales = "free_x", strip.position="right"
  coord_flip() + 
  labs(x="Latitude", y="mean NASC of Sandeel (200kHz)")
#

#== center of gravity ==#
center_of_mass <- function(x,y,w){
  c(crossprod(x,w)/sum(w), crossprod(y,w)/sum(w))}
center_of_mass(data$Latitude, data$Longitude, data$sA)



#acos(sin(pi*(Latitude)/180.0)*sin(pi*(Latitudestart)/180.0)+cos(pi*(Latitude)/180.0)*cos(pi*(Latitudestart)/180.0)*cos(pi*(Longitude)/180.0-pi*(Longitudestart)/180.0))*6378
