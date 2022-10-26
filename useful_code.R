#### delete column ####
df$X <- NULL  # delete 1 column
df <- df[, -c(21,23)]  # deletes columns 21 and 23
df <- df[, -c(21:23)]  # delete columns 21 through 23
rm(df)  # delete whole dataframe

#### edit charactor ####
df$XX<-gsub("_abc","",as.character(df$XX)) # remove "_abc" from df$XX
gsub("abc", "def", df$XX) # change characters in column XX from "abc" to "def"
#* if one slash(\) needs to be replaced then "\\\", if blakish(/) then "\\/"   *#
grepl("abc", object) #check if the object contains "abc", return TRUE or FALSE
colnames(df)[grepl("abc",colnames(df))] # extract colname which contains "abc"

#### check specific row or column in the dataframe ###
df[5000:5005, 12:14]  #5000 to 5005 rows, 12, 13, 14 columns

#### count number of rows #### 
nrow (df)  # count total rows of a dataframe
nrow (1.df) + nrow (2.df)  # count total rows of a 1.df and 2.df
nrow (df [df$X == "XXXX",])  # count rows under specific condition
nrow (transect.df [transect.df$TRANSECT == "T1",])  #count how many records(rows) that are T1
sum(transect.df$TRANSECT == "T1")   #count how many records(rows) that are T1
table(df$Mission)    #count row number of each factors
dt[, .(cnt= sum(.N)), by= c("XX1", "XX2", "XX3")] # count row number by column "XX1", "XX2", "XX3"
aggregate(data = data, x ~ group, function(x) length(unique(x)))   # count number of unique values in a group
sum((df$XX == df2$XX), na.rm = TRUE)  # count number of TRUE when comparing to objects     
          

#### export csv file  ####
write.csv(transect.df,"C:/Users/komiy/Desktop/jugyo/BIO399_MSc/Data/MyData.csv", row.names = FALSE)

#### check data-type  ####
sapply(transect.df,class)

#### max, min, mean  ####
max(df$col, na.rm = TRUE) #ignore NA value (na.rm = TRUE)
max(df[df$col=="XXXX",]$value) #see max "value" when col=="XXXX"
mean(df$col, na.rm = TRUE)
aggregate(GUST_WND_MEAN ~ i.TRANSECT, data=result82, mean) #calculate mean GUST_WND_MEAN of each TRANSECT
aggregate(PELAG ~ TRANSECT, data=data$Transect55_38.df, mean)  #calculate mean GUST_WND_MEAN of each TRANSECT
aggregate(data$column, list(data$category,data$Frequency),  mean) # mean of "column" by each category & frequency

#### check "na" in Data ####
any(is.na(others.df))
colSums(is.na(df)) # number of NA in each column
any(is.infinite(data$Transect82_200.df$TOTAL)) # check infinite number (the decimal continues infinitely)
df$XXX <- zoo::na.approx(bf$XXX) #replace NaN by interpolating previous and next non-NaN values

#### get first 3 row by condition ####
head(data$Transect55_38.df[data$Transect55_38.df$TRANSECT=="T2",],n=3)  #first 3 row of 2nd Transect
tail(data$Transect55_38.df[data$Transect55_38.df$TRANSECT=="T2",],n=3)  #


#### memory error ####
#e.g.) cannot allocate vector of size 1.5 Gb#
memory.size() #Checking your memory size
memory.limit() #Checking the set limit
memory.limit(size=56000) #expanding your memory 64bits:56000

#### scientific number -> regular number 
format(coef(fit_lm3), scientific = FALSE)
format(-3.076e-03, scientific = FALSE)

#### random number/ sampling ####
a <- rpois(100, lambda=20) # 100 row, mean=20, poisson distribution
sample(unique(dt$id),1)   ## choose 1 from all "id"


#### plot window ####
dev.new() #open a new plot window
dev.off() #shut down the window
#Error in plot.new() : figure margins too large
xll() # to create a float window for a large plot
#to save plot
png(paste0("plot.png") , width = 800, height = 600)
ggplot() # + ...
dev.off()


#### decimal ####
round(x, digits = 2)

#### change order ####
dt <- dt[order(XX),] #row order (numeric) by column XX
dt$XX <- factor(dt$XX, #row order by group (if there are 4 groups A, B, C, D in dt$XX)
                levels = c('A', 'B', 'C', 'D'),
                ordered = TRUE)

#### remove environment ####
rm(list = ls()) #remove all
rm(list=setdiff(ls(), c("gps.dt", "my_theme", "School_SD.dt", "School_EROS.dt"))) #remove except for ...



#### change df name dynamically ####
assign(paste("df", "2", sep = "_"), df)


#### histrogram ####
hist(df$XXX, breaks = 30, xlim = c(0, 100)) # break max-min into 30
hist(df$XXX, breaks = c(1, 10, 25, 50, 75), xlim = c(0, 100))
