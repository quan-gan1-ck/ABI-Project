library(ggplot2)
library(gridExtra)
library(WVPlots)
library(dplyr)
library(cowplot)
library(corrplot)
library(rminer)

## The company has shared it dataset and requested to have an answer on the following areas: 
###1. What changes company should bring to reduce the number of absenteeism? 
###2. How much losses every month can we project in 2011 if same trend of absenteeism continues?

#Loading the file "Absenteeism_at_work.csv"

## Important extra note: suggestion regarding the right path of te file, independently of the location
fc <- paste(dirname(rstudioapi::getSourceEditorContext()$path),
            '/Data/Absenteeism_at_work.csv', sep="")
			
fc <- "Data/Absenteeism_at_work.csv" 
dat <- read.csv(fc, header = TRUE, sep = ";")

# Data Preparation

#Dimension of data
dim(dat)

#Names of data
names(dat)

#Structure of data
str(dat)

#Check for abnormal values
sort(unique(dat$Month.of.absence))
dat[dat$Month.of.absence == 0, ]
nrow(dat[dat$Month.of.absence == 0, ])    # Luckily, there are not many invalid instances.
### dizer que estas 3 instancias podem ser as previsões feitas pelos autores do artigo que refere o data set. 
##To solve of the anomalies in Month of absence column, we remove the invalid cells.
dat <- dat[dat$Month.of.absence != 0, ]

#There are attributes which contain redundant information, i.e., with more than 90% of the objects with the same value. These attributes are:
freq_atr <- c()
for(i in 1:ncol(dat)){
  freq_atr[i] <- max(table(dat[, i]))/nrow(dat)
}
freq_atr <- round(freq_atr, 2)
ia <- which(freq_atr >= 0.9)
dat <- dat[, -ia]


## Add the name of the categorical atributes

labels_Reasons <- c("0" = "Infectious and Parasitic", "1" = "Neoplasms", "2" = "Blood and Immune Mechanism", 
                   "3" = "Endocrine, Nutritional and Metabolic", "4" = "Mental and Behavioural", 
                   "5" = "Nervous System", "6" = "Eye and Adnexa", "7" = "Ear and Mastoid Process", 
                   "8" = "Circulatory System", "9" = "Respiratory System", "10" = "Digestive System", 
                   "11" = "Skin and Subcutaneous Tissue", "12" = "Musculoskeletal System and Connective Tissue",
                   "13" = "Genitourinary System", "14" = "Pregnancy, Childbirth and Puerperium", 
                   "15" = "Perinatal Period", "16" = "Congenital and Chromosomal Malformations", 
                   "17" = "Clinical and Laboratory Findings", "18" = "Injury, Poisoning and other Consequences",
                   "19" = "Ext. causes of Morbidity and Mortality", "21" = "Health Status and Health Services", 
                   "22" = "Patient Follow-up", "23" = "Medical Consultation", "24" = "Blood Donation", 
                   "25" = "Laboratory Examination", "26" = "Unjustified Absence", "27" = "Physiotherapy", 
                   "28" = "Dental Consultation")

labels_Months <- c("1" = "Jan", "2" = "Feb", "3" = "Mar",
                   "4" = "Apr", "5" = "May", "6" = "Jun",
                   "7" = "Jul", "8" = "Aug", "9" = "Sep",
                   "10" = "Oct", "11" = "Nov", "12" = "Dec")

labels_DayW <- c("2" = "Monday", "3" = "Tuesday", 
                 "4" = "Wednesday", "5" = "Thursday", 
                 "6" = "Friday")

labels_Seasons <- c("1" = "Summer", "2" = "Autumn", 
                    "3" = "Winter", "4" = "Spring")

labels_Educ <- c("1" = "Highschool", "2" = "Graduate", 
                 "3" = "Postgraduate", "4" = "Master and Doctrate")

labels_Son <- c("0" = "Son = 0", "1" = "Son = 1", "2" = "Son = 2", 
                "3" = "Son = 3",  "4" = "Son = 4")


# Basic Statistics

#Distribution of every numeric variable
summary(dat)

r <- ggplot(dat, aes(x = Reason.for.absence)) + geom_bar() + labs(x = "Reason for Absence")
m <- ggplot(dat, aes(x = factor(Month.of.absence, labels = labels_Months))) + geom_bar() + labs(x = "Month of Absence")
d <- ggplot(dat, aes(x = factor(Day.of.the.week, labels = labels_DayW))) + geom_bar() + labs(x = "Day of the Week")
s <- ggplot(dat, aes(x = factor(Seasons, labels = labels_Seasons))) + geom_bar() + labs(x = "Seasons")
t <- ggplot(dat, aes(x = Transportation.expense)) + geom_bar() + labs(x = "Transportation Expense")
di <- ggplot(dat, aes(x = Distance.from.Residence.to.Work)) + geom_bar() + labs(x = "Distance from Residence")
se <- ggplot(dat, aes(x = Service.time)) + geom_bar() + labs(x = "Service Time")
a <- ggplot(dat, aes(x = Age)) + geom_bar() + labs(x = "Age")
w <- ggplot(dat, aes(x = Work.load.Average.day)) + geom_bar() + labs(x = "Work Load Average Day")
h <- ggplot(dat, aes(x = Hit.target)) + geom_bar() + labs(x = "Hit Target")
e <- ggplot(dat, aes(x = factor(Education, labels = labels_Educ))) + geom_bar() + labs(x = "Education") + 
  theme(axis.text.x = element_text(size = 6))
so <- ggplot(dat, aes(x = factor(Son, labels = labels_Son))) + geom_bar() + labs(x = "Son")
sd <- ggplot(dat, aes(x = factor(Social.drinker, labels = c("0", "1")))) + geom_bar() + labs(x = "Social Drinker")
p <- ggplot(dat, aes(x = Pet)) + geom_bar() + labs(x = "Pet")
w <- ggplot(dat, aes(x = Weight)) + geom_bar() + labs(x = "Weight")
h <- ggplot(dat, aes(x = Height)) + geom_bar() + labs(x = "Height")
b <- ggplot(dat, aes(x = Body.mass.index)) + geom_bar() + labs(x = "Body Mass Index")
ab <- ggplot(dat, aes(x = Absenteeism.time.in.hours)) + geom_bar() + labs(x = "Absenteeism Time in Hours")

grid.arrange(r, m, d, s, t, di, se, a, w, h, e, so, sd, p, w, h, b, ab, 
             nrow = 6, ncol = 3)

## The attribute disciplinary failure is taken into consideration and it was found it had no obvious part on target variable, for example


freqM <- c()
for(m in 1:12)
  freqM <- c(freqM, sum(dat$Absenteeism.time.in.hours[dat$Month.of.absence == m]))

barplot(freqM, names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
        main = "Total Absenteeism Time Hours over Months", las = 1, 
        xlab = "Month", ylab = "Absenteeism time hours")

barplot(c(sum(dat$Absenteeism.time.in.hours[dat$Day.of.the.week==2]),
          sum(dat$Absenteeism.time.in.hours[dat$Day.of.the.week==3]), 
          sum(dat$Absenteeism.time.in.hours[dat$Day.of.the.week==4]),
          sum(dat$Absenteeism.time.in.hours[dat$Day.of.the.week==5]),
          sum(dat$Absenteeism.time.in.hours[dat$Day.of.the.week==6])), 
        names.arg = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
        main = "Total Absenteeism Time Hours over Day of the Week", las = 0,
        xlab = "Day of the week", ylab = "Absenteeism time hours")

barplot(c(sum(dat$Absenteeism.time.in.hours[dat$Seasons==1]),
          sum(dat$Absenteeism.time.in.hours[dat$Seasons==2]), 
          sum(dat$Absenteeism.time.in.hours[dat$Seasons==3]),
          sum(dat$Absenteeism.time.in.hours[dat$Seasons==4])), 
        names.arg = c("Summer", "Autumn", "Winter", "Spring"), 
        main = "Total Absenteeism Time Hours over Seasons",
        xlab = "Season", ylab = "Absenteeism time hours")


#---------------
# max(dat$Absenteeism.time.in.hours)   # = 120 -> 3 weeks
# hist(dat$Absenteeism.time.in.hours, breaks = 20, 
#      xlab = "Absenteeism time in hours", main = "Frequency of Absenteeism Time")  #### 709 das faltas correspondem a faltas correspondentes a 10 horas, ie, pelo menos 1 dia de trabalho
# 
# hist(dat$Absenteeism.time.in.hours[dat$Son != 0], breaks = 20, 
#      xlab = "Absenteeism time in hours",
#      main = "Frequency of Absenteeism Time of Employees with Sons")
# 
# hist(dat$Absenteeism.time.in.hours[dat$Son == 0], breaks = 20, 
#      xlab = "Absenteeism time in hours",
#      main = "Frequency of Absenteeism Time of Employees without sons")
#---------------


ggplot(data = dat, aes(x = Absenteeism.time.in.hours)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Son, labeller = labeller(Son = labels_Son)) +
  labs(x = "Absenteeism time in hours", y = "Frequency") +
  ggtitle("Frequency of Absenteeism Time by Number of Sons") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dat, aes(x = Day.of.the.week, y = Absenteeism.time.in.hours)) +
  geom_bar(stat="identity") +
  facet_wrap(~Seasons, labeller = labeller(Seasons = labels_Seasons)) +
  labs(x = "Day of the week", y = "Absenteeism time in hours") +
  scale_x_discrete(limits=c("", "Monday","Tuesday","Wednesday", "Thursday", "Friday")) +
  ggtitle("Total of Absenteeism Time in each Day of the Week by Season of the Year") +
  theme(axis.text.x = element_text(angle = 90)) 

ggplot(data = dat, aes(x = Day.of.the.week, y = Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Month.of.absence, labeller = labeller(Month.of.absence = labels_Months)) +
  labs(x = "Day of the week", y = "Absenteeism time in hours") +
  scale_x_discrete(limits=c("", "Monday","Tuesday","Wednesday", "Thursday", "Friday")) +
  ggtitle("Total of Absenteeism Time in each Day of the Week by Month") +
  theme(axis.text.x = element_text(angle = 90))

#Education vs Reason for absence 
# Conclusions: the result is that most of population with absence time is Highschool. Acrecentar a confirmação de (a ser verdade) maior parte da população ter o grau Highschool.
# TO DO: um gráfico com a dvisão por educação no dataset. Este gráfico irá confirmar que maior parte da população tem como grau educacional: Hightschool
ggplot(data = dat, aes(x = Reason.for.absence, y = Absenteeism.time.in.hours)) +
  geom_bar(stat="identity") +
  facet_wrap(~Education, labeller = labeller(Education = labels_Educ)) +
  labs(x = "Reason for absence", y = "Absenteeism time in hours") +
  ggtitle("Total of Absenteeism Time of each Reason for Absence by Education Level")


table(dat$Social.drinker)
# Close to the half of employees drink alcohol(418/319),so the attempted analysis can be taken into consideration that the it can be a element that influence the target variable.

ggplot(dat, aes(x = Age, y = Absenteeism.time.in.hours, fill = Social.drinker)) + 
  geom_bar(stat = 'identity', position = position_dodge()) + 
  ylab("Absenteeism time in hours") +
  scale_x_continuous(breaks = c(seq(25, 60, 5)), limits = c(25,60)) +
  ggtitle("Total of Absenteeism Time by Age of Employees")


reasonPerc <-  as.data.frame(dat %>% 
                               group_by(Reason.for.absence) %>%
                               summarise(count= n(), 
                                         percent = round(count*100 / nrow(dat), 1)) %>%
                               arrange(desc(count)))

ggplot(reasonPerc, aes(x = reorder(Reason.for.absence, percent), y = percent, fill = Reason.for.absence)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  theme(legend.position = 'none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + 
  xlab('Reason for absence') +
  ylab('Percent') + 
  labs(title = 'Percentage of Reason for Absence') + 
  scale_x_discrete(labels = as.character(labels_Reasons[c(as.character(sort(reorder(reasonPerc$Reason.for.absence, reasonPerc$percent))))]))
# The top four of them cover 50% of the resons for absence
#* medical consultation
#* dental consultation
#* physiotherapy
#* Disease of genitourinary system
##The unjusitified absence amounts to 4.5% of total.

reason <- group_by(dat, Reason.for.absence) %>% summarise(num = sum(Absenteeism.time.in.hours))
most <- head(arrange(reason, desc(num)))
par(mar=c(13, 4, 4, 2)) 
barplot(most$num, names.arg = labels_Reasons[c(as.character(most$Reason.for.absence))], 
        main="Reasons with most Absenteeism Time",
        ylab = "Absenteeism time in hours", las = 2, cex.names = 0.8)

least <- head(arrange(reason, num))
par(mar=c(15, 4, 4, 2)) 
barplot(least$num, names.arg = labels_Reasons[c(as.character(least$Reason.for.absence))], 
        main="Reasons with least Absenteeism Time", 
        ylab = "Absenteeism time in hours", las = 2, cex.names = 0.8)


## pearson correlation between all variables 
#in plot:
corrplot(cor(dat[2:ncol(dat)]), type="lower", order="hclust")
#in matrix:
corrMat <- cor(dat[2:ncol(dat)])

ggplot(dat, aes(x = Body.mass.index, y = Weight)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title = 'Correlation between Weight and Body Mass Index', 
       x = 'Body mass index', y = 'Weight')

ggplot(dat, aes(x = Age, y = Service.time)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title = 'Correlation between Service Time and Age', 
       x = 'Age', y = 'Service time')
#Here trend of service time across age is taken. And they have positive correlation.

#ScatterHist(dat, "Absenteeism.time.in.hours", "Seasons", title = "xxx")
#ScatterHist(dat, "Absenteeism.time.in.hours", "Age", title = "xx")

## Tendo em conta o mail que enviaste, este gráfico pode ser removido.
ggplot(dat, aes(x = Distance.from.Residence.to.Work, y = Absenteeism.time.in.hours)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title = 'Correlation between Distance from Residence to Work and Absenteeism Time', 
       x = 'Distance from residence', y = 'Absence time in hours')
#Absence related to Distance to work
# Conclusions: Similarly to the last plot, in this, also, we can confirm that the distance was not a problem related to the absences.

#Transportation expenses related to Absenteeism in hours
# Conclusions: the result is the expected, as we can see in both previous
ggplot(dat, aes(x = Transportation.expense, y = Absenteeism.time.in.hours)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title = 'Correlation between Transportation Expenses and Absenteeism Time',
       x = 'Transportation expense', y = 'Absenteeism time in hours')





# Predictive Analysis


###-------------------------------------------Regression Task-------------------------------------------###


# split data in train and test data
atrib <- c(2:ncol(dat))
datR <- dat[, atrib]
split_dat <- holdout(datR$Absenteeism.time.in.hours, ratio = 0.7, seed = 2811)
target_trueValue <- datR[split_dat$ts, ]$Absenteeism.time.in.hours

models <- c("ctree", "rpart", "svm", "mlp", "randomForest", "xgboost", "cubist", "lm", "mars")

list_RegModels <- list()

## whith fit function:
for (m in models) {
  mod <- fit(Absenteeism.time.in.hours ~ ., datR[split_dat$tr, ],
           model = m, task = "reg")
  pred <- predict(mod, datR[split_dat$ts, ])
  err <- mmetric(target_trueValue, pred, metric = c("MAE", "RMSE"))
  
  list_RegModels[[m]] <- list()
  list_RegModels[[m]][[1]] <- mod
  list_RegModels[[m]][[2]] <- pred
  list_RegModels[[m]][[3]] <- err

}


list_RegModMining <- list()

## whith mining function (10-fold):
for (m in models) {
  mod <- mining(Absenteeism.time.in.hours ~ ., datR, 
                model = m, method = c("kfold", 10, 2811),
                Runs = 20)
  err <- mmetric(mod, metric = c("MAE", "RMSE"))
  
  list_RegModMining[[m]] <- list()
  list_RegModMining[[m]][[1]] <- mod
  list_RegModMining[[m]][[2]] <- err
  
}

mae <- c()
rmse <- c()

for(i in 1:length(models)){
  mae <- c(mae, round(as.numeric(list_RegModels[[i]][[3]][1]), 2))
  rmse <- c(rmse, round(as.numeric(list_RegModels[[i]][[3]][2]), 2))
}

L <- data.frame("MAE" = mae, 
                "RMSE" = rmse)
row.names(L) <- models

print(L)

## best results
L[L$MAE == min(L$MAE),]
L[L$RMSE == min(L$RMSE),]


L_kf <- vector("list", 9) # list of minings

for(i in 1:length(models))
  L_kf[[i]] <- list_RegModMining[[i]][[1]]

mgraph(L_kf, graph = "REC", 
       leg = list(pos = c(14, 0.4),
                  leg = c("ctree", "rpart", "svm", "mlp", "randomForest", "xgboost", "cubist", "lm", "mars")),  
       col=c("forestgreen", "blue", "red", "saddlebrown", "green", "hotpink", "cyan", "darkmagenta", "orange"),
       main = "REC curve", xval = 20, Grid = 5)




###-------------------------------------------Classification Task-------------------------------------------###


### de forma a prever se um funcionário estaria ausente por curta ou longa duração
### e como target -> [0:120], dividimos em grupos como:
# <= 2        ---> até 2h de trabalho
# >2 & <= 4   ---> até meio dia de trabalho (4h) 
# >4 & <= 8   ---> até um dia de trabalho (8h)
# >8 & <= 40  ---> até uma semana de trabalho (40h)
# >40         ---> mais que uma semana de trabalho

classes <- c()
for (i in 1:nrow(dat)) {
  if(dat[i, ncol(dat)] <= 2)
    classes <- c(classes, "Class 1")
  else if(dat[i, ncol(dat)] > 2 & dat[i, ncol(dat)] <= 4)
    classes <- c(classes, "Class 2")
  else if(dat[i, ncol(dat)] > 4 & dat[i, ncol(dat)] <= 8)
    classes <- c(classes, "Class 3")
  else if(dat[i, ncol(dat)] > 8 & dat[i, ncol(dat)] <= 40)
    classes <- c(classes, "Class 4")
  else if(dat[i, ncol(dat)] > 40)
    classes <- c(classes, "Class 5")
}

datC <- dat
datC$Class <- classes
datC$Class <- as.factor(datC$Class)
### convertemos assim o problema em um problema de classificação

barplot(table(datC$Class))


# split data in train and test data
atrib <- c(2:18, 20)
datC <- datC[, atrib]
split_dat <- holdout(datC$Class, ratio = 0.7, seed = 2811)
target_trueValue <- datC[split_dat$ts, ]$Class

models <- c("ctree", "rpart", "svm", "mlp", "randomForest", "xgboost", "lda", "naiveBayes")

list_ClassModels <- list()

## whith fit function:
for (m in models) {
  mod <- fit(Class ~ ., datC[split_dat$tr, ],
             model = m, task = "class")
  pred <- predict(mod, datC[split_dat$ts, ])
  err <- mmetric(target_trueValue, pred, metric = c("ACC", "KAPPA"))
  
  list_ClassModels[[m]] <- list()
  list_ClassModels[[m]][[1]] <- mod
  list_ClassModels[[m]][[2]] <- pred
  list_ClassModels[[m]][[3]] <- err
  
}


list_ClassModMining <- list()

## whith mining function (10-fold):
for (m in models) {
  print(m)
  mod <- mining(Class ~ ., datC, 
                model = m, method = c("kfold", 10, 2811),
                Runs = 20)
  err <- mmetric(mod, metric = c("ACC", "PRECISION"))
  
  list_ClassModMining[[m]] <- list()
  list_ClassModMining[[m]][[1]] <- mod
  list_ClassModMining[[m]][[2]] <- err
  
}

acc <- c()
precision <- c()

for(i in 1:length(models)){
  acc <- c(acc, round(as.numeric(list_ClassModels[[i]][[3]][1]), 2))
  precision <- c(precision, round(as.numeric(list_ClassModels[[i]][[3]][2]), 2))
}

cL <- data.frame("ACC" = acc, 
                 "PRECISION" = precision)
row.names(cL) <- models

print(cL)

## best results
cL[cL$ACC == max(cL$ACC),]
cL[cL$PRECISION == max(cL$PRECISION),]


cL_kf <- vector("list", 9) # list of minings

for(i in 1:length(models))
  cL_kf[[i]] <- list_ClassModMining[[i]][[1]]

# ##ver esta função está a dar erro
# mgraph(cL_kf, graph = "REC", 
#        leg = list(pos = c(14, 0.4),
#                   leg = c("ctree", "rpart", "svm", "mlp", "randomForest", "xgboost", "boosting", "lda", "naiveBayes")),  
#        col=c("forestgreen", "blue", "red", "saddlebrown", "green", "hotpink", "cyan", "darkmagenta", "orange"),
#        main = "REC curve", xval = 20, Grid = 5)







############### TRY remove some attributs:


## Class:
datC$Education <- NULL
datC$Weight <- NULL
datC$Height <- NULL
datC$Body.mass.index <- NULL
datC$


#-----
list_ClassModels2 <- list()

## whith fit function:
for (m in models) {
  mod <- fit(Class ~ ., datC[split_dat$tr, ],
             model = m, task = "class")
  pred <- predict(mod, datC[split_dat$ts, ])
  err <- mmetric(target_trueValue, pred, metric = c("ACC", "KAPPA"))
  
  list_ClassModels2[[m]] <- list()
  list_ClassModels2[[m]][[1]] <- mod
  list_ClassModels2[[m]][[2]] <- pred
  list_ClassModels2[[m]][[3]] <- err
  
}

acc <- c()
precision <- c()

for(i in 1:length(models)){
  acc <- c(acc, round(as.numeric(list_ClassModels[[i]][[3]][1]), 2))
  precision <- c(precision, round(as.numeric(list_ClassModels[[i]][[3]][2]), 2))
}

cL2 <- data.frame("ACC" = acc, 
                 "PRECISION" = precision)
row.names(cL2) <- models

# best results
cL2[cL2$ACC == max(cL2$ACC),]
cL2[cL2$PRECISION == min(cL2$PRECISION),]

#------
