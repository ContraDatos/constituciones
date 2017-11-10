##################################### LOAD LIBRARIES ##################################################
library(data.table);
library(foreach);
library(doParallel);
library(readstata13)

##################################### GLOBAL PATHS ##################################################
root_path<-"/home/jesus/Desktop/home/contradata/catalonia/";
input_path<-file.path(root_path,"input/");
output_path<-file.path(root_path,"output/");
scripts_path<-file.path(root_path,"scripts/R/");

##################################### SOURCE SCRIPTS ##################################################


##################################### GLOBAL VARIABLES ##################################################
cores<-1;

################################## GLOBAL FUNCTIONS ##################################################

##################################### BEGIN ##################################################
registerDoParallel(cores=cores)

##################################### LOAD DATA ##################################################
dat<-fread(file.path(input_path,"ccpcce","ccpcce_v1_2.txt"))


##################################### CURRENT CONSTITUTIONS ##################################################
max_year<-dat[,list(max_year=max(year)),by="country"]
dat<-merge(dat,max_year,by="country");
dat<-dat[max_year==2013]

################################## [1] GENERAL ITERATION ############################

##################################### CREATION YEAR ##################################################
# Get year of last constitution creation
origin<-dat[evnttype=="new"];
origin<-origin[,list(creation_year=max(year)),by="country"];


##################################### REMOVE SUSPENDED CONSTITUTIONS ##################################################

# Get year of last constitution suspension
suspension<-dat[evnttype=="suspension"];
suspension<-suspension[,list(suspension_year=max(year)),by="country"];

# Merge creation with suspension
origin<-merge(origin,suspension,by="country",all.x = TRUE)

# Remove suspended constitutions
origin<-origin[is.na(suspension_year) | (suspension_year < creation_year)];


##################################### TENURE ##################################################

# Get tenure
origin$tenure<-max(dat$year)-origin$creation_year;

# Remove too recent constitutions
origin<-origin[tenure > 21]

# Sort by tenure
origin<-origin[order(tenure)];

# Get Spain position (75 oldest - 60%)
which(origin$country=="Spain")
100*which(origin$country=="Spain")/nrow(origin)


##################################### AMENDMENTS PER YEAR ##################################################
# Get amendments per year
amendments<-dat[evnttype=="amendment"];

# Merge to obtain creation year
amendments<-merge(origin,amendments,by="country")
amendments<-amendments[year>=creation_year];

# Get total amendments
amendments<-amendments[,list(n_amendments=(COUNT=.N)),by="country"];
amendments<-merge(origin,amendments,by="country",all.x = TRUE)
amendments[is.na(amendments$n_amendments)]$n_amendments<-0;
amendments<-amendments[,c("country","n_amendments"),with=F]

# Get amendments per year
amendments<-merge(amendments,origin,by="country");
amendments$amendments_per_year<-round(amendments$n_amendments/amendments$tenure,4);

# Sort by amendments per year
amendments<-amendments[order(-amendments_per_year)];

# Get Spain position (91 less amendments per year -> 72%)
which(amendments$country=="Spain")
100*which(amendments$country=="Spain")/nrow(amendments)

##################################### FINAL RESULTS ##################################################
# Save to final results
res<-amendments;

write.table(res,file.path(output_path,"results.csv"),sep=";",col.names = TRUE,row.names = FALSE)

##################################### STATISTICS ##################################################
summary(amendments$amendments_per_year)
amendments$amendments_per_year[amendments$country=="Spain"]

##################################### PLOTS ##################################################
boxplot(amendments$amendments_per_year,main="Countries Constitutions Amendments Per Year")
stripchart(amendments$amendments_per_year[amendments$country=="Spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - EUROPE ##################################################
europe_countries<-unique(fread(file.path(input_path,"europe_countries"))$V2)
europe_countries<-tolower(europe_countries)
amendments$country<-tolower(amendments$country)
amendments$country<-sapply(strsplit(amendments$country,split="\\("),"[",1)
amendments$country<-sapply(strsplit(amendments$country,split="/"),"[",1)
amendments$country<-gsub(" ","",amendments$country)
europe_amendments<-amendments[country %in% europe_countries]
bad<-amendments[!(country %in% europe_countries)]
europe_amendments<-rbind(europe_amendments,bad[country=="german federal republic"])
europe_amendments<-rbind(europe_amendments,bad[country=="bosnia-herzegovina"])

# Sort by amendments per year
europe_amendments<-europe_amendments[order(-amendments_per_year)];

# Get Spain position (26 less amendments per year -> 76%)
which(europe_amendments$country=="spain")
100*which(europe_amendments$country=="spain")/nrow(europe_amendments)

summary(europe_amendments$amendments_per_year)
europe_amendments$amendments_per_year[europe_amendments$country=="spain"]
##################################### PLOTS - EUROPE ##################################################
boxplot(europe_amendments$amendments_per_year,main="European Countries Constitutions Amendments Per Year")
stripchart(europe_amendments$amendments_per_year[europe_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - UE ##################################################
ue_countries<-unique(fread(file.path(input_path,"europe_countries"))[V11=="EU"]$V2)
ue_countries<-tolower(ue_countries)
ue_amendments<-amendments[country %in% ue_countries]
bad<-europe_amendments[!(country %in% ue_countries)]


# Sort by amendments per year
ue_amendments<-ue_amendments[order(-amendments_per_year)];

# Get Spain position (17 less amendments per year -> 85%)
which(ue_amendments$country=="spain")
100*which(ue_amendments$country=="spain")/nrow(ue_amendments)

summary(ue_amendments$amendments_per_year)
ue_amendments$amendments_per_year[ue_amendments$country=="spain"]
##################################### PLOTS - UE ##################################################
boxplot(ue_amendments$amendments_per_year,main="UE Countries Constitutions Amendments Per Year")
stripchart(ue_amendments$amendments_per_year[ue_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 



################################## [2] CLOSE TENURE ITERATION ############################

##################################### CREATION YEAR ##################################################
# Get year of last constitution creation
origin<-dat[evnttype=="new"];
origin<-origin[,list(creation_year=max(year)),by="country"];

##################################### REMOVE SUSPENDED CONSTITUTIONS ##################################################

# Get year of last constitution suspension
suspension<-dat[evnttype=="suspension"];
suspension<-suspension[,list(suspension_year=max(year)),by="country"];

# Merge creation with suspension
origin<-merge(origin,suspension,by="country",all.x = TRUE)

# Remove suspended constitutions
origin<-origin[is.na(suspension_year) | (suspension_year < creation_year)];


##################################### TENURE ##################################################

# Get tenure
origin$tenure<-max(dat$year)-origin$creation_year;

# Remove too recent constitutions
origin<-origin[tenure <= 40 & tenure >= 30]

# Sort by tenure
origin<-origin[order(tenure)];

# Get Spain position (75 oldest - 60%)
which(origin$country=="Spain")
100*which(origin$country=="Spain")/nrow(origin)


##################################### AMENDMENTS PER YEAR ##################################################
# Get amendments per year
amendments<-dat[evnttype=="amendment"];

# Merge to obtain creation year
amendments<-merge(origin,amendments,by="country")
amendments<-amendments[year>=creation_year];

# Get total amendments
amendments<-amendments[,list(n_amendments=(COUNT=.N)),by="country"];
amendments<-merge(origin,amendments,by="country",all.x = TRUE)
amendments[is.na(amendments$n_amendments)]$n_amendments<-0;
amendments<-amendments[,c("country","n_amendments"),with=F]

# Get amendments per year
amendments<-merge(amendments,origin,by="country");
amendments$amendments_per_year<-amendments$n_amendments/amendments$tenure;

# Sort by amendments per year
amendments<-amendments[order(-amendments_per_year)];

# Get Spain position (91 less amendments per year -> 72%)
which(amendments$country=="Spain")
100*which(amendments$country=="Spain")/nrow(amendments)

##################################### FINAL RESULTS ##################################################
# Save to final results
res<-amendments;

write.table(res,file.path(output_path,"results.csv"),sep=";",col.names = TRUE,row.names = FALSE)

##################################### STATISTICS ##################################################
summary(amendments$amendments_per_year)
amendments$amendments_per_year[amendments$country=="Spain"]

##################################### PLOTS ##################################################
boxplot(amendments$amendments_per_year,main="Countries Constitutions Amendments Per Year")
stripchart(amendments$amendments_per_year[amendments$country=="Spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - EUROPE ##################################################
europe_countries<-unique(fread(file.path(input_path,"europe_countries"))$V2)
europe_countries<-tolower(europe_countries)
amendments$country<-tolower(amendments$country)
amendments$country<-sapply(strsplit(amendments$country,split="\\("),"[",1)
amendments$country<-sapply(strsplit(amendments$country,split="/"),"[",1)
amendments$country<-gsub(" ","",amendments$country)
europe_amendments<-amendments[country %in% europe_countries]
bad<-amendments[!(country %in% europe_countries)]
europe_amendments<-rbind(europe_amendments,bad[country=="german federal republic"])
europe_amendments<-rbind(europe_amendments,bad[country=="bosnia-herzegovina"])


# Sort by amendments per year
europe_amendments<-europe_amendments[order(-amendments_per_year)];

# Get Spain position (26 less amendments per year -> 76%)
which(europe_amendments$country=="spain")
100*which(europe_amendments$country=="spain")/nrow(europe_amendments)

summary(europe_amendments$amendments_per_year)
europe_amendments$amendments_per_year[europe_amendments$country=="spain"]
##################################### PLOTS - EUROPE ##################################################
boxplot(europe_amendments$amendments_per_year,main="European Countries Constitutions Amendments Per Year")
stripchart(europe_amendments$amendments_per_year[europe_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - UE ##################################################
ue_countries<-unique(fread(file.path(input_path,"europe_countries"))[V11=="EU"]$V2)
ue_countries<-tolower(ue_countries)
ue_amendments<-amendments[country %in% ue_countries]
bad<-europe_amendments[!(country %in% ue_countries)]


# Sort by amendments per year
ue_amendments<-ue_amendments[order(-amendments_per_year)];

# Get Spain position (17 less amendments per year -> 85%)
which(ue_amendments$country=="spain")
100*which(ue_amendments$country=="spain")/nrow(ue_amendments)

summary(ue_amendments$amendments_per_year)
ue_amendments$amendments_per_year[ue_amendments$country=="spain"]
##################################### PLOTS - UE ##################################################
boxplot(ue_amendments$amendments_per_year,main="UE Countries Constitutions Amendments Per Year")
stripchart(ue_amendments$amendments_per_year[ue_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 


################################## [3] SAME DIFFICULTY ITERATION ############################

##################################### CREATION YEAR ##################################################
# Get year of last constitution creation
origin<-dat[evnttype=="new"];
origin<-origin[,list(creation_year=max(year)),by="country"];

##################################### REMOVE SUSPENDED CONSTITUTIONS ##################################################

# Get year of last constitution suspension
suspension<-dat[evnttype=="suspension"];
suspension<-suspension[,list(suspension_year=max(year)),by="country"];

# Merge creation with suspension
origin<-merge(origin,suspension,by="country",all.x = TRUE)

# Remove suspended constitutions
origin<-origin[is.na(suspension_year) | (suspension_year < creation_year)];


##################################### TENURE ##################################################

# Get tenure
origin$tenure<-max(dat$year)-origin$creation_year;

# Merge with difficulty
difficulty<-data.table(read.dta13(file.path(input_path,"ccpcce","ADData.dta")))

max_years<-difficulty[,list(max_year=max(year)),by="country"]
difficulty<-merge(difficulty,max_years,by="country")
difficulty<-difficulty[year == max_year]
difficulty<-difficulty[,c("country","ad_ak"),with=F]

origin<-merge(origin,difficulty,by="country")
origin<-origin[ad_ak == origin[country=="Spain"]$ad_ak]
origin<-origin[tenure > 21]

# Sort by tenure
origin<-origin[order(tenure)];

# Get Spain position (75 oldest - 60%)
which(origin$country=="Spain")
100*which(origin$country=="Spain")/nrow(origin)


##################################### AMENDMENTS PER YEAR ##################################################
# Get amendments per year
amendments<-dat[evnttype=="amendment"];

# Merge to obtain creation year
amendments<-merge(origin,amendments,by="country")
amendments<-amendments[year>=creation_year];

# Get total amendments
amendments<-amendments[,list(n_amendments=(COUNT=.N)),by="country"];
amendments<-merge(origin,amendments,by="country",all.x = TRUE)
amendments[is.na(amendments$n_amendments)]$n_amendments<-0;
amendments<-amendments[,c("country","n_amendments"),with=F]

# Get amendments per year
amendments<-merge(amendments,origin,by="country");
amendments$amendments_per_year<-amendments$n_amendments/amendments$tenure;

# Sort by amendments per year
amendments<-amendments[order(-amendments_per_year)];

# Get Spain position (91 less amendments per year -> 72%)
which(amendments$country=="Spain")
100*which(amendments$country=="Spain")/nrow(amendments)

##################################### FINAL RESULTS ##################################################
# Save to final results
res<-amendments;

write.table(res,file.path(output_path,"results.csv"),sep=";",col.names = TRUE,row.names = FALSE)

##################################### STATISTICS ##################################################
summary(amendments$amendments_per_year)
amendments$amendments_per_year[amendments$country=="Spain"]

##################################### PLOTS ##################################################
boxplot(amendments$amendments_per_year,main="Countries Constitutions Amendments Per Year")
stripchart(amendments$amendments_per_year[amendments$country=="Spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - EUROPE ##################################################
europe_countries<-unique(fread(file.path(input_path,"europe_countries"))$V2)
europe_countries<-tolower(europe_countries)
amendments$country<-tolower(amendments$country)
amendments$country<-sapply(strsplit(amendments$country,split="\\("),"[",1)
amendments$country<-sapply(strsplit(amendments$country,split="/"),"[",1)
amendments$country<-gsub(" ","",amendments$country)
europe_amendments<-amendments[country %in% europe_countries]
bad<-amendments[!(country %in% europe_countries)]
europe_amendments<-rbind(europe_amendments,bad[country=="german federal republic"])
europe_amendments<-rbind(europe_amendments,bad[country=="bosnia-herzegovina"])


# Get Spain position (26 less amendments per year -> 76%)
which(europe_amendments$country=="spain")
100*which(europe_amendments$country=="spain")/nrow(europe_amendments)

summary(europe_amendments$amendments_per_year)
europe_amendments$amendments_per_year[europe_amendments$country=="spain"]
##################################### PLOTS - EUROPE ##################################################
boxplot(europe_amendments$amendments_per_year,main="European Countries Constitutions Amendments Per Year")
stripchart(europe_amendments$amendments_per_year[europe_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 

##################################### STATISTICS - UE ##################################################
ue_countries<-unique(fread(file.path(input_path,"europe_countries"))[V11=="EU"]$V2)
ue_countries<-tolower(ue_countries)
ue_amendments<-amendments[country %in% ue_countries]
bad<-europe_amendments[!(country %in% ue_countries)]


# Get Spain position (17 less amendments per year -> 85%)
which(ue_amendments$country=="spain")
100*which(ue_amendments$country=="spain")/nrow(ue_amendments)

summary(ue_amendments$amendments_per_year)
ue_amendments$amendments_per_year[ue_amendments$country=="spain"]
##################################### PLOTS - UE ##################################################
boxplot(ue_amendments$amendments_per_year,main="UE Countries Constitutions Amendments Per Year")
stripchart(ue_amendments$amendments_per_year[ue_amendments$country=="spain"], data = InsectSprays, 
           vertical = TRUE, method = "jitter", 
           pch = 17, col = "red", bg = "bisque", 
           add = TRUE) 


################################## [4] AMENDMENTS BY DIFFICULTY ############################

##################################### CREATION YEAR ##################################################
# Get year of last constitution creation
origin<-dat[evnttype=="new"];
origin<-origin[,list(creation_year=max(year)),by="country"];

##################################### REMOVE SUSPENDED CONSTITUTIONS ##################################################

# Get year of last constitution suspension
suspension<-dat[evnttype=="suspension"];
suspension<-suspension[,list(suspension_year=max(year)),by="country"];

# Merge creation with suspension
origin<-merge(origin,suspension,by="country",all.x = TRUE)

# Remove suspended constitutions
origin<-origin[is.na(suspension_year) | (suspension_year < creation_year)];


##################################### TENURE ##################################################

# Get tenure
origin$tenure<-max(dat$year)-origin$creation_year;

# Keep only same difficulty constitutions
difficulty<-data.table(read.dta13(file.path(input_path,"ccpcce","ADData.dta")))

max_years<-difficulty[,list(max_year=max(year)),by="country"]
difficulty<-merge(difficulty,max_years,by="country")
difficulty<-difficulty[year == max_year]
difficulty<-difficulty[,c("country","ad_ak"),with=F]

origin<-merge(origin,difficulty,by="country")

origin<-origin[!is.na(ad_ak)]

# Sort by tenure
origin<-origin[order(tenure)];

# Get Spain position (75 oldest - 60%)
which(origin$country=="Spain")
100*which(origin$country=="Spain")/nrow(origin)


##################################### AMENDMENTS PER YEAR ##################################################
# Get amendments per year
amendments<-dat[evnttype=="amendment"];

# Merge to obtain creation year
amendments<-merge(origin,amendments,by="country")
amendments<-amendments[year>=creation_year];

# Get total amendments
amendments<-amendments[,list(n_amendments=(COUNT=.N)),by="country"];
amendments<-merge(origin,amendments,by="country",all.x = TRUE)
amendments[is.na(amendments$n_amendments)]$n_amendments<-0;
amendments<-amendments[,c("country","n_amendments"),with=F]

# Get amendments per year
amendments<-merge(amendments,origin,by="country");
amendments$amendments_per_year<-amendments$n_amendments/amendments$tenure;

# Sort by amendments per year
amendments<-amendments[order(-amendments_per_year)];

# Get mean amendments_per_year by nivel of amendment difficulty
mean_by_diff<-amendments[,list(amendments_per_year=mean(amendments_per_year)),by="ad_ak"]
mean_by_diff<-mean_by_diff[order(ad_ak)]
mean_by_diff



##################################### STATISTICS - EUROPE ##################################################
europe_countries<-unique(fread(file.path(input_path,"europe_countries"))$V2)
europe_countries<-tolower(europe_countries)
amendments$country<-tolower(amendments$country)
amendments$country<-sapply(strsplit(amendments$country,split="\\("),"[",1)
amendments$country<-sapply(strsplit(amendments$country,split="/"),"[",1)
amendments$country<-gsub(" ","",amendments$country)
europe_amendments<-amendments[country %in% europe_countries]
bad<-amendments[!(country %in% europe_countries)]
europe_amendments<-rbind(europe_amendments,bad[country=="german federal republic"])
europe_amendments<-rbind(europe_amendments,bad[country=="bosnia-herzegovina"])

# Get mean amendments_per_year by nivel of amendment difficulty
mean_by_diff<-europe_amendments[,list(amendments_per_year=mean(amendments_per_year)),by="ad_ak"]
mean_by_diff<-mean_by_diff[order(ad_ak)]
mean_by_diff


##################################### STATISTICS - UE ##################################################
ue_countries<-unique(fread(file.path(input_path,"europe_countries"))[V11=="EU"]$V2)
ue_countries<-tolower(ue_countries)
ue_amendments<-amendments[country %in% ue_countries]
bad<-europe_amendments[!(country %in% ue_countries)]

# Get mean amendments_per_year by nivel of amendment difficulty
mean_by_diff<-ue_amendments[,list(amendments_per_year=mean(amendments_per_year)),by="ad_ak"]
mean_by_diff<-mean_by_diff[order(ad_ak)]
mean_by_diff



################################## [5] REGRESSION MODEL ############################
# Data pre-processing
X<-ue_amendments[,c("ad_ak","amendments_per_year"),with=F];
X<-X[ad_ak <= 6]
setnames(X,"amendments_per_year","y")

# Compute model mse
mse<-mean((predictions-y_test)^2)
null_mse<-mean((null_predictions-y_test)^2)

# Check R-squared
model <- lm(y ~ .,data=X)
X$ad_ak<-rnorm(nrow(X),mean = 0,sd = 10)
random_model <- lm(y ~.,data=X)
summary(model)$r.squared
summary(random_model)$r.squared
