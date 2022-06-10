library("dplyr")
library("knitr")
library("texreg")
library("plm")
library("xml2")
library("xlsx")
library("rvest")
library("stringr")
library(stringi)
library("XML")
library("stats")
library("tidyr")
library(lmtest)
library(sandwich)
library(ggplot2)

#### getting the data ####
polity <- read.xlsx("p5v2018.xls", 1)
wdi_data <- read.csv("full_WDI_data.csv")
stability <- read.table(file = "clipboard", sep = "\t", header=TRUE)
resource_rents <- read.csv("all_rents.csv")

#### cleaning the data ####
#stability
col_names <- colnames(stability)
col_names[1:4] <- c("Country_Name", "Country_Code", "Series_Name", "Series_Code")
col_names[5:25] <- str_sub(col_names[5:25], start = 2, end = 5)
colnames(stability) <- col_names
stability_cleaned <- filter(.data = stability, Series_Code == "PV.EST")
stability_cleaned[stability_cleaned == ".."] <- NA
fully_covered_rows <- c()
for (i in 1:213) {
        if (sum(is.na(stability_cleaned[i,-25])) == 0) {
                fully_covered_rows <- c(fully_covered_rows, i) 
        }
}
stability_cleaned <- stability_cleaned[fully_covered_rows,-25]
for (i in 5:24) {
        stability_cleaned[,i] <- as.numeric(stability_cleaned[,i])
}
stability_cleaned<- gather(data = stability_cleaned, key = Year, value = Score, colnames(stability_cleaned)[5:24])[,c(-2,-4)]


# resource rents
resource_rents[resource_rents == ".."] <- NA
col_names <- colnames(resource_rents)
col_names[1:4] <- c("Country_Name", "Country_Code", "Series_Name", "Series_Code")
col_names[5:65] <- str_sub(col_names[5:65], start = 2, end = 5)
colnames(resource_rents) <- col_names
resource_rents <- resource_rents[,c(1:4, 41:64)]
resource_rents <- resource_rents[-869:-1069,]


simple_function <- function(data) {
        fully_covered_countries <- c()
        for (i in unique(data[,1])) {
                row_numbers <- which(data[,1] == i)
                if (sum(is.na(data[row_numbers,1:27])) == 0)
                        fully_covered_countries <- c(fully_covered_countries, i)
        }
        fully_covered_countries
}
fully_covered_countries <- simple_function(resource_rents)
check_check <- unique(resource_rents[,1])[which(!unique(resource_rents[,1]) %in% fully_covered_countries)]
check_data <- filter(.data = resource_rents, Country_Name %in% check_check) #здесь мы обнаружили сразу несколько стран,
# у которых отсутствуют только несколько значений на исследуемом нами промежутке. Мы подставили эти средние значения двух
# ближайших наблюдений для того, чтобы включить эти страны в выборку. 
resource_rents[181,c(12,13)] <- c(0, as.numeric(resource_rents[181,14])/2) #Республика Конго
resource_rents[c(37,38,39,40),27] <- c(0,0,0,mean(as.numeric(resource_rents[40,c(24,25,26)]))) #Аруба
resource_rents[c(261,262,263,264),c(5,6)] <- rep(0, 8) #Фаросские острова
resource_rents[669,10] <- mean(as.numeric(resource_rents[669, c(9, 11)])) #Сенегал
# Еще раз используем функцию simple_function
cleaned_resource_rents <- filter(.data = resource_rents[,-28], Country_Name %in% fully_covered_countries)
sum(is.na(cleaned_resource_rents)) #Пропущенных данных нет
for (i in 5:27) {
        cleaned_resource_rents[,i] <- as.numeric(cleaned_resource_rents[,i])
}
cleaned_resource_rents <- gather(data = cleaned_resource_rents, key = Year, value = Score, colnames(cleaned_resource_rents[,5:27]))[,-c(2,4)] %>%
        spread(key = Series_Name, value = Score)

MA_function <- function(data, period, column) {
        data$MA <- NA
        data[,2] <- as.numeric(data[,2])
        for (i in unique(data[,1])) {
                temp_data <- filter(.data = data, Country_Name == i)
                for (j in sort(unique(temp_data[,2]))[period:length(unique(temp_data[,2]))]) {
                        data[which(data[,1] == i & data[,2] == j),length(data[1,])] <- mean(temp_data[which(temp_data[,2] == j - period + 1):which(temp_data[,2] == j),column])
                }
        }
        data
}
test <- MA_function(cleaned_resource_rents, 5, 6)

find_big_changes <- function(data) {
        ma_changes <- data.frame()
        for (i in unique(data[,1])) {
                temp_data <- filter(.data = data, Country_Name == i)
                ma_changes <- rbind(ma_changes, data.frame(country = i, change = max(temp_data[,7], na.rm = TRUE) - min(temp_data[,7], na.rm = TRUE)))
        }
        #ma_changes[,2] <- format(ma_changes[,2], scientific = FALSE)
        ma_changes
}
test_2 <- find_big_changes(test)
big_change_count <- test_2[which(test_2[,2] >= 10),1]

# polity
polity_cleaned <- filter(.data = polity, year >= 1996)
polity_function <- function(data) {
        fully_covered_countries <- c()
        for (i in unique(data[,5])) {
                temp_data <- filter(.data = data, country == i)
                if (sum(is.na(temp_data[,12])) == 0) {
                        fully_covered_countries <- c(fully_covered_countries, i)
                }
        }
        result <- filter(.data = data, country %in% fully_covered_countries)
        result
}
polity_cleaned <- polity_function(polity_cleaned)
polity_cleaned <- polity_cleaned[,c(5,6,12)]

years_check_function <- function(data) {
        fully_covered_years <- c()
        for (i in unique(data[,1])) {
                #if (i == "Serbia") {
                #        browser()
                #}
                temp_data <- filter(.data = data, Country_Name == i)
                if (sum(unique(data[,2]) %in% temp_data[,2]) == length(unique(data[,2]))) {
                        fully_covered_years <- c(fully_covered_years, i)   
                }
        }
        fully_covered_years
}
fcy <- years_check_function(polity_cleaned)

# wdi indicators
col_names <- colnames(wdi_data)
col_names[1:4] <- c("Country_Name", "Country_Code", "Series_Name", "Series_Code")
col_names[5:65] <- str_sub(col_names[5:65], start = 2, end = 5)
colnames(wdi_data) <- col_names
wdi_cleaned <- wdi_data[-1520:-1867,c(1:4,41:63)]
wdi_cleaned[wdi_cleaned == ".."] <- NA
wdi_function <- function(data) {
        fully_covered_countries <- c()
        for (i in unique(data[,1])) {
                temp_data <- filter(.data = data, Country_Name == i, Series_Name %in% unique(data[,3])[c(1,2,6)])
                if (sum(is.na(temp_data)) == 0) {
                        fully_covered_countries <- c(fully_covered_countries, i)
                }
        }
        result <- filter(.data = data, Country_Name %in% fully_covered_countries, Series_Name %in% unique(data[,3])[c(1,2,6)])
        result
}
wdi_cleaned <- wdi_function(wdi_cleaned)
wdi_cleaned <- gather(data = wdi_cleaned, key = Year, value = Score, colnames(wdi_cleaned[,5:27]))
wdi_cleaned <- spread(data = wdi_cleaned, key = Series_Name, value = Score)


# names problem
country_names_wb <- unique(stability_cleaned[,1])[which(unique(stability_cleaned[,1]) %in% unique(cleaned_resource_rents[,1]) &
                                                                unique(stability_cleaned[,1]) %in% unique(wdi_cleaned[,1]))]
sum(country_names_wb %in% unique(stability_cleaned[,1]) & country_names_wb %in% unique(cleaned_resource_rents[,1]) & 
                                                                                               country_names_wb %in% unique(wdi_cleaned[,1]))
country_names_polity <- unique(polity_cleaned[,5])
different_names <- country_names_wb[!country_names_wb %in% country_names_polity]
polity_cleaned_copy <- polity_cleaned
incorrect_names <- list(c("Cape Verde", "Cabo Verde"), c("Congo-Brazzaville", "Congo, Rep."), c("Congo Brazzaville", "Congo, Rep."),
                        c("Congo Kinshasa", "Congo, Dem. Rep."), c("Egypt", "Egypt, Arab Rep."), c("Swaziland", "Eswatini"),
                        c("Gambia", "Gambia, The"), c("Ivory Coast", "Cote D'Ivoire"), c("Iran", "Iran, Islamic Rep."),
                        c("Korea South", "Korea, Rep."), c("Kyrgyzstan", "Kyrgyz Republic"), c("Laos", "Lao PDR"),
                        c("Macedonia", "North Macedonia"), c("Russia", "Russian Federation"), c("UAE", "United Arab Emirates"),
                        c("United States                   ", "United States"), c("Yemen", "Yemen, Rep."))
for (i in incorrect_names) {
        polity_cleaned[which(polity_cleaned[,5] == i[1]),5] <- i[2]
}
incorrect_names <- list(c("Comoro Is.", "Comoros"), c("C. Verde Is.", "Cabo Verde"), c("Czech Rep.", "Czech Republic"),
                        c("FRG/Germany", "Germany"), c("Dom. Rep.", "Dominican Republic"), c("ROK", "Korea, Rep."),
                        c("Macedonia", "North Macedonia"), c("P. N. Guinea", "Papua New Guinea"), c("Russia","Russian Federation"),
                        c("Slovakia", "Slovak Republic"), c("S. Africa", "South Africa"), c("Trinidad-Tobago", "Trinidad and Tobago"),
                        c("USA", "United States"), c("UK", "United Kingdom"))
for (i in incorrect_names) {
        pol_power_cleaned[which(pol_power_cleaned[,1] == i[1]),1] <- i[2]
}


sum(country_names_wb %in% unique(polity_cleaned[,1]))
sample_names <- country_names_wb[country_names_wb %in% unique(polity_cleaned[,1])] #попадают наблюдения с неполным комплектом по годам
# Сербия, к примеру
sample_names <- country_names_wb[country_names_wb %in% fcy]




# gathering the data in one dataset
gathering_func <- function(names, years, polity, stability, wdi, resource_rents) {
        for (i in 1:length(names)) {
                #if (i == 114) {
                #        browser()
                #}
                for (j in 1:length(years)) {
                        temp_set <- data.frame(Country = names[i], 
                                               Year = years[j], 
                                               polity2 = polity[which(polity[,1] == names[i] & polity[,2] == years[j]),3],
                                               GDP_Growth = wdi[which(wdi[,1] == names[i] & wdi[,2] == years[j]),3],
                                               GDP_per_capita = wdi[which(wdi[,1] == names[i] & wdi[,2] == years[j]),4],
                                               Population = wdi[which(wdi[,1] == names[i] & wdi[,2] == years[j]),5],
                                               Stability = stability[which(stability[,1] == names[i] & stability[,3] == years[j]),4],
                                               Mineral_rents = resource_rents[which(resource_rents[,1] == names[i] & resource_rents[,2] == years[j]),3],
                                               Natural_gas_rents = resource_rents[which(resource_rents[,1] == names[i] & resource_rents[,2] == years[j]),4],
                                               Oil_rents = resource_rents[which(resource_rents[,1] == names[i] & resource_rents[,2] == years[j]),5],
                                               Total_natural_resources_rent = resource_rents[which(resource_rents[,1] == names[i] & resource_rents[,2] == years[j]),6],
                                               Population_total = wdi[which(wdi[,1] == names[i] & wdi[,2] == years[j]),5],
                                               )
                        if (i == 1 & j == 1) {
                                final_dataset <- temp_set
                        } else {
                                final_dataset <- rbind(final_dataset, temp_set)
                        }
                }
        }
        final_dataset
}

final_dataset <- gathering_func(sample_names, unique(stability_cleaned[,3]), polity_cleaned, stability_cleaned, wdi_cleaned, cleaned_resource_rents)

for (i in 3:12) {
        final_dataset[,i] <- as.numeric(final_dataset[,i])
}

#### analysis ####
#preparing data
add_dominant_resource <- function(data) {
        data$dominant_resource <- NA
        resources <- c("Minerals", "Natural Gas", "Oil")
        for (i in 1:length(data[,1])) {
                max_value <- max(data[i,c(8,9,10)])
                if (max_value/data[i,11] >= 0.5 & max_value >= 5) {
                      data[i,length(data[1,])] <- resources[which.max(data[i,c(8,9,10)])]
                } else {
                        data[i,length(data[1,])] <- "None"
                }
        }
        data[,length(data[1,])] <- as.factor(data[,length(data[1,])])
        data
}
final_dataset <- add_dominant_resource(final_dataset)

add_dominant_resource_II <- function(data) {
        data$dominant_resource_II <- NA
        resources <- c("Minerals", "Natural Gas", "Oil")
        for (i in unique(data[,1])) {
                print(i)
                temp_data <- filter(.data = data, Country == i)
                max_mean <- which.max(colMeans(temp_data[,c(8,9,10)]))
                data[which(data[,1] == i),length(data[1,])] <- resources[max_mean]
        }
        data[,length(data[1,])] <- as.factor(data[,length(data[1,])])
        data
}
final_dataset <- add_dominant_resource_II(final_dataset)

little_function <- function(data) {
        dependent_countries <- c()
        for (i in unique(data[,1])) {
                temp_data <- filter(.data = data, Country == i)
                if (mean(temp_data[,11]) >= 10) {
                        dependent_countries <- c(dependent_countries, i)
                }
        }
        dependent_countries
}
dependent_countries <- little_function(final_dataset)
dep_count_data <- filter(.data = final_dataset, Country %in% dependent_countries)
big_change_data <- filter(.data = final_dataset_lag, Country %in% big_change_count)
oil_dep_count_data <- filter(.data = dep_count_data, dominant_resource_II == "Oil")
gas_dep_count_data <- filter(.data = dep_count_data, dominant_resource_II == "Natural Gas")
min_dep_count_data <- filter(.data = dep_count_data, dominant_resource_II == "Minerals")
oil_count_data <- filter(.data = final_dataset, dominant_resource_II == "Oil")
gas_count_data <- filter(.data = final_dataset, dominant_resource_II == "Natural Gas")
min_count_data <- filter(.data = final_dataset, dominant_resource_II == "Minerals")

#first version of a regime type function
regime_type_I <- function(data) {
        data$regime_type_I <- NA
        for (i in 1:length(data[,1])) {
                if (data[i,3] < -3) {
                        data[i,length(data[1,])] <- "Authoritarian"
                } else {
                        if (data[i,3] > 4) {
                                data[i,length(data[1,])] <- "Democratic"
                        } else {
                                data[i,length(data[1,])] <- "Moderate"
                        }
                }
        }
        data[,length(data[1,])] <- as.factor(data[,length(data[1,])])
        data
}

regime_type_II <- function(data) {
        data$regime_type_II <- NA
        for (i in unique(data[,1])) {
                rows <- which(data[,1] == i)
                mn <- mean(data[rows,3])
                if (mn < -3) {
                        type <- "Authoritarian"
                } else {
                        if (mn > 3) {
                                type <- "Democratic"
                        } else {
                                type <- "Moderate"
                        }
                }
                data[rows,length(data[1,])] <- type
        }
        data
}

final_dataset <- regime_type_I(final_dataset)
final_dataset <- regime_type_II(final_dataset)

final_dataset$regime_type_II <- as.factor(final_dataset$regime_type_II)
final_dataset$dominant_resource_II <- as.factor(final_dataset$dominant_resource_II)

# dataset with lagged variables
final_dataset_lag <- filter(.data = final_dataset, !(Year %in% c(1996, 1998, 2000)))
final_dataset_lag <- pdata.frame(final_dataset_lag, index = c("Country", "Year"), row.names = TRUE)
final_dataset_lag$lag_tnrr_1 <- stats::lag(final_dataset_lag[,11], k = 1)
final_dataset_lag$lag_tnrr_2 <- stats::lag(final_dataset_lag[,11], k = 2)
final_dataset_lag$lag_tnrr_3 <- stats::lag(final_dataset_lag[,11], k = 3)
final_dataset_lag$lag_tnrr_4 <- stats::lag(final_dataset_lag[,11], k = 4)


# forming datasets for analysis
data_overall <- pdata.frame(final_dataset, index = c("Country", "Year"), row.names = TRUE)
data_overall$other_source_rent <- data_overall$Total_natural_resources_rent - data_overall$Mineral_rents - data_overall$Natural_gas_rents - data_overall$Oil_rents
data_dep_only <- pdata.frame(dep_count_data, index = c("Country", "Year"), row.names = TRUE)
democracies <- filter(.data = final_dataset, regime_type_II == "Democratic")
authoritarian <- filter(.data = final_dataset, regime_type_II == "Authoritarian")
moderate <- filter(.data = final_dataset, regime_type_II == "Moderate")

parts_of_the_world = c(c("Albania", "Europe"), c("Algeria", "Africa"), c("Angola", "Africa"), c("Argentina", "South America"),
                       c("Armenia", "Europe"), c("Australia", 'Oceania'), c("Austria", "Europe"), c("Azerbaijan", "Europe"),
                       c("Bahrain", 'Asia'), c("Bangladesh", "Asia"), c("Belarus", "Europe"))

# New day - new me
model_0 = plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
model_polity = plm(Stability ~ Total_natural_resources_rent*polity2 + GDP_Growth + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
model_1 = lm(Stability ~ Total_natural_resources_rent*regime_type_II + GDP_Growth + GDP_per_capita + log(Population_total) + factor(Country), data = data_overall)
model_2 = plm(Stability ~ Mineral_rents + Natural_gas_rents + Oil_rents + other_source_rent + GDP_Growth + GDP_per_capita + log(Population_total), data = data_overall, model = "within")


model_1 = plm(Stability ~ Total_natural_resources_rent*regime_type + GDP_Growth + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
model_2 = plm(Stability ~ Total_natural_resources_rent*dominant_resource + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
model_3 = plm(Stability ~ Total_natural_resources_rent*dominant_resource + Total_natural_resources_rent*regime_type_II + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
model_4 = plm(Stability ~ Total_natural_resources_rent*dominant_resource*regime_type_II + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "within")

# me of trr*democratic and trr*moderate 
model1_me = c(model_1$coefficients[2], model_1$coefficients[2]+model_1$coefficients[149], model_1$coefficients[2]+model_1$coefficients[150])
se = sqrt(c(vcov(model_1)[2,2], vcov(model_1)[2,2] + vcov(model_1)[149,149] + vcov(model_1)[2,149], vcov(model_1)[2,2] + vcov(model_1)[150,150] + vcov(model_1)[2,150]))
plot_data = data.frame(me = model1_me, lb = model1_me - qnorm(0.975)*se, ul = model1_me + qnorm(0.975)*se, regime_type = c("Authoritarian", "Democratic", "Moderate"))


p<- ggplot(plot_data, aes(x=regime_type, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ul), width=.2,
                      position=position_dodge(0.9)) + theme_minimal() + xlab("Regime Type") + ylab("ME(TRR)")
print(p)

# me of trr with polity interaction
model_polity_me = model_polity$coefficients[1] + unique(data_overall$polity2)*model_polity$coefficients[6]
se = sqrt(model_polity$vcov[1,1] + (unique(data_overall$polity2)^2)*model_polity$vcov[6,6] + 2*unique(data_overall$polity2)*model_polity$vcov[1,6])
plot_data = data.frame(me = model1_me, lb = model1_me - qnorm(0.975)*se, ul = model1_me + qnorm(0.975)*se, polity_values = unique(data_overall$polity2))


p<- ggplot(plot_data, aes(x=polity_values, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ul), width=.2,
                      position=position_dodge(0.9)) + theme_minimal() + xlab("Polity values") + ylab("ME(TRR)")
print(p)



# A try to draw me for regime type
model1_me = c(model_1$coefficients[1], model_1$coefficients[1] + model_1$coefficients[6], model_1$coefficients[1] + model_1$coefficients[7])
se = sqrt(c(model_1$vcov[1,1], model_1$vcov[1,1] + model_1$vcov[6,6] + 2*(model_1$vcov[1,6]), model_1$vcov[1,1] + model_1$vcov[7,7] + 2*(model_1$vcov[1,7])))
plot_data = data.frame(me = model1_me, lb = model1_me - qnorm(0.975)*se, ul = model1_me + qnorm(0.975)*se, regime_type = c("Authoritarian", "Democratic", "Moderate"))


p<- ggplot(plot_data, aes(x=regime_type, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ul), width=.2,
                      position=position_dodge(0.9)) + theme_minimal()
print(p)

# A try to draw me for dominant resource type
model2_me = c(model_2$coefficients[1], model_2$coefficients[1] + model_2$coefficients[6], model_2$coefficients[1] + model_2$coefficients[7])
se = sqrt(c(model_2$vcov[1,1], model_2$vcov[1,1] + model_2$vcov[6,6] + 2*(model_2$vcov[1,6]), model_2$vcov[1,1] + model_2$vcov[7,7] + 2*(model_2$vcov[1,7])))
plot_data = data.frame(me = model2_me, lb = model2_me - qnorm(0.975)*se, ub = model2_me + qnorm(0.975)*se, regime_type = c("Minerals", "Natural Gas", "Oil"))

p<- ggplot(plot_data, aes(x=regime_type, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                      position=position_dodge(.9)) + theme_minimal()
print(p)

# A try to draw me for fourth model
model4_me = c(model_4$coefficients[1], model_4$coefficients[1] + model_4$coefficients[6], model_4$coefficients[1] + model_4$coefficients[7], 
              model_4$coefficients[1] + model_4$coefficients[8], model_4$coefficients[1] + model_4$coefficients[9], 
              model_4$coefficients[1] + model_4$coefficients[6] + model_4$coefficients[8] + model_4$coefficients[10],
              model_4$coefficients[1] + model_4$coefficients[7] + model_4$coefficients[8] + model_4$coefficients[11],
              model_4$coefficients[1] + model_4$coefficients[6] + model_4$coefficients[9] + model_4$coefficients[12],
              model_4$coefficients[1] + model_4$coefficients[7] + model_4$coefficients[9] + model_4$coefficients[13])
se = c(model_4$vcov[1,1],
            model_4$vcov[1,1] + model_4$vcov[6,6] + 2*model_4$vcov[1,6],
            model_4$vcov[1,1] + model_4$vcov[7,7] + 2*model_4$vcov[1,7],
            model_4$vcov[1,1] + model_4$vcov[8,8] + 2*model_4$vcov[1,8],
            model_4$vcov[1,1] + model_4$vcov[9,9] + 2*model_4$vcov[1,9],
            model_4$vcov[1,1] + model_4$vcov[6,6] + model_4$vcov[8,8] + model_4$vcov[10,10] + 2*(model_4$vcov[1,6] + model_4$vcov[1,8] + model_4$vcov[1,10] + model_4$vcov[6,8] + model_4$vcov[6,10] + model_4$vcov[8,10]),
            model_4$vcov[1,1] + model_4$vcov[7,7] + model_4$vcov[8,8] + model_4$vcov[11,11] + 2*(model_4$vcov[1,7] + model_4$vcov[1,8] + model_4$vcov[1,11] + model_4$vcov[7,8] + model_4$vcov[7,11] + model_4$vcov[8,11]),
            model_4$vcov[1,1] + model_4$vcov[6,6] + model_4$vcov[9,9] + model_4$vcov[12,12] + 2*(model_4$vcov[1,6] + model_4$vcov[1,9] + model_4$vcov[1,12] + model_4$vcov[6,9] + model_4$vcov[6,12] + model_4$vcov[9,12]),
            model_4$vcov[1,1] + model_4$vcov[7,7] + model_4$vcov[9,9] + model_4$vcov[13,13] + 2*(model_4$vcov[1,7] + model_4$vcov[1,9] + model_4$vcov[1,13] + model_4$vcov[7,9] + model_4$vcov[7,13] + model_4$vcov[9,13]))



plot_data = data.frame(me = model4_me, lb = model4_me - qnorm(0.975)*se, ub = model4_me + qnorm(0.975)*se, groups = c("A-M", "A-G", "A-O", "D-M", "M-M", 'D-G', "D-O", "M-G", "M-O"))

p<- ggplot(plot_data, aes(x=groups, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                      position=position_dodge(.9)) + theme_minimal()
print(p)

#without M-G
plot_data <- plot_data[-8,]
p<- ggplot(plot_data, aes(x=groups, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                      position=position_dodge(.9)) + theme_minimal()
print(p)

#without M-G
plot_data <- plot_data[-6,]
p<- ggplot(plot_data, aes(x=groups, y=me)) + 
        geom_point() +
        geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                      position=position_dodge(.9)) + theme_minimal()
print(p)


#modeling the data
overall_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "within")
dep_count_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + GDP_per_capita  + log(Population_total), data = data_dep_only, model = "within")
demo_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = democracies, model = "within", index = c("Country", "Year"))
auth_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = authoritarian, model = "within", index = c("Country", "Year"))
mod_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = moderate, model = "within", index = c("Country", "Year"))
big_change_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = big_change_data, model = "within", index = c("Country", "Year"))
gas_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = gas_count_data, model = "within", index = c("Country", "Year"))
oil_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = oil_count_data, model = "within", index = c("Country", "Year"))
min_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total), data = min_count_data, model = "within", index = c("Country", "Year"))
pooled_model <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita + log(Population_total), data = data_overall, model = "pool")


lag_model_1 <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total) + lag_tnrr_1, data = final_dataset_lag, model = "within", index = c("Country", "Year"))
lag_model_2 <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total) + lag_tnrr_1 + lag_tnrr_2, data = final_dataset_lag, model = "within", index = c("Country", "Year"))
lag_model_3 <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total) + lag_tnrr_1 + lag_tnrr_2 + lag_tnrr_3, data = final_dataset_lag, model = "within", index = c("Country", "Year"))
lag_model_4 <- plm(Stability ~ Total_natural_resources_rent + GDP_Growth + polity2 + GDP_per_capita  + log(Population_total) + lag_tnrr_1 + lag_tnrr_2 + lag_tnrr_3 + lag_tnrr_4, data = final_dataset_lag, model = "within", index = c("Country", "Year"))



test_data_frame <- read.csv("finite_data_copy.csv")
uik <- test_data_frame[(test_data_frame[,10] >= 2016) & (test_data_frame[,14] == "regional"),c(8, 9, 10)]
uik[1,]
write.csv(uik, file = "uik_nums_regional.csv", fileEncoding = "UTF-8")
help("write.csv")

write.csv(final_dataset,"rent_data.csv", row.names = FALSE)
