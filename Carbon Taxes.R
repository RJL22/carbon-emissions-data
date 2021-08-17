library(tidyverse)
library(dbplyr)
library(haven)
library(Matrix)
library(lfe)
library(SDMTools)
library(broom)

# import data
Data <- read.csv("C:/Users/rache/Downloads/Data Spreadsheet - Comprehensive Data Set (ALL COUNTRIES) (2).csv")
View(Data)

# cleaning + formatting data
Data <- subset(Data, select = -c(NULL_VAR_Difference.in.Emissions, 
                                 NULL_VAR_Percentage.change.since.1990, 
                                 NULL_VAR_GDP...change))
typeof(Data$Population)
Population <- as.numeric(Data$Population)
Population.Change <- as.numeric(Data$Population.Change)

View(Data)

Sweden <- subset(Data, Country == "Sweden")
#View(Sweden)
Norway <- subset(Data, Country == "Norway")
#View(Norway)
China <- subset(Data, Country == "China")
#View(China)
SouthKorea <- subset(Data, Country == "South Korea (Korea Rep.)")
#View(SouthKorea)

# Finding correlation coefficients
cor(Data$X..Difference.in.Emissions, 
    Data$Tax.status, use = "complete.obs")
# -0.11955
cor(Data$X..Difference.in.Emissions, 
    Data$Population, use = "complete.obs")
cor(Data$X..Difference.in.Emissions, 
    Data$Population.Change, use = "complete.obs")
cor(Data$X..Difference.in.Emissions, 
    Data$Nominal.GDP..USD., use = "complete.obs")
cor(Data$X..Difference.in.Emissions, 
    Data$GDP...change, use = "complete.obs")
# 0.0990341
cor(Data$X..Difference.in.Emissions, 
    Data$GDP.per.capita, use = "complete.obs")
cor(Data$X..Difference.in.Emissions, 
    Data$Urban.pop.....total.pop., use = "complete.obs")
# -0.1542478

# Presence of Carbon Tax vs. Percent Change Carbon Emissions Linear Regression
# ALL
lmod <- lm(X..Difference.in.Emissions ~ Tax.status, Data)
summary(lmod)

ggplot(data = Data)+ geom_point(aes(x = Tax.status,
                                    y= X..Difference.in.Emissions), color = "red") #+ geom_smooth(aes(x = X..Difference.in.Emissions, 
                                                                  #                  y = Tax.status), method = "lm", se = F)
# greater range of difference in emissions for countries with carbon tax
# than countries without carbon taxes

ggplot(Data, mapping = aes(x = Country, y = X..Difference.in.Emissions, fill = Country)) + geom_boxplot()


# 2-Way Frequency Table
attach(Data)
mytable <- table(Tax.status, X..Difference.in.Emissions) # A will be rows, B will be columns
mytable # print table

# 3-Way Frequency Table
mytable <- xtabs(X..Difference.in.Emissions ~ Tax.status, data = Data)
ftable(mytable) # print table


summary(mytable) # chi-square test of independence


margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages




# Each country
lmod_Sweden <- lm(X..Difference.in.Emissions ~ Tax.status, Sweden)
summary(lmod_Sweden)

lmod_Norway <- lm(X..Difference.in.Emissions ~ Tax.status, Norway)
summary(lmod_Norway)

lmod_China <- lm(X..Difference.in.Emissions ~ Tax.status, China)
summary(lmod_China)

lmod_SKorea <- lm(X..Difference.in.Emissions ~ Tax.status, SouthKorea)
summary(lmod_SKorea)

# Highly significant for countries that do not have carbon tax. Not for 
# countries that already have carbon taxes


# Difference-in-differences estimate
panel <- Data %>%
    mutate(country = as_factor(Country), 
           year = as.integer(Year)) %>%
    mutate(is_post = (year >= 1994), 
           treated_group = (country %in% c("Sweden", "Norway")), 
           treat = is_post*treated_group)

# Descriptive stats
count(panel, is_post, treated_group, treat)

# Graphical summary
ggplot(panel, aes(x = year, y = Emissions..tonnes., group = country, color = country)) + 
    geom_line() +
    geom_point() +
    scale_color_viridis_d()
    geom_vline(xintercept = 1990, linetype = "dashed")


# 2nd version spreadsheet  
library(readxl)
ss2 <- read_excel("C:/Users/rache/Downloads/Data Spreadsheet (2).xlsx", 
                        +     col_types = c("text", "numeric", "text", 
                                            +         "text", "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric"))
View(ss2)

# DID
panel_ss <- ss2 %>%
    mutate(country = as_factor(Country), 
           year = as.integer(Year)) %>%
    mutate(is_post = (year >= 1994), 
           treated_group = (country %in% c("Sweden", "Norway")), 
           treat = is_post*treated_group) %>%
    mutate(Emissions = Emissions/1e7)

count(panel_ss, is_post, treated_group, treat)


ggplot(panel_ss, aes(x = year, y = Emissions..tonnes., group = country, color = country)) + 
    geom_line() +
    geom_point() +
    scale_color_viridis_d()
geom_vline(xintercept = 1990, linetype = "dashed")

# DID coefficients
felm(Emissions ~ treat, panel_ss) %>% tidy()
felm(Emissions ~ treat | is_post + treated_group, panel_ss) %>% tidy()
felm(Emissions ~ treat | year + country| 0 | country, panel_ss) %>% tidy()

# ss3
ss3 <- read_excel("C:/Users/rache/Downloads/Data Spreadsheet (2).xlsx", 
                  +     col_types = c("text", "numeric", "text", 
                                      +         "text", "numeric", "numeric", "numeric", 
                                      +         "numeric", "numeric", "numeric", 
                                      +         "numeric", "numeric", "numeric", 
                                      +         "numeric", "numeric", "numeric", 
                                      +         "numeric", "numeric", "numeric", 
                                      +         "numeric", "numeric"))
View(ss3)
panel_ss1 <- ss3 %>%
    mutate(country = as_factor(Country), 
           year = as.integer(Year)) %>%
    mutate(is_post = (year >= 1994), 
           treated_group = (country %in% c("Norway", "Sweden")), 
           treat = is_post*treated_group) %>%
    mutate(Emissions = Emissions/1e7)

count(panel_ss1, is_post, treated_group, treat)


ggplot(panel_ss1, aes(x = year, y = Emissions, group = country, color = country)) + 
    geom_line() +
    geom_point() +
    scale_color_viridis_d()
geom_vline(xintercept = 1990, linetype = "dashed")

# DID coefficients
felm(Emissions ~ treat, panel_ss1) %>% tidy()
felm(Emissions ~ treat | is_post + treated_group, panel_ss1) %>% tidy()
felm(Emissions ~ treat | year + country| 0 | country, panel_ss1) %>% tidy()

felm(Emissions ~ treat + is_post + treated_group + year + country + GDPpercapita + Urbanpop + EmissionsperGDP, panel_ss1) %>% tidy()





# linear regression model
sweden <- subset(ss3, Country == "Sweden")
View(sweden)
lmods <- lm(Emissions ~ Swedencarbontax, sweden)
summary(lmods)

china <- subset(ss3, Country == "China")
skorea <- subset(ss3, Country == "South Korea (Korea Rep.)")

control_group <- merge(china, skorea, by="Year")
View(control_group)

urbanpop <- control_group$Urbanpop.x + control_group$Urbanpop.y
pop <- control_group$Population.x + control_group$ Population.y
capita <- control_group$GDPpercapita.x + control_group$GDPpercapita.y
GDP <- control_group$NominalGDP.x + control_group$NominalGDP.y
energy <- control_group$EnergyIntensity.x + control_group$EnergyIntensity.y
popchange <- control_group$PopulationChange.x + control_group$PopulationChange.y
emissionchange <- control_group$DifferenceinEmissions.x + control_group$DifferenceinEmissions.y
percentemissionchange <- control_group$PercentDifferenceinEmissions.x + control_group$PercentDifferenceinEmissions.y
GDPchange <- control_group$GDPpercentchange.x + control_group$GDPpercentchange.y
emissionsperGDP <- control_group$EmissionsperGDP.x + control_group$EmissionsperGDP.y

lmodcontrol <- lm(Emissions.x + Emissions.y ~ urbanpop + pop + capita + GDP + energy + popchange + 
                      emissionchange + percentemissionchange + GDPchange + emissionsperGDP, control_group)
summary(lmodcontrol)

# number 4 BAby!
norway <- read_excel("C:/Users/rache/Downloads/Data Spreadsheet (3) 1.xlsx", 
                     +     sheet = "norway", col_types = c("text", 
                                                           +         "numeric", "text", "text", "numeric", 
                                                           +         "numeric", "numeric", "numeric", 
                                                           +         "numeric", "numeric", "numeric", 
                                                           +         "numeric", "numeric", "numeric", 
                                                           +         "numeric", "numeric", "numeric", 
                                                           +         "numeric", "numeric", "numeric", 
                                                           +         "numeric"))
View(norway)
lmodn <- lm(Emissions ~ Norwaycarbontax, norway)
summary(lmodn)
lmodn2 <- lm(Emissions ~ Norwaycarbontax2, norway)
summary(lmodn2)


# Aneri's synthetic control code
Book1...Book1 <- read.csv("C:/Users/rache/Downloads/Book1 - Book1.csv")
View(Book1...Book1)


dataprep.out<-
    dataprep(
        foo = synthetic_control_data,
        predictors = c("co2_per_capita", "co2_per_gdp", "co2_per_unit_energy", "gdp", "energy_per_gdp"),
        predictors.op = "mean",
        dependent = "co2_per_gdp",
        unit.variable = "Number",
        time.variable = "year",
        treatment.identifier = "Sweden (t)",
        controls.identifier = c(101:123),
        time.predictors.prior = c(1968:1990),
        time.optimize.ssr = (1968:2017),
        time.plot = 1968:2017,
        
    )

sweden <- subset(Book1...Book1, country=="Sweden")
sweden_pretax <- subset(sweden, year <= 1991)
post_sweden <- subset(sweden, year > 1991)
plot(co2 ~ year, sweden_pretax)

Y1 <- post_sweden$co2
Y0 <- Y1 * 28

x <- seq(from = 1, to = 56, by = 1)
xm <- 1
sweden$synth_result <- sum(((x*sweden$co2+38)
                            -xm)^2)*Y0*Y1
synth_result
plot(synth_result ~ year, sweden)
