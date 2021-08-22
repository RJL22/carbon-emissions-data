#causal impact
#change "NOR" to "SWE" for sweden
install.packages("CausalImpact")
library(CausalImpact)

df <- read.csv("Book1.csv")
df <- subset(synthetic_control_data, iso_code=="NOR")

yrs <- c(1968:2016)
time.points <- as.Date(ISOdate(yrs, 1, 1))

ibcl <- ts(df$co2_per_gdp)
#covariants: "co2_per_capita", "co2_per_unit_energy", "gdp", "energy_per_gdp", "population"
co2.gdp <- ts(df$co2_per_capita)
co2.energy <- ts(df$co2_per_unit_energy)
gdp <- ts(df$gdp)
energy.gdp <- ts(df$energy_per_gdp)
population <- ts(df$population)

data <- zoo(cbind(ibcl, co2.gdp, co2.energy, gdp, energy.gdp, population), time.points)
head(data)

pre.period <- as.Date(c("1988-01-01", "1990-01-01"))
post.period <- as.Date(c("1991-01-01", "2017-01-01"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact, "report")


#Chow Test
#https://ourworldindata.org/co2/country/sweden?country=SWE~NOR

#read data
Chow_Test_Data <- read.csv("Data Spreadsheet - Chow Test and Synthetic Data (1).csv")

#subset data
dummy_variable <- subset(Chow_Test_Data, year < 1990)
variable <- subset(Chow_Test_Data, year > 1989)

#sweden Chow Test
one.way <- aov(year ~ sweden_co2_per_gdp, data = Chow_Test_Data)
summary(one.way)

RSS1 <- aov(year ~ sweden_co2_per_gdp, data = dummy_variable)
summary(RSS1)

RSS2 <- aov(year ~ sweden_co2_per_gdp, data = variable)
summary(RSS2)

qf(.95, df1=3, df2=49)
install.packages("sjPlot")
library(sjPlot)
dist_f(deg.f1 = 3, deg.f2 = 49)

#Norway Chow Test
two.way <- aov(year ~ norway_co2_per_gdp, data = Chow_Test_Data)
summary(two.way)

RSS1N <- aov(year ~ norway_co2_per_gdp, data = dummy_variable)
summary(RSS1N)

RSS2N <- aov(year ~ norway_co2_per_gdp, data = variable)
summary(RSS2N)

qf(.95, df1=3, df2=49)
dist_f(deg.f1 = 3, deg.f2 = 49)

#China Chow Test
three.way <- aov(year ~ china_co2_per_gdp, data = Chow_Test_Data)
summary(three.way)

RSS1C<- aov(year ~ china_co2_per_gdp, data = dummy_variable)
summary(RSS1C)

RSS2C <- aov(year ~ china_co2_per_gdp, data = variable)
summary(RSS2C)

qf(.95, df1=3, df2=67)
dist_f(deg.f1 = 3, deg.f2 = 67)

#South Korea Chow Test
four.way <- aov(year ~ south_korea_co2_per_gdp_1, data = Chow_Test_Data)
summary(four.way)

RSS1SK<- aov(year ~ south_korea_co2_per_gdp_1, data = dummy_variable)
summary(RSS1SK)

RSS2SK <- aov(year ~ south_korea_co2_per_gdp_1, data = variable)
summary(RSS2SK)

qf(.95, df1=3, df2=67)
dist_f(deg.f1 = 3, deg.f2 = 67)