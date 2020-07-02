# Johns Hopkins COVID-19 data set

# Confirmed cases
urlfile = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# Deaths cases
# urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# Recovered cases
# urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

datos = read.csv(url(urlfile))

# Full base
todos = data.frame(datos[,-(1:4)])

# Countries with confirmed cases
paises = unique(datos$Country.Region)

# Adding by country with cases
X = matrix(numeric(0),length(paises),ncol(todos))
j = 0
for(i in paises){
  j = j+1
  y = (datos[datos$Country.Region==i,-(1:4)])
  d = dim(y)[1]
  if(d==1) X[j,] = as.numeric(y)
  if(d!=1) X[j,] = apply(y,2,sum)
}

# Variable publication date
# The maximum date must be changed to the updated one
fecha <- as.character(seq(as.Date("2020-01-22"), 
            as.Date("2020-06-13"), by = "day")) # change here "2020-MM-DD"

# Complete base, countries in column and date in rows
casos = data.frame(t(X))
colnames(casos) = paises
casos$Day = fecha
head(casos)

