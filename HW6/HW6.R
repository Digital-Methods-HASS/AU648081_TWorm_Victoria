install.packages("gapminder")
library(gapminder)
head(gapminder)

#task 1
#Define a defensive function that calculates the Gross Domestic Product of a 
#nation from the data available in the gapminder dataset. You can use the 
#population and GDPpercapita columns for it. Using that function, calculate the 
#GDP of Denmark in the following years: 1967, 1977, 1987, 1997, 2007, and 2017.

#I create a function which takes three parameters. The if sentences are used to 
#check for the data entries year and country. Then I calculate the GDP by
#multiplying. Afterwards, I use the cbind function to add a column with GDP
#to the dataset. 

GDP_Y_C <- function(dat, year=NULL, country=NULL){
  if(!is.null(year)){
    dat <- dat[dat$year %in% year,]
  }
  if (!is.null(country)){
    dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  new <- cbind(dat, gdp=gdp)
  return(new)
}

#I print this to check what the data looks like for Denmark.
GDP_Y_C(gapminder, country="Denmark")

#Then I print these to calculate each year. It could probably have been done
#smarter, but this also works.
GDP_Y_C(gapminder, year=1967, country="Denmark")
GDP_Y_C(gapminder, year=1977, country="Denmark")
GDP_Y_C(gapminder, year=1987, country="Denmark")
GDP_Y_C(gapminder, year=1997, country="Denmark")
GDP_Y_C(gapminder, year=2007, country="Denmark")
GDP_Y_C(gapminder, year=2017, country="Denmark")

#Answers for task 1 in the format "year: calculated gdp"
1967: 77116977700
1977: 103920280028
1987: 128771236166
1997: 157476118456
2007: 192906627081
2017: NULL

#task 2
#Write a script that loops over each country in the gapminder dataset, 
#tests whether the country starts with a ‘B’ , and prints out whether the life 
#expectancy is smaller than 50, between 50 and 70, or greater than 70. 
#(Hint: remember the grepl function, and review the Control Flow tutorial)

#I create values for low and high expectancy
low <- 50 
high <- 70
 
#I use the grepl function to search for countries starting with a capital B. 
#Afterwards, we loop over all the values to find the ones with B, which we then
#store in this_country. Then a variable with the average life expectancy.
#Then I test the different high and low values against each other using if
#statements. To make it look nicer when printed, extra words are added.

countryB <- grep("^B", unique(gapminder$country), value = TRUE)
for (country in countryB) { 
  this_country <- gapminder[gapminder$country == country, ] 
  avg_exp <- mean(this_country$lifeExp, na.rm = TRUE)
  
  if(avg_exp < low) {
    cat("Average life expectancy in", country, "is less than", low, "\n") 
  } else if(avg_exp > low && avg_exp < high) {
    cat("Average life expectancy in", country, "is between", low, "and", high, "\n")
  } else {
    cat("Average life expectancy in", country, "is greater than", high, "\n")
  }
  rm(avg_exp)
} 

#Answers for task 2 as they are printed in the console
Average life expectancy in Bahrain is between 50 and 70 
Average life expectancy in Bangladesh is less than 50 
Average life expectancy in Belgium is greater than 70 
Average life expectancy in Benin is less than 50 
Average life expectancy in Bolivia is between 50 and 70 
Average life expectancy in Bosnia and Herzegovina is between 50 and 70 
Average life expectancy in Botswana is between 50 and 70 
Average life expectancy in Brazil is between 50 and 70 
Average life expectancy in Bulgaria is between 50 and 70 
Average life expectancy in Burkina Faso is less than 50 
Average life expectancy in Burundi is less than 50 
