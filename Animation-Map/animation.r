library(Quandl)
# Not required but removes a warning message--- 
Quandl.auth('yourauthcode')
Quandl.search('violent crime')

vcData <- Quandl("FBI_UCR/USCRIME_TYPE_VIOLENTCRIMERATE")

save(vcData,file='vcData.rda')
load("vcData.rda")
# dim(vcData)
# head(vcData,2)
# tail(vcData, 2)

vcData$Yearonly = 2010:1960  # Creating a new column with Years
vcData = vcData[, -1]  # Dropping the existing 'Year' column
vcData = vcData[, -52]  # Dropping the column with average for the country
vcData = vcData[, -9]  # Dropping the column for Washington DC, the 9th column

# Rename the variables and put them in data.frame
years <- data.frame(year2010 = colMeans(vcData[1, ], na.rm = TRUE), 
                    year2009 = colMeans(vcData[2, ], na.rm = TRUE), 
                    year2008 = colMeans(vcData[3, ], na.rm = TRUE), 
                    year2007 = colMeans(vcData[4, ], na.rm = TRUE), 
                    year2006 = colMeans(vcData[5, ], na.rm = TRUE), 
                    year2005 = colMeans(vcData[6, ], na.rm = TRUE), 
                    year2004 = colMeans(vcData[7, ], na.rm = TRUE), 
                    year2003 = colMeans(vcData[8, ], na.rm = TRUE), 
                    year2002 = colMeans(vcData[9, ], na.rm = TRUE), 
                    year2001 = colMeans(vcData[10, ], na.rm = TRUE), 
                    year2000 = colMeans(vcData[11, ], na.rm = TRUE), 
                    year1999 = colMeans(vcData[12, ], na.rm = TRUE), 
                    year1998 = colMeans(vcData[13, ], na.rm = TRUE), 
                    year1997 = colMeans(vcData[14, ], na.rm = TRUE), 
                    year1996 = colMeans(vcData[15, ], na.rm = TRUE), 
                    year1995 = colMeans(vcData[16, ], na.rm = TRUE),
                    year1994 = colMeans(vcData[17, ], na.rm = TRUE),
                    year1993 = colMeans(vcData[18, ], na.rm = TRUE),
                    year1992 = colMeans(vcData[19, ], na.rm = TRUE),
                    year1991 = colMeans(vcData[20, ], na.rm = TRUE),
                    year1990 = colMeans(vcData[21, ], na.rm = TRUE),
                    year1989 = colMeans(vcData[22, ], na.rm = TRUE),
                    year1988 = colMeans(vcData[23, ], na.rm = TRUE),
                    year1987 = colMeans(vcData[24, ], na.rm = TRUE),
                    year1986 = colMeans(vcData[25, ], na.rm = TRUE),
                    year1985 = colMeans(vcData[26, ], na.rm = TRUE),
                    year1984 = colMeans(vcData[27, ], na.rm = TRUE),
                    year1983 = colMeans(vcData[28, ], na.rm = TRUE),
                    year1982 = colMeans(vcData[29, ], na.rm = TRUE),
                    year1981 = colMeans(vcData[30, ], na.rm = TRUE),
                    year1980 = colMeans(vcData[31, ], na.rm = TRUE),
                    year1979 = colMeans(vcData[32, ], na.rm = TRUE),
                    year1978 = colMeans(vcData[33, ], na.rm = TRUE),
                    year1977 = colMeans(vcData[34, ], na.rm = TRUE),
                    year1976 = colMeans(vcData[35, ], na.rm = TRUE),
                    year1975 = colMeans(vcData[36, ], na.rm = TRUE),
                    year1974 = colMeans(vcData[37, ], na.rm = TRUE),
                    year1973 = colMeans(vcData[38, ], na.rm = TRUE),
                    year1972 = colMeans(vcData[39, ], na.rm = TRUE),
                    year1971 = colMeans(vcData[40, ], na.rm = TRUE),
                    year1970 = colMeans(vcData[41, ], na.rm = TRUE),
                    year1969 = colMeans(vcData[42, ], na.rm = TRUE),
                    year1968 = colMeans(vcData[43, ], na.rm = TRUE),
                    year1967 = colMeans(vcData[44, ], na.rm = TRUE),
                    year1966 = colMeans(vcData[45, ], na.rm = TRUE),
                    year1965 = colMeans(vcData[46, ], na.rm = TRUE),
                    year1964 = colMeans(vcData[47, ], na.rm = TRUE),
                    year1963 = colMeans(vcData[48, ], na.rm = TRUE),
                    year1962 = colMeans(vcData[49, ], na.rm = TRUE),
                    year1961 = colMeans(vcData[50, ], na.rm = TRUE),
                    year1960 = colMeans(vcData[51, ], na.rm = TRUE))

years$State <- row.names(years)
years <- years[-51, ]  # Remove the row for Yearonly, which is irrelevant.


# check this awesome guy who create a pretty American map
# http://loloflargenumbers.com/blog/#.UxjjxsRDvmt
load("all_us.rda")

# Seems like a tradition to ignore District of Columbia
uslessdc <- all_us[all_us$STATE_NAME != "District of Columbia", ]
uslessdc$STATE_NAME <- factor(uslessdc$STATE_NAME)

# bring value of years to map data
# As you can see from here, match is an awesome function that acts just like join command in SQL
# Any Data that is divided by state in US can be used in this way to visualize the result
# Amazing huh?!
uslessdc$CrimeRate <- years[, 1][match(uslessdc$STATE_NAME, years$State)]

library(ggplot2)
library(animation)
saveHTML({
  for (i in 51:1) {
    # bring value of decade to map data
    uslessdc$CrimeRate <- years[, i][match(uslessdc$STATE_NAME, years$State)]  
    ggchoropleth <- ggplot(data = uslessdc, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = CrimeRate)) + 
      geom_polygon(color = "black") + 
      ggtitle(paste("Violent Crime Rate in", names(years[i]))) + 
      xlab("") + ylab("")  + 
      theme(legend.position = "top")
    print(ggchoropleth)
  }
}, img.name = "yearsplots", imgdir = "yearsplots", htmlfile = "yearsplots.html", 
outdir = getwd(), autobrowse = FALSE, ani.height = 400, ani.width = 600, 
verbose = FALSE, autoplay = TRUE, title = "Violent Crime Rates")



