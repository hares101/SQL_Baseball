library("lattice")
library("ggplot2")



install.packages('RSQLite')
library('RSQLite')
db = dbConnect(SQLite(), dbname ='lahman2013.sqlite')
b = dbListTables(db)

#What years does the data cover? are there data for each of these years?
dbGetQuery(db, 'SELECT COUNT(DISTINCT(yearID)) FROM Batting;')

alltables = sapply(b, function(x) dbReadTable(db, x)) #list of all the tables
unlist(alltables)
range(unlist(sapply(alltables, function(x) (x$yearID)))) #find range of entire data
#find range of each table to see if it starts or ends later
unlist(sapply(alltables, function(x) range(x$yearID))) 

#Here we have the range of years for each table. The tables going from (-inf,inf) 
#do not have years in them. There is at least some data for each year going back to 1871, 
#but not all tables have each year.

#What team won the World Series in 2000?
dbGetQuery(db, "SELECT teamID FROM Teams WHERE WSwin = 'Y' and yearID = '2000';")

#Do you see a relationship between the number of games won in a season and 
#winning the World Series?

wins_ws = dbGetQuery(db, "SELECT W, WSwin FROM Teams")
wsbox = ggplot(wins_ws, aes(x = factor(WSWin), y=W, fill = (WSWin)))
wsbox + geom_boxplot(outlier.colour = 'blue') + xlab('World Series Winner/Loser')+ 
  ylab('Number of Wins')
#We see WS winners have a higher median, and get the idea that WS winners tend 
#to win more games per season. Non WS winners box is more spread out because there
#are 29 teams, and the minimum for WS loser is much lower than the minumum for WS winners.

#In 2003, what were the three highest salaries?#
salaries = dbGetQuery(db, "SELECT salary FROM Salaries WHERE yearID = 2003;")
unique_sal = unique(salaries)
sort(unique_sal[,'salary'], decreasing = TRUE)[1:3] #sort salaries, then get the top 3

#For 1999, compute the total payroll of each of the different teams. Next
#compute the team payrolls for all years in the database for which we have
#salary information. Display these in a plot.

payroll = dbGetQuery(db, "SELECT salary,teamID FROM Salaries WHERE yearID = 1999")
payroll
#get a table of the total payroll of 1999
sort(tapply(payroll[,'salary'],payroll[,'teamID'],sum))

#create a function to get salary of any year
payroll_year = function(year, data){
  query = "SELECT salary, teamID FROM Salaries WHERE yearID = "
  query = paste0(query, year, ';')
  team_pay = dbGetQuery(data, query)
  sort(tapply(team_pay[,'salary'],team_pay[,'teamID'], sum))
}

salary_years = dbGetQuery(db,"SELECT yearID, salary FROM Salaries")
x = split(salary_years, salary_years$yearID)

#call function on each year
allyears_pay = (sapply(x, function(y) payroll_year(y[1,1], db)))
data_salary = unlist(allyears_pay)
Salary = data_salary
datacol = names(Salary)
Team = gsub('[0-9]*\\.','',datacol)
Year = gsub('\\.[A-Z]*', '', datacol)
salarydata = data.frame(Salary, Team, Year)
head(salarydata)
rownames(salarydata) = NULL
salary_plot = ggplot(salarydata, aes(x = Year, y = Salary, group = Team))
salary_plot + geom_line(colour = 'blue', size = 1) + facet_wrap(~Team) +
  theme(axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = c(seq(1985,2013,5)), expand = c(.01,0)) + 
  ggtitle('Salary of All Teams On Year')

#We see a positive trend in salary increase, which could be due to inflation. However,
# some team salaries do increase more than others

#Study the change in salary over time. Have salaries kept up with inflation, 
#fallen behind, or grown faster?

cpidata = read.csv("SeriesReport-20141217012354.csv")
colnames(cpidata) #get names of CPI indez
CPI  = cpidata[11:39,"X.3"] #only get the CPI's of the years we want
CPI = as.numeric(levels(CPI))[CPI] #coerce them into a numeric vector
CPI
cpi_new = dbGetQuery(db, "SELECT yearID as Year, AVG(salary) as 
                     Salary FROM Salaries GROUP BY yearID") 
salary_cpi = cbind(CPI, cpi_new)
#create adjusted salary based on CPI formula
Adjusted_Salary = (salary_cpi$CPI[[29]] / salary_cpi$CPI) * (salary_cpi$Salary)
salary_adjustcpi = cbind(Adjusted_Salary, salary_cpi)
names(salary_adjustcpi)
inflationplot = ggplot(salary_adjustcpi, aes(x = Year))
inflationplot + geom_line(aes(y = Salary))  + geom_line(aes(y = Adjusted_Salary)) 

#We see on this plot that the inflation line for the most part has the same distance to the
#regular salary rate, meaning that salary was fairly constant with inflation. 
#However, at times in the early 90s and late 2000s that gap shrunk, 
#meaning that salaries were increasing faster than inflation.

#Has the distribution of home runs for players increased over the years?

homeruns = dbGetQuery(db, "SELECT HR,yearID,AB FROM Teams")
head(homeruns)
names(homeruns)
standardHR = homeruns$HR / homeruns$AB #standardize HR's over time
newHR = cbind(standardHR, homeruns) #put standardized HR's in dataframe

#plot shows us that homeruns have been increasing over time.
HRplot = ggplot(homeruns, aes(x = yearID, y = standardHR))
HRplot + geom_line() + xlab("Years") + ylab("Standardized Home Runs") + ggtitle("Homeruns Over Time")


#More Wins = More Attendance?
attend = dbGetQuery(db, "SELECT W,attendance,Rank,yearID,teamID FROM Teams")
newattend = (attend$attendance / (2013-1871))
range(attend$attendance, na.rm = TRUE)
ggplot(attend, aes(x = W, y = newattend)) + geom_point() + xlab('Number of Wins per Season') + 
  ylab('Average Attendance From 1871-2013') + ggtitle('Do People Like Watching Winning Teams?')

#We find that there is more attendance during winning seasons

