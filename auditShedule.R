# Function that generates a random quarterly audit schedule
# audits cannot be conducted on the same month, week, calender day,
# weekday, or around the same calender day or weekday within two consecutive quarters
audit = function() {
    
    # date and string editor packages
    suppressWarnings(suppressMessages(library(chron)))
    suppressWarnings(suppressMessages(library(Hmisc)))
    suppressWarnings(suppressMessages(library(stringr)))
    
    # presents user message and stores string in a list
    input = function(string) {
        as.list(capitalize(readline(cat(strwrap(string)))))
    }
    
    # prompts that requires a user to input employee names and the year of the audit schedule
    name = input("Hello! Thank you for using Audit Scheduler. 
                  Please enter the name of your first employee
                  and press [enter].")
    cat("\014")
    
    others = "Y"
    while(others == "Y") {
        others = input("Are there more employees you would like to Schedule (Y/N)?")
        cat("\014")
        if(others != "Y" & others != "N") {
            others = input("Stop fucking around.
                            Are there more employees you would like to Schedule (Y/N)?")
            cat("\014")
        }
            if(others == "N") {
                next        
        }
        names = input("Please enter the name of the next employee you would like to schedule.")
        cat("\014")
        names = append(name, names)
        }
        names
    
}
    
    year = input("Please enter the year you would like to schedule your audit.")
    cat("\014")
      
    # Create work days to randomly sample 
    dts = seq.dates(paste("1/01/", year, sep = ""), paste("12/31/", year, sep = ""))
    holidays = c(paste("01/01", year, sep = ""),                                     # New Year's
                 paste("07/04", year, sep = ""),                                     # Independence Day
                 paste("12/25", year, sep = ""),                                     # Christmas
                 tail(dts[months(dts) == "Jan" & weekdays(dts) == "Mon"], n = 2)[1], # MLK Day
                 tail(dts[months(dts) == "May" & weekdays(dts) == "Mon"], n = 1),    # Memorial Day
                 head(dts[months(dts) == "Sep" & weekdays(dts) == "Mon"], n = 1),    # Labor Day
                 tail(dts[months(dts) == "Nov" & weekdays(dts) == "Thu"], n = 1))    # Thanksgiving
    
    # Trim Sundays and federal holidays from dates, and group dates into quarters            
    dates = dts[weekdays(dts) != "Sun" & dts != holidays]
    Q1 = dates[quarters(dates) == "1Q"]
    Q2 = dates[quarters(dates) == "2Q"]
    Q3 = dates[quarters(dates) == "3Q"]
    Q4 = dates[quarters(dates) == "4Q"]
         
        # Function that sequences the months within a quarter
        monthNo = function(quarter) {
            month = as.character(unique(months(quarter)))
            data.frame(month, no = c(1:3))
        }
        
        #Function that calculates the week number of the month given a date
        weekNo = function(day) {
            ceiling(as.numeric(format(as.Date(day, "%m/%d/%Y"), "%d")) / 7)
        }
        
    auditDates = data.frame()    
        for(i in 1:length(names)) {
            first = sample(Q1, 1)
            
            
            
    
    
    
    
    
    
    
    
    
}

