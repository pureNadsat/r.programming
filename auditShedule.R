
# program that is in compliance with random audit policy:
# ensures employees/cash entities are scheduled so that no two consecutive audits will be conducted
# on the same month, week, week day, calander day, around the same day, or the last week 
# of the last month of a quarter to give the appearance of "randomness".
# also ensures that no two employees will be audited on the same day.
# constructs monthly audit of vault as well.
# if it was up to me, audits would be completely random.

# function call to screw around with 
audit = function() {
    
    # packages for date and string manipulation
    suppressWarnings(suppressMessages(library(chron)))
    suppressWarnings(suppressMessages(library(Hmisc)))
    suppressWarnings(suppressMessages(library(stringr)))
    
    # presents user message and stores string
    input = function(string) {
        c(capitalize(readline(
            cat(strwrap(string)
                ))))
    }
    
    # prompts that requires a user to input employee/entity names
    name = strsplit(input('Hello! Thank you for using Audit Scheduler. Please enter the names of your employees/entities
                  and press [enter].'), ' ' )
    cat('\014')

    name = sapply(name, capitalize)
    
    # checks to see if the vault is present in the name vector
    if ( 'Vault' %in% name ) {
        name = name[name != 'Vault']
        vault = TRUE
    } else {
        vault = FALSE
    }
    
    # asks for audit year
    year = input('Please enter the year you would like to schedule your audit.')
    cat('\014')
      
    # vectors of calender dates and holidays according to year
    dts = seq.dates(paste('1/01/', year, sep = ''), paste('12/31/', year, sep = ''))
    
    holidays = c(dates(paste('01/01/', year, sep = '')),                             # New Year's
                 dates(paste('07/04/', year, sep = '')),                             # Independence Day
                 dates(paste('11/11/', year, sep = '')),                             # Veteran's Day
                 dates(paste('12/25/', year, sep = '')),                             # Christmas
                 tail(dts[months(dts) == 'Jan' & weekdays(dts) == 'Mon'], n = 2)[1], # MLK Day
                 tail(dts[months(dts) == 'May' & weekdays(dts) == 'Mon'], n = 1),    # Memorial Day
                 head(dts[months(dts) == 'Sep' & weekdays(dts) == 'Mon'], n = 1),    # Labor Day
                 tail(dts[months(dts) == 'Nov' & weekdays(dts) == 'Thu'], n = 1)     # Thanksgiving
                 )    
    
    # trim Sundays and federal holidays from vector of dates          
    workDts = dts[!(weekdays(dts) == 'Sun' | dts %in% holidays)]
    
    # extracts the calander day of a date
    dayNo = function(date) {
        as.numeric(format(as.Date(date, '%m/%d/%Y'), '%d'))
        }
    
    # range of dates function
    dateRange = function(date) {
        dateR = ifelse(dayNo(date) %in% c(1:9), 1,
                       ifelse(dayNo(date) %in% c(10:19), 2,
                              ifelse(dayNo(date) %in% c(20:29), 3, 4)
                              )
                       )
    }
    
    # function that calculates the week number of the month 
    # given the first week day of the month from selected date
    weekNo = function(date) {
        mon = dates(dts[months(dts) == months(date)])
        end = ifelse(weekdays(mon[1]) == 'Sun', 7,
                    ifelse(weekdays(mon[1]) == 'Mon', 6,
                    ifelse(weekdays(mon[1]) == 'Tue', 5,
                    ifelse(weekdays(mon[1]) == 'Wed', 4,
                    ifelse(weekdays(mon[1]) == 'Thu', 3,
                    ifelse(weekdays(mon[1]) == 'Fri', 2, 1)
                    )))))
           
            week1 = mon[1:end]
            week2 = tail(week1, n = 1) + 1:7
            week3 = tail(week2, n = 1) + 1:7
            week4 = tail(week3, n = 1) + 1:7
            week5 = mon[(22+length(week1)):(28+length(week1))]
            
            ifelse(dates(date) %in% week1, 1,
                  ifelse(dates(date) %in% week2, 2,
                  ifelse(dates(date) %in% week3, 3,
                  ifelse(dates(date) %in% week4, 4,
                  ifelse(dates(date) %in% week5, 5, 6)
                  ))))    
        }
    
    # function that supplies the number of the month for a quarter given a date
    monthNo = function(date) {
        month = as.numeric(format(as.Date(date, '%m/%d/%Y'), '%m')
                           )
        ifelse(month %in% c(1, 4, 7, 10), 1,
               ifelse(month %in% c(2, 5, 8 , 11), 2, 3)
               )
    }
    
    # function to be used for monthly vault schedule; extracts the number of the month as a numeric element
    trueMonth = function(date) {
        sMonth = as.numeric(format(as.Date(date, '%m/%d/%Y'), '%m')
        )
    }
    
    # data frame of working days to sample
    df = data.frame(dts, 
                    day = dayNo(dts),
                    weekDay = weekdays(dts),
                    week = sapply(dts, weekNo),
                    month = monthNo(dts), 
                    quart = quarters(dts)
                    )
    
    # exclude Sundays, federal holidays, 
    # and the last week of the last month of the quarter from data frame
    df = df[dts %in% workDts,]
    df = df[!(df$month == 3 & df$week == 5),]
    
    # determines if they are employees or entities being scheduled;
    # skips employee algorithm if only vault is present in name vector
    if ( length(name) > 0 ) {
    
    # split into quarters
    firstQ = df[df$quart == '1Q',]
    secondQ = df[df$quart == '2Q',]
    thirdQ = df[df$quart == '3Q',]
    forthQ = df[df$quart == '4Q',]

    # psuedo-random sampling algorithm using pseudo-random sampling function
    auditSchedule = data.frame()
    for ( i in 1:length(name) ) {
        
        if ( i == 1 ) {
        
            first = firstQ[sample(nrow(firstQ), 1
                              ),]
        
            sD = secondQ[!(secondQ$weekDay == first$weekDay
                     | secondQ$week == first$week 
                     | secondQ$month == first$month
                     | dateRange(secondQ$dts) == dateRange(first$dts)
                       ),]
    
            second = sD[sample(nrow(sD), 1
                           ),]
        
            thD = thirdQ[!(thirdQ$day == first$day
                     | dateRange(thirdQ$dts) == dateRange(second$dts)
                     | thirdQ$weekDay == first$weekDay
                     | thirdQ$weekDay == second$weekDay
                     | thirdQ$week %in% c( first$week, second$week )
                     | thirdQ$month %in% c( first$month, second$month )
                       ),]
        
            third = thD[sample(nrow(thD), 1
                           ),]
        
            forthD = forthQ[!(dateRange(forthQ$dts) == dateRange(third$dts)
                        | forthQ$day %in% c( first$day, second$day, third$day )
                        | forthQ$weekDay == first$weekDay
                        | forthQ$weekDay == second$weekDay
                        | forthQ$weekDay == third$weekDay
                        | forthQ$week == third$week
                    ),]
        
            forth = forthD[sample(nrow(forthD), 1
                ),]

        } else if ( i %% 2 == 0 ) {
            
                fD = firstQ[!(firstQ$day %in% auditSchedule[1:i, 3]
                            | firstQ$week == first$week 
                            | firstQ$weekDay == first$weekDay
                            | firstQ$month == first$month
                            ),]
            
                first = fD[sample(nrow(fD), 1
                        ),]
            
                sD = secondQ[!(secondQ$day %in% auditSchedule[1:i, 9] 
                            | secondQ$day == first$day
                            | secondQ$week %in% c( first$week, second$week )
                            | secondQ$weekDay == first$weekDay
                            | secondQ$weekDay == second$weekDay
                            | secondQ$month %in% c( first$month, second$month )
                       ),]
    
                second = sD[sample(nrow(sD), 1
                            ),]
        
            thD = thirdQ[!(thirdQ$day %in% auditSchedule[1:i, 15]
                        | dateRange(thirdQ$dts) == dateRange(second$dts)
                        | thirdQ$week %in% c( first$week, second$week )
                        | thirdQ$weekDay == first$weekDay
                        | thirdQ$weekDay == second$weekDay
                        | thirdQ$month %in% c( second$month, third$month )
                       ),]
        
            third = thD[sample(nrow(thD), 1
                           ),]
        
            forthD = forthQ[!(forthQ$day %in% auditSchedule[1:i, 21]
                        | forthQ$day == first$day
                        | dateRange(forthQ$dts) %in% dateRange(c(second$dts, third$dts))
                        | forthQ$weekDay == third$weekDay
                        | forthQ$month %in% c( third$month, forth$month )
                        ),]
        
            forth = forthD[sample(nrow(forthD), 1
                    ),]
        
        } else {
            
            fD = firstQ[!(firstQ$day %in% auditSchedule[1:i, 3]
                        | firstQ$week == first$week
                        | firstQ$month %in% auditSchedule[(i-2):(i-1), 6]
                            ),]
            
            first = fD[sample(nrow(fD), 1
                     ),]
            
            sD = secondQ[!(secondQ$month %in% auditSchedule[(i-2):(i-1), 12]
                         | secondQ$day %in% auditSchedule[1:i, 9] 
                         | dateRange(secondQ$dts) == dateRange(first$dts)
                         | secondQ$week == first$week 
                         | secondQ$weekDay == first$weekDay 
                       ),]
    
            second = sD[sample(nrow(sD), 1
                           ),]
        
            thD = thirdQ[!(thirdQ$day %in% auditSchedule[1:i, 15]
                         | thirdQ$day %in% c( first$day, second$day )
                         | dateRange(thirdQ$dts) == dateRange(second$dts)
                         | thirdQ$week %in% c( first$week, second$week )
                         | thirdQ$weekDay == second$weekDay
                         | thirdQ$month %in% c( first$month, second$month )
                       ),]
        
            third = thD[sample(nrow(thD), 1
                           ),]
        
            forthD = forthQ[!(forthQ$month %in% auditSchedule[(i-2):(i-1), 24]
                            | forthQ$day %in% auditSchedule[1:(i-1), 21]
                            | forthQ$day %in% c( first$day, second$day, third$day, forth$day )
                            | forthQ$weekDay == third$weekDay
        ),]
        
        forth = forthD[sample(nrow(forthD), 1
        ),]
            
        }
    
        # fills data frame with names of employees and dates
        auditSchedule = rbind(auditSchedule, cbind(name[i], first, second,
                              third, forth)
                              )       
    }
    
    # removes, renames, and reformats columns of df
    auditSchedule = auditSchedule[, !names(auditSchedule) %in% c('day', 'quart')]
    auditSchedule[,c(2, 6, 10, 14)] = sapply(auditSchedule[,c(2, 6, 10, 14)], 
                                             function(x) as.character(format(
                                             as.Date(x, '%m/%d/%Y'), '%m/%d')
                                             )
                                             )
                                                                
    names(auditSchedule)[1] = 'Name'
    names(auditSchedule)[c(2, 6, 10, 14)] = 'Date'
    names(auditSchedule)[c(2, 6, 10, 14)+1] = 'Day'
    names(auditSchedule)[c(2, 6, 10, 14)+2] = 'Week'
    names(auditSchedule)[c(2, 6, 10, 14)+3] = 'Month'
    
    } 
   
    
    
    # if vault is listed to be scheduled, create 'random' audit of non-repeating
    # calender days, weekdays, and weeks
    if ( isTRUE(vault) ) {

        vaultSchedule = data.frame()
        for ( i in 1:12 ) {
            if ( i == 1 ) {
                datesVault = df[sample(nrow(df[trueMonth(df$dts) == i,]) , 1
                ),]
            } else if (i < 4) { 
               possibleVault = df[trueMonth(df$dts) == i 
                                  & !(df$week %in% vaultSchedule[i-1, 4] 
                                  | df$day %in% vaultSchedule[1:(i-1), 2]
                                  | df$weekDay %in% vaultSchedule[i-1, 3]
                                  ),]
               datesVault = possibleVault[sample(nrow(possibleVault), 1
               ),]
                } else {
                    possibleVault = df[trueMonth(df$dts) == i 
                                       & !(df$week %in% vaultSchedule[(i-3):(i-1), 4] 
                                       | df$day %in% vaultSchedule[1:(i-1), 2]
                                       | df$weekDay %in% vaultSchedule[(i-3):(i-1), 3]
                                       ),]
                    datesVault = possibleVault[sample(nrow(possibleVault), 1 
                    ),]
            }
        vaultSchedule = rbind(vaultSchedule, datesVault)
        }
        
        vaultSchedule = vaultSchedule[, !names(vaultSchedule) %in% c('day', 'month', 'quart')]
        vaultSchedule[1] = sapply(vaultSchedule[1], 
                                  function(x) as.character(format(
                                  as.Date(x, '%m/%d/%Y'), '%m/%d'
                                  )
                                  )
                                  ) 
        
        names(vaultSchedule)[1] = 'Date'
        names(vaultSchedule)[2] = 'Day'
        names(vaultSchedule)[3] = 'Week'
    }
    
    ## prints schedule of employees/entities and gives the option to write to a csv file
    
    # prints schedule if there are employees and the vault entered
    if ( length(name) > 0 & isTRUE(vault) ) {
        
        message('Quarterly Audits')    
        print(auditSchedule, row.names = FALSE)
        message('Vault Audits')
        print(vaultSchedule, row.names = FALSE)
        
        csvFile = input('...Would you like to export the audit schedules to an Excel file (Y/N)?')
        cat('\014')
        
        if ( csvFile == 'Y' ) {
            write.csv(auditSchedule, file = paste('audit', year, '.csv', sep = ''), row.names = FALSE)
            write.csv(vaultSchedule, file = paste('auditVault', year, '.csv', sep = ''), row.names = FALSE)
        }
        
            # prints schedule if there are only employees entered
            } else if ( length(name) > 0 & !isTRUE(vault) ) {
       
                message('Quarterly Audits')
                print(auditSchedule, row.names = FALSE)
            
                csvFile = input('...Would you like to export the audit schedule to an Excel file (Y/N)?')
                cat('\014')
           
                if ( csvFile == 'Y' ) {
                    write.csv(auditSchedule, file = paste('audit', year, '.csv', sep = ''), row.names = FALSE)
                }
                # prints schedule if only the vault is entered 
                } else if ( length(name) == 0 & isTRUE(vault) ) {
                
                    message('Vault Audits')
                    print(vaultSchedule, row.names = FALSE)
                
                    csvFile = input('...Would you like to export the vault audit schedule to an Excel file (Y/N)?')
                    cat('\014')
                
                    if ( csvFile == 'Y' ) {
                        write.csv(vaultSchedule, file = paste('auditVault', year, '.csv', sep = ''), row.names = FALSE)
                }
            
        }

}
                
                
    
    
   
    