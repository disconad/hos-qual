## Takes two arguments, 2char state and outcome name.
## reads given csv file and returns a character vector with the hospital name with the 
## lowest 30-day mortality for the state and outcome specified.
## Hospital.Name = hospital name 
## Outcomes = 'heart attack', 'heart failure', 'pneumonia'
## Any hospital without data on the particular set is excluded.
## Ties are decided alphabetically.

best <- function(state, outcome) {
        ## Read outcome data
        dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (!state %in% dataset[,7]){
                stop("invalid state")       
        }
        valid_out <-c('heart attack','heart failure','pneumonia')
        if (!outcome %in% valid_out){
                stop("invalid outcome")  
        }
        ndata<-dataset$State == state     
        statedata<-dataset[ndata,]

        #mha<-min(statedata[,ha], na.rm = TRUE)
        #mhf<-min(statedata[,hf], na.rm = TRUE)
        #mpn<-min(statedata[,pn], na.rm = TRUE)
        
        if (outcome == 'heart attack'){
                statedata[,11]<-as.numeric(statedata[,11])
                y<-order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, statedata$Hospital.Name, na.last = NA)
                return(statedata$Hospital.Name[y[1]])
                   
                    }
        if (outcome == 'heart failure'){
                statedata[,17]<-as.numeric(statedata[,17])
                y<-order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, statedata$Hospital.Name)
                return(statedata$Hospital.Name[y[1]])
        }
        if (outcome == 'pneumonia'){
                statedata[,23]<-as.numeric(statedata[,23])
                y<-order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, statedata$Hospital.Name)
                return(statedata$Hospital.Name[y[1]])
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
}

