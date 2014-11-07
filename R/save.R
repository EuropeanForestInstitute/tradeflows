#' Save a data frame to the Excel format, with column names
#'
#' This function is not needed use xlsx::write.xlsx() directly
#' But developing the function below might be usefull
#' @param dtf data frame
#' @param filename name of the excel file
#' @param path folder where the file will be saved
#'
#' Todo: could use deparse(substitute(dtf)) to create a filename from the objects name
#' this might not be needed,
#' as the ?xlsx explains
#' write.xlsx(USArrests, file="file.xlsx")
savedtf2excel <- function(dtf, filename, path="data-raw/"){
    wb = xlsx::createWorkbook()
    # header style
    cs3 <- xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) + xlsx::Border()
    sheet  =  xlsx::createSheet(wb, sheetName="TotalConsumersWelfare")
    xlsx::addDataFrame(dtf, sheet, row.names=FALSE, colnamesStyle=cs3)
    xlsx::saveWorkbook(wb, paste("dWelfare/Welfare ", allScenarios$scenarioName$high,".xlsx", sep=""))
}


#' Save a list of data frames to Excel format
#'
#' creates an excel file containing sheets for each data frame in the list.
#' sheet names are created from the data frame names in the list.
#' Data frames have to be named.
#'
# A list of data frame such as str(list(france = swdfr))
#'       modifiy this function or create a new function that
#'       saves a list of data frames to excel sheets could test if dtf is a list
#' @param ldtf Named list of Data Frames
savelistdtf2excel <- function(ldtf, filename, path){
    NULL
}
