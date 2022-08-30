#' Create a National Student Clearinghouse request file
#'
#' Create a National Student Clearinghouse request file in the required format.
#' This requires a configuration object which contains required elements.
#' nsc_conf <- nsc_config(
#'                 schoolCode=YOURFICECODE,
#'                 branchCode=YOURBRANCHCODE,
#'                 schoolName=YOURSCHOOLNAME
#'             )
#' DOB must be either a date or a character string with format "%Y%m%d".
#'
#' @param df Dataframe to convert into format for submission to NSC
#' @param config A configuration object for NSC files
#' @param inquiryType One of CO (Longitudinal Cohort), DA (Declined Admission), PA (Prior Attendance), or SE (Subsequent Enrollment)
#' @param path Path where NSC file will be saved (default=getwd())
#' @param fn Name of the NSC file (default=same name as the dataframe with "_SE.tsv" appended)
#' @param search If the dataframe does not include a 'Search Begin Date' field, this will be used (default=TODAY)
#' @param enrolledStudents Are these students currently enrolled? Set to FALSE for PA query to allow SSN.
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr coalesce
#' @importFrom readr write_tsv
#'
nsc_request <- function(df,
                        config,
                        inquiryType="SE",
                        path=getwd(),
                        fn=paste0(deparse(substitute(df)),"_SE.tsv"),
                        search=format(Sys.Date(),"%Y%m%d"),
                        enrolledStudents=ifelse(inquiryType=="SE",FALSE,TRUE)
                        ) {

    if (missing(df)) {
        stop("Dataframe not provided")
    }

    df2 <- df

    names(df2) <- toupper(names(df2))

    names(df2)[names(df2) == colnames(df2)[grepl("FIR", colnames(df2))]] <- "FirstName"
    names(df2)[names(df2) == colnames(df2)[grepl("MI", colnames(df2))]] <- "MiddleInitial"
    names(df2)[names(df2) == colnames(df2)[grepl("LAS", colnames(df2))]] <- "LastName"
    names(df2)[names(df2) == colnames(df2)[grepl("SUF", colnames(df2))]] <- "Suffix"
    names(df2)[names(df2) == colnames(df2)[grepl("RET", colnames(df2))]] <- "ReturnRequestField"
    names(df2)[names(df2) == colnames(df2)[grepl("SEA", colnames(df2))]] <- "SearchBeginDate"

    # Verify dataframe includes the required fields:
    #     FirstName, MiddleInitial, LastName, Suffix, DOB
    if (any(!(c("FirstName", "LastName", "DOB") %in% colnames(df2)))) {
        stop("Dataframe does not include all of the required fields: FirstName, LastName, DOB")
    }

    if (!(inquiryType %in% c("CO", "DA", "PA", "SE"))) {
        stop(paste0("Inquiry Type must be one of CO, DA, PA, or SE [", inquiryType, "]"))
    }

    if ( "SSN" %in% colnames(df2) &
         (inquiryType!="PA" | (inquiryType=="PA" & enrolledStudents==TRUE)) ) {
        warning(paste0("SSN provided but ignored - inquiry(",inquiryType,"), enrolled(",enrolledStudents,")"))
    }

    if (missing(path)) {
        path <- getwd()
        warning(paste("Path set to",path))
    }

    # If search is just YYYY, set to YYYY0101, if just YYYYMM, set to YYYYMM01.
    if (nchar(search) == 4) {
        search %<>% paste0("0101")
        warning(paste("search changed to", search))
    } else if (nchar(search) == 6) {
        search %<>% paste0("01")
        warning(paste("search changed to", search))
    } else if (nchar(search) == 7) {
        search <- paste0( substr(search, 1, 4),
                          substr(search, 6, 7),
                          paste0("01") )
        warning(paste("search changed to", search))
    }

    if (missing(fn)) {
        # Get the name of 'df' and use as filename
        fn <- paste0(deparse(substitute(df)),"_",inquiryType,".tsv")
        warning(paste("fn set to",fn))
    }

    nscFile <- file.path(path,fn)

    # Ensure DOB is a date formatted as YYYYMMDD

    # Ensure all fields are padded to the correct lengths and create 'nsc' dataframe
    r <- data.frame( a = 1:length(df2$FirstName) )
    r$RecordType <- "D1"
    # Allow this to be overridden for PA query of non-enrolled students
    if (inquiryType=="PA" & enrolledStudents==FALSE & "SSN" %in% colnames(df2)) {
        r$SSN <- df2$SSN
    } else {
        r$SSN <- trimws("         ") # SSN cannot be provided in SE request, or
                                     # on PA requests for enrolled students
    }

    clean <- function(x) iconv(x,to="ASCII//TRANSLIT")

    r$FirstName <- trimws(substring(format(clean(df2$FirstName),width=20),1,20))

    if ("MiddleInitial" %in% colnames(df2)) {
        r$MI <- dplyr::coalesce(substring(clean(df2$MiddleInitial),1,1),"")
    } else {
        r$MI <- trimws(" ")
    }

    r$LastName <- trimws(substring(format(clean(df2$LastName),width=20),1,20))

    if ("Suffix" %in% colnames(df2)) {
        r$Suffix <- trimws(substring(format(clean(df2$Suffix),width=5),1,5))
        r$Suffix <- stringr::str_replace(r$Suffix,"\\.","") # Remove trailing periods
    } else {
        r$Suffix <- trimws("     ")
    }

    if (class(df2$DOB)=="Date") {
        r$DOB <- format(df2$DOB,"%Y%m%d")
    } else {
        r$DOB <- df2$DOB
    }

    # Check if the following optional fields are provided:
    #     SearchBeginDate, ReturnRequestField
    if (!("ReturnRequestField" %in% colnames(df2))) {
        warning("ReturnRequestField not provided - you may have difficulty matching return records")
    }

    if (!("SearchBeginDate" %in% colnames(df2))) {
        r$SearchBeginDate <- search
        warning(paste("SearchBeginDate not provided - defaulting to",search))
    } else {
        if (class(df2$SearchBeginDate)=="Date") {
            r$SearchBeginDate <- format(df2$SearchBeginDate,"%Y%m%d")
        } else {
            r$SearchBeginDate <- df2$SearchBeginDate
        }
    }

    r$Blank <- trimws(" ")

    r$SchoolCode <- config$schoolCode
    r$BranchCode <- config$branchCode

    if ("ReturnRequestField" %in% colnames(df2)) {
        r$ReturnRequestField <- trimws(format(df2$ReturnRequestField,width=50))
    } else {
        r$ReturnRequestField <- trimws(format(" ",width=50))
    }

    r$a<-NULL

    h <- data.frame( "H1",
                     config$schoolCode,
                     config$branchCode,
                     trimws(substring(format(config$schoolName,width=40),1,40)),
                     format(Sys.Date(), "%Y%m%d"),
                     inquiryType,
                     "I",
                     stringsAsFactors = FALSE )

    t <- data.frame("T1",as.character(nrow(r)+2,0), stringsAsFactors = FALSE)

    # Write out the file
    readr::write_tsv(h, path=nscFile, append=FALSE, col_names=FALSE, na="NA")
    readr::write_tsv(r, path=nscFile, append=TRUE,  col_names=FALSE, na="NA")
    readr::write_tsv(t, path=nscFile, append=TRUE,  col_names=FALSE, na="NA")

    return(r)
}

#' Create a National Student Clearinghouse configuration object
#'
#' Create a National Student Clearinghouse configuration object. All fields are strings.
#' nsc_conf <- nsc_config(
#'                 schoolCode="YOURFICECODE",
#'                 branchCode="YOURBRANCHCODE",
#'                 schoolName="YOURSCHOOLNAME"
#'             )
#'
#' @param schoolCode NSC school code
#' @param branchCode NSC branch code
#' @param schoolName NSC school name
#' @export
nsc_config <- function( schoolCode, branchCode, schoolName ) {
    return( data.frame(
                schoolCode=schoolCode,
                branchCode=branchCode,
                schoolName=schoolName
            )
          )
}

#' Create a National Student Clearinghouse Prior Attendance (PA) request file
#'
#' Create a National Student Clearinghouse Prior Attendance (PA) request file
#' in the required format. 
#' This requires a configuration object which contains required elements.
#' nsc_conf <- nsc_config(
#'                 schoolCode="YOURFICECODE",
#'                 branchCode="YOURBRANCHCODE",
#'                 schoolName="YOURSCHOOLNAME"
#'             )
#' DOB must be either a date or a character string with format "%Y%m%d".
#'
#' @param df Dataframe to convert into format for submission to NSC
#' @param config A configuration dataframe for NSC files
#' @param path Path where NSC file will be saved (default=getwd())
#' @param fn Name of the NSC file (default=same name as the dataframe with "_SE.tsv" appended)
#' @param search If the dataframe does not include a 'Search Begin Date' field, this will be used (default=TODAY)
#' @param enrolledStudents Are these students currently enrolled? Set to FALSE for PA query to allow SSN.
#' @export
#'
nsc_request_pa <- function(df,
                           config,
                           path=getwd(),
                           fn=paste0(deparse(substitute(df)),"_PA.tsv"),
                           search=format(Sys.Date(),"%Y%m%d"),
                           enrolledStudents=TRUE) {

    return(nsc_request(df,config,path=path,fn=fn,search=search,inquiryType="PA",enrolledStudents=enrolledStudents))
}


#' Create a National Student Clearinghouse Subsequent Enrollment (SE) request file
#'
#' Create a National Student Clearinghouse Subsequent Enrollment (SE) request file
#' in the required format. 
#' This requires a configuration object which contains required elements.
#' nsc_conf <- nsc_config(
#'                 schoolCode="YOURFICECODE",
#'                 branchCode="YOURBRANCHCODE",
#'                 schoolName="YOURSCHOOLNAME"
#'             )
#' DOB must be either a date or a character string with format "%Y%m%d".
#'
#' @param df Dataframe to convert into format for submission to NSC
#' @param config A configuration dataframe for NSC files
#' @param path Path where NSC file will be saved (default=getwd())
#' @param fn Name of the NSC file (default=same name as the dataframe with "_SE.tsv" appended)
#' @param search If the dataframe does not include a 'Search Begin Date' field, this will be used (default=TODAY)
#' @export
#'
nsc_request_se <- function(df,
                           config,
                           path=getwd(),
                           fn=paste0(deparse(substitute(df)),"_SE.tsv"),
                           search=format(Sys.Date(),"%Y%m%d")) {

    return(nsc_request(df,config,path=path,fn=fn,search=search,inquiryType="SE",enrolledStudents=FALSE))
}
