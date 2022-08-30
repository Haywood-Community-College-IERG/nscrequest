#' Convert National Student Clearinghouse return file to database format
#'
#' Convert the National Student Clearinghouse return file to a database format
#' that is easier to work with. Each student will have one record per college
#' attended. Additional records are added for each degree earned. If a student
#' did not attend a college that reports to the NSC, then Record Found Y/N will
#' be 'N'.
#'
#' @param fn Full path to the details file provided by NSC
#' @keywords file
#' @export
#' @importFrom tidyr fill
#' @importFrom magrittr `%<>%` `%>%`
#' @importFrom tibble as_tibble
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr filter mutate select group_by starts_with last bind_rows left_join arrange row_number n
#' @importFrom rlang .data
#'
nsc_return_se_convert <- function(fn) {

    # Load the specified file into a dataframe
    students <- tibble::as_tibble(
        readr::read_csv(fn, col_types=readr::cols(.default = readr::col_character()))
        )

    # get students with no activity into no_act, remove from students
    no_act <- students %>% dplyr::filter( .data$`Record Found Y/N` == 'N' ) %>%
        # Remove graduation columns - they will be added back later
        dplyr::select( .data$`Your Unique Identifier`:.data$`Search Date` )

    # ...For the rest, convert the dates into dates
    students %<>% dplyr::filter( .data$`Record Found Y/N` == 'Y' ) %>%
        dplyr::mutate( `Enrollment Begin` = as.Date( .data$`Enrollment Begin`, "%Y%m%d" ),
                       `Enrollment End` = as.Date( .data$`Enrollment End`, "%Y%m%d" ),
                       `Enrollment Days` = .data$`Enrollment End` - .data$`Enrollment Begin`
        )

    # Fix the graduation records
    graduated <- students %>%
        # Now keep only the graduated records
        dplyr::filter( .data$`Graduated?` == 'Y' ) %>%
        dplyr::select( .data$`Last Name`,
                       .data$`First Name`,
                       .data$`Middle Initial`,
                       .data$`Name Suffix`,
                       .data$`College Sequence`,
                       .data$`Graduated?`,
                       .data$`Graduation Date`,
                       .data$`Degree Title`,
                       .data$`Degree Major 1`, .data$`Degree CIP 1`,
                       .data$`Degree Major 2`, .data$`Degree CIP 2`,
                       .data$`Degree Major 3`, .data$`Degree CIP 3`,
                       .data$`Degree Major 4`, .data$`Degree CIP 4`
                     ) %>%
        dplyr::mutate( `Degree Title` = ifelse(is.na(.data$`Degree Title`), "UNKNOWN", .data$`Degree Title`),
                       `Degree Major 1` = ifelse(is.na(.data$`Degree Major 1`), "UNKNOWN", .data$`Degree Major 1`),
                       `Degree CIP 1` = ifelse(is.na(.data$`Degree CIP 1`), "UNKNOWN", .data$`Degree CIP 1`)
                     )

    # Remove graduation records as they were handled above
    students %<>% dplyr::filter( .data$`Graduated?` == 'N' ) %>%
        # Remove graduation columns - they will be added back later
        dplyr::select( -.data$`Graduated?`, -.data$`Graduation Date`, -dplyr::starts_with("Degree") ) %>%
        # Add a row index
        dplyr::mutate( RowNumber = dplyr::row_number() ) %>%
        #
        # We need to fill down the CollegeSequence value since it is missing for
        #    subsequent records
        dplyr::group_by( .data$`Last Name`,
                         .data$`First Name`,
                         .data$`Middle Initial`,
                         .data$`Name Suffix`,
                         .data$`Requester Return Field`,
                         .data$`Enrollment Begin` ) %>%
        tidyr::fill( .data$`College Sequence` ) %>%
        # Now regroup so we can add the number of Semesters at Institution,
        #     keeping one row with earliest Begin date and the latest End date
        dplyr::group_by( .data$`Last Name`,
                         .data$`First Name`,
                         .data$`Middle Initial`,
                         .data$`Name Suffix`,
                         .data$`Requester Return Field`,
                         .data$`College Sequence`
                         ) %>%
        dplyr::mutate( `Semesters at Institution` = dplyr::n(),
                       SemesterIndex = dplyr::row_number(),
                       `Enrollment Begin` = min(.data$`Enrollment Begin`),
                       `Enrollment End` = max(.data$`Enrollment End`),
                       `Total Enrollment Days` = sum(.data$`Enrollment Days`),
                       `Last Enrollment Major 1` = dplyr::last(.data$`Enrollment Major 1`),
                       `Last Enrollment CIP 1` = dplyr::last(.data$`Enrollment CIP 1`),
                       `Last Enrollment Major 2` = dplyr::last(.data$`Enrollment Major 2`),
                       `Last Enrollment CIP 2` = dplyr::last(.data$`Enrollment CIP 2`)
                       ) %>%
        dplyr::filter(.data$SemesterIndex == 1) %>%
        #dplyr::filter( RowNumber==max(RowNumber) ) %>%
        # Drop the RowNumber variable as it is no longer needed
        dplyr::select( -.data$RowNumber, -.data$SemesterIndex )


    students %<>%
        # Bring students with no activity back into the data frame
        dplyr::bind_rows(no_act) %>%
        # Bring in graduation data
        dplyr::left_join(graduated,
                         by = c("Last Name", "First Name", "Middle Initial", "Name Suffix", "College Sequence")
                        ) %>%
        dplyr::mutate( `Graduated?` = ifelse(is.na(.data$`Graduated?`), "N", .data$`Graduated?`)) %>%
        dplyr::arrange( .data$`Last Name`,
                        .data$`First Name`,
                        .data$`Middle Initial`,
                        .data$`Name Suffix`,
                        .data$`Requester Return Field`,
                        .data$`College Sequence`
                      ) %>%
        dplyr::select( .data$`Last Name`,
                       .data$`First Name`,
                       .data$`Middle Initial`,
                       .data$`Name Suffix`,
                       .data$`Requester Return Field`,
                       .data$`Record Found Y/N`,
                       .data$`Search Date`,
                       .data$`College Sequence`,
                       .data$`College Code/Branch`,
                       .data$`College Name`,
                       .data$`College State`,
                       .data$`2-year / 4-year`,
                       .data$`Public / Private`,
                       .data$`Enrollment Begin`,
                       .data$`Enrollment End`,
                       .data$`Enrollment Status`,
                       .data$`Class Level`,
                       .data$`Enrollment Major 1`,
                       .data$`Enrollment CIP 1`,
                       .data$`Enrollment Major 2`,
                       .data$`Enrollment CIP 2`,
                       .data$`Last Enrollment Major 1`,
                       .data$`Last Enrollment CIP 1`,
                       .data$`Last Enrollment Major 2`,
                       .data$`Last Enrollment CIP 2`,
                       .data$`Semesters at Institution`,
                       .data$`Total Enrollment Days`,
                       .data$`Graduated?`,
                       .data$`Graduation Date`,
                       .data$`Degree Title`,
                       .data$`Degree Major 1`,
                       .data$`Degree CIP 1`,
                       .data$`Degree Major 2`,
                       .data$`Degree CIP 2`,
                       .data$`Degree Major 3`,
                       .data$`Degree CIP 3`,
                       .data$`Degree Major 4`,
                       .data$`Degree CIP 4`
                     )

    return(students)
}
