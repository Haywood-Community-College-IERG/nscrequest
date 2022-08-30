curr_date <- format(Sys.Date(),"%Y%m%d")

nsc_conf <- nsc_config(
    schoolCode="999999",
    branchCode="00",
    schoolName="Test School Name"
)

nsc_conf_correct <- data.frame(schoolCode="999999",
                               branchCode="00",
                               schoolName="Test School Name")

df1 <- data.frame(ID = c(1001, 1002, 1003),
                 First = c("Jane", "John", "Luke"),
                 Middle = c("Alison", "Jeffries", "J."),
                 Last = c("Smith", "Doe", "Skywalker"),
                 Suffix = c("", "Jr.", ""),
                 DOB = c("1978-01-01", "1985-08-22", "1977-05-04"),
                 SearchDate = c("1998-01-01", "2005-08-22", "1997-05-04"))

df1_correct <- data.frame( RecordType = c("D1","D1","D1"),
                           SSN = c("","",""),
                           FirstName = c("Jane", "John", "Luke"),
                           MI = c("A", "J", "J"),
                           LastName = c("Smith", "Doe", "Skywalker"),
                           Suffix = c("", "Jr", ""),
                           DOB = c("1978-01-01", "1985-08-22", "1977-05-04"),
                           SearchBeginDate = c("1998-01-01", "2005-08-22", "1997-05-04"),
                           Blank = c("","",""),
                           SchoolCode = c("999999","999999","999999"),
                           BranchCode = c("00","00","00"),
                           ReturnRequestField = c("","","")
                         )

df2 <- data.frame(ID = c(1001, 1002, 1003),
                  First = c("Jane", "John", "Luke"),
                  Middle = c("Alison", "Jeffries", "J."),
                  Last = c("Smith", "Doe", "Skywalker"),
                  Suffix = c("", "Jr.", ""),
                  DOB = c("1978-01-01", "1985-08-22", "1977-05-04"))


df2_correct <- data.frame( RecordType = c("D1","D1","D1"),
                           SSN = c("","",""),
                           FirstName = c("Jane", "John", "Luke"),
                           MI = c("A", "J", "J"),
                           LastName = c("Smith", "Doe", "Skywalker"),
                           Suffix = c("", "Jr", ""),
                           DOB = c("1978-01-01", "1985-08-22", "1977-05-04"),
                           SearchBeginDate = c(curr_date,curr_date,curr_date),
                           Blank = c("","",""),
                           SchoolCode = c("999999","999999","999999"),
                           BranchCode = c("00","00","00"),
                           ReturnRequestField = c("","","")
)
