# National Student Clearinghouse request/response processing

The following functions have been defined for working with National Student
Clearinghouse (NSC) request and response files:

* `nsc_request()` - Create a NSC request file in the required format.
* `nsc_config()` - Create a NSC request configuration object.
* `nsc_request_pa()` - Create a NSC PA request file.
* `nsc_request_se()` - Create a NSC SE request file.
* `nsc_return_se_convert()` - Create a modified response file.


## `nsc_config( schoolCode, branchCode, schoolName )`

Create a NSC request configuration object. Must include the following parameters:

* `schoolCode`: The school's FICE code
* `branchCode`: The school's branch code for this data request
* `schoolName`: The name of the school

## `nsc_request( df, config, inquiryType="SE", path=getwd(), fn=paste0(deparse(substitute(df)),"_SE.tsv"), search=format(Sys.Date(),"%Y%m%d"), enrolledStudents=ifelse(inquiryType=="SE",FALSE,TRUE) )`

Create a NSC request file with the appropriate inquiry type.

## `nsc_request_se( df, config, path=getwd(), fn=paste0(deparse(substitute(df)),"_SE.tsv"), search=format(Sys.Date(),"%Y%m%d") )`

Create a NSC request file for subsequent enrollment type (SE).

This is just syntactic sugar for a call to `nsc_request()` with `inquiryType="SE"`.

## `nsc_request_pa( df, config, path=getwd(), fn=paste0(deparse(substitute(df)),"_SE.tsv"), search=format(Sys.Date(),"%Y%m%d"), enrolledStudents=ifelse(inquiryType=="SE",FALSE,TRUE) )`

Create a NSC request file for prior attendance type (PA).

This is just syntactic sugar for a call to `nsc_request()` with `inquiryType="PA"`.
