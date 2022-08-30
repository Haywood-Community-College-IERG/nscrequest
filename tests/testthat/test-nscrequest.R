test_that("nsc_config returns a proper configuration object",{
    expect_equal(nsc_conf, nsc_conf_correct)
    })
test_that("nsc_request with inquiryType=PA returns properly formatted file", {
    expect_equal(nsc_request( df1, nsc_conf, inquiryType = "PA", write=FALSE ),
                 df1_correct)
    })
test_that("nsc_request_pa returns properly formatted file", {
    expect_equal(nsc_request_pa( df1, nsc_conf, write=FALSE ),
                 df1_correct)
    })
test_that("nsc_request with inquiryType=SE returns properly formatted file", {
    expect_equal(nsc_request( df1, nsc_conf, inquiryType = "SE", write=FALSE ),
                 df1_correct)
    })
test_that("nsc_request_se returns properly formatted file", {
    expect_equal(nsc_request_se( df1, nsc_conf, write=FALSE ),
                 df1_correct)
    })
test_that("nsc_request with inquiryType=PA and no SerchDates returns properly formatted file", {
    expect_equal(nsc_request( df2, nsc_conf, inquiryType = "PA", write=FALSE ),
                 df2_correct)
})
