
context("adddot2logfile")
test_that("add dot also works when the file doesn't exist yet", {
    logfile <- tempfile(fileext = ".log")
    # Need to first create the temporary directory , otherwise R complains with a message similar to this
    # cannot open file '/tmp/RtmpYaFeSR/file3d1b61f9b484.log': No such file or directory
    if(!file.exists(tempdir())) dir.create(tempdir())
    adddot2logfile(logfile)
})
