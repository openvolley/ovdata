context("example files")

test_that("example scout files work", {
    chc <- c("190301_kats_beds", "mlafin_braslovce_nkbm", "pm06", "clickscout", "2017_avl_mens_heat_vs_utssu", "2017_avl_womens_heat_vs_utssu", "stuttgart_schwerin_2018")

    x <- ovdata_example(chc)
    expect_type(x, "character")
    expect_equal(length(x), length(chc))
    expect_true(all(file.exists(x)))

    x <- ovdata_example(chc[1:2], as = "parsed")
    expect_type(x, "list")
    expect_equal(length(x), 2)
    expect_true(all(sapply(x, inherits, "datavolley")))

    x <- ovdata_example("2017_avl_womens_heat_vs_utssu", as = "parsed") ## simplify by default
    expect_s3_class(x, "peranavolley")

    x <- ovdata_example("2017_avl_womens_heat_vs_utssu", as = "parsed", simplify = FALSE)
    expect_type(x, "list")
    expect_s3_class(x[[1]], "peranavolley")

    x <- ovdata_example(c("2017_avl_mens_heat_vs_utssu", "2017_avl_womens_heat_vs_utssu"), as = "parsed")
    expect_type(x, "list")
    expect_s3_class(x[[1]], "peranavolley")

    ## test that German example uses correct parms when reading
    x <- ovdata_example("stuttgart_schwerin_2018", as = "parsed")
    chk <- x$plays
    chk <- unique(chk[chk$skill == "Block", c("evaluation_code", "evaluation")])
    expect_equal(chk$evaluation[which(chk$evaluation_code == "=")], "Invasion")
    expect_equal(chk$evaluation[which(chk$evaluation_code == "/")], "Error")
})
