test_that("ScoreCalc calculates percentiles and action levels correctly", {

  df <- data.frame(
    sex = c("f", "m"),
    age = c(8, 15),
    height = c(120, 125),
    waist = c(55, 60),
    homa = c(1.2, 1.4),
    sbp = c(100, 105),
    dbp = c(65, 70),
    crp = c(6, 2),
    trg = c(0.9, 1.0),
    hdl = c(1.1, 1.0)
  )

  result <- ScoreCalc(
    df,
    return_values = c("percentile", "cutoff.levels")
  )

  expect_equal(nrow(result), 2)

  expect_true(all(c(
    "waist_percentile",
    "homa_percentile",
    "sbp_percentile",
    "dbp_percentile",
    "crp_percentile",
    "trg_percentile",
    "hdl_percentile"
  ) %in% names(result)))

  # CRP supports the older age
  expect_false(is.na(result$crp_percentile[2]))

  # Age 15 is outside the IDEFICS range for these variables
  expect_true(is.na(result$waist_percentile[2]))
  expect_true(is.na(result$homa_percentile[2]))
  expect_true(is.na(result$sbp_percentile[2]))
  expect_true(is.na(result$dbp_percentile[2]))
  expect_true(is.na(result$trg_percentile[2]))
  expect_true(is.na(result$hdl_percentile[2]))

  expect_equal(
    result$hdl_percentile,
    c(3.61214191307519e-06, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$height_percentile,
    c(0.0451617720789443, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$homa_percentile,
    c(0.67496379032534, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$trg_percentile,
    c(1.91292377792771e-28, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$waist_percentile,
    c(0.493334606610262, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$sbp_percentile,
    c(0.523532114847159, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$dbp_percentile,
    c(0.616152122915867, NA_real_),
    tolerance = 1e-8
  )

  expect_equal(
    result$crp_percentile,
    c(0.988553314401349, 0.906628776100773),
    tolerance = 1e-8
  )

  expect_equal(
    as.character(result$adiposity.level),
    c("none", NA)
  )

  expect_equal(
    as.character(result$crp.level),
    c("Elevated", "Not elevated")
  )

  expect_equal(
    as.character(result$blood_pressure.level),
    c("none", NA)
  )

  expect_equal(
    as.character(result$blood_lipids.level),
    c("action", NA)
  )

  expect_equal(
    as.character(result$blood_glu_insu.level),
    c("none", NA)
  )

  expect_equal(
    as.character(result$overall.level),
    c("none", NA)
  )

  # test that these are still ordered factors
  expect_true(is.ordered(result$adiposity.level))
  expect_true(is.ordered(result$crp.level))
  expect_true(is.ordered(result$blood_pressure.level))
  expect_true(is.ordered(result$blood_lipids.level))
  expect_true(is.ordered(result$blood_glu_insu.level))
  expect_true(is.ordered(result$overall.level))
})

test_that("ScoreCalc calculates MetS scores correctly", {

  df <- data.frame(
    sex = c("f", "m"),
    age = c(6, 7),
    height = c(120, 125),
    waist = c(55, 60),
    homa = c(1.2, 1.4),
    sbp = c(100, 105),
    dbp = c(65, 70),
    trg = c(0.9, 1.0),
    hdl = c(1.1, 1.0)
  )

  result <- ScoreCalc(
    df,
    return_values = c("z.score", "MetS")
  )

  expect_equal(nrow(result), 2)

  # Individual z-scores
  expect_equal(
    result$hdl_z.score,
    c(-4.31525188448336, -4.48867529435654),
    tolerance = 1e-8
  )

  expect_equal(
    result$height_z.score,
    c(0.609665891082147, 0.0984843676761193),
    tolerance = 1e-8
  )

  expect_equal(
    result$homa_z.score,
    c(0.877749682432048, 1.05143277344152),
    tolerance = 1e-8
  )

  expect_equal(
    result$trg_z.score,
    c(-10.9998932011916, -14.3507613273616),
    tolerance = 1e-8
  )

  expect_equal(
    result$waist_z.score,
    c(0.856480976069964, 1.54420804481588),
    tolerance = 1e-8
  )

  expect_equal(
    result$sbp_z.score,
    c(-0.00969139821591998, 0.432103854546215),
    tolerance = 1e-8
  )

  expect_equal(
    result$dbp_z.score,
    c(0.248971228792127, 1.09563069791059),
    tolerance = 1e-8
  )

  # MetS score
  expect_equal(
    result$MetS,
    c(-1.48845008456399, -1.57153492201673),
    tolerance = 1e-8
  )

  # Standardized MetS score
  expect_equal(
    result$MetS_z.score,
    c(-0.640386411854575, -0.692780237962467),
    tolerance = 1e-8
  )

  # Internal consistency: MetS must follow the documented formula
  expected_MetS <-
    result$waist_z.score +
    result$homa_z.score +
    0.5 * (
      result$sbp_z.score +
        result$dbp_z.score +
        result$trg_z.score -
        result$hdl_z.score
    )

  expect_equal(
    result$MetS,
    expected_MetS,
    tolerance = 1e-10
  )
})
