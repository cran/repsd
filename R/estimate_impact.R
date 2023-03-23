#' Estimate the effect size difference between focal and composite group abilities
#'
#' @param responses The `data.frame` of responses, including the `focal_column`.
#' @param focal_column The `numeric` location of the focal column.
#' @param focal_id The `numeric`, `character`, or `logical` value that identifies
#' the focal group.
#'
#' @return A `numeric` estimate of the impact as the effect size *D*, e.g., the
#' standardized mean theta difference between the focal group and the composite
#' (total) group abilities. This estimate is rounded to 3 decimal places.
#'
#' @importFrom stats var
#' @export

estimate_impact <-
  function(
    responses = timmsData,
    focal_column = 21,
    focal_id = 1) {
    ###############################################
    ########## testing argument values ############
    if (!(is.data.frame(responses) | is.matrix(responses))) {
      stop('responses was not provided as a data.frame or a matrix.\nPlease provide the data in one of these classes.')
    }
    if (!(is.numeric(focal_column) | is.character(focal_column))) {
      stop('focal_column needs to be a numeric or character value.')
    }
    focal_columnTest <- tryCatch(
      expr = {
        if (is.character(focal_column)) {
          focal_column = which(colnames(responses) == focal_column)
        }
        responses[,focal_column]
        responses[,-focal_column]
        "Subsetting works."
      },
      error = function(e) e
    )
    if (any(class(focal_columnTest) == 'error')) {
      stop('Problems with subsetting the focal column.\nPlease test that your responses data can be subsetting with responses[,focal_column] and responses[,-focal_column] (if focal_column is numeric) or that the correct column name was proved for focal_column (if focal_column is character).')
    }
    focalIDTest <- tryCatch({
      if (is.character(focal_column)) {
        focal_column = which(colnames(responses) == focal_column)
      }
      responses[responses[,focal_column] == focal_id, ]
    },
    error = function(e) e
    )
    if (any(class(focalIDTest) == 'error')) {
      stop('Problems with the focal_id argument - error when attempting to subset the data.\nPlease check your provided focal_column and focal_id values and try again.')
    }
    if (nrow(focalIDTest) == 0) {
      stop('Problems with identifying members of the focal group - no observations found.\nPlease check your provided focal_column and focal_id values and try again.')
    }
    ###############################################
    response_matrix = responses[, -focal_column]

    composite_ss = nrow(responses)
    focal_ss     = sum(responses[, focal_column] == focal_id)

    composite_mean = rowSums(response_matrix) |>
      mean()
    composite_var  = rowSums(response_matrix) |>
      var()

    focal_mean = rowSums(response_matrix[responses[, focal_column] == focal_id, ]) |>
      mean()
    focal_var  = rowSums(response_matrix[responses[, focal_column] == focal_id, ]) |>
      var()

    pooled_sd  =
      sqrt(
        ((composite_ss - 1) * composite_var + (focal_ss - 1) * focal_var) /
          (composite_ss + focal_ss - 2)
      )

    standardized_focal_mean =
      ((focal_mean - composite_mean) /
         pooled_sd) |>
      round(digits = 3)

    return(standardized_focal_mean)
  }
