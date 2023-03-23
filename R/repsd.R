#' repsd
#'
#' @param responses data.frame, matrix, or similar object which includes the item
#' responses and the focal group ID column.
#' @param focalColumn numeric or character. The location or name of the column
#' that holds the focal group data.
#' @param focalGroupID numeric or character. The value that identifies the focal
#' group.
#' @param anchorItems either `NULL` or a vector of the anchorItems names or
#' numeric column locations. If `NULL`, all items are used for calculating the
#' total test score for stratifying individuals. If a vector, the specified items
#' are used to calculate the total test score for stratifying individuals.
#' @param numStrata numeric. How many strata for matching should be used?
#'
#' @return Matrix of repsd values for each item.
#' @export

repsd <-
  function(responses = timmsData,
           focalColumn = 21,
           focalGroupID = 1,
           anchorItems = NULL,
           numStrata = 4) {
    ###############################################
    ########## testing argument values ############
    # if (!is.numeric(focalSample)) {
    #   error('focalSample needs to be a numeric value.')
    # }
    # if (!is.numeric(focalProp) | focalProp <= 0 | focalProp >= 1) {
    #   error('focalProp needs to be a numeric value between 0 and 1,')
    # }
    if (!(is.data.frame(responses) | is.matrix(responses))) {
      stop('responses was not provided as a data.frame or a matrix.\nPlease provide the data in one of these classes.')
    }
    if (!(is.numeric(focalColumn) | is.character(focalColumn))) {
      stop('focalColumn needs to be a numeric or character value.')
    }
    focalColumnTest <- tryCatch(
      expr = {
        if (is.character(focalColumn)) {
          focalColumn = which(colnames(responses) == focalColumn)
        }
        responses[,focalColumn]
        responses[,-focalColumn]
        "Subsetting works."
      },
      error = function(e) e
    )
    if (any(class(focalColumnTest) == 'error')) {
      stop('Problems with subsetting the focal column.\nPlease test that your responses data can be subsetting with responses[,focalColumn] and responses[,-focalColumn] (if focalColumn is numeric) or that the correct column name was proved for focalColumn (if focalColumn is character).')
    }
    focalIDTest <- tryCatch({
      if (is.character(focalColumn)) {
        focalColumn = which(colnames(responses) == focalColumn)
      }
      responses[responses[,focalColumn] == focalGroupID, ]
      },
      error = function(e) e
    )
    if (any(class(focalIDTest) == 'error')) {
      stop('Problems with the focalGroupID argument - error when attempting to subset the data.\nPlease check your provided focalColumn and focalGroupID values and try again.')
    }
    if (nrow(focalIDTest) == 0) {
      stop('Problems with identifying members of the focal group - no observations found.\nPlease check your provided focalColumn and focalGroupID values and try again.')
    }
    if (!is.null(anchorItems)) {
      if (is.numeric(anchorItems)) {
        numericAnchorItems <- tryCatch({
          ttscore_foc_test = rowSums(responses[,anchorItems])
        },
        error = function(e) e)
        if (any(class(numericAnchorItems) == 'error')) {
          stop('Problems with specifying the anchorItems. Please check that the numeric vector of values provided only includes the column location of the items you wish to use for creating the total test score and try again.')
        }
      } else if (is.character(anchorItems)) {
        characterAnchorItems <- tryCatch({
          ttscore_foc_test = rowSums(responses[,anchorItems])
        },
        error = function(e) e)
        if (any(class(characterAnchorItems) == 'error')) {
          stop('Problems with specifying the anchorItems. Please check that the character vector of values provided only includes the column names of the items you wish to use for creating the total test score and try again.')
        }
      }
    }
    if (any(!is.numeric(numStrata),
            !isTRUE(suppressWarnings(as.integer(numStrata)) > 2))) {
      stop('numStrata needs to be a numeric value, and that value needs to be an integer greater than 2.\nPlease try again.')
    }
    ###############################################

    ###############################################
    ########### calculated setup values ###########
    ###############################################
    focalSample <-
      nrow(
        subset(responses,
               responses[, focalColumn] == focalGroupID)
      )

    focalProp <-
      focalSample / nrow(responses)

    ###############################################
    ############ calculating repsd ################
    ###############################################

    # creating strata
    itemresp_foc <-
      subset(responses,
             responses[, focalColumn] == focalGroupID)
    ttscore_foc <-
      if (is.null(anchorItems)) {
        rowSums(itemresp_foc[,-focalColumn])  # I am cutting the strata based on total score of focal group only!
      } else {
        rowSums(itemresp_foc[,anchorItems])
      }
    ttscore <-
      if (is.null(anchorItems)) {
        rowSums(responses[,-focalColumn])
      } else {
        rowSums(responses[,anchorItems])
      }

    breaks_foc <-
      seq(min(ttscore_foc),
          max(ttscore_foc),
          length.out = numStrata + 1)
    stratum_group <-
      cut(
        ttscore,
        breaks = breaks_foc,
        include.lowest = TRUE,
        labels = FALSE
      )

    itemresp <-
      cbind(responses,
            stratum_group,
            ttscore)

    # removing nonfoc persons with total score higher than max focal total score

    maxi <-
      max(ttscore_foc)
    itemresp <-
      subset(itemresp,
             itemresp[, 'ttscore'] <= maxi)
    num_removed_over_foc_max <-
      (focalSample / focalProp) - nrow(itemresp)

    # removing nonfoc persons with total score lower than min focal total score

    mini <-
      min(ttscore_foc)
    itemresp <-
      subset(itemresp,
             itemresp[, 'ttscore'] >= mini)
    num_removed_under_foc_min <-
      (focalSample / focalProp) - nrow(itemresp)

    ### handling cases of low N for focal group within strata

    focal_N_in_strata <-
      c()  # figuring out number of focal in each strata

    for (s in c(1:numStrata)) {
      sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s) # DON'T HARDCODE THE STRATA COLUMN!
      sub_data_foc <- subset(sub_data, sub_data[, focalColumn] == focalGroupID)
      focal_N_in_strata <-
        c(focal_N_in_strata, length(sub_data_foc[, 1]))

    }

    flags <-
      c()  # flagging strata with less than 10 focal group members
    for (z in 1:numStrata) {
      flag <- ifelse(focal_N_in_strata[z] < 10, focal_N_in_strata[z], 0)
      flags <- c(flags, flag)
    }

    total_focal_removed <-
      sum(flags)  # number of focal persons removed
    new_focal_ss <-
      focalSample - total_focal_removed  # sample size to use in repsd

    ### handling cases of low N for non-focal group within strata

    non_focal_N_in_strata <-
      c()  # figuring out number of nonfocal in each strata

    for (s in c(1:numStrata)) {
      sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s)
      sub_data_nonfoc <- subset(sub_data, sub_data[, focalColumn] != focalGroupID)
      non_focal_N_in_strata <-
        c(non_focal_N_in_strata, length(sub_data_nonfoc[, 1]))

    }

    flags <-
      c()  # flagging strata with less than 10 nonfocal group members
    for (z in 1:numStrata) {
      flag <-
        ifelse(non_focal_N_in_strata[z] < 10, non_focal_N_in_strata[z], 0)
      flags <- c(flags, flag)
    }

    total_non_focal_removed <-
      sum(flags)  # number of focal persons removed

    # calculating resd for each item

    repsd_each_item <- c()

    for (column in 1:(ncol(responses) - 1)) {
      brackets <- c()

      for (s in 1:numStrata) {
        sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s)
        sub_data_foc <- subset(sub_data, sub_data[, focalColumn] == focalGroupID)
        sub_data_non_foc <- subset(sub_data, sub_data[, focalColumn] != focalGroupID)

        if (length(sub_data_foc[, 1] < 10)) {
          # ignoring strata with less than 10 focal examinees
          brackets <- brackets
        }

        if (length(sub_data_non_foc[, 1] < 10)) {
          # ignoring strata with less than 10 nonfocal examinees
          brackets <- brackets
        }

        if ((length(sub_data_foc[, 1] >= 10)) &
            (length(sub_data_non_foc[, 1] >= 10))) {
          # using only strata with 10+ foc and nonfoc

          psg <- mean(sub_data_foc[, column])
          psc <- mean(sub_data[, column])
          nsg <- length(sub_data_foc[, column])
          ng <-
            new_focal_ss                # ensuring weighting of resd is based on corrected sample size
          bracket <- ((psg - psc) ^ 2) * (nsg / ng)
          brackets <- c(brackets, bracket)
        }
      }

      repsd <- sqrt(sum(brackets))

      repsd_each_item <- c(repsd_each_item, repsd)
    }
    return(
      list('repsd_each_item' = repsd_each_item,
           'total_focal_removed' = total_focal_removed,
           'total_non_focal_removed' = total_non_focal_removed,
           'num_removed_over_foc_max' = total_focal_removed
      )
    )

  }
globalVariables('timmsData')
globalVariables('timmsDiscrim')
globalVariables('timmsDiffic')
