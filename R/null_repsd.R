#' null_repsd
#'
#' @param item_count numeric. How many items?
#' @param focal_sample numeric. How large is the focal sample?
#' @param focal_prop numeric, between 0 and 1 (exclusive). What is the proportion
#' of the focal sample compared to the rest of the data?
#' @param numStrata numeric. How many strata for matching should be used?
#' @param impact numeric. What is the expected, standardized mean difference
#' between the focal group's mean theta and the composite group's mean theta
#' (i.e., standardized focal mean - composite mean). See details for further explanation.
#' @param item_params_a numeric vector. What are the discrimination parameters
#' of the items in the data set?
#' @param item_params_b numeric vector. What are the difficulty parameters of
#' the items in the data set?
#' @param anchorItems either `NULL` or a vector of the anchorItems names or
#' numeric column locations. If `NULL`, all items are used for calculating the
#' total test score for stratifying individuals. If a vector, the specified items
#' are used to calculate the total test score for stratifying individuals.
#' @param iterations numeric. How many iterations for the function to run?
#' Defaults to 10000.
#' @param verbose logical. If `TRUE` (default), prints a `progress::progress_bar()`
#' in the console to allow tracking of the state of the distribution generation.
#'
#' @return An `item_count` x `iterations` data.frame with simulated repsd values
#'  for each item.
#' @export
#'
#' @importFrom stats rnorm runif
#' @importFrom progress progress_bar


null_repsd <- function(item_count = 20,
                       focal_sample = 88,
                       focal_prop = .09,
                       numStrata = 4,
                       impact = estimate_impact(),
                       item_params_a = timmsDiscrim,
                       item_params_b = timmsDiffic,
                       anchorItems = NULL,
                       iterations = 10000,
                       verbose = TRUE) {
  ###############################################
  ########## testing argument values ############
  if (iterations < 1 | !is.numeric(iterations)) {
    stop("The iterations needs to be provided as single, numeric value.")
  }
  if (!is.numeric(item_count) | item_count < 1) {
    stop("The item_count needs to be a numeric, integer value greater than 0.")
  }
  if (!is.numeric(focal_sample) | focal_sample < 1) {
    stop("The focal_sample needs to be a numeric, integer value greater than 0.")
  }
  if (!is.numeric(focal_prop) | focal_prop <= 0 | focal_prop >= 1) {
    stop("The focal_sample needs to be a numeric value between 0 and 1.")
  }
  if (any(!is.numeric(numStrata),
          !isTRUE(suppressWarnings(as.integer(numStrata)) > 2))) {
    stop('numStrata needs to be a numeric value, and that value needs to be an integer greater than 2.\nPlease try again.')
  }
  if (!is.numeric(impact)) {
    stop('The impact value needs to be a numeric value.')
  }
  if (length(item_params_a) != item_count) {
    stop('The provided number of item_params_a does not equal the provided item_count.')
  }
  if (length(item_params_b) != item_count) {
    stop('The provided number of item_params_b does not equal the provided item_count.')
  }
  if (!is.numeric(iterations) | iterations < 1) {
    warning('The specified iterations value was incorrect. Setting to the default value of 10,000.')
    iterations = 10000
  }
  if (numStrata > item_count) {
    stop('The number of strata cannot be greater than the item_count')
  }

  null_repsd_est <- c()
  if (verbose) {
    pb <- progress::progress_bar$new(total = iterations)
  }
  message('Beginning repsd Null Distribution Estimation.\n')

  for (i in 1:iterations) {
    if (verbose) pb$tick()
    totalss <- focal_sample / focal_prop
    thetafoc <- stats::rnorm(focal_sample, impact, 1)
    nonfocss <-
      (focal_sample / focal_prop) - focal_sample #see how formula was obtained in scanned notes in pub folder
    nonfocmean <-
      # ((impact + 1) - (impact * focal_prop)) / (1 - focal_prop) #see how formula was obtained in scanned notes in pub folder
      -(impact*focal_prop) / (1 - focal_prop)
    thetanonfoc <- stats::rnorm(nonfocss, nonfocmean, 1)
    theta <- c(thetafoc, thetanonfoc)
    thetaframe <- data.frame("theta" = theta,
                             "focal" = c(rep(1, focal_sample), rep(0, (nonfocss))))

    # using item parameters from mplus 2pl run on the data
    a <- item_params_a
    b <- item_params_b
    itemparam <- data.frame("item" = c(1:item_count),
                            "a" = a,
                            "b" = b)

    twopl <- function(iparam, theta, i) {
      ((exp(iparam[i, 2] * (
        theta - iparam[i, 3]
      ))) /
        (1 + (exp(
          iparam[i, 2] * (theta - iparam[i, 3])
        ))))
    }

    data <- c()

    for (j in 1:item_count) {
      data <- cbind(data,
                    ifelse(twopl(itemparam, theta, j) >= stats::runif(length(theta)), 1, 0))
    }

    data <-
      cbind(data, c(rep(1, focal_sample), rep(0, (
        totalss - focal_sample
      ))))

    ttscore <-
      if (is.null(anchorItems)) {
        rowSums(data[, 1:item_count])
      } else {
        rowSums(data[, anchorItems])
      }

    stratum_group <-
      cut(
        ttscore,
        breaks = seq(min(ttscore), max(ttscore), length.out = numStrata + 1),
        include.lowest = TRUE,
        labels = FALSE
      )

    data <- cbind(data, stratum_group, ttscore)

    data_temp <- subset(data, data[, (item_count + 1)] == 1)
    maxi <- max(data_temp[, (item_count + 3)])
    data <- subset(data, data[, (item_count + 3)] <= maxi)

    ## getting new focal sample size , after removing strata with less than 10
    data_helping <- subset(data, data[, (item_count + 1)] == 1)
    stratum_group_temp <- data_helping[, item_count + 2]
    ns <- table(stratum_group_temp)

    flags <- c()
    for (z in 1:numStrata) {
      flag <- ifelse(ns[z] < 10, ns[z], 0)
      flags <- c(flags, flag)
    }

    total_removed <- sum(flags, na.rm = TRUE)
    focal_sample_used <- focal_sample - total_removed


    # calculating resd for each item
    repsd_each_item <- c()

    for (items_again in 1:item_count) {
      brackets <- c()

      for (s in unique(stratum_group)) {
        sub_data <- subset(data, data[, (item_count + 2)] == s)
        sub_data_foc <-
          subset(sub_data, sub_data[, (item_count + 1)] == 1)

        if (length(sub_data_foc[, items_again]) < 10) {
          # ignoring strata with less than 10 focal examinees
          brackets <- brackets
        }

        if (length(sub_data_foc[, items_again]) >= 10) {
          # using only strata with 10 or more focal examinees

          psg <- mean(sub_data_foc[, items_again])
          psc <- mean(sub_data[, items_again])
          nsg <- length(sub_data_foc[, items_again])
          ng <- focal_sample_used
          bracket <- ((psg - psc) ^ 2) * (nsg / ng)
          brackets <- c(brackets, bracket)
        }
      }

      repsd <- sqrt(sum(brackets))
      repsd_each_item <- c(repsd_each_item, repsd)
    }

    null_repsd_est <- rbind(null_repsd_est, repsd_each_item)

  }
  return(null_repsd_est)
}
