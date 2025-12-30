# ------------------------------------------------------------
# Internal flag registry
# This object maps flag types and items to plotting functions
# ------------------------------------------------------------

.flag_registry <- list(
  nation = list(
    PRChina = "plot_P.R.CHINA_flag",
    ROC_Beiyang = "plot_ROC_Beiyang_flag",
    ROC_KMT = "plot_ROC_KMT_flag"
  ),
  party = list(
    CCP="plot_CCP",
    KMT="plot_KMT"
  )
)

#' List available flag types
#'
#' This function returns all supported flag categories.
#'
#' @return A character vector of available flag types.
#' @export
#'
#' @examples
#' typeFlag()
typeFlag <- function() {
  names(.flag_registry)
}

#' List available flags under a given type
#'
#' @param type A character string specifying the flag type.
#'
#' @return A character vector of available flags under the given type.
#' @export
#'
#' @examples
#' itemType("nation")
itemType <- function(type) {
  if (!type %in% names(.flag_registry)) {
    stop("Unsupported type. Use typeFlag() to see available options.")
  }
  names(.flag_registry[[type]])
}

#' Plot a Chinese national or party flag
#'
#' This is the unified interface for plotting flags in ggChinaFlag.
#'
#' @param type Flag category. Available options can be queried via typeFlag().
#' @param item Specific flag name under the given type.
#' @param ... Additional parameters passed to the underlying plotting function.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plotCNFlag(type = "nation", item = "PRC")
#' }
plotCNFlag <- function(type, item, ...) {

  if (!type %in% names(.flag_registry)) {
    stop("Unsupported type. Use typeFlag() to see available options.")
  }

  if (!item %in% names(.flag_registry[[type]])) {
    stop(
      paste0(
        "Unsupported item for type '", type,
        "'. Use itemType(\"", type, "\") to see available options."
      )
    )
  }

  fun_name <- .flag_registry[[type]][[item]]

  plot_fun <- get(fun_name, envir = asNamespace("ggChinaFlag"))

  plot_fun(...)
}


