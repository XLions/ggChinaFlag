#' 存储中国历史与政治旗帜的中英文名称
#'
#' 提供一个标准化的名称列表，供 \code{\link{plotCNFlag}} 等函数使用。
#' 包含中华人民共和国国旗、中华民国相关历史旗帜、中国共产党党旗以及
#' 中国国民党党旗的中英文对照名称，便于统一调用和界面展示。
#'
#' @param lang 字符串，指定返回名称的语言。
#'   取值为 \code{"Chinese"}（默认，返回中文名称）或 \code{"English"}（返回英文名称）。
#'
#' @return 返回一个 \code{list}，包含两个命名元素：
#'   \itemize{
#'     \item 若 \code{lang = "Chinese"}：元素名为 \code{国旗} 与 \code{政党}，均为字符向量。
#'     \item 若 \code{lang = "English"}：元素名为 \code{National Flags} 与 \code{Political Parties}。
#'   }
#'   每个向量中的字符串与底层绘图函数一一对应。
#'
#' @examples
#' # 获取中文名称列表
#' FlatStorge()
#'
#' # 获取英文名称列表
#' FlatStorge(lang = "English")
#'
#' @seealso
#' \code{\link{plotCNFlag}} 使用本函数返回的名称进行匹配绘图。
#'
#' @export
FlatStorge<-function(lang=c('Chinese','English')){
  lang <- match.arg(lang)
  if(lang=='Chinese'){
    return(
      list(
        国旗=c('中华人民共和国国旗','中华民国青天白日旗',
             '中华民国北洋政府五色旗','武昌起义铁血十八星旗'),
        政党=c('中国共产党党旗','中国国民党党旗')
      )
    )
  }else{
    return(
      list(
        `National Flags` = c(
          "Flag of the People's Republic of China",
          "Flag of the Republic of China (Blue Sky, White Sun, and Red Earth)",
          "Five-Color Flag of the Beiyang Government of the Republic of China",
          "Iron-Blood 18-Star Flag of the Wuchang Uprising"
        ),
        `Political Parties` = c(
          "Flag of the Communist Party of China",
          "Flag of the Kuomintang (Blue Sky and White Sun flag)"
        )
      )
    )
  }
}





#' 根据输入的中英文名称绘制中国历史或政治旗帜
#'
#' 此函数提供一个统一接口，根据输入的旗帜或党旗名称（支持中文或英文）
#' 自动调用对应的底层绘图函数，返回一个 `ggplot` 对象。
#'
#' @param input 字符串，指定要绘制的旗帜名称。可以是中文或英文，
#'   例如 `"中华人民共和国国旗"`、`"中华民国青天白日旗"`、
#'   `"Flag of the Communist Party of China"` 等。具体支持名称见
#'   \code{\link{FlatStorge}}。
#' @param label 逻辑值，是否显示标题与文字标注。默认为 \code{TRUE}。
#'
#' @return 一个 \code{ggplot} 对象，可直接打印或通过
#'   \code{ggsave()} 保存。
#'
#' @details
#' 函数内部通过 \code{\link{FlatStorge}} 获取内置的中英文名称列表，
#' 自动检测输入语言并进行匹配，然后转发至以下绘图函数之一：
#' \itemize{
#'   \item \code{\link{plot_P.R.CHINA_flag}}
#'   \item \code{\link{plot_ROC_KMT_flag}}
#'   \item \code{\link{plot_ROC_Beiyang_flag}}
#'   \item \code{\link{plot_Han18Star}}
#'   \item \code{\link{plot_CCP}}
#'   \item \code{\link{plot_KMT}}
#' }
#' 若输入名称无法识别，将抛出错误。
#'
#' @examples
#' \dontrun{
#' # 绘制中华人民共和国国旗（带标题）
#' plotCNFlag("中华人民共和国国旗")
#'
#' # 绘制中国国民党党旗（无文字）
#' plotCNFlag("中国国民党党旗", label = FALSE)
#'
#' # 使用英文名称绘制五色旗
#' plotCNFlag("Five-Color Flag of the Beiyang Government of the Republic of China")
#'
#' # 绘制武昌起义十八星旗
#' plotCNFlag("Iron-Blood 18-Star Flag of the Wuchang Uprising")
#' }
#'
#' @seealso
#' \code{\link{FlatStorge}} 获取内置名称列表，
#' \code{\link{plot_P.R.CHINA_flag}} 等底层绘图函数。
#'
#' @export
plotCNFlag <- function(input, label = TRUE) {
  # 获取中英文名称列表
  cn_list <- FlatStorge(lang = "Chinese")
  en_list <- FlatStorge(lang = "English")

  # 拼接成扁平字符向量（保持顺序一致）
  names_cn <- c(cn_list$国旗, cn_list$政党)
  names_en <- c(en_list$`National Flags`, en_list$`Political Parties`)

  # 尝试匹配中文或英文名称
  idx <- match(input, names_cn)
  if (is.na(idx)) {
    idx <- match(input, names_en)
  }
  if (is.na(idx)) {
    stop("输入名称无法识别，请使用内置标准名称（中英文均可）。")
  }

  # 绘图函数列表，顺序与名称列表完全对应
  flag_funcs <- list(
    plot_P.R.CHINA_flag,   # 1. 中华人民共和国国旗
    plot_ROC_KMT_flag,     # 2. 中华民国青天白日满地红旗
    plot_ROC_Beiyang_flag, # 3. 北洋政府五色旗
    plot_Han18Star,        # 4. 武昌起义铁血十八星旗
    plot_CCP,              # 5. 中国共产党党旗
    plot_KMT               # 6. 中国国民党党旗
  )

  # 调用对应函数，传递 label 参数
  flag_funcs[[idx]](label = label)
}
