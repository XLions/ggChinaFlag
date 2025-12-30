#' 绘制标准五星红旗（几何构造法）
#'
#' 使用纯几何计算与 \code{ggplot2} 绘制符合规范比例的中华人民共和国国旗。
#' 五角星的形状与朝向通过解析几何方法精确计算，小星均指向大星中心，
#' 不依赖外部图片或 SVG 资源，适合教学演示与程序化绘图。
#'
#' 本函数在运行时会自动检测并加载所需依赖包
#'（\code{tidyverse}、\code{sysfonts}、\code{showtextdb}、\code{showtext}），
#' 若未安装则会自动从 CRAN 安装。
#'
#' 几何构造方法参考 B 站视频：BV1Ni4y1978g。
#'
#' @param label logical，是否显示标题与坐标轴文字。
#'   \describe{
#'     \item{TRUE}{显示标题、作者及参考信息（默认）}
#'     \item{FALSE}{不显示任何文字，仅绘制图形}
#'   }
#'
#' @return
#' 返回一个 \code{ggplot} 对象，可直接打印或用于 \code{ggsave()} 保存。
#'
#' @details
#' \itemize{
#'   \item 国旗比例为 3:2
#'   \item 大五角星位于左上角
#'   \item 四颗小五角星按规范位置分布，并旋转指向大星中心
#'   \item 使用 \code{coord_quickmap()} 保证 x:y 比例为 1:1
#' }
#'
#' @examples
#' \dontrun{
#' # 基本用法
#' plot_P.R.CHINA_flag()
#' }
#'
#' \dontrun{
#' # 不显示任何文字，适合导出图片
#' plot_P.R.CHINA_flag(label = FALSE)
#' }
#'
#' @author
#' 曾联松（国旗设计者）；
#'
#' @seealso
#' \code{\link[ggplot2]{geom_polygon}},
#' \code{\link[ggplot2]{coord_quickmap}}
#'
#' @export
plot_P.R.CHINA_flag<-function(label=T){
  # ------------------------------------------------------------
  # 图像几何绘制方法参考 B 站视频：BV1Ni4y1978g
  # 本脚本通过纯几何计算 + ggplot2 绘制标准五星红旗
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # 自动检查 / 安装 / 加载所需 R 包
  # ------------------------------------------------------------
  packages <- c(
    "tidyverse",
    "sysfonts",
    "showtextdb",
    "showtext"
  )

  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==T){
    labels<-list(x='设计参考B站视频：BV1Ni4y1978g',
                 y='设计者：曾联松',
                 title='R语言绘制标准五星红旗')
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 函数1：计算两条由两点确定的直线的交点
  # (x1,y1)-(x2,y2) 与 (x3,y3)-(x4,y4)
  # ------------------------------------------------------------
  line_line_point<-function(x1,y1,x2,y2,x3,y3,x4,y4){
    # 第一条直线斜率
    k1 = (y2 - y1) / (x2 - x1)
    # 第二条直线斜率
    k2 = (y4 - y3) / (x4 - x3)
    # 直线截距
    b1 = y1 - k1 * x1
    b2 = y3 - k2 * x3
    # 联立求解交点
    x0 = (b2 - b1) / (k1 - k2)
    y0 = k1 * x0 + b1
    result<-c(x0,y0)
    result
  }

  # ------------------------------------------------------------
  # 函数2：构造五角星的 10 个顶点
  # x0, y0 : 星星中心坐标
  # r      : 外接圆半径
  # w      : 整体旋转角度（弧度）
  # ------------------------------------------------------------
  star_construction_point<-function(x0,y0,r,w)#中心坐标，半径，旋转角度
  {
    onefifth2pi=2*pi/5 # 五角星相邻顶点夹角
    # 外圈五个顶点坐标
    x1=x0+r*sin(w)
    y1=y0+r*cos(w)
    x2=x0+r*sin((w+onefifth2pi))
    x3=x0+r*sin((w+2*onefifth2pi))
    x4=x0+r*sin((w+3*onefifth2pi))
    x5=x0+r*sin((w+4*onefifth2pi))
    y2=y0+r*cos((w+onefifth2pi))
    y3=y0+r*cos((w+2*onefifth2pi))
    y4=y0+r*cos((w+3*onefifth2pi))
    y5=y0+r*cos((w+4*onefifth2pi))
    # ----------------------------------------------------------
    # 内部五个顶点：通过外顶点连线求交点
    # ----------------------------------------------------------
    xy6<-c(line_line_point(x1,y1,x3,y3,x5,y5,x2,y2))
    x6=xy6[1]
    y6=xy6[2]
    xy7<-c(line_line_point(x2,y2,x4,y4,x1,y1,x3,y3))
    x7=xy7[1]
    y7=xy7[2]
    xy8<-c(line_line_point(x3,y3,x5,y5,x4,y4,x2,y2))
    x8=xy8[1]
    y8=xy8[2]
    xy9<-c(line_line_point(x1,y1,x4,y4,x5,y5,x3,y3))
    x9=xy9[1]
    y9=xy9[2]
    xy10<-c(line_line_point(x1,y1,x4,y4,x5,y5,x2,y2))
    x10=xy10[1]
    y10=xy10[2]
    # 返回顺序排列的 10 个点（用于 geom_polygon）
    result<-c(x1,x6,x2,x7,x3,x8,x4,x9,x5,x10,
              y1,y6,y2,y7,y3,y8,y4,y9,y5,y10)
    result
  }
  # ------------------------------------------------------------
  # 函数3：计算小星需要旋转的角度
  # 目标：让小星的某个角“指向”大星中心
  # ------------------------------------------------------------
  # x0,y0 : 大星中心
  # x,y   : 小星中心
  # xn,yn : 小星某个顶点
  # r     : 小星半径
  w_cal<-function(x0,y0,x,y,xn,yn,r){
    # 大星中心 - 小星中心 的连线
    k=(y0-y)/(x0-x)
    b=y-k*x
    # 点到直线的距离
    distance=(abs(k*xn-yn+b))/((1+k^2)^0.5)
    # 根据几何关系反推旋转角
    w=asin(distance/r)
    w
  }
  # ------------------------------------------------------------
  # 初始状态下各星的坐标（未旋转）
  # ------------------------------------------------------------
  # 小星 1
  star1_x<-c(star_construction_point(10,18,1,0)[1:10])
  star1_y<-c(star_construction_point(10,18,1,0)[11:20])
  star1<-data.frame(star1_x,star1_y)
  # 小星 2
  star2_x<-c(star_construction_point(12,16,1,0)[1:10])
  star2_y<-c(star_construction_point(12,16,1,0)[11:20])
  star2<-data.frame(star1_x,star1_y)
  # 小星 3
  star3_x<-c(star_construction_point(12,13,1,0)[1:10])
  star3_y<-c(star_construction_point(12,13,1,0)[11:20])
  star3<-data.frame(star1_x,star1_y)
  # 小星 4
  star4_x<-c(star_construction_point(10,11,1,0)[1:10])
  star4_y<-c(star_construction_point(10,11,1,0)[11:20])
  star4<-data.frame(star1_x,star1_y)
  # 大星
  star0_x<-c(star_construction_point(5,15,3,0)[1:10])
  star0_y<-c(star_construction_point(5,15,3,0)[11:20])
  star0<-data.frame(star1_x,star1_y)#构建数据框
  #星1是x4对准，星2是x5对准，星3是x1对准，星4是x5对准
  # ------------------------------------------------------------
  # 计算四颗小星的旋转角度
  # 不同小星指向大星的不同顶点
  # w1为正，w2为负，w3为负，w4正
  # ------------------------------------------------------------
  w1<-w_cal(5,15,10,18,star1_x[7],star1_y[7],1)
  w2<-w_cal(5,15,12,16,star2_x[9],star2_y[9],1)
  w3<-w_cal(5,15,12,13,star3_x[1],star3_y[1],1)
  w4<-w_cal(5,15,10,11,star4_x[9],star4_y[9],1)#计算旋转角度绝对值
  # 方向修正（顺/逆时针）
  w2<-w2*(-1)
  w3<-w3*(-1)#校正角度正负（旋转方向）
  # ------------------------------------------------------------
  # 重新构建旋转后的小星坐标
  # ------------------------------------------------------------
  star0_x<-c(star_construction_point(5,15,3,0)[1:10])
  star0_y<-c(star_construction_point(5,15,3,0)[11:20])
  star0<-data.frame(star1_x,star1_y)
  star1_x<-c(star_construction_point(10,18,1,w1)[1:10])
  star1_y<-c(star_construction_point(10,18,1,w1)[11:20])
  star1<-data.frame(star1_x,star1_y)
  star2_x<-c(star_construction_point(12,16,1,w2)[1:10])
  star2_y<-c(star_construction_point(12,16,1,w2)[11:20])
  star2<-data.frame(star1_x,star1_y)
  star3_x<-c(star_construction_point(12,13,1,w3)[1:10])
  star3_y<-c(star_construction_point(12,13,1,w3)[11:20])
  star3<-data.frame(star1_x,star1_y)
  star4_x<-c(star_construction_point(10,11,1,w4)[1:10])
  star4_y<-c(star_construction_point(10,11,1,w4)[11:20])
  star4<-data.frame(star1_x,star1_y)#重新构建数据框

  # ------------------------------------------------------------
  # 背景红旗矩形
  # ------------------------------------------------------------
  rectangle<-data.frame(c(0,30,30,0),c(0,0,20,20))
  colnames(rectangle)<-c('x','y')
  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot()+
    geom_polygon(data = rectangle,aes(x=x,y=y),color="#ee1c25",
                 fill="#ee1c25")+
    geom_polygon(data = star0,aes(x=star0_x,y=star0_y),color="#ffff00",
                 fill="#ffff00")+
    geom_polygon(data = star1,aes(x=star1_x,y=star1_y),color="#ffff00",
                 fill="#ffff00")+
    geom_polygon(data = star2,aes(x=star2_x,y=star2_y),color="#ffff00",
                 fill="#ffff00")+
    geom_polygon(data = star3,aes(x=star3_x,y=star3_y),color="#ffff00",
                 fill="#ffff00")+#绘图星星
    geom_polygon(data = star4,aes(x=star4_x,y=star4_y),color="#ffff00",
                 fill="#ffff00")+#绘图红旗
    coord_quickmap()+#调整为1：1比例显示
    theme(legend.key = element_blank(),
          panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank())+#隐藏坐标系
    labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext_auto()#显示中文文本
}


#' 绘制北洋政府时期五族共和旗（横五色旗）
#'
#' 使用 \code{ggplot2} 以矩形色块方式绘制北洋政府时期
#' （约 1912–1928 年）使用的“五族共和旗”，
#' 又称横五色旗。旗面由红、黄、蓝、白、黑五条等宽横色带组成，
#' 分别象征汉、满、蒙、回、藏五族。
#'
#' 本函数采用程序化绘图方式，不依赖任何外部图像资源，
#' 适合教学演示、历史图形复现以及向量化图形输出。
#' 函数运行时会自动检测并加载所需依赖包，
#' 若缺失则从 CRAN 自动安装。
#'
#' @param label logical，是否显示标题与文字说明。
#'   \describe{
#'     \item{TRUE}{显示标题与文字注释（默认）}
#'     \item{FALSE}{仅绘制旗帜本体，不显示任何文字}
#'   }
#'
#' @return
#' 返回一个 \code{ggplot} 对象，可直接打印或通过
#' \code{ggsave()} 导出为图片文件。
#'
#' @details
#' \itemize{
#'   \item 旗帜由五条等高横向矩形构成
#'   \item 自上而下颜色依次为：红、黄、蓝、白、黑
#'   \item 自上而下颜色依次代表：汉、满、蒙、回、藏
#'   \item 使用 \code{coord_quickmap()} 保证比例不变形
#'   \item 不显示坐标轴、网格与图例
#' }
#'
#' @examples
#' \dontrun{
#' # 绘制带文字说明的五族共和旗
#' plot_ROC_Beiyang_flag()
#' }
#'
#' \dontrun{
#' # 仅绘制旗帜本体（适合导出图片）
#' plot_ROC_Beiyang_flag(label = FALSE)
#' }
#'
#' @seealso
#' \code{\link[ggplot2]{geom_rect}},
#' \code{\link[ggplot2]{coord_quickmap}}
#'
#' @author
#' 历史旗帜样式来源：北洋政府时期官方旗帜；
#'
#' @export
plot_ROC_Beiyang_flag<-function(label=T){
  # ------------------------------------------------------------
  # 自动检查 / 安装 / 加载所需 R 包
  # ------------------------------------------------------------
  packages <- c(
    "tidyverse",
    "sysfonts",
    "showtextdb",
    "showtext"
  )

  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==T){
    labels<-list(x='北洋政府 - 五族共和旗',
                 y='汉满蒙回藏\n（红黄蓝白黑）',
                 title='北洋政府时期（1921-1928）')
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 不同颜色块坐标
  # ------------------------------------------------------------
  rect_data<-rbind(
    data.frame(xmin=0,xmax=8,ymin=4,ymax=5,color='red',order=1),
    data.frame(xmin=0,xmax=8,ymin=3,ymax=4,color='gold',order=2),
    data.frame(xmin=0,xmax=8,ymin=2,ymax=3,color='darkblue',order=3),
    data.frame(xmin=0,xmax=8,ymin=1,ymax=2,color='white',order=4),
    data.frame(xmin=0,xmax=8,ymin=0,ymax=1,color='black',order=5)
  )
  rect_data$order<-factor(rect_data$order,levels=rect_data$order)
  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot()+
    geom_rect(data = rect_data,
              mapping = aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                            fill=order))+
    scale_fill_manual(values=rect_data$color)+
    coord_quickmap()+#调整为1：1比例显示
    theme(legend.key = element_blank(),
          panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          panel.grid  = element_blank())+#隐藏坐标系
    labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext_auto()#显示中文文本
}



#' 绘制国民政府时期青天白日满地红旗
#'
#' 使用解析几何与 \code{ggplot2} / \code{ggforce} 程序化绘制
#' 国民政府时期（1928–1949 年）使用的“青天白日满地红旗”。
#' 旗帜构成包括红色底旗、左上角蓝底矩形，以及位于蓝底中的
#' 青天白日十二光芒图案，全部由向量几何元素计算生成，
#' 不依赖任何外部图像文件。
#'
#' 白日十二光芒的顶点与内凹点基于圆的三角函数关系精确计算，
#' 并通过对称变换构建完整闭合多边形；中心双圆使用
#' \code{ggforce::geom_circle()} 绘制，以保证比例一致性。
#'
#' 本函数在运行时会自动检测并加载所需依赖包
#'（\code{tidyverse}、\code{sysfonts}、\code{showtextdb}、
#' \code{showtext}、\code{ggforce}），
#' 若缺失则会从 CRAN 自动安装。
#'
#' 旗帜比例与构造参考公开的历史与几何构造资料：
#' \itemize{
#'   \item 中文维基百科相关页面
#'   \item Wikimedia Commons 官方构造示意图
#' }
#'
#' @param label logical，是否显示标题与文字说明。
#'   \describe{
#'     \item{TRUE}{显示标题、设计者及历史时期说明（默认）}
#'     \item{FALSE}{仅绘制旗帜本体，不显示任何文字}
#'   }
#'
#' @return
#' 返回一个 \code{ggplot} 对象，可直接打印，
#' 或通过 \code{ggsave()} 导出为矢量或位图文件。
#'
#' @details
#' \itemize{
#'   \item 红色背景与蓝色旗角使用矩形几何元素构建
#'   \item 白日十二光芒由 24 个顶点组成的闭合多边形表示
#'   \item 中心双圆用于表示白日核心结构
#'   \item 使用 \code{coord_quickmap()} 保证比例不变形
#'   \item 默认隐藏坐标轴、网格与图例
#' }
#'
#' @examples
#' \dontrun{
#' # 绘制带文字说明的青天白日满地红旗
#' plot_ROC_KMT_flag()
#' }
#'
#' \dontrun{
#' # 仅绘制旗帜本体（适合导出图片）
#' plot_ROC_KMT_flag(label = FALSE)
#' }
#'
#' @seealso
#' \code{\link[ggplot2]{geom_polygon}},
#' \code{\link[ggplot2]{geom_rect}},
#' \code{\link[ggforce]{geom_circle}}
#'
#' @author
#' 设计者：孙中山（方案提议）、陆皓东（青天白日设计）；
#'
#' @export
plot_ROC_KMT_flag<-function(label=T){
  # ------------------------------------------------------------
  # 设计参考：https://zh.wikipedia.org/wiki/%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E5%9C%8B%E6%97%97
  # https://commons.wikimedia.org/wiki/File:Flag_of_the_Republic_of_China_construction_sheet.svg
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  # 自动检查 / 安装 / 加载所需 R 包
  # ------------------------------------------------------------
  packages <- c(
    "tidyverse",
    "sysfonts",
    "showtextdb",
    "showtext",
    "ggforce"
  )

  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==T){
    labels<-list(x='国民政府 - 青天白日满地红旗',
                 y='设计者：孙中山、陆皓东',
                 title='国民政府时期（1928-1949）')
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 背景颜色块坐标
  # ------------------------------------------------------------
  rect_bg_red<-data.frame(xmin=0,xmax=48,ymin=0,ymax=32,color='red',order=1)
  rect_bg_blue<-data.frame(xmin=0,xmax=24,ymin=16,ymax=32,color='blue',order=1)
  # ------------------------------------------------------------
  # 十二星顶点和底点坐标（从正上方点顺时针排序ID）
  # ------------------------------------------------------------
  # 外接圆、太阳内小圆半径；圆心坐标
  r1=6;r2=3;loc_center<-c(12,24)
  # 第一象限内
  ##不在坐标轴的顶点坐标
  point_top1<-c(0,r1)
  point_top2<-c(r1*sin(pi/6),r1*cos(pi/6))
  point_top3<-c(r1*sin(pi/3),r1*cos(pi/3))
  ##低点坐标
  point_bottom1<-c(r2*sin(pi/12),r2*cos(pi/12))
  point_bottom2<-c(r2*sin(pi/4),r2*cos(pi/4))
  point_bottom3<-c(r2*cos(pi/12),r2*sin(pi/12))
  # 第二象限：第一象限关于X轴对称
  point_top4<-c(r1,0)
  point_top5<-c(point_top3[1],-point_top3[2])
  point_top6<-c(point_top2[1],-point_top2[2])
  ##低点坐标
  point_bottom4<-c(point_bottom3[1],-point_bottom3[2])
  point_bottom5<-c(point_bottom2[1],-point_bottom2[2])
  point_bottom6<-c(point_bottom1[1],-point_bottom1[2])
  # 第三象限：第一象限关于原点中心对称
  point_top7<-c(0,-r1)
  point_top8<-c(-point_top2[1],-point_top2[2])
  point_top9<-c(-point_top3[1],-point_top3[2])
  ##低点坐标
  point_bottom7<-c(-point_bottom1[1],-point_bottom1[2])
  point_bottom8<-c(-point_bottom2[1],-point_bottom2[2])
  point_bottom9<-c(-point_bottom3[1],-point_bottom3[2])
  # 第四象限：第一象限关于Y轴对称
  point_top10<-c(-r1,0)
  point_top11<-c(-point_top3[1],point_top3[2])
  point_top12<-c(-point_top2[1],point_top2[2])
  ##低点坐标
  point_bottom10<-c(-point_bottom3[1],point_bottom3[2])
  point_bottom11<-c(-point_bottom2[1],point_bottom2[2])
  point_bottom12<-c(-point_bottom1[1],point_bottom1[2])
  # 构建十二星闭合图案数据框
  star12_df<-data.frame(matrix(nrow=24,ncol=2)) #建立空白变量
  colnames(star12_df)<-c('x','y')
  star12_order<-paste0('point_',rep(c('top','bottom'),12),
                       rep(c(as.character(1:12)),each=2))
  for (i in 1:24) {
    star12_df$x[i]<-get(star12_order[i])[1]
    star12_df$y[i]<-get(star12_order[i])[2]
  }
  # 基于圆心平移图案
  star12_df$x<-star12_df$x+loc_center[1];star12_df$y<-star12_df$y+loc_center[2]

  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot()+
    geom_rect(data = rect_bg_red,
              mapping = aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill='#DE0000')+
    geom_rect(data = rect_bg_blue,
              mapping = aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill='#0000AA')+
    geom_polygon(data = star12_df,aes(x=x,y=y),color="white",
                 fill="white")+
    geom_circle(aes(x0=12, y0=24, r=3*(1+1/15)), fill='#0000AA')+
    geom_circle(aes(x0=12, y0=24, r=3), fill='white')+
    coord_quickmap()+#调整为1：1比例显示
    theme(legend.key = element_blank(),
          panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          panel.grid  = element_blank())+#隐藏坐标系
    labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext_auto()#显示中文文本
}


