
# ggChinaFlag

**ggChinaFlag** is an R package for programmatic construction and visualization of Chinese national, historical, political, organisational and military flags using **ggplot2** and analytic geometry.

本包基于解析几何方法，使用 **ggplot2** 纯代码方式绘制中国近现代不同时期的国旗、政党旗帜、组织旗帜、区旗及军事旗帜，
不依赖任何外部图片资源，适用于教学演示、历史图形复现以及可重复的矢量化绘图场景。

---

## ✨ Features | 功能特点

- 📐 完全基于几何计算构造旗帜 （不依赖外部图片）Pure geometric construction (no image files)  
- 🎨 **ggplot2** 生成的矢量图 Vector graphics based on **ggplot2**  
- 🏳️ 支持多种历史国旗与政党标志 Supports multiple historical flags and party emblems  
- 🚩 支持中国共产主义青年团团旗（依据 GB/T 40055-2021）Supports the CYLC flag (per GB/T 40055-2021)  
- 🎖️ 支持中国人民解放军军旗及陆军、海军、空军、火箭军、军事航天部队、网络空间部队、信息支援部队、联勤保障部队等军种旗 Supports the PLA flag and service branch flags (Army, Navy, Air Force, Rocket Force, Aerospace Force, Cyberspace Force, Information Support Force, Joint Logistics Support Force)  
- 🔍 统一接口 `plotCNFlag()`，可通过中文或英文名称直接调用所有旗帜 Unified interface `plotCNFlag()` to plot any supported flag by Chinese or English name  

---

## 📦 Usage | 使用方法

### Install  安装

```r
install.packages("ggChinaFlag") # From CRAN

# install.packages("devtools")
devtools::install_github("XLions/ggChinaFlag") # From GitHub
```

### Main function 主函数

`plotCNFlag(input, label = TRUE)`

- `input` : 旗帜名称，支持中文或英文（详见下方列表）。
- `label` : 是否显示标题与文字说明（默认 `TRUE`）。

```r
library(ggChinaFlag)

# 绘制中华人民共和国国旗
plotCNFlag("中华人民共和国国旗")

# 使用英文名称绘制（不显示文字标签）
plotCNFlag("Iron-Blood 18-Star Flag of the Wuchang Uprising", label = FALSE)

# 绘制共青团团旗
plotCNFlag("中国共产主义青年团团旗")

# 绘制解放军海军军旗
plotCNFlag("中国人民解放军海军军旗")
plotCNFlag("PLA Navy Flag", label = FALSE)
```

### See available flag names 查看可用的旗帜名称

```r
FlagStorage()                # 默认 lang = "Chinese"

# 中文名称
FlagStorage("Chinese")
# 英文名称
FlagStorage("English")
```

返回的列表包含 `国旗` / `National Flags`、`政党` / `Political Parties`、`区旗` / `Regional Flags`、`组织` / `Organizations` 和 `军事` / `Military` 五个类别，每个类别下列出可用旗帜名称，可直接传入 `plotCNFlag()`。

### Current supported flags 当前支持的旗帜

以下所有旗帜均可通过 `plotCNFlag()` 使用中文或英文名称直接调用，也可通过对应的专用函数调用。

| 类别 | 中文名称 | English name | 专用函数 |
|------|----------|--------------|----------|
| 🇨🇳 国旗 | 中华人民共和国国旗 | Flag of the People's Republic of China | `plot_P.R.CHINA_flag()` |
|  | 中华民国青天白日旗 | Flag of the Republic of China (Blue Sky, White Sun, and Red Earth) | `plot_ROC_KMT_flag()` |
|  | 中华民国北洋政府五色旗 | Five-Color Flag of the Beiyang Government of the Republic of China | `plot_ROC_Beiyang_flag()` |
|  | 武昌起义铁血十八星旗 | Iron-Blood 18-Star Flag of the Wuchang Uprising | `plot_Han18Star()` |
| 🚩 政党 | 中国共产党党旗 | Flag of the Communist Party of China | `plot_CCP()` |
|  | 中国国民党党旗 | Flag of the Kuomintang (Blue Sky and White Sun flag) | `plot_KMT()` |
| 🚩 区旗 | 香港特别行政区区旗 🇭🇰 | Regional Flag of the Hong Kong Special Administrative Region | `plot_HK_SAR_flag()` |
|  | 澳门特别行政区区旗 🇲🇴 | Regional Flag of the Macao Special Administrative Region | `plot_Macao_SAR_flag()` |
| 🚩 组织 | 中国共产主义青年团团旗 | Flag of the Communist Youth League of China | `plot_CYLC()` |
| 🎖️ 军事 | 中国人民解放军军旗 | General PLA Flag | `plot_PLA(subtype = "general")` |
|  | 中国人民解放军陆军军旗 | PLA Ground Force Flag | `plot_PLA(subtype = "陆军")` |
|  | 中国人民解放军海军军旗 | PLA Navy Flag | `plot_PLA(subtype = "海军")` |
|  | 中国人民解放军空军军旗 | PLA Air Force Flag | `plot_PLA(subtype = "空军")` |
|  | 中国人民解放军火箭军军旗 | PLA Rocket Force Flag | `plot_PLA(subtype = "火箭军")` |
|  | 中国人民解放军军事航天部队军旗 | PLA Aerospace Force Flag | `plot_PLA(subtype = "军事航天部队")` |
|  | 中国人民解放军网络空间部队军旗 | PLA Cyberspace Force Flag | `plot_PLA(subtype = "网络空间部队")` |
|  | 中国人民解放军信息支援部队军旗 | PLA Information Support Force Flag | `plot_PLA(subtype = "信息支援部队")` |
|  | 中国人民解放军联勤保障部队军旗 | PLA Joint Logistics Support Force Flag | `plot_PLA(subtype = "联勤保障部队")` |

> 注：`plot_PLA()` 的 `subtype` 参数也接受英文名称或缩写（如 `"Navy"`、`"PLA Rocket Force"` 等），详见函数文档。

---

## 📖 Background | 历史背景

This package is intended for **educational and academic use only**.  
All flag designs follow publicly available historical construction specifications.

本包仅用于教学、科研和历史展示用途，  
旗帜构造参考公开历史资料，不涉及任何政治立场。

---

## 📜 License

GPL-3 © Zhaoshuo Liu

---

## 👤 Author

**Zhaoshuo Liu**  
ORCID: [0009-0007-3615-5724](https://orcid.org/0009-0007-3615-5724)
