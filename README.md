
# ggChinaFlag

**ggChinaFlag** is an R package for programmatic construction and visualization of Chinese national, historical and political flags using **ggplot2** and analytic geometry.

本包基于解析几何方法，使用 **ggplot2** 纯代码方式绘制中国近现代不同时期的国旗及政党旗帜，
不依赖任何外部图片资源，适用于教学演示、历史图形复现以及可重复的矢量化绘图场景。

---

## ✨ Features | 功能特点

- 📐 完全基于几何计算构造旗帜 （不依赖外部图片）Pure geometric construction (no image files)  
- 🎨 **ggplot2**生成的矢量图 Vector graphics based on **ggplot2**  
- 🏳️ 支持多种历史国旗与政党标志  Supports multiple historical flags and party emblems  

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
```

### See available flag names 查看可用的旗帜名称

```r
FlatStorge()                # 默认 lang = "Chinese" （毕竟中国旗帜）

# 中文名称
FlatStorge("Chinese")
# 英文名称
FlatStorge("English")
```

返回的列表包含 `国旗` / `National Flags` 和 `政党` / `Political Parties` 两个类别，
每个类别下列出可用旗帜名称，可直接传入 `plotCNFlag()`。

### Current supported flags 当前支持的旗帜

| 类别 | 中文名称 | English name |
|------|----------|--------------|
| 🇨🇳 国旗 | 中华人民共和国国旗 🇨🇳 | Flag of the People's Republic of China |
|  | 中华民国青天白日旗 | Flag of the Republic of China (Blue Sky, White Sun, and Red Earth) |
|  | 中华民国北洋政府五色旗 | Five-Color Flag of the Beiyang Government of the Republic of China |
|  | 武昌起义铁血十八星旗 | Iron-Blood 18-Star Flag of the Wuchang Uprising |
| 🚩 政党 | 中国共产党党旗 | Flag of the Communist Party of China |
|  | 中国国民党党旗 | Flag of the Kuomintang (Blue Sky and White Sun flag) |
| 🚩 区旗 | 香港特别行政区区旗 🇭🇰 | Regional Flag of the Hong Kong Special Administrative Region |
|  | 澳门特别行政区区旗 🇲🇴 | Regional Flag of the Macao Special Administrative Region |
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
