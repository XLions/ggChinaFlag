# ggChinaFlag

**ggChinaFlag** is an R package for programmatic construction and visualization of Chinese national and historical flags using **ggplot2** and analytic geometry.

本包基于解析几何方法，使用 **ggplot2** 纯代码方式绘制中国近现代不同时期的国旗，
不依赖任何外部图片资源，适用于教学演示、历史图形复现以及可重复的矢量化绘图场景。

---

## ✨ Features | 功能特点

- 📐 Pure geometric construction (no image files)
- 🎨 Vector graphics based on **ggplot2**
- 📚 Suitable for teaching, demonstration, and reproducible research
- 🏳️ Support multiple historical flags of China

- 完全基于几何计算构造旗帜  
- 不依赖 PNG / SVG 等外部图片  
- 输出为高质量矢量图  
- 支持中国近现代多种历史国旗  

---

## 📦 Usage | 使用方法

### Install from GitHub 安装

```r
# install.packages("devtools")
devtools::install_github("XLions/ggChinaFlag")
```

### Main function

#### `plotCNFlag(type, item, ...)`

Render a Chinese national or party flag.

- `type`: Flag category. Use `typeFlag()` to see available options.
- `item`: Specific flag name under the given type. Use `itemType(type)` to see available options.

```r
# Plot the national flag of the People's Republic of China
plotCNFlag(type = "nation", item = "PRChina")
```

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
ORCID: 0009-0007-3615-5724
