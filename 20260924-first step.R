# ==========================================
# 1. 环境准备与数据加载
# ==========================================
setwd("C:/Users/zhang/Desktop")

library(dplyr)
library(readxl)
library(writexl) 

# === 核心文件路径设置 ===
FILE_NAME <- "Summary-MCM CG-2.xlsx"                       # <--- 输入的原始文件名
RESULT_SEPARATE_EXCEL <- "Summary-MCM CG-2Summary.xlsx" # <--- 输出的独立Sheet文件名

# ==========================================
# 2. 批量循环处理所有 Sheet（仅执行公式转换，保持结构独立）
# ==========================================
sheet_names <- excel_sheets(FILE_NAME)
all_converted_sheets <- list() # 创建空列表，用于存放每张表转换后的独立数据框

cat("开始处理原始结果，保持多Sheet结构独立输出...\n")

for (sheet in sheet_names) {
  cat(sprintf("正在处理表: %s ...\n", sheet))
  
  # 1. 读取当前 sheet
  data1 <- read_xlsx(FILE_NAME, sheet = sheet)
  
  # 🔒 安全机制：强制规范前两列列名（确保第一列为距离Distance，第二列为时间Time）
  colnames(data1)[1:2] <- c("Distance", "Time")
  
  # 🧪 核心公式代入：将第一列的原始距离线性映射为 单链 DNA 长度 (nt)
  # 💡 提示：如果您连公式也不想带、需要完全百分之百的仪器物理原始数值，可以在这行代码最前面加 # 号注释掉
  data1$Distance <- ((data1$Distance - 5.97) / (8.44 - 5.97)) * 17853
  
  # 2. 仅提取转换后的 时间 和 长度，不进行任何清洗、截断、平滑或移位归零
  data1_processed <- data1 %>%
    select(
      `Time` = Time,
      `Distance` = Distance
    )
  
  # 🔑 核心逻辑：以原本的 sheet 名称作为 List 的键名存储
  all_converted_sheets[[sheet]] <- data1_processed
}

# ==========================================
# 3. 批量导出为多 Sheet 的 Excel 文件
# ==========================================
cat("\n正在写出多Sheet Excel文件...\n")

# 🌟 直接将包含多个数据框的 List 传给 write_xlsx，即可实现每个数据框独立为一个 Sheet
write_xlsx(all_converted_sheets, RESULT_SEPARATE_EXCEL)

cat(sprintf("\n✅ 转换完成！独立的【多Sheet时间-Distance表格】已成功保存至桌面: %s\n", RESULT_SEPARATE_EXCEL))