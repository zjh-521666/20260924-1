# ==========================================
# 1. 环境准备与数据加载
# ==========================================
setwd("C:/Users/zhang/Desktop")

library(ggplot2)
library(dplyr)
library(zoo)
library(readxl)
library(data.table)
library(writexl) 

# === 核心文件路径设置 ===
FILE_NAME <- "Summary-MCM CG-2Summary.xlsx"          # <--- 如果你的文件名不同，请在这里修改
RESULT_EXCEL <- "批量处理结果汇总.xlsx"
RESULT_PDF <- "批量图表汇总.pdf"

# === 第一阶段：基础清洗参数 ===
SMOOTH_WINDOW <- 200     # 滑动平均窗口大小
DROP_TIME <- 150         # 初始抛弃的前 N 秒时间

# === 第二阶段：快速增长期提取参数 ===
SLOPE_THRESHOLD <- 0.1    # 1. 瞬时斜率门槛
MIN_POINTS <- 5          # 2. 整个片段最少包含的点数
MIN_DISTANCE_GAINED <- 5 # 3. 纵坐标最小增长量门槛
TOLERANCE_GAP <- 5        # 4. 允许内部断裂/停留的最大点数
REVERSION_RATIO <- 0.2    # 5. 回撤容忍比例 (0.2 即 20%)
LOOKAHEAD_WINDOW <- 10   # 6. 向后观察防跌落的点数

# ==========================================
# 2. 核心提取算法
# ==========================================
find_and_fit_growth <- function(df, threshold, min_length, min_distance, max_gap_points, reversion_ratio, lookahead_window) {
  rapid_indices <- which(df$Instant_Slope > threshold & !is.na(df$Instant_Slope))
  if (length(rapid_indices) == 0) return(data.frame()) 
  
  breaks <- c(0, cumsum(diff(rapid_indices) > max_gap_points))
  groups <- split(rapid_indices, breaks)
  
  candidates <- list()
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    start_idx <- g[1]
    end_idx <- min(g[length(g)] + 1, nrow(df)) 
    
    if ((end_idx - start_idx) >= min_length) {
      dist_gained <- df$Distance_smooth[end_idx] - df$Distance_smooth[start_idx]
      if (!is.na(dist_gained) && dist_gained > min_distance) {
        candidates[[length(candidates) + 1]] <- list(start_idx = start_idx, end_idx = end_idx, dist_gained = dist_gained)
      }
    }
  }
  if (length(candidates) == 0) return(data.frame())
  
  final_regions <- list()
  for (i in seq_along(candidates)) {
    curr <- candidates[[i]]
    curr_end <- curr$end_idx
    
    if (i < length(candidates)) {
      look_until <- min(curr_end + lookahead_window, candidates[[i+1]]$start_idx)
    } else {
      look_until <- min(curr_end + lookahead_window, nrow(df))
    }
    is_valid <- TRUE 
    
    if (look_until > curr_end) {
      post_peak_data <- df$Distance_smooth[curr_end:look_until]
      min_in_valley <- min(post_peak_data, na.rm = TRUE) 
      drop_dist <- df$Distance_smooth[curr_end] - min_in_valley 
      if (drop_dist > reversion_ratio * curr$dist_gained) {
        is_valid <- FALSE 
      }
    }
    
    if (is_valid) {
      segment_data <- df[curr$start_idx:curr$end_idx, ]
      fit_model <- lm(Distance_smooth ~ Time, data = segment_data)
      final_regions[[length(final_regions) + 1]] <- data.frame(
        start_time = df$Time[curr$start_idx],
        end_time = df$Time[curr$end_idx],
        distance_gained = as.numeric(curr$dist_gained),
        fit_rate = as.numeric(coef(fit_model)["Time"]),        
        fit_intercept = as.numeric(coef(fit_model)["(Intercept)"]),
        r_squared = as.numeric(summary(fit_model)$r.squared)        
      )
    }
  }
  result_df <- bind_rows(final_regions)
  if(nrow(result_df) == 0) return(data.frame()) else return(result_df)
}

# ==========================================
# 3. 批量循环处理所有 Sheet
# ==========================================
# 自动获取 Excel 文件中所有的 Sheet 名称 (你的自定义命名将在这里被抓取)
sheet_names <- excel_sheets(FILE_NAME)

all_results <- list() # 用于存放每张表的结果
all_plots <- list()   # 用于存放每张表的绘图对象

cat("开始批量处理...\n")

for (sheet in sheet_names) {
  cat(sprintf("正在处理表: %s ...\n", sheet))
  
  # 1. 读取当前 sheet
  data1 <- read_xlsx(FILE_NAME, sheet = sheet)
  
  # 2. 基础清洗与截断
  data1_temp <- data1 %>%
    arrange(Time) %>%
    filter(Time > DROP_TIME) %>%
    mutate(Distance_smooth = rollmean(Distance, k = SMOOTH_WINDOW, fill = NA))
  
  if(nrow(data1_temp) == 0) next # 防止空数据报错
  
  min_idx <- which.min(data1_temp$Distance_smooth)
  
  data1_processed <- data1_temp %>%
    slice(min_idx:n()) %>%
    mutate(
      Time = Time - first(Time),
      zero_ref = first(Distance_smooth),
      Distance = Distance - zero_ref,
      Distance_smooth = Distance_smooth - zero_ref
    ) %>%
    select(-zero_ref) %>%
    mutate(
      Instant_Slope = (lead(Distance_smooth) - Distance_smooth) / (lead(Time) - Time)
    )
  
  # 3. 提取整体特征
  max_idx <- which.max(data1_processed$Distance_smooth)
  max_time <- data1_processed$Time[max_idx]
  max_dist <- data1_processed$Distance_smooth[max_idx]
  overall_rate <- max_dist / max_time
  
  # 4. 寻找局部快速增长期 (限定在最高点之前)
  data_for_growth <- data1_processed %>% slice(1:max_idx)
  
  fitted_regions <- find_and_fit_growth(
    df = data_for_growth, threshold = SLOPE_THRESHOLD, min_length = MIN_POINTS, 
    min_distance = MIN_DISTANCE_GAINED, max_gap_points = TOLERANCE_GAP,
    reversion_ratio = REVERSION_RATIO, lookahead_window = LOOKAHEAD_WINDOW
  )
  
  # 5. 整理当前 sheet 的汇总数据
  if (nrow(fitted_regions) > 0) {
    # 如果有快速增长期，将全局数据和局部数据横向拼接，第一列填入自定义的 sheet 名字
    sheet_summary <- fitted_regions %>%
      mutate(
        `轨迹名称(Sheet)` = sheet,
        `整体最高点时间(s)` = round(max_time, 2),
        `整体最高点距离` = round(max_dist, 2),
        `整体平均速率` = round(overall_rate, 4),
        `片段序号` = row_number(),
        `片段起始时间(s)` = round(start_time, 2),
        `片段结束时间(s)` = round(end_time, 2),
        `片段距离增量` = round(distance_gained, 2),
        `片段速率(Rate)` = round(fit_rate, 4),
        `拟合优度(R2)` = round(r_squared, 4)
      ) %>%
      select(
        `轨迹名称(Sheet)`, `整体最高点时间(s)`, `整体最高点距离`, `整体平均速率`,
        `片段序号`, `片段起始时间(s)`, `片段结束时间(s)`, `片段距离增量`, `片段速率(Rate)`, `拟合优度(R2)`
      )
  } else {
    # 如果没有找到快速增长期，依然保留该 sheet 的全局数据，片段填 NA
    sheet_summary <- data.frame(
      `轨迹名称(Sheet)` = sheet, `整体最高点时间(s)` = round(max_time, 2),
      `整体最高点距离` = round(max_dist, 2), `整体平均速率` = round(overall_rate, 4),
      `片段序号` = NA, `片段起始时间(s)` = NA, `片段结束时间(s)` = NA, 
      `片段距离增量` = NA, `片段速率(Rate)` = NA, `拟合优度(R2)` = NA,
      check.names = FALSE
    )
  }
  
  all_results[[sheet]] <- sheet_summary
  
  # 6. 生成图表并存入列表
  p <- ggplot(data1_processed, aes(x = Time)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
    geom_line(aes(y = Distance), color = "gray70", alpha = 0.5, linewidth = 0.5) +
    geom_line(aes(y = Distance_smooth), color = "black", linewidth = 0.8) +
    annotate("point", x = max_time, y = max_dist, color = "darkorange", size = 3) +
    annotate("segment", x = 0, y = 0, xend = max_time, yend = max_dist, 
             color = "darkorange", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = max_time / 2, y = max_dist / 2, 
             label = paste("Overall Rate =", round(overall_rate, 4)), 
             vjust = -1.5, color = "darkorange", fontface = "bold", size = 3) +
    theme_minimal() + 
    labs(
      title = paste("Sheet:", sheet, "- Pre-Peak Rapid Phases"), # 标题会自动显示你的命名
      subtitle = "Orange: Overall rate | Red: Solid rapid growth regions",
      y = "Distance (Zeroed)", x = "Time (Zeroed)"
    )
  
  if (nrow(fitted_regions) > 0) {
    for (i in 1:nrow(fitted_regions)) {
      t_start <- fitted_regions$start_time[i]
      t_end <- fitted_regions$end_time[i]
      slope <- fitted_regions$fit_rate[i]
      intercept <- fitted_regions$fit_intercept[i]
      
      y_start <- slope * t_start + intercept
      y_end <- slope * t_end + intercept
      
      p <- p + 
        annotate("segment", x = t_start, y = y_start, xend = t_end, yend = y_end, color = "red", linewidth = 1.2) +
        annotate("point", x = c(t_start, t_end), y = c(y_start, y_end), color = "red", size = 2) +
        annotate("text", x = (t_start + t_end)/2, y = max(y_start, y_end), 
                 label = paste("Rate:", round(slope, 3)), vjust = -1, color = "red", fontface = "bold", size = 3)
    }
  }
  
  all_plots[[sheet]] <- p
}

# ==========================================
# 4. 汇总导出数据与图表
# ==========================================
cat("\n整合数据中...\n")
final_table <- bind_rows(all_results)

# 控制台打印最终表格 (方便随时预览)
print(as.data.frame(final_table), row.names = FALSE)

# 1. 导出 Excel 总表
write_xlsx(final_table, RESULT_EXCEL)
cat(sprintf("✅ 数据已成功汇总并保存至桌面: %s\n", RESULT_EXCEL))

# 2. 导出所有图表到一个多页的 PDF 中
cat("正在生成 PDF 图表汇编...\n")
pdf(RESULT_PDF, width = 10, height = 6) 
for (sheet in names(all_plots)) {
  print(all_plots[[sheet]])
}
invisible(dev.off()) # 关闭 PDF 设备
cat(sprintf("✅ 图表已全部保存至桌面: %s\n", RESULT_PDF))

cat("全部批量处理完成！\n")