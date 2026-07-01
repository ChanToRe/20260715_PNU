# 03_8_산점도_단일.R
# 고고학 자료 탐색적 데이터 분석(EDA) - 단일 산점도 고해상도 출력

library(ggplot2)

# 한글 폰트 설정 (Windows 폰트 데이터베이스에 등록)
windowsFonts(Malgun = windowsFont("Malgun Gothic"))

# 데이터 로드
df <- read.csv("Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 데이터 전처리 (수치형 컬럼 변환 및 결측치 제거)
df$가마크기_길이 <- as.numeric(df$가마크기_길이)
df$가마크기_폭 <- as.numeric(df$가마크기_폭)
df_scat <- subset(df, !is.na(가마크기_길이) & !is.na(가마크기_폭))

# 대옹_출토 변수 가공
df_scat$대옹_출토 <- ifelse(df_scat$대옹_출토 == "●", "출토", "미출토")

# ggplot 작성 (Identity position, theme_bw, Set1 팔레트, color=대옹_출토, 범례 좌상단, 지정 타이틀 중앙정렬)
p <- ggplot(df_scat, aes(x = 가마크기_길이, y = 가마크기_폭, color = 대옹_출토)) +
  geom_point(position = "identity", size = 3, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "가마크기와 대옹출토 여부 관계", x = "가마 길이 (m)", y = "가마 폭 (m)", color = "대옹 출토여부") +
  theme_bw(base_size = 19.8, base_family = "Malgun") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11 * 1.8),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80", linewidth = 0.3),
    legend.title = element_text(size = 8 * 1.8),
    legend.text = element_text(size = 7 * 1.8)
  )

# 저장 규격 정의 (5:5 비율, 300dpi)
width_inch <- 5
height_inch <- 5
dpi_val <- 300

# 1. TIFF 형식 저장 (LZW 압축 적용)
tiff("Results/산점도.tiff", width = width_inch * dpi_val, height = height_inch * dpi_val, res = dpi_val, compression = "lzw")
print(p)
dev.off()

# 2. JPEG 형식 저장
jpeg("Results/산점도.jpeg", width = width_inch * dpi_val, height = height_inch * dpi_val, res = dpi_val, quality = 95)
print(p)
dev.off()

# 3. PNG 형식 저장
png("Results/산점도.png", width = width_inch * dpi_val, height = height_inch * dpi_val, res = dpi_val)
print(p)
dev.off()

cat("단일 산점도 TIFF, JPEG 및 PNG 파일 저장이 완료되었습니다.\n")
