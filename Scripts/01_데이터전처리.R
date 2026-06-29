# 1. 데이터 불러오기
file_path <- "Data/4.4.실습파일1_중부지역토기가마의구조및출토유물.csv"
df <- read.csv(file_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 2. 가마크기 필터링 함수 정의 및 적용
# 가마크기_길이 또는 가마크기_폭의 값이 '?', '-', 괄호 포함, 또는 빈칸인 행 제외
is_valid <- function(x) {
  !is.na(x) & x != "" & x != "?" & x != "-" & !grepl("(", x, fixed = TRUE) & !grepl(")", x, fixed = TRUE)
}

# subset 명령어를 우선적으로 사용하여 데이터 필터링
df_clean <- subset(df, is_valid(가마크기_길이) & is_valid(가마크기_폭))

# 가마크기 값을 수치형(Numeric)으로 변환
df_clean$가마크기_길이 <- as.numeric(df_clean$가마크기_길이)
df_clean$가마크기_폭 <- as.numeric(df_clean$가마크기_폭)

# 3. 재임방식 변환
# 재임방식이 '='이거나 '?'인 경우 '알 수 없음'으로 변환
# (실제 데이터의 '-' 및 빈값 ""도 결측치를 의미하므로 함께 '알 수 없음'으로 처리)
# '무시설식'은 그대로 유지, 그 외(할석, 할석+도침, 홈+할석, 홈내기식 등)는 '유시설식'으로 변환
df_clean$연소부_재임방식 <- ifelse(df_clean$연소부_재임방식 == "=" | 
                                 df_clean$연소부_재임방식 == "?" | 
                                 df_clean$연소부_재임방식 == "-" | 
                                 df_clean$연소부_재임방식 == "", "알 수 없음",
                               ifelse(df_clean$연소부_재임방식 == "무시설식", "무시설식", "유시설식"))

# 4. 권역별 데이터 분할
# split 함수를 사용하여 리스트 형태로 분할
df_split <- split(df_clean, df_clean$권역)

# 개별 데이터 프레임으로도 분할 저장
df_A <- subset(df_clean, 권역 == "A")
df_B <- subset(df_clean, 권역 == "B")
df_etc <- subset(df_clean, 권역 == "기타")