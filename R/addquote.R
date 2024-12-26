addquote <- function(input_string) {

if (base::missing(input_string)) {
cat(" \033[1;36m# 명령문 예제 ---------- \033[0m", '\n' )
cat(" \033[1;36m 변수리스트 <- '변수1, 변수2, 변수3' \033[0m", '\n' )
return( cat(" \033[1;36m addquote(변수리스트) \033[0m", '\n' )) }

words <- strsplit(input_string, ",")[[1]]
words <- trimws(words) # 각 단어의 앞뒤 공백 제거
# 각 단어에 홑따옴표를 붙이고, 콤마로 구분된 문자열 생성
quoted_words <- paste0("'", words, "'")
result <- paste(quoted_words, collapse = ", ")
return(result)
}
