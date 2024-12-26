find_col <- function(dataset_name, ...) {

if (base::missing(dataset_name)) {
cat(" \033[1;36m# 명령문 예제 ---------- \033[0m", '\n' )
return( cat("\033[1;36m 명령문: find_col(df, 변수명1, 변수명2) \033[0m", '\n') )
}

# 데이터셋 이름을 문자열로 변환 후 실제 객체로 가져오기
dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)

# 선택된 변수 이름을 가져오기
var_names <- as.character(substitute(list(...)))[-1] # 첫 번째 항목은 "list"

# 변수들의 인덱스 찾기
column_indices <- match(var_names, names(dataset))

# 결과 반환
return(setNames(column_indices, var_names))
}

