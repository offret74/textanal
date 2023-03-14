# R에 KoNLP 설치하기

install.packages("multilinguer")
library(multilinguer)
install_jdk()
#이때 mac 비밀번호를 입력해줘야한다.

install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
