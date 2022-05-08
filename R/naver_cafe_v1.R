naver_cafe_v1 <- function(mykeywords, maxPage){

  remDR <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
  remDR$open()


  search_word <- c()
  cafe_link <- c()


  for (keyWord in mykeywords){
    for (pageNo in 1:maxPage){
      urls <- paste0("https://section.cafe.naver.com/ca-fe/home/search/articles?q=", keyWord, "&p=", pageNo)
      remDR$navigate(urls)
      Sys.sleep(1)
      
      get_url <- GET(urls)
      my_html <- read_html(get_url)
      
      source <- remDR$getPageSource()[[1]]
      html_source <- read_html(source)
      
      cafe_nodes <- html_nodes(html_source, 'div.detail_area > a')
      cafe_url <- html_attr(cafe_nodes, 'href')
      
      cafe_link <- c(cafe_link, cafe_url)
      for (i in 1:12) {search_word <- c(search_word, keyWord)}
    }
  }
  
  
  cafe_df <- data.frame(search_word, cafe_link)
  cafe_df$title <- ""
  cafe_df$date <- ""
  cafe_df$member <- ""
  cafe_df$text <- ""
  cafe_df$comment <- ""
  cafe_df$views <- ""
  
  
  for (i in 1:length(cafe_df$cafe_link)){
    remDR$navigate(cafe_df$cafe_link[i])
    find_frame <- remDR$findElements(using = 'css selector', value = '#cafe_main')
    remDR$switchToFrame(find_frame[[1]])
    Sys.sleep(1)
    source <- remDR$getPageSource()[[1]]
    html_source <- read_html(source)
    
    title1 <- html_text(html_nodes(html_source, 'h3.title_text'))
    date1 <- html_text(html_nodes(html_source, '#app > div > div > div.ArticleContentBox > div.article_header > div.WriterInfo > div > div.article_info > span.date'))
    member1 <- html_text(html_nodes(html_source, '#app > div > div > div.ArticleContentBox > div.article_header > div.WriterInfo > div > div.profile_info > div'))
    text1 <- html_text(html_nodes(html_source, '#app > div > div > div.ArticleContentBox > div.article_container > div.article_viewer > div > div.content.CafeViewer > div > div'))
    comment1 <- html_text(html_nodes(html_source, 'ul.comment_list'))
    view1 <- as.numeric(gsub('조회 ', '', html_text(html_nodes(html_source, 'span.count'))))
    
    
    try({cafe_df$title[i] <- title1},
        silent = T)
    try({cafe_df$date[i] <- date1},
        silent = T)
    try({cafe_df$member[i] <- member1},
        silent = T)
    try({cafe_df$text[i] <- text1},
        silent = T)
    try({cafe_df$comment[i] <- comment1}, 
        silent = T)
    try({cafe_df$views[i] <- view1},
        silent = T)
  
    }

  return(cafe_df)

}