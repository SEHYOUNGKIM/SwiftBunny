daum_news_for_LDA <- function(category, maxPage){
  print("thank you seHyoung!!")
  news_href <- c()
  for (pageNo in 1:maxPage){ 
    urls <- paste0('https://news.daum.net/breakingnews', category, '?page=', pageNo)
    
    get_url <- GET(urls) 
    my_html <- read_html(get_url) 
    my_nodes <- html_nodes(my_html, 'strong > a.link_txt')
    my_href <- html_attr(my_nodes, 'href') 
    news_href <- c(news_href, my_href)
    
  }
  
  
  news_text <- c()
  for (news_url in news_href){
    get_url1 <- GET(news_url)
    my_html1 <- read_html(get_url1)
    
    my_nodes1 <- html_nodes(my_html1, 'section')
    news <- html_text(my_nodes1) 
    news_text <- c(news_text, news)
    
    return(news_text)
  }
}
