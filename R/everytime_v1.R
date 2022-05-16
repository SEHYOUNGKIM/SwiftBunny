login_everytime <- function(id, pw){
  remDR <<- remoteDriver(port = 4445L, browserName = "chrome") #사용할 브라우저 
  remDR$open()
  
  remDR$navigate('https://everytime.kr/')
  Sys.sleep(1)
  
  login_button <- remDR$findElement(using = "css selector", value = "body > aside > div.login > a.button.login")
  
  login_button$clickElement()
  
  id_elem <- remDR$findElement(using = 'css selector', value = '#container > form > p:nth-child(1) > input')
  id_elem$setElementAttribute("value", id)
  
  pw_elem <- remDR$findElement(using = 'css selector', value = '#container > form > p:nth-child(2) > input')
  pw_elem$setElementAttribute("value", pw)
  
  remDR$findElement(using = 'css selector', value = '#container > form > p.submit > input')$clickElement()
  Sys.sleep(1)
}


everytime_myCatergory <- function(category){
  if(category == '자유게시판'){
    remDR$navigate('https://everytime.kr/377391')  
  }else if (category == '비밀게시판'){
    remDR$navigate('https://everytime.kr/258788')
  }else if (category == '졸업생게시판'){
    remDR$navigate('https://everytime.kr/385983')  
  }else if (category == '새내기게시판'){
    remDR$navigate('https://everytime.kr/385892')  
  }else if (category == '시사이슈'){
    remDR$navigate('https://everytime.kr/482604')  
  }else if (category == '정보게시판'){
    remDR$navigate('https://everytime.kr/258790')  
  }else if (category == '홍보게시판'){
    remDR$navigate('https://everytime.kr/367461')  
  }else if (category == '동아리학회'){
    remDR$navigate('https://everytime.kr/418796')  
  }else if (category == '취업진로'){
    remDR$navigate('https://everytime.kr/377470')
  }else{
    print('정확히 입력해주세요.')
  }
}


get_everytime_data <- function(max_range){
  
  title <- c()
  content <- c()
  date <- c()
  
  
  for (page in 1:max_range){
  for (num in 1:20){
    urls <- paste0('#container > div.wrap.articles > article:nth-child(', num+1 ,') > a')
    
    remDR$findElement(using = 'css selector', value = urls)$clickElement()
    Sys.sleep(1)
    source <- remDR$getPageSource()[[1]]
    html_source <- read_html(source)
  
    try({
      content_node <- html_nodes(html_source, '#container > div.wrap.articles > article > a > p')
      content <- c(content, html_text(content_node))
      
      title_node <- html_nodes(html_source, '#container > div.wrap.articles > article > a > h2')
      title <- c(title, html_text(title_node))
      
      date_node <- html_nodes(html_source, '#container > div.wrap.articles > article > a > div.profile > time')
      date <- c(date, html_text(date_node))
    
    }, silent = T)
    
    remDR$goBack()
    Sys.sleep(2)
  }
  
  remDR$findElement(using = 'css selector', value = '#container > div.wrap.articles > div.pagination > a')$clickElement()
  Sys.sleep(2)
  }
  
  return(data.frame(title, date, content))
}


lectures <- function(keyword){
  remDR$navigate('https://everytime.kr/lecture')
  Sys.sleep(1)
  
  lecture_elem <- remDR$findElement(using = 'css selector', value = '#container > form > input.keyword')
  lecture_elem$setElementAttribute("value", keyword)

  remDR$findElement(using = 'css selector', value = '#container > form > input.submit')$clickElement()
  Sys.sleep(1)
  
    
  source <- remDR$getPageSource()[[1]]
  html_source <- read_html(source)  
  my_nodes <- html_nodes(html_source, 'div > a.lecture')
  links <- html_attr(my_nodes, 'href')
  links

  text <- c()
  rating <- c()
  professor <- c()
  
  for (link in links) {
    remDR$navigate(paste0('https://everytime.kr/',link))
    Sys.sleep(1)
    
    try({
    source <- remDR$getPageSource()[[1]]
    html_source <- read_html(source)
    
    text_nodes <- html_nodes(html_source, 'div.articles > article > p.text')
    text1 <- html_text(text_nodes)
    text <- c(text, text1)
    
    rating_nodes <- html_nodes(html_source, 'p.rate > span > span.on')
    rating1 <- html_attr(rating_nodes, 'style')
    rating <- c(rating, rating1)
    
    prof_nodes <- html_nodes(html_source, '#container > div.side.head > p:nth-child(2) > span')
    professor1 <- html_text(prof_nodes)
    
    prof_rating <- html_nodes(html_source, '#container > div.side.article > div.rating > div.rate > span > span.value')
    
    if (html_text(prof_rating) != 0){
    for (i in 1:length(text1)){
    professor <- c(professor, professor1)}
    }}
    
    )}

  
  return(data.frame(text, rating, professor))
}


