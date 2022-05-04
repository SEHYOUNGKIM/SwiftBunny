fifaOnline_Stats_v1 <- function(myPlayer) {
  remDR <- remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
  urls <- "https://fifaonline4.inven.co.kr/dataninfo/player/"
  
  remDR$open()
  remDR$navigate(urls)
  
  
  
  fifa_ovr <- c()
  fifa_wage <- c()
  fifa_fast <- c()
  fifa_mfast <- c()
  fifa_physical <- c()
  fifa_height <- c()
  fifa_rating <- c()
  fifa_position <- c()
  result_players <- c()


  for (player in myPlayer){
  
    
    player_name <- remDR$findElement("css selector", "#fifaonline4Db > div.fifa4.db_filter > ul.fifa4.player_filter.down.clearfix.act > li:nth-child(16) > div.fifa4.value.clearfix > input")
    player_name$setElementAttribute("value", player)
  
  
    click_elem <- remDR$findElement("css selector", '#fifaonline4Db > div.fifa4.db_filter > ul.fifa4.player_filter.down.clearfix.act > li.fifa4.btn_area.clearfix > button.btn.search')
    click_elem$clickElement()
    
    Sys.sleep(1)
  
  
    source <- remDR$getPageSource()[[1]]
    html_source <- read_html(source)
  
    nodes <- html_nodes(html_source,'span > b')
    fifa_players <- html_text(nodes)

  
    for (num in 1:length(fifa_players)) {
      selector_ovr <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(2) > p')
      selector_wage <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(3) > p')
      selector_fast <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(4) > p')
      selector_mfast <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(5) > p')
      selector_physical <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(6) > p')
      selector_height <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(7) > p')
      selector_rating <- paste0('#list_table > tbody > tr:nth-child(',num, ') > td:nth-child(8) > p')
      selector_position <- paste0('#list_table > tbody > tr:nth-child(',num,') > td.info.clearfix > div.fifa4.player_info.clearfix > a.fifa4.text_area > span.fifa4.stat.clearfix > span.fifa4.preferredposition1')

      ovr <- as.numeric(html_text(html_nodes(html_source, selector_ovr)))
      wage <- as.numeric(html_text(html_nodes(html_source, selector_wage)))
      fast <- as.numeric(html_text(html_nodes(html_source, selector_fast)))
      mfast <- as.numeric(html_text(html_nodes(html_source, selector_mfast)))
      physical <- as.numeric(html_text(html_nodes(html_source, selector_physical)))
      height <- as.numeric(html_text(html_nodes(html_source, selector_height)))
      rating <- as.numeric(html_text(html_nodes(html_source, selector_rating)))
      positions <- html_attr(html_nodes(html_source, selector_position), 'data-position')
      
      fifa_ovr <- c(fifa_ovr, ovr)
      fifa_wage <- c(fifa_wage, wage)
      fifa_fast <- c(fifa_fast, fast)
      fifa_mfast <- c(fifa_mfast, mfast)
      fifa_physical <- c(fifa_physical, physical)
      fifa_height <- c(fifa_height, height)
      fifa_rating <- c(fifa_rating, rating)
      fifa_position <- c(fifa_position, positions)
      result_players <- c(result_players, player)
    }
    
    
  }
  
  
  fifaOnline <- data.frame(result_players, fifa_ovr,fifa_wage, fifa_fast, fifa_mfast, fifa_physical, fifa_height, fifa_rating, fifa_position)
  return(fifaOnline)



}

