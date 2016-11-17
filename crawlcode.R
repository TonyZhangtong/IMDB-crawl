#http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1

library(rvest)
base_url <-"https://www.facebook.com/widgets/like.php?width=280&show_faces=1&layout=standard&href="
url <- read.csv(file="3.csv", header=TRUE)
url <- url[,1]
i <- 1
for(i_url in url)
{
  lego_movie <- read_html(i_url)
  movie_title <- lego_movie %>%
    html_nodes(xpath = '//div[@class="title_wrapper"]/h1/text()') %>%
    html_text()
  movie_title <- movie_title[1]
  if(length(movie_title) == 0){
    movie_title <- NA
  }
  
  rating <- lego_movie %>% 
    html_nodes("strong span") %>%
    html_text() %>%
    as.numeric()
  if(length(rating) == 0){
    rating <- NA
  }
  
  cast <- lego_movie %>%
    html_nodes("#titleCast .itemprop span") %>%
    html_text()
  if(length(cast) == 0){
    cast <- NA
  }
  
  actor_1_name <- cast[1]
  actor_2_name <- cast[2]
  actor_3_name <- cast[3]
  
  director_name <- lego_movie %>%
    html_nodes(xpath = '//*[@id="title-overview-widget"]/div[3]/div[1]/div[2]/span[1]/a/span') %>%
    html_text()
  director_name <- director_name[1]
  if(length(director_name) == 0){
    director_name <- NA
  }
  
  duration <- lego_movie %>%
    html_nodes("#titleDetails > div:nth-child(21) > time") %>%
    html_text()
  duration<-unlist(strsplit(duration," "))
  duration<-duration[1]
  duration<-as.numeric(duration)
  if(length(duration) == 0){
    duration <- NA
  }
  
  country <- lego_movie %>%
    html_nodes("#titleDetails > div:nth-child(4) > a") %>%
    html_text()
  country <- paste0(country, collapse="|")
  if(length(country) == 0){
    country <- NA
  }
  
  language <- lego_movie %>%
    html_nodes("#titleDetails > div:nth-child(5) > a") %>%
    html_text()
  language <- paste0(language, collapse="|")
  if(length(language) == 0){
    language <- NA
  }
  
  plot_keywords <- lego_movie %>%
    html_nodes(xpath = '//a/span[@itemprop="keywords"]/text()') %>%
    html_text()
  plot_keywords <- paste0(plot_keywords, collapse="|")
  if(length(plot_keywords) == 0){
    plot_keywords <- NA
  }
  
  Release_Date <- lego_movie %>%
    html_nodes("#titleYear > a") %>%
    html_text() %>%
    as.numeric()
  if(length(Release_Date) == 0){
    Release_Date <- NA
  }
  
  genre <- lego_movie %>%
    html_nodes(xpath = '//div[@itemprop="genre"]//a/text()') %>%
    html_text()
  genre <- paste0(genre, collapse="|")
  if(length(genre) == 0){
    genre <- NA
  }
  
  buget <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleDetails"]/div[7]/text()') %>%
    html_text()
  buget <- gsub("\n","",buget)
  buget <- gsub(" ","",buget)
  buget <- gsub(",","",buget)  
  buget<-unlist(strsplit(buget," "))
  buget <- buget[1]
  if(length(buget) == 0){
    buget <- NA
  }
  
  ratio <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleDetails"]/div[15]/text()') %>%
    html_text()
  ratio <- gsub(" ","",ratio)
  ratio <- gsub("\n","",ratio)
  ratio<-unlist(strsplit(ratio," "))
  if(length(ratio) == 0){
    ratio <- NA
  }
  
  color <- lego_movie %>%
    html_nodes("#titleDetails > div:nth-child(23) > a") %>%
    html_text()
  color <- paste0(color, collapse="|")
  if(length(color) == 0){
    color <- NA
  }
  
  gross <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleDetails"]/div[9]/text()')%>%
    html_text()
  gross <- gsub(" ","",gross)
  gross <- gsub("\n","",gross)
  gross <- gsub("$","",gross)
  gross <- gsub(",","",gross)
  gross <- gsub("(usa)","",gross)  
  gross<-unlist(strsplit(gross," "))
  gross <- gross[1]
  if(length(gross) == 0){
    gross <- NA
  }
  
  num_voted_users <- lego_movie %>%
    html_nodes(xpath = '//span[@itemprop="ratingCount"]/text()')%>%
    html_text()
  num_voted_users <- gsub(",","",num_voted_users)
  if(length(num_voted_users) == 0){
    num_voted_users <- NA
  }
  
  num_user <- lego_movie %>%
    html_nodes(xpath = '//span/a[contains(@href, "reviews")]/text()')%>%
    html_text()
  num_user<-unlist(strsplit(num_user," "))
  if(length(num_user) == 0){
    num_user <- NA
  }
  num_user_for_reviews<-num_user[1]
  num_user_for_critic<-num_user[3]
  
  content_rating <- lego_movie %>%
    html_nodes(xpath = '//*[@id="title-overview-widget"]/div[2]/div[2]/div/div[2]/div[2]/div/text()')%>%
    html_text()
  content_rating <- gsub(" ","",content_rating)
  content_rating <- gsub("\n","",content_rating)
  content_rating<-content_rating[2]
  if(length(content_rating) == 0){
    content_rating <- NA
  }
  
  url_fblike<-paste0(base_url,i_url,sep="")
  movie_facebook_like <- read_html(url_fblike)%>%
    html_nodes(xpath = '//*[@id="u_0_2"]')%>%
    html_text()
  movie_facebook_like<-unlist(strsplit(movie_facebook_like," "))
  movie_facebook_like <-movie_facebook_like[1]
  if(length(movie_facebook_like) == 0){
    movie_facebook_like <- NA
  }
  
  
  actor_url <- "http://www.imdb.com"
  actor_1_facebook <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleCast"]/table/tr[2]/td[2]//a/@href')%>%
    html_text()
  actor_1_facebook <- actor_1_facebook[1]
  actor_1_facebook <-paste0(actor_url,actor_1_facebook,sep="")
  actor_1_facebook <-paste0(base_url,actor_1_facebook,sep="")
  actor_1_facebook<- gsub("?ref_=tt_cl_t1","",actor_1_facebook)
  actor_1_facebook <- read_html(actor_1_facebook) %>%
    html_nodes(xpath = '//*[@id="u_0_2"]')%>%
    html_text()
  actor_1_facebook <-unlist(strsplit(actor_1_facebook," "))
  actor_1_facebook <-actor_1_facebook[1]
  if(length(actor_1_facebook) == 0){
    actor_1_facebook <- NA
  }
  
  
  
  actor_url <- "http://www.imdb.com"
  actor_2_facebook <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleCast"]/table/tr[3]/td[2]//a/@href')%>%
    html_text()
  actor_2_facebook <- actor_2_facebook[1]
  actor_2_facebook<-paste0(actor_url,actor_2_facebook,sep="")
  actor_2_facebook<-paste0(base_url,actor_2_facebook,sep="")
  actor_2_facebook<- gsub("?ref_=tt_cl_t2","",actor_2_facebook)
  actor_2_facebook <- read_html(actor_2_facebook) %>%
    html_nodes(xpath = '//*[@id="u_0_2"]')%>%
    html_text()
  actor_2_facebook <-unlist(strsplit(actor_2_facebook," "))
  actor_2_facebook <-actor_2_facebook[1]
  if(length(actor_2_facebook) == 0){
    actor_2_facebook <- NA
  }
  
  
  actor_url <- "http://www.imdb.com"
  actor_3_facebook <- lego_movie %>%
    html_nodes(xpath = '//*[@id="titleCast"]/table/tr[4]/td[2]//a/@href')%>%
    html_text()
  actor_3_facebook <- actor_3_facebook[1]
  actor_3_facebook<-paste0(actor_url,actor_3_facebook,sep="")
  actor_3_facebook<-paste0(base_url,actor_3_facebook,sep="")
  actor_3_facebook<- gsub("?ref_=tt_cl_t3","",actor_3_facebook)
  actor_3_facebook <- read_html(actor_3_facebook) %>%
    html_nodes(xpath = '//*[@id="u_0_2"]')%>%
    html_text()
  actor_3_facebook <-unlist(strsplit(actor_3_facebook," "))
  actor_3_facebook <-actor_3_facebook[1]
  if(length(actor_3_facebook) == 0){
    actor_3_facebook <- NA
  }
  
  director_facebook <- lego_movie %>%
    html_nodes(xpath = '//*[@id="title-overview-widget"]/div[3]/div[1]/div[2]/span//a/@href')%>%
    html_text()
  director_facebook<- gsub("?ref_=tt_ov_dr","",director_facebook)
  director_facebook<-paste0(actor_url,director_facebook,sep="")
  director_facebook<-paste0(base_url,director_facebook,sep="")
  director_facebook <- director_facebook[1]
  director_facebook <- read_html(director_facebook) %>%
    html_nodes(xpath = '//*[@id="u_0_2"]')%>%
    html_text()
  director_facebook <-unlist(strsplit(director_facebook," "))
  director_facebook <-director_facebook[1]
  if(length(director_facebook) == 0){
    director_facebook <- NA
  }
  director_facebook
  
  result <- data.frame(movie_title,country,plot_keywords,genre,director_name,actor_1_name,actor_2_name,actor_3_name,rating,duration,language,Release_Date,buget,gross,color,ratio,num_user_for_reviews,num_user_for_critic,content_rating,movie_facebook_like,actor_1_facebook,actor_2_facebook,actor_3_facebook,director_facebook,i_url)  
  if(i == 1){ 
    results <- result
  }
  if(i > 1){
    results <- rbind(results,result)
  }
  i <- i + 1
}
write.csv(results,file="1.csv")

