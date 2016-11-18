library("rvest")

#Enter the base URL
base_url <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=1950,2016&page="

#Create a list for storing movie links
Page<- list()

#Get number of v index pages URL
v<-1:100

#Loop to get all indexed page
for(i in v) {

#Get full URL for every index page
url<- paste0(base_url,i,"&ref_=adv_nxt")

#Get the content of the index page. If login is needed, an http request method might need to be used
index_page<- read_html(url)

#Use the CSS selector to get partial movie links from one indexed page.
Page_Link<- index_page%>%
  html_nodes(".lister-item-header")%>%
  html_node("a")%>%
  html_attr("href")%>%
  paste0("http://www.imdb.com/",.)

#Send movie links for one page to 
Page[[length(Page)+1]]<- Page_Link
df<-c(Page,recursive=TRUE)
}


# Export movie links to the csv file.
write.csv(df,file="3.csv")

