library(rvest)
library(purrr)

Review_rating<-vector()

for(pc in 1:100)
{
  url_base_Seltos<- "https://www.cardekho.com/kia/seltos/user-reviews/%d"
  page <-read_html(sprintf(url_base_Seltos,pc))
  print(pc)
  Reviews = html_nodes(page, xpath = '//div[@class="readReviewBox"]')
  j=0
  for (i in Reviews)
  {
    j = j+1
    Review_rating[j]=getRating(i)
    data.frame(
    #Review_Title = html_text(html_nodes(i,xpath = '//div//h3//a'))
    Review = html_text(html_nodes(i,xpath = '//div[@class="contentspace"]//p')),
    #Review_Date = html_text(html_nodes(i,xpath = '//div[@class="authorSummary"]//div[@class="date"]')),
    Review_rating
    )
  }->Kia_Seltos_reviews_1
  write.table(Kia_Seltos_reviews_1, "f:\\isb\\taba\\Kia_Seltos_reviews3.csv", row.names = FALSE, append = T)
}



for(pc in 1:100)
{
  url_base_Seltos<- "https://www.cardekho.com/jeep/compass/user-reviews/%d"
  page <-read_html(sprintf(url_base_Seltos,pc))
  print(pc)
  Reviews = html_nodes(page, xpath = '//div[@class="readReviewBox"]')
  j=0
  for (i in Reviews)
  {
    j = j+1
    Review_rating[j]=getRating(i)
    data.frame(
      #Review_Title = html_text(html_nodes(i,xpath = '//div//h3//a'))
      Review = html_text(html_nodes(i,xpath = '//div[@class="contentspace"]//p')),
      #Review_Date = html_text(html_nodes(i,xpath = '//div[@class="authorSummary"]//div[@class="date"]')),
      Review_rating
    )
  }->Kia_Seltos_reviews_1
  write.table(Kia_Seltos_reviews_1, "f:\\isb\\taba\\jeep_compass_review.csv", row.names = FALSE, append = T)
}


for(pc in 1:100)
{
  url_base_Seltos<- "https://www.cardekho.com/mg/hector/user-reviews/%d"
  page <-read_html(sprintf(url_base_Seltos,pc))
  print(pc)
  Reviews = html_nodes(page, xpath = '//div[@class="readReviewBox"]')
  j=0
  for (i in Reviews)
  {
    j = j+1
    Review_rating[j]=getRating(i)
    data.frame(
      #Review_Title = html_text(html_nodes(i,xpath = '//div//h3//a'))
      Review = html_text(html_nodes(i,xpath = '//div[@class="contentspace"]//p')),
      #Review_Date = html_text(html_nodes(i,xpath = '//div[@class="authorSummary"]//div[@class="date"]')),
      Review_rating
    )
  }->Kia_Seltos_reviews_1
  write.table(Kia_Seltos_reviews_1, "f:\\isb\\taba\\mg_hector_review.csv", row.names = FALSE, append = T)
}

getRating<-function(i)
{
  length(html_nodes(i,'.icon-star-full-fill'))
  #print(length(html_nodes(i,'.icon-star-full-fill')))
}