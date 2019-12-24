library(rvest)
library(purrr)

url_base_Seltos<- "https://www.cardekho.com/kia/seltos/user-reviews/%d"

map_df(1:100,function(i){
  page <-read_html(sprintf(url_base_Seltos,i))
  data.frame(Review = html_text(html_nodes(page,".contentheight")))
             #Review_Title = html_text(html_nodes(page,".name")))
             #Review_Date = html_text(html_nodes(page,".date"))
              })->Kia_Seltos_reviews

write.csv(Kia_Seltos_reviews,'C:\\Users\\Prashant\\Desktop\\Kia_Seltos_reviews.csv', row.names = FALSE)

url_base_Compass<- "https://www.cardekho.com/jeep/compass/user-reviews/%d"

map_df(1:100,function(i){
  page <-read_html(sprintf(url_base_Compass,i))
  data.frame(Review = html_text(html_nodes(page,".contentheight")))
             #Review_Title = html_text(html_nodes(page,".name")))
            #Review_Date = html_text(html_nodes(page,".date"))
              })->Jeep_Compass_reviews

write.csv(Jeep_Compass_reviews,'C:\\Users\\Prashant\\Desktop\\Jeep_Compass_reviews.csv', row.names = FALSE)

url_base_Hector<- "https://www.cardekho.com/mg/hector/user-reviews/%d"

map_df(1:100,function(i){
  page <-read_html(sprintf(url_base_Hector,i))
  data.frame(Review = html_text(html_nodes(page,".contentheight")))
            #Review_Title = html_text(html_attrs(page,".name")))
            #Review_Date = html_text(html_nodes(page,".date"))
              })->MG_Hector_reviews

write.csv(MG_Hector_reviews,'C:\\Users\\Prashant\\Desktop\\MG_Hector_reviews.csv', row.names = FALSE)

