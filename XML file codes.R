#install.packages("XML")
library(XML)
library(xml2)
library(tidyverse)

# One XML file check

file_name <- "C:/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Hackathon/Check/article_0000000001.xml"

xmldata <- xmlToDataFrame(file_name)

file_name2 <- "/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Hackathon/Check/article_0000000000.xml"
articles_parsed<-xmlParse(file_name2)
articles_parsed
detaildf1<-xmlToDataFrame(nodes = getNodeSet(articles_parsed, "/PubmedArticle"))
detaildf1

detaildf2<-xmlToDataFrame(nodes = getNodeSet(articles_parsed, "/PubmedArticle/MedlineCitation/Article/Abstract/AbstractText"))
detaildf2



getArticleData <- function(file_name) {
  tryCatch(
  expr = {
    print("inside function:")
    print(file_name)
    articles_parsed<-xmlParse(file_name)
    article_text<-xmlToDataFrame(nodes = getNodeSet(articles_parsed, "/PubmedArticle/MedlineCitation/Article/Abstract/AbstractText"))
    result_df <- as.data.frame(article_text$text)
    return(result_df)
  },
  error = function(e) {
    message('Error caught:')
    print(file_name)
    print(e)
    return(NULL)
  }
  )
}

getArticleData(file_name2)

merged_df_50k <- lapply(all.files[1:50000], getArticleData) %>% bind_rows()



# Article field is used for the contents

library(xml2)
library(dplyr)

setwd("C:/Users/gmith/case-report-dataset/data_unzip2/articles")
all.files <- list.files(pattern="\\.xml", path=getwd(), full.names=TRUE)

all.files[1:2]

dfList <- lapply(all.files, function(x){
  xml <- xmlParse(x)
  pointAttribs <- xpathSApply(doc=xml, path="//point",  xmlAttrs)
  # TRANSPOSE XPATH LIST TO DF 
  df <- data.frame(t(pointAttribs))
  return(df)
})

merged_df <- lapply(all.files, xmlToDataFrame) %>% bind_rows()

{  doc <- file %>% XML::xmlInternalTreeParse()
  nodeset <- XML::getNodeSet(doc, x)
  XML::xmlToDataFrame(nodeset, stringsAsFactors = FALSE) %>%
    dplyr::as_data_frame()
}

doc <- xmlParse("C:/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Hackathon/Check/*xml")
df <- xmlToDataFrame(doc, nodes=getNodeSet(doc, "//ROW_DUASDIA"))

isRStudio <- Sys.getenv("RSTUDIO") == "1"

save(merged_df_20k, file = "xml_50K.Rda")
load("xml_50K.Rda")

merged_df_20k$text <- merged_df_20k$`article_text$text`
################################################################################


xml_unnest_freq <- merged_df_20k %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%  
  count(word, sort = TRUE) %>%
  filter(n > 500) %>% 
  mutate(word=reorder(word, n)) 

fre_hist <- xml_unnest_freq%>%
  top_n(20) %>% 
  ggplot(aes(word, n))+ 
  geom_col()+
  xlab(NULL)+
  coord_flip() 

print(fre_hist)

################################################################################

diabetes <- merged_df_20k %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%  
  count(word1, word2, sort = TRUE) %>% 
  filter(word1 == "diabetes") %>% 
  top_n(20)

diabetes %>% 
  ggplot(aes(word2, n))+ 
  geom_col()+
  xlab(NULL)+
  coord_flip() 

save(diabetes, file = "diabetes.Rda")


################################################################################
AFINN <- get_sentiments("afinn")

xml_bigrams <- merged_df_20k %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 == "diabetes") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup() 

xml_bigrams%>% 
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words appearing to \"diabetes\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

save(xml_bigrams, file = "xml_bigram.Rda")

################################################################################

token_ungroup <- merged_df_20k %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2, sort=TRUE) %>%
  ungroup()

total_words <- token_ungroup %>% 
  group_by(word1) %>% 
  summarise(total=sum(n))

token_words <- left_join(token_ungroup, total_words)
save(token_words, file = "token_words.Rda")

freq_by_rank <- token_words %>% 
  mutate(rank = row_number(),
         `term frequency` = n/total)


freq_by_rank %>%
  filter(word2 == "diabetes") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  filter(`term frequency` > 0.02) %>% 
  ggplot(aes(word1, `term frequency`))+ 
  geom_col()+
  xlab(NULL)+
  coord_flip() 

  
  
################################################################################
