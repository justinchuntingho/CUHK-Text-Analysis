# ---
# title: "Basic Text Analysis Using R (workshop material)"
# author: "Justin Ho"
# last updated: "05/10/2021"
# ---


# =================================== Setting Up =======================================

# Downloading the datasets
download.file("https://raw.githubusercontent.com/justinchuntingho/CUHK-Text-Analysis/main/snp.csv", "snp.csv")
download.file("https://raw.githubusercontent.com/justinchuntingho/CUHK-Text-Analysis/main/scotelection2021.csv", "scotelection2021.csv")

# Installing the packages
install.packages("quanteda")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
install.packages("magrittr")
install.packages("wordcloud")
install.packages("ggplot2")

# Loading the necessary packages
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(magrittr)

# =================================== Basic Text Analysis =======================================

# Loading the documents
df_snp <-  read.csv("snp.csv", stringsAsFactors = FALSE)
corpus_snp <- corpus(df_snp, text_field = "text")

# The followings are not necessary steps, but it is always a good idea to view a portion of your data
corpus_snp[1:10] # print the first 10 documents
ndoc(corpus_snp) # Number of Documents
nchar(corpus_snp[1:10]) # Number of character for the first 10 documents
ntoken(corpus_snp[1:10]) # Number of tokens for the first 10 documents
ntoken(corpus_snp[1:10], remove_punct = TRUE) # Number of tokens for the first 10 documents after removing punctuation

# Meta-data
docvars(corpus_snp)

# Defining custom stopwords
customstopwords <- c("s", "http", "stopword")

# Creating DFM
tokens_snp <- tokens(corpus_snp, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_url = TRUE,
                     verbose = TRUE)
dfm_snp <- dfm(tokens_snp)

# Inspecting the results
topfeatures(dfm_snp, 30) 

# What is it with "make"?
kwic(tokens_snp, "make", 3)

# Plotting a histogram
library(ggplot2)
scotfeatures <- topfeatures(dfm_snp, 100)  # Putting the top 100 words into a new object
data.frame(list(term = names(scotfeatures), frequency = unname(scotfeatures))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Doing it again, removing stop words this time!
dfm_snp <- dfm_remove(dfm_snp, c(stopwords('english'), customstopwords))

# Inspecting the results again
topfeatures(dfm_snp, 30) 

# Top words again
scotfeatures <- topfeatures(dfm_snp, 100)  # Putting the top 100 words into a new object
data.frame(list(term = names(scotfeatures), frequency = unname(scotfeatures))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Wordcloud
textplot_wordcloud(dfm_snp)

# =================================== Comparison Across Groups =======================================

# Loading the documents
df_all <-  read.csv("scotelection2021.csv", stringsAsFactors = FALSE)
corpus_all <- corpus(df_all, text_field = "text")
tokens_all <- tokens(corpus_all, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     verbose = TRUE, 
                     remove_url = TRUE)
dfm_all <- dfm(tokens_all)
dfm_all <- dfm_remove(dfm_all, c(stopwords('english'), customstopwords))

# Grouping by party
dfm_all_gp <- dfm_group(dfm_all, groups = snsname)
topfeatures(dfm_all_gp, 30, groups = snsname)

# Subsetting by docvars
corpus_con <- corpus_subset(corpus_all, snsname == "Scottish Conservatives")
tokens_con <- tokens(corpus_con)
kwic(tokens_con, "indyref2", 3)


png("wordcloud.png", 1000, 1000)
textplot_wordcloud(dfm_all_gp, max_size = 1.5, comparison = TRUE, labelsize = 2, labeloffset = -0.001)
dev.off()

# =================================== Keyword Analysis =======================================

# Comparing only Scottish Conservatives and Scottish Labour
dfm_conlab <- corpus_all %>% 
  corpus_subset(snsname %in% c("Scottish Conservatives", "Scottish Labour Party")) %>%
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE, 
         verbose = TRUE, 
         remove_url = TRUE) %>% 
  dfm() %>% 
  dfm_remove(c(stopwords('english'), customstopwords))
tstat_key <- textstat_keyness(dfm_conlab, 
                              target = docvars(dfm_conlab, "snsname") == "Scottish Conservatives")
textplot_keyness(tstat_key, n = 30)

# A user-defined function to plot comparison cloud
keyness_cloud <- function(x, a = "A", b = "B", acol = "#00C094", bcol = "#F8766D", w = 600, h = 600, maxword = 500, png = TRUE){
  set.seed(1024)
  #Select all word with p-value <= 0.05 and then make a comparison wordcloud
  kwdssig <- data.frame(term = x$feature, chi2 = x$chi2, p=x$p) %>% 
    dplyr::filter(x$p <= 0.05) %>% 
    dplyr::select(term, chi2)
  row.names(kwdssig) <- kwdssig$term
  kwdssig$a <- kwdssig$chi2
  kwdssig$b <- kwdssig$chi2
  kwdssig$b[kwdssig$b > 0] <- 0
  kwdssig$a[kwdssig$a < 0] <- 0
  kwdssig <- kwdssig[,-1:-2]
  colnames(kwdssig) <- c(a, b)
  if (png) {
    png(paste0(deparse(substitute(x)), ".png"), width = w, height = h)
    wordcloud::comparison.cloud(kwdssig, random.order=FALSE, colors = c(acol, bcol),scale=c(6,.6), title.size=3, max.words = maxword)
    dev.off()
  } else {
    wordcloud::comparison.cloud(kwdssig, random.order=FALSE, colors = c(acol, bcol),scale=c(6,.6), title.size=3, max.words = maxword)
  }
}

keyness_cloud(tstat_key, # Name of the keyness result object
              a = "Scottish Conservatives", # Name of the target corpus
              b = "Scottish Labour", # Name of the reference corpus
              acol = "blue", # Colour of the target corpus
              bcol = "red", # Colour of the reference corpus
              png = TRUE) # Save to png?
