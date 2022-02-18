library(dplyr)
library(scholar)
R.utils::setOption("scholar_site","https://scholar.google.co.uk")

get_scholar_ids <- function(role=c("Academic staff","Emeritus faculty","Professional team","Researchers","PhD students","Alumni"), pauze=2) {
  
  people <- rvest::read_html("https://www.ucl.ac.uk/pals/research/experimental-psychology/people/")
  
  roles <- people %>%
    rvest::html_elements(".people-roles") %>%
    rvest::html_elements("a") %>%
    rvest::html_text()
  
  roles_ids <- sapply(role, function(x) which(roles == x))
  
  people_ul <- people %>%
    rvest::html_elements(".people")
  
  out <- data.frame()
  
  for(i in roles_ids) {
    names <- people_ul[i] %>% 
      rvest::html_elements("a") %>% 
      rvest::html_text2()
    names <- as.character(lapply(sapply(names, function(x) strsplit(x,"\\n")), function(x) x[1]))
    urls <- people_ul[i] %>% 
      rvest::html_elements("a") %>% 
      rvest::html_attr("href")
    out <- rbind(out,
                 data.frame(role=roles[i], name=names, url=urls))
  }
  
  out$scholar_url <- ""
  for(i in 1:nrow(out)) {
    tmp <- out$url[i] %>%
      rvest::read_html() %>%
      rvest::html_elements(".person-googlescholar") %>%
      rvest::html_elements("a") %>% 
      rvest::html_attr("href")
    if(length(tmp) > 0) out$scholar_url[i] <- tmp
  }
  
  out$scholar_id <- NA
  for(i in 1:nrow(out)) {
    if(length(out$scholar_url[i])>0) {
      out$scholar_id[i] <- strsplit(strsplit(out$scholar_url[i],c("user=","&"))[[1]][2],"&")[[1]][1]
    }
  }
  
  for(i in 1:nrow(out)) {
    if(is.na(out$scholar_id[i])) {
      tmp <- strsplit(out$name[i]," ")[[1]]
      sid <- ""
      sid <- try(scholar::get_scholar_id(first_name = tmp[1], last_name = tmp[length(tmp)], affiliation = c("UCL","University College London")))
      Sys.sleep(pauze)
      out$scholar_id[i] <- sid
    }
  }
  return(out)
}
researcher_details <- get_scholar_ids(role=c("Academic staff","Researchers"))

researcher_details[researcher_details$name == "Daniel Richardson",]$scholar_id <- "G3GJdhAAAAAJ"

# build data base with papers
all_papers <- data.frame()
site <- getOption("scholar_site")
for(i in 1:nrow(researcher_details)) {
  if(!is.na(researcher_details$scholar_id[i])) {
    id <- researcher_details$scholar_id[i]
    tmp <- scholar::get_publications(id)
    tmp$name <- researcher_details$name[i]
    tmp$scholar_id <- researcher_details$scholar_id[i]
    tmp$abstract <- NA
    for(j in 1:nrow(tmp)) {
      article <- tmp[j,"pubid"]
      url_base <- paste0(site, "/citations?",
                         "view_op=view_citation&hl=en&citation_for_view=")
      url_tail <- paste(id, article, sep=":")
      url <- paste0(url_base, url_tail)
      res <- scholar::get_scholar_resp(url)
      httr::stop_for_status(res, "get user id / article information")
      doc <- rvest::read_html(res)
      tmp$abstract[j] <- doc %>%
        rvest::html_element("#gsc_oci_descr") %>%
        rvest::html_text2()
      Sys.sleep(2)
    }
    all_papers <- rbind(all_papers,tmp)
  }
}

