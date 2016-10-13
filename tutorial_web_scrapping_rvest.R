#package rvest

library(xml2)
library(httr)
library(magrittr)
library(rvest)
library(selectr)
?google_form

#parse an html page

#function read_html()

google=read_html("http://google.com",encoding = "ISO-8859-1")

google%>%
  xml_structure()

google%>%
  html_nodes("div")

#forming a string

minimal=read_html("<!doctype html>
                  <meta charset=utf-8>
                  <title>blah</title>
                  <p>I'm the content")
minimal
minimal%>%
  xml_structure()

#parse forms in a html page

#function html_form

#example

html_form(read_html("https://hadley.wufoo.com/forms/libraryrequire-quiz/"))
html_form(read_html("https://hadley.wufoo.com/forms/r-journal-submission/"))
box_office=read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
box_office
box_office%>%
  html_node("form")%>%
  html_form()

#select nodes from html document
#html_nodes

ateam=read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
html_nodes(ateam,"center")
html_nodes(ateam,"center font")
html_nodes(ateam, "center font b")

ateam%>%html_nodes("center")%>%html_nodes("td")
ateam%>%html_nodes("center")%>%html_nodes("font")%>%html_nodes("b")
  
td=ateam%>%html_nodes("center")%>%html_nodes("td")
td
td%>%html_nodes("font")
ateam%>%html_nodes("table")%>%extract2(1)%>%html_nodes("img")

#find all images in the first two tables

images=ateam%>%html_nodes("table")%>%extract(1:2)%>%html_nodes("img")
  

#########**************Scrapping data from Wikipedia***************###########
##############################################################################

library(xml2)
  
help(package='rvest')
