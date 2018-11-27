# gold-price-analysis

Dashboard for daily gold price over time. Made with R-Shiny and Plotly.

## Chart 1: Time-series plots: (from 1979)
1) Daily Gold Price
2) 1-day differences
3) 7-day differences
4) 30-day differences

## Chart 2: Rolling correlation with yields of Treasury bills, notes and bonds. (from 1990)
See: https://www.springer.com/cda/content/document/cda_downloaddocument/9780387279657-c1.pdf?SGWID=0-0-45-169676-p59330694 for an explanation of rolling correlation for forecasting.

## Steps to run app:

Type the following into an R console and press ENTER:

*library(shiny)*

*runGitHub( “daily-gold-price”, "kalyani-subbiah")*

Or type the following into an R console and press ENTER:

*library(shiny)*

*runGist("63e9b4194eb2889645666120c95a258c")*

(Run install.packages('shiny') if above does not work)

#### Data sources
Quandl APIs from World Gold Council and US Treasury. 

###### Notes
Ongoing. 
To be added:
Rolling correlation with spot exchange rates of four largest gold consumers: China, India, US, Germany.
