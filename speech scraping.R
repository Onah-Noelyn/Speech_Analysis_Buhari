library('rvest')

#scraped each speech one at a time - any idea on geting the loop to work would be appreciated
#Used This==========================================================================================================
speech <- read_html('http://statehouse.gov.ng/news/national-broadcast-by-h-e-muhammadu-buhari-president-of-the-federal-republic-of-nigeria-on-the-58th-independence-anniversary-1st-october-2018/')
str(speech)
h2 <- speech%>%
  html_nodes('section')
pageText <- speech%>%
  html_nodes('p')%>%
  html_text()
pageText <- paste(pageText, collapse = ' ') #join everything together
writeLines(pageText, "Speech52")

#Loop===============================================================================================================
url <- c('https://statehouse.gov.ng/news/president-muhammadu-buhari-on-the-20th-anniversary-celebration-of-the-international-criminal-court-the-hague-netherlands/', 
         'https://statehouse.gov.ng/news/president-muhammadu-buharis-address-at-the-commissioning-of-the-abuja-light-rail-system/',
         'https://statehouse.gov.ng/news/president-muhammadu-buharis-remarks-at-the-apc-grand-finale-of-the-ekiti-guber-national-campaign-rally-in-ado-ekiti-ekiti-state/',
         'https://statehouse.gov.ng/news/president-muhammadu-buharis-remarks-at-his-meeting-with-the-christian-association-of-nigeria-can-from-19-northern-states-and-abuja/',
         'https://statehouse.gov.ng/news/president-muhammadu-buharis-speech-at-the-commissioning-of-calabar-rice-seedling-plant-calabar-cross-river/',
         'https://statehouse.gov.ng/news/president-muhammadu-buharis-address-at-the-commissioning-ceremony-of-nigerian-navy-reference-hospital-in-calabar-cross-river-state/',
         'https://statehouse.gov.ng/news/president-buharis-closing-remarks-at-the-apc-convention-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-speech-at-the-opening-ceremony-of-the-67th-international-press-institute-ipi-world-congress-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-speech-at-the-signing-of-the-2018-appropriation-bill-into-law-at-presidential-villa-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-statement-at-the-hosting-of-members-of-the-diplomatic-corps-for-ramadan-iftar/',
         'https://statehouse.gov.ng/news/president-buharis-remarks-at-the-investiture-honouring-the-heroes-of-june-12-1993/',
         'https://statehouse.gov.ng/news/president-buhari-declares-june-12-the-new-democracy-day/', 
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-61st-meeting-of-the-unwto-caf/',
         'https://statehouse.gov.ng/news/president-buharis-remarks-at-the-signing-of-the-not-too-young-to-run-bill-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-address-in-commemoration-of-the-2018-democracy-day-celebration/',
         'https://statehouse.gov.ng/news/president-buharis-remarks-at-the-2018-nigeria-democracy-day-lecture-at-icc-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-message-on-national-childrens-day-may-27-2018/',
         'https://statehouse.gov.ng/news/president-buharis-remarks-at-dinner-with-apc-south-west-leaders-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-commissioning-of-the-efcc-headquarters-in-abuja/',
         'https://statehouse.gov.ng/news/press-release-nigerias-agricultural-revolution-on-course-president-buhari/',
         'https://statehouse.gov.ng/news/press-release-press-statement-by-president-buhari-during-his-visit-to-the-united-states-on-april-30th-2018/',
         'https://statehouse.gov.ng/news/president-buharis-address-on-the-report-submitted-by-the-all-progressives-congress-national-executive-technical-committee/',
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-apc-nec-meeting-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-inauguration-of-the-national-food-security-council-in-abuja/',
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-occasion-of-receiving-the-release-of-dapchi-school-girls/',
         'https://statehouse.gov.ng/news/president-buharis-address-at-the-commissioning-of-sunti-golden-sugar-estate-in-niger-state/',
         'https://statehouse.gov.ng/news/president-buharis-address-on-his-official-visit-to-yobe-state/',
         'https://statehouse.gov.ng/news/president-buharis-speech-at-a-meeting-with-stakeholders-in-the-rice-value-chain/',
         'https://statehouse.gov.ng/news/speech-by-president-buhari-at-the-61st-independence-day-anniversary-of-ghana/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-the-first-adamawa-state-anti-corruption-summit/',
         'https://statehouse.gov.ng/news/speech-president-buharis-remarks-at-the-commissioning-of-comprehensive-special-school-lafia-nasarawa-state-february-6-2018/',
         'https://statehouse.gov.ng/news/press-release-president-buharis-letter-to-senate-president-on-benue-killings/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-the-30th-ordinary-session-of-assembly-of-heads-of-state-and-government-of-the-african-union/',
         'https://statehouse.gov.ng/news/speech-president-buhari-at-the-launch-of-new-locomotives-and-coaches-for-the-kaduna-abuja-train-service-january-4-2018/',
         'https://statehouse.gov.ng/news/speech-president-buhari-at-the-commissioning-of-the-kaduna-inland-dry-port-january-4-2018/',
         'https://statehouse.gov.ng/news/new-year-address-by-president-buhari-jan-1-2018/',
         'https://statehouse.gov.ng/news/statement-president-buhari-on-fuel-scarcity/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-the-international-climate-change-summit-in-paris/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-the-inauguration-of-the-tripartite-minimum-wage-committee/',
         'https://statehouse.gov.ng/news/speech-president-buharis-remarks-at-the-2017-all-nigeria-judges-conference-of-the-supreme-courts/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-his-state-visit-to-ebonyi-state/',
         'https://statehouse.gov.ng/news/speech-president-buharis-remarks-at-fecs-special-retreat-on-education-held-in-abuja/',
         'https://statehouse.gov.ng/news/speech-president-buharis-2018-budget-address/',
         'https://statehouse.gov.ng/news/speech-president-buharis-speech-at-launch-of-2018-armed-forces-emblem/',
         'https://statehouse.gov.ng/news/speech-president-buharis-address-at-the-nec-meeting-in-abuja/',
         'https://statehouse.gov.ng/news/speech-statement-by-his-excellency-muhammadu-buhari-president-of-the-federal-republic-of-nigeria-at-the-9th-summit-of-the-d-8-organisation-for-economic-cooperation-istanbul-turkey-on-october-20-20/',
         'https://statehouse.gov.ng/news/speech-president-buhari-at-the-inauguration-of-the-national-economic-council-nec/',
         'http://statehouse.gov.ng/news/president-muhammadu-buharis-address-at-the-opening-ceremony-of-the-2018-nigerian-bar-association-annual-general-conference-abuja/',
         'http://statehouse.gov.ng/news/president-muhammadu-buharis-address-at-the-nec-meeting-of-the-apc-party/',
         'http://statehouse.gov.ng/news/president-muhammadu-buhari-remarks-at-the-women-political-aspirants-summit-in-abuja/',
         'http://statehouse.gov.ng/news/president-buharis-remarks-at-the-opening-ceremony-of-the-high-level-dialogue-between-chinese-and-african-leaders-and-business-representatives/',
         'http://statehouse.gov.ng/news/president-buharis-speech-at-the-high-level-meeting-on-united-against-tuberculosis-global-action-against-global-threat/',
         'http://statehouse.gov.ng/news/national-broadcast-by-h-e-muhammadu-buhari-president-of-the-federal-republic-of-nigeria-on-the-58th-independence-anniversary-1st-october-2018/')

for (i in 1:length(url)){
  
  
  speech <- read_html(url[i])
  str(speech)
  h2 <- speech%>%
    html_nodes('section')
  pageText <- speech%>%
    html_nodes('p')%>%
    html_text()
  pageText <- paste(pageText, collapse = ' ') #join everything together
  
  writeLines(as.character(pageText), paste0(gsub(" ", "_", url[i]), ".text"))
}
