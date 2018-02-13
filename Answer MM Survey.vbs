url_full = "https://docs.google.com/forms/d/e/1FAIpQLSfCrpvvvfWGH2M_AUGZCfSJQmrfUh7VClTMf4qGZQU9_1YlNA/viewform?usp=sf_link"

Set objIE = CreateObject("InternetExplorer.Application")

objIE.navigate url_full
wscript.sleep 3000

set text_box = objIE.document.getElementsByClassName("quantumWizTextinputPapertextareaContentArea exportContentArea")
set submit_btn = objIE.document.getElementsByClassName("quantumWizButtonPaperbuttonLabel exportLabel")

text_box.item(0).children(0).innerText = "9;9;10;12"
text_box.item(1).children(0).innerText = "9;9;10;12"
text_box.item(2).children(0).innerText = "9;9;15;12"

submit_btn.item(1).click
wscript.sleep 3000

''''''All done, close everything

objIE.application.Quit
set objIE = Nothing
WScript.Quit