dim id(14)
dim name(14)

id(0) = "6806320"
id(1) = "421325"
id(2) = "926530"
id(3) = "3860289"
id(4) = "6133835"
id(5) = "7988766"
id(6) = "8240768"
id(7) = "11328354"
id(8) = "15094589"
id(9) = "917412"
id(10) = "1005126"
id(11) = "3622026"
id(12) = "6470408"
id(13) = "10494440"
id(14) = "11725560"

name(0) = "AJ"
name(1) = "Devon"
name(2) = "Andrew"
name(3) = "Ty"
name(4) = "Cory"
name(5) = "Naps"
name(6) = "Chad"
name(7) = "Caleb"
name(8) = "Perry"
name(9) = "Gina"
name(10) = "Janel"
name(11) = "Issac"
name(12) = "Lauren"
name(13) = "Natalie"
name(14) = "Tay"

for j = 0 to 14

url_full = "http://games.espn.com/tournament-challenge-bracket/2017/en/entry?entryID=" & id(j)

Set objIE = CreateObject("InternetExplorer.Application")
Set fso = CreateObject("Scripting.FileSystemObject")

objIE.navigate url_full
wscript.sleep 5000

Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Brackets.txt", 8, True)
set ele_class = objIE.document.getElementsByClassName("picked")

for x = 0 to ele_class.length-1
if x = 56 then i = 2 else i = 0
fl.Writeline(name(j) & ";" & id(j) & ";" & ele_class.item(x).children(0+i).innertext & ";" & ele_class.item(x).children(1+i).innertext & ";" & ele_class.item(x).children(2+i).innertext)
next

fl.Close


Set fl = Nothing
Set fso = Nothing
objIE.application.Quit
set objIE = Nothing

next

''''''All done, close everything
msgbox "done"
WScript.Quit