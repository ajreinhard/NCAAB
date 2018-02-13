dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\2010-11 url.txt",1)

on error resume next

do until fl.atendofstream
i = fl.readline


xHttp.Open "GET", "http://data.ncaa.com" & i & "/boxscore.json", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\Box\" & replace(i,"/","_") & ".json", 2
    .close
end with



xHttp.Open "GET", "http://data.ncaa.com" & i & "/pbp.json", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\PBP\" & replace(i,"/","_") & ".json", 2
    .close
end with

loop

on error goto 0

fl.close

Set fl = Nothing
Set fso = Nothing
Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






