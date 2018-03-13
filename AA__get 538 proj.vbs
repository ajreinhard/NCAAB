dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")


xHttp.Open "GET", "https://projects.fivethirtyeight.com/march-madness-api/2018/madness.json", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\NCAAB Elo\538 Model " & date_time_string(Now()) & ".json", 2
    .close
end with

Set bStrm = Nothing
Set xHttp = Nothing
Set fso = Nothing

msgbox "Done"

function leadzero(k)
if k < 10 then
leadzero = "0" & k
else
leadzero = k
end if
end function

function date_time_string(k)
date_time_string = year(k) & "_" & leadzero(month(k)) & "_" & leadzero(day(k)) & " " & leadzero(hour(k)) & "_" & leadzero(minute(k)) & "_" & leadzero(second(k))
end function
