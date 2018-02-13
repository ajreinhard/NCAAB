dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

Set fso = CreateObject("Scripting.FileSystemObject")

start_date = DateSerial(2013,11,4)
end_date = start_date+154
division = "3"

'10/29/1984
'10/28/1985
'10/27/1986
'11/2/1987
'10/31/1988
'10/30/1989
'10/29/1990
'11/4/1991
'11/2/1992
'11/1/1993
'10/31/1994
'10/30/1995
'10/28/1996
'10/27/1997
'10/26/1998
'11/1/1999
'10/30/2000
'10/29/2001
'11/4/2002
'11/3/2003
'11/1/2004
'10/31/2005
'10/30/2006
'11/5/2007
'11/3/2008
'11/2/2009
'11/1/2010
'10/31/2011
'11/5/2012
'11/4/2013
'11/3/2014
'11/2/2015
'10/31/2016


for i = start_date to end_date

'url = "http://data.ncaa.com/game/basketball-men/d1/2011/02/12/temple-dayton/gameinfo.json"
url = "http://data.ncaa.com/jsonp/scoreboard/basketball-men/d" & division & "/" & year(i) & "/" & leadzero(month(i)) & "/" & leadzero(day(i)) & "/scoreboard.html"
'msgbox url

xHttp.Open "GET", url, False
xHttp.Send

On Error Resume Next
with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\board raw\" & date_string(i) & ".txt", 2
    .close
end with


Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\board raw\" & date_string(i) & ".txt",1)
Set fl2 = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\NCAAB Elo\NCAA Direct\board json\" & date_string(i) & ".json",2, true)


new_text = fl.ReadAll
fl2.write(mid(new_text, 17, len(new_text)-18))

fl.close
fl2.close

new_text = ""
Set fl = Nothing
Set fl2 = Nothing


On Error goto 0

next


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

function date_string(k)
date_string = year(k) & "_" & leadzero(month(k)) & "_" & leadzero(day(k))
end function
