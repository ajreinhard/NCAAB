dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")


for i = 0 to 88000 step 100

xHttp.Open "GET", "https://www.sports-reference.com/cbb/play-index/tgl_finder.cgi?request=1&match=game&year_min=2011&year_max=2018&comp_schl_rk=eq&val_schl_rk=ANY&comp_opp_rk=eq&val_opp_rk=ANY&game_type=A&is_range=N&c1stat=pts&c1comp=gt&c2stat=ast&c2comp=gt&c3stat=opp_pts&c3comp=gt&c4stat=opp_ast&c4comp=gt&order_by=date_game&offset=" & i, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\NCAAB Elo\games\games" & i/100 + 1 & ".txt", 2
    .close
end with

next


Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"






