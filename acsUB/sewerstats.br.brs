10000 ! replace S:\acsUB\sewerstats.br
10010 ! Calculates average customers billed and average sewer charges over a given time period
10020   library "S:\Core\Library": fncno,fntop,fntos,fnfra,fnlbl,fntxt,fncmdkey,fnacs,fnxit,fngethandle
10030   dim cnam$*40,resp$(2)*40,z$*10,a$*10,transkey$*19,dt(1),chg(1),ccnt(1),cycle(1,1),cyclecnt(1),avg(2,1),txt$*23
10040   fn_main
10050   def fn_main
10060     fncno(cno,cnam$)
10070     fntop("S:\acsUB\sewerstats","Sewer Statistics")
10080     fn_getrange
10090     fn_openfiles
10100     fn_calculate
10110     fn_closefiles
10120   fnend 
10130   def fn_getrange
10140     fntos("sewerstats-1")
10150     fnfra(1,1,3,40,"Date Range for Statistics","Enter the range of dates for which you want to calculate sewer charge statistics")
10160     fnlbl(1,1,"Beginning Date:",22,1,0,1)
10170     fntxt(1,25,12,0,1,"3",0,"Enter the date of the first billing cycle to be included. ",1)
10180     let resp$(1)=str$(date("mmddyy")-3)
10190     fnlbl(2,1,"Ending Date:",22,1,0,1)
10200     fntxt(2,25,12,0,1,"3",0,"Enter the date of the last billing cycle to be included. ",1)
10210     let resp$(2)=str$(date("mmddyy"))
10220     fncmdkey("Next",1,1,0,"Calculate sewer statistics.")
10230     fncmdkey("Cancel",5,0,1,"Returns to menu.")
10240     fnacs("sewerstats-1",0,mat resp$,ckey)
10250     if ckey=5 then let fnxit
10260     let d1=val(resp$(1)) : let d2=val(resp$(2)) : let yrend=d1+10000 : let yrcnt=1
10270     do while yrend<d2
10280       mat dt(yrcnt)=dt
10290       let dt(yrcnt)=yrend
10300       let yrend+=10000 : let yrcnt+=1
10310     loop 
10320     mat dt(yrcnt)=dt : let dt(yrcnt)=d2 : mat chg(yrcnt) : mat ccnt(yrcnt) : mat cycle(yrcnt,1) : mat cyclecnt(yrcnt) : mat avg(2,yrcnt+1)
10330   fnend 
10340   def fn_calculate
10350     let totcust=lrec(h_ubmstr) : custidx=0
10360     do 
10370 NEXTCUST: read #h_ubmstr,using CUSTFORM: z$ eof ENDOFCUST
10380       custidx+=1 : pr f "10,10,C 35": "Reading customer "&str$(custidx)&" of "&str$(totcust)
10390       let transkey$=z$&cnvrt$("N 8",0)&cnvrt$("N 1",0)
10400       read #h_ubtrans,using TRANSFORM,key>=transkey$: a$,tdate,tcode,seweramt nokey NEXTCUST : goto PROCESSTRANS
10410 NEXTTRANS: read #h_ubtrans,using TRANSFORM: a$,tdate,tcode,seweramt eof NEXTCUST
10420 PROCESSTRANS: ! 
10430       if z$=a$ then ! this is the correct customer
10440         if tdate>=d1 and tdate<=d2 and seweramt>0 and tcode=1 then ! include this transaction in the average
10450           for j = 1 to yrcnt
10460             if tdate<=dt(j) then 
10470               for k = 1 to udim(cycle,2)
10480                 if cycle(j,k)=0 then 
10490                   cycle(j,k)=tdate : cyclecnt(j)+=1 : goto TOT
10500                 else 
10510                   if tdate=cycle(j,k) then goto TOT
10520                 end if 
10530               next k
10540               mat cycle(yrcnt,k)=cycle : cycle(j,k)=tdate : cyclecnt(j)+=1
10550 TOT:          chg(j)+=seweramt : ccnt(j)+=1 : goto NEXTTRANS
10560             end if 
10570           next j
10580         end if 
10590         goto NEXTTRANS
10600       else 
10610         goto NEXTCUST
10620       end if 
10630     loop 
10640 ENDOFCUST: for j = 1 to yrcnt
10650       avg(1,j)=int(ccnt(j)/cyclecnt(j)) : avg(2,j)=round(chg(j)/ccnt(j),2)
10660     next j
10670     avg(1,yrcnt+1)=int(sum(ccnt)/sum(cyclecnt)) : avg(2,yrcnt+1)=round(sum(chg)/sum(ccnt),2)
10680     fn_showresults
10690   fnend 
10700   def fn_showresults
10710     lncnt=2+yrcnt
10720     fntop("S:\acsUB\sewerstats2","Sewer Statistics")
10730     fntos("sewerstats-2")
10740     fnlbl(1,1,"Date Range:",19)
10750     fnlbl(1,22,"Avg. Bills Per Cycle:",23)
10760     fnlbl(1,47,"Avg. Bill:",12)
10770     open #(h_prn:=fngethandle): "Name="&env$('Q')&"\UBmstr\Sewerstats"&wsid$&".txt,Replace,RecL=5000",display,output 
10780     pr #h_prn: 'Call Print.MyOrientation("Portrait")'
10790     pr #h_prn: 'Call Print.MyFontSize(14)'
10800     pr #h_prn: 'Call Print.MyFontBold(True)'
10810     pr #h_prn: 'Call Print.AddText("Sewer Statistics from '&str$(d1)&' to '&str$(d2)&'",20,10)'
10820     pr #h_prn: 'Call Print.MyFontSize(10)'
10830     pr #h_prn: 'Call Print.AddText("Date Range:",10,20)'
10840     pr #h_prn: 'Call Print.AddText("Avg. Bills Per Cycle",50,20)'
10850     pr #h_prn: 'Call Print.AddText("Avg. Bill",120,20)'
10860     pr #h_prn: 'Call Print.MyFontBold(False)'
10870     for j = 1 to udim(avg,2)
10880       if j=1 then let dtstart=d1 else let dtstart=dt(j-1)+1
10890       if j=udim(avg,2) then let txt$="Total Avg" else let txt$=str$(dtstart)&"-"&str$(dt(j))
10900       fnlbl(j+1,1,txt$,19)
10910       fnlbl(j+1,22,str$(avg(1,j)),23)
10920       fnlbl(j+1,47,str$(avg(2,j)),12)
10930       pr #h_prn: 'Call Print.AddText("'&txt$&'",10,'&str$(20+j*7)&')'
10940       pr #h_prn: 'Call Print.AddText("'&str$(avg(1,j))&'",50,'&str$(20+j*7)&')'
10950       pr #h_prn: 'Call Print.AddText("$'&trim$(cnvrt$("NZ 10.2",avg(2,j)))&'",120,'&str$(20+j*7)&')'
10960     next j
10970     fncmdkey("Print",2,0,0,"Print these statistics.")
10980     fncmdkey("Done",1,1,0,"Return to menu.")
10990     fnacs("sewerstats-1",0,mat resp$,ckey)
11000     pr #h_prn: 'Call Print.EndDoc'
11010     if ckey=2 then execute 'System -W -C "'&os_filename$("S:\Core\PrAce.exe")&'" '&os_filename$('UBmstr\Sewerstats"&wsid$&".txt"') ! "sy -W -C S:\Core\PrAce UBmstr\Sewerstats"&wsid$&".txt"
11020   fnend 
11030   def fn_openfiles
11040     open #(h_ubmstr:=fngethandle): "Name="&env$('Q')&"\UBmstr\customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndex.h"&str$(cno),internal,input,keyed 
11050 CUSTFORM: form c 10
11060     open #(h_ubtrans:=fngethandle): "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&str$(cno),internal,input,keyed 
11070 TRANSFORM: form c 10,n 8,n 1,pos 28,pd 4.2
11080   fnend 
11090   def fn_closefiles
11100     close #h_ubmstr: 
11110     close #h_ubtrans: 
11120     fnxit
11130   fnend 
