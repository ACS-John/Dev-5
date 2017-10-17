10000 ! Replace S:\acsUB\ubprtbl1_purdy (City of Purdy)
10010 ! pr bills (new format)
10020 ! ______________________________________________________________________
10030   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt
10040   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
10050   on error goto ERTN
10060 ! ______________________________________________________________________
10070   dim resp$(10)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128
10080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
10090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
10100 ! ______________________________________________________________________
10110   fncno(cno,cnam$)
10120   fnLastBillingDate(d1)
10130   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
10140   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
10150   close #21: 
10160   at$(1)=cnam$
10170   z=21
10180   at$(1)=trim$(at$(1))(1:z)
10190   x=len(at$(1)) : y=z-x
10200   at$(1)=rpt$(" ",int(y/2))&at$(1)
10210   z=26
10220   for j=2 to udim(at$)
10230     at$(j)=trim$(at$(j))(1:z)
10240     x=len(at$(j)) : y=z-x
10250     at$(j)=rpt$(" ",int(y/2))&at$(j)
10260   next j
10270   linelength=62
10280   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in account order
10290   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence
10300 ! ______________________________________________________________________
10310 SCREEN1: ! 
10320   a$="" : prtbkno=0
10330   fntos(sn$="UBPrtBl1-1")
10340   pf=26 : ll=24 : respc=0
10350 ! fnLBL(1,1,"Service From:",LL,1)
10360 !  fnTXT(1,PF,8,8,1,"1",0,TT$)
10370 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
10380 ! fnLBL(2,1,"Service To:",LL,1)
10390 !  fnTXT(2,PF,8,8,1,"1")
10400 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
10410   fnlbl(1,1,"Message on Bill:",ll,1)
10420   fntxt(1,pf,30,30)
10430   resp$(respc+=1)=mg$(1)
10440   fntxt(2,pf,30,30)
10450   resp$(respc+=1)=mg$(2)
10460   fntxt(3,pf,30,30)
10470   resp$(respc+=1)=mg$(3)
10480   fnlbl(4,1,"Date of Billing:",ll,1)
10490   fntxt(4,pf,8,8,1,"1")
10500   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
10510   fnlbl(5,1,"Starting Route/Sequence:",ll,1)
10520   fncombof("ubm-act-nam",5,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2)
10530   resp$(respc+=1)="[All]"
10540   fnlbl(6,1,"Route Number:",ll,1)
10550   fncmbrt2(6,pf)
10560   resp$(respc+=1)="[All]"
10570   fnchk(7,pf,"Select Accounts to Print",1)
10580   resp$(respc+=1)="False"
10590   fncmdset(3)
10600   fnacs(sn$,0,mat resp$,ck)
10610   if ck=5 then goto ENDSCR
10620   d1=val(resp$(4))
10630   mg$(1)=resp$(1)
10640   mg$(2)=resp$(2)
10650   mg$(3)=resp$(3)
10660   if resp$(5)="[All]" then 
10670     a$=""
10680   else 
10690     a$=lpad$(trim$(resp$(5)(1:10)),10)
10700   end if 
10710   if resp$(6)="[All]" then 
10720     prtbkno=0
10730   else 
10740     prtbkno=val(resp$(6))
10750   end if 
10760   if resp$(7)="True" then sl1=1 else sl1=0
10770   if trim$(a$)<>"" then 
10780     read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
10790     st1=1
10800   end if 
10810 L480: form pos 1,c 10,pos 1741,n 2,n 7
10820   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
10830   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
10840   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
10850 ! ______________________________________________________________________
10860   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
10870   fnPa_open("Landscape")
10890 ! ______________________________________________________________________
10900 ! IF SL1=0 THEN GOSUB SORT1
10910 L570: if sl1=1 then goto SCREEN3
10920   if s5=0 then goto L640
10930 L590: read #7,using L600: r6 eof F5_CANCEL
10940 L600: form pos 1,pd 3
10950   read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ norec L590
10960   read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey L590
10970   goto L650
10980 L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route eof F5_CANCEL
10990 L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2
11000   if prtbkno=0 then goto L680
11010   if prtbkno><route then goto F5_CANCEL
11020 L680: if f><d1 then goto L570
11030   if st1=0 then goto HERE
11040   if st1$=z$ then st1=0 else goto L570
11050 HERE: ! 
11060 ! read alternate billing address
11070   read #3,using L740,key=z$: mat ba$ nokey L810
11080 L740: form pos 11,4*c 30
11090   e1=0 : mat pe$=("")
11100   for j=1 to 4
11110     if rtrm$(ba$(j))<>"" then 
11120       e1=e1+1
11130       pe$(e1)=ba$(j)
11140     end if 
11150   next j
11160   goto L950
11170 ! ______________________________________________________________________
11180 L810: e1=0 : mat pe$=("")
11190   for j=2 to 4
11200     if rtrm$(e$(j))<>"" then 
11210       e1=e1+1
11220       pe$(e1)=e$(j)
11230     end if 
11240   next j
11250   goto L950
11260 ! ______________________________________________________________________
11270 F5_CANCEL: ! 
11280   close #1: ioerr L890
11290 L890: close #3: ioerr L900
11300 L900: ! 
11310 ! close #20: ioerr L920
11320 L920: fnpa_finis
11330   goto ENDSCR
11340 ! ______________________________________________________________________
11350 L950: ! 
11360   pb=bal-g(11)
11370 ! ______________print bill routine______________________________________
11380   fn_vbprint
11390 ! _____________end of pr routine______________________________________
11400   bct(2)=bct(2)+1
11410 ! accumulate totals
11420   goto L570
11430 ! ______________________________________________________________________
11440 SCREEN3: ! 
11450   sn$="UBPrtBl1-2"
11460   fntos(sn$)
11470   txt$="Account (blank to stop)"
11480   fnlbl(1,1,txt$,31,1)
11490   if trim$(a$)="" then goto L1070 else goto L1080
11500 L1070: if z$<>"" then 
11510     txt$="Last Account entered was "&z$
11520     fnlbl(3,1,txt$,44,1)
11530   else 
11540     txt$=""
11550     fnlbl(3,1,txt$,44,1)
11560   end if 
11570 L1080: fncmbact(1,17) ! 
11580   resp$(1)=a$
11590   fncmdset(11): fnacs(sn$,0,mat resp$,ck)
11600   if ck=5 then goto F5_CANCEL
11610   a$=lpad$(trim$(resp$(1)(1:10)),10)
11620   if trim$(a$)="" then goto F5_CANCEL
11630   read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey SCREEN3
11640   goto HERE
11650 ! ______________________________________________________________________
11660 SORT1: ! SELECT & SORT
11670   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1410
11680   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
11690   s5=1
11700   if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
11710   restore #2,search>=routekey$: 
11720 L1210: read #2,using L1220: z$,f,route eof END5
11730 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
11740   if prtbkno=0 then goto L1250
11750   if prtbkno><route then goto END5
11760 L1250: if f><d1 then goto L1210
11770   zip5$=cr$=""
11780   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
11790 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
11800   goto L1210
11810 ! ______________________________________________________________________
11820 END5: close #6: 
11830   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
11840 L1330: form pos 1,c 128
11850   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
11860   write #9,using L1330: "Mask 1,19,C,A"
11870   close #9: 
11880   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
11890 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
11900   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
11910   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
11920 L1410: return 
11930 ! ______________________________________________________________________
11940 ENDSCR: ! pr totals screen
11950   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
11960   fntos(sn$="Bills-Total")
11970   mylen=23 : mypos=mylen+2
11980   respc=0
11990   fnlbl(1,1,"Total Bills Printed:",mylen,1)
12000   fntxt(1,mypos,8,0,1,"",1)
12010   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
12040   fncmdset(52)
12050   fnacs(sn$,0,mat resp$,ck)
12060 XIT: fnxit
12070 ! ______________________________________________________________________
12080 ERTN: fnerror(program$,err,line,act$,"xit")
12090   if uprc$(act$)<>"PAUSE" then goto L1550
12100   execute "List -"&str$(line) : pause : goto L1550
12110   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
12120 L1550: execute act$
12130   goto ERTN
12140 ! ______________________________________________________________________
12150   def fn_vbprint
12160 ! -- Printer Program for New Laser Utility Bills
12170     checkcounter+=1
12180     if checkcounter=1 then xmargin=0 : ymargin=0
12190     if checkcounter=2 then xmargin=139 : ymargin=0
12200     if checkcounter=3 then xmargin=0 : ymargin=108
12210     if checkcounter=4 then xmargin=139 : ymargin=108 : checkcounter=0
12220 ! ___________________________
12230 ! - CONSTANTS
12240     lyne=3
12260     pr #20: 'Call Print.MyFont("Lucida Console")'
12270     pr #20: "Call Print.MyFontSize(10)"
12280     fnpa_txt(e$(2),xmargin+6,lyne*2.5-1+ymargin)
12290     fnpa_txt("#"&z$,xmargin+50,lyne*2.5-1+ymargin)
12300 ! pr #20: 'Call Print.MyOrientation("Landscape")'
12330 ! pr #20: 'Call Print.MyFontColor("Green")'
12350     pr #20: 'Call Print.MyFont("Lucida Console")'
12360     pr #20: 'Call Print.MyFontSize(10)'
12370     pr #20: 'Call Print.MyFontBold(False)'
12400     pr #20: 'Call Print.MyFontColor("Black")'
12410     pr #20: 'Call Print.AddText("THIS BILL IS NOW DUE AND",'&str$(xmargin+6)&','&str$(lyne*7+ymargin)&')'
12420     pr #20: 'Call Print.AddText("PAYABLE",'&str$(xmargin+6)&','&str$(lyne*8+ymargin)&')'
12430     pr #20: 'Call Print.AddText("Billing Date: ",'&str$(xmargin+6)&','&str$(lyne*10+ymargin)&')'
12440     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+34)&','&str$(lyne*10+ymargin)&')'
12450     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(lyne*11+1+ymargin)&','&str$(linelength)&',0)'
12460     pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+14)&','&str$(lyne*12+ymargin)&')'
12470     pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+37)&','&str$(lyne*12+ymargin)&')'
12480     pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+54)&','&str$(lyne*12+ymargin)&')'
12490 ! ___________________________
12500 PRINTGRID: meter=13
12510     pr #20: 'Call Print.MyFontSize(8)'
12520 ! d(1)=123456789 : d(3)=123456789 : g(1)=123456.89 : g(2)=123456.89 : d(9)=123456789 : d(11)=123456789 : g(4)=123456.89 : g(5)=123456.89 : g(6)=123456.89 : g(8)=123456.89 : g(9)=123456.89 : pB=123456.89
12530     if g(1) then 
12540       pr #20: 'Call Print.AddText("WA",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12550       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+10)&','&str$(lyne*meter+ymargin)&')'
12560       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+29)&','&str$(lyne*meter+ymargin)&')'
12570       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12580     end if 
12590     if g(2) then 
12600       pr #20: 'Call Print.AddText("SW",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12610       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12620     end if 
12630     if g(4) then 
12640       pr #20: 'Call Print.AddText("GS",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12650       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+10)&','&str$(lyne*(meter)+ymargin)&')'
12660       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+29)&','&str$(lyne*(meter)+ymargin)&')'
12670       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12680     end if 
12690     if g(5) then 
12700       pr #20: 'Call Print.AddText("WS",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12710       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12720     end if 
12730     if g(6) then 
12740       pr #20: 'Call Print.AddText("SS",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12750       pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12760     end if 
12770     if g(8) then 
12780       pr #20: 'Call Print.AddText("OC",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12790       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12800     end if 
12810     if g(9) then 
12820       pr #20: 'Call Print.AddText("TX",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12830       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12840     end if 
12850     if pb then 
12860       pr #20: 'Call Print.AddText("PB",'&str$(xmargin+6)&','&str$(lyne*(meter+=1)+ymargin)&')'
12870       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+49)&','&str$(lyne*meter+ymargin)&')'
12880     end if 
12890     pr #20: 'Call Print.MyFontSize(10)'
12900 ! ___________________________
12910     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(lyne*23+1+ymargin)&','&str$(linelength)&',0)'
12920     pr #20: 'Call Print.AddText("Pay Now:",'&str$(xmargin+5)&','&str$(lyne*24+ymargin)&')'
12930     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
12940     pr #20: 'Call Print.AddText("Pay After 10th",'&str$(xmargin+5)&','&str$(lyne*25.5+ymargin)&')'
12950     if bal>0 then 
12960       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
12970     else 
12980       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
12990     end if 
13000     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(lyne*27+1+ymargin)&','&str$(linelength)&',0)'
13010     fnpa_txt(mg$(1),xmargin+5,lyne*28+ymargin)
13020     fnpa_txt(mg$(2),xmargin+5,lyne*29+ymargin)
13030     fnpa_txt(mg$(3),xmargin+5,lyne*30+ymargin)
13040     pr #20: 'Call Print.MyFontSize(8)'
13050     fnpa_txt('#'&trim$(z$),xmargin+88,lyne*12+ymargin)
13060     addy=13
13070     fnpa_txt(e$(2),xmargin+88,lyne*(addy+=1)+ymargin)
13080     fnpa_txt(e$(3),xmargin+88,lyne*(addy+=1)+ymargin)
13090     fnpa_txt(e$(4),xmargin+88,lyne*(addy+=1)+ymargin)
13130     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+84)&','&str$(lyne*29+ymargin)&')'
13160     if bal>0 then 
13170       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*29+ymargin)&')'
13180     end if 
13190     if bal<=0 then 
13200       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*29+ymargin)&')'
13210     end if 
13220     if checkcounter=0 then 
13230       fnpa_newpage
13240     end if 
13250   fnend 
13260 ! ______________________________________________________________________
