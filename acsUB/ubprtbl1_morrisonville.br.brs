10000 ! Replace S:\acsUB\ubprtbl1
10010 ! pr bills (new format)
10020 ! ______________________________________________________________________
10030   library "S:\Core\Library": fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt
10040   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
10050   on error goto ERTN
10060 ! ______________________________________________________________________
10070   dim resp$(10)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128
10080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
10090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,billstart$*5,billend$*5
10100 ! ______________________________________________________________________
10110   fncno(cno,cnam$)
10120   fnd1(d1)
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
10410   fnlbl(1,1,"Penalty Due Date:",ll,1)
10420   fntxt(1,pf,8,8,1,"1",0,tt$)
10430   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
10440   fnlbl(2,1,"Billing Period From:",ll,1)
10450   fntxt(2,pf,5,5)
10460   resp$(respc+=1)=""
10470   fnlbl(3,1,"Billing Period To:",ll,1)
10480   fntxt(3,pf,5,5)
10490   resp$(respc+=1)=""
10500   fnlbl(4,1,"Message on Bill:",ll,1)
10510   fntxt(4,pf,30,30)
10520   resp$(respc+=1)=mg$(1)
10530   fntxt(5,pf,30,30)
10540   resp$(respc+=1)=mg$(2)
10550   fntxt(6,pf,30,30)
10560   resp$(respc+=1)=mg$(3)
10570   fnlbl(7,1,"Date of Billing:",ll,1)
10580   fntxt(7,pf,8,8,1,"1")
10590   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
10600   fnlbl(8,1,"Starting Route/Sequence:",ll,1)
10610   fncombof("ubm-act-nam",8,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2)
10620   resp$(respc+=1)="[All]"
10630   fnlbl(9,1,"Route Number:",ll,1)
10640   fncmbrt2(9,pf)
10650   resp$(respc+=1)="[All]"
10660   fnchk(10,pf,"Select Accounts to Print",1)
10670   resp$(respc+=1)="False"
10680   fncmdset(3)
10690   fnacs(sn$,0,mat resp$,ck)
10700   if ck=5 then goto ENDSCR
10710   d1=val(resp$(7))
10720   billstart$=resp$(2)
10730   billend$=resp$(3)
10740   d4=val(resp$(1))
10750   mg$(1)=resp$(4)
10760   mg$(2)=resp$(5)
10770   mg$(3)=resp$(6)
10780   if resp$(8)="[All]" then 
10790     a$=""
10800   else 
10810     a$=lpad$(trim$(resp$(8)(1:10)),10)
10820   end if 
10830   if resp$(9)="[All]" then 
10840     prtbkno=0
10850   else 
10860     prtbkno=val(resp$(9))
10870   end if 
10880   if resp$(10)="True" then sl1=1 else sl1=0
10890   if trim$(a$)<>"" then 
10900     read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
10910     st1=1
10920   end if 
10930 L480: form pos 1,c 10,pos 1741,n 2,n 7
10940   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
10950   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
10960   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
10970 ! ______________________________________________________________________
10980   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
10990   fnPa_open("Landscape")
11010 ! ______________________________________________________________________
11020 ! IF SL1=0 THEN GOSUB SORT1
11030 L570: if sl1=1 then goto SCREEN3
11040   if s5=0 then goto L640
11050 L590: read #7,using L600: r6 eof F5_CANCEL
11060 L600: form pos 1,pd 3
11070   read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ norec L590
11080   read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey L590
11090   goto L650
11100 L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route eof F5_CANCEL
11110 L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2
11120   if prtbkno=0 then goto L680
11130   if prtbkno><route then goto F5_CANCEL
11140 L680: if f><d1 then goto L570
11150   if st1=0 then goto HERE
11160   if st1$=z$ then st1=0 else goto L570
11170 HERE: ! 
11180 ! read alternate billing address
11190   read #3,using L740,key=z$: mat ba$ nokey L810
11200 L740: form pos 11,4*c 30
11210   e1=0 : mat pe$=("")
11220   for j=1 to 4
11230     if rtrm$(ba$(j))<>"" then 
11240       e1=e1+1
11250       pe$(e1)=ba$(j)
11260     end if 
11270   next j
11280   goto L950
11290 ! ______________________________________________________________________
11300 L810: e1=0 : mat pe$=("")
11310   for j=2 to 4
11320     if rtrm$(e$(j))<>"" then 
11330       e1=e1+1
11340       pe$(e1)=e$(j)
11350     end if 
11360   next j
11370   goto L950
11380 ! ______________________________________________________________________
11390 F5_CANCEL: ! 
11400   close #1: ioerr L890
11410 L890: close #3: ioerr L900
11420 L900: ! 
11430 ! close #20: ioerr L920
11440 L920: fnpa_finis
11450   goto ENDSCR
11460 ! ______________________________________________________________________
11470 L950: ! 
11480   pb=bal-g(11)
11490 ! ______________print bill routine______________________________________
11500   fn_vbprint
11510 ! _____________end of pr routine______________________________________
11520   bct(2)=bct(2)+1
11530 ! accumulate totals
11540   goto L570
11550 ! ______________________________________________________________________
11560 SCREEN3: ! 
11570   sn$="UBPrtBl1-2"
11580   fntos(sn$)
11590   txt$="Account (blank to stop)"
11600   fnlbl(1,1,txt$,31,1)
11610   if trim$(a$)="" then goto L1070 else goto L1080
11620 L1070: if z$<>"" then 
11630     txt$="Last Account entered was "&z$
11640     fnlbl(3,1,txt$,44,1)
11650   else 
11660     txt$=""
11670     fnlbl(3,1,txt$,44,1)
11680   end if 
11690 L1080: fncmbact(1,17) ! 
11700   resp$(1)=a$
11710   fncmdset(11): fnacs(sn$,0,mat resp$,ck)
11720   if ck=5 then goto F5_CANCEL
11730   a$=lpad$(trim$(resp$(1)(1:10)),10)
11740   if trim$(a$)="" then goto F5_CANCEL
11750   read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey SCREEN3
11760   goto HERE
11770 ! ______________________________________________________________________
11780 SORT1: ! SELECT & SORT
11790   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1410
11800   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
11810   s5=1
11820   if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
11830   restore #2,search>=routekey$: 
11840 L1210: read #2,using L1220: z$,f,route eof END5
11850 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
11860   if prtbkno=0 then goto L1250
11870   if prtbkno><route then goto END5
11880 L1250: if f><d1 then goto L1210
11890   zip5$=cr$=""
11900   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
11910 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
11920   goto L1210
11930 ! ______________________________________________________________________
11940 END5: close #6: 
11950   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
11960 L1330: form pos 1,c 128
11970   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
11980   write #9,using L1330: "Mask 1,19,C,A"
11990   close #9: 
12000   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
12010 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
12020   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
12030   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
12040 L1410: return 
12050 ! ______________________________________________________________________
12060 ENDSCR: ! pr totals screen
12070   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
12080   fntos(sn$="Bills-Total")
12090   mylen=23 : mypos=mylen+2
12100   respc=0
12110   fnlbl(1,1,"Total Bills Printed:",mylen,1)
12120   fntxt(1,mypos,8,0,1,"",1)
12130   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
12160   fncmdset(52)
12170   fnacs(sn$,0,mat resp$,ck)
12180 XIT: fnxit
12190 ! ______________________________________________________________________
12200 ERTN: fnerror(program$,err,line,act$,"xit")
12210   if uprc$(act$)<>"PAUSE" then goto L1550
12220   execute "List -"&str$(line) : pause : goto L1550
12230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
12240 L1550: execute act$
12250   goto ERTN
12260 ! ______________________________________________________________________
12270   def fn_vbprint
12280 ! -- Printer Program for New Laser Utility Bills
12290     checkcounter+=1
12300     if checkcounter=1 then xmargin=0 : ymargin=0
12310     if checkcounter=2 then xmargin=139 : ymargin=0
12320     if checkcounter=3 then xmargin=0 : ymargin=108
12330     if checkcounter=4 then xmargin=139 : ymargin=108 : checkcounter=0
12340 ! ___________________________
12350 ! - CONSTANTS
12360     lyne=3
12370     character=1.5
12380 ! pr #20: 'Call Print.MyOrientation("Landscape")'
12390     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',55,'&str$(lyne*3+3)&',True)'
12400     pr #20: "Call Print.MyFontBold(True)"
12410     pr #20: 'Call Print.MyFontSize(12)'
12420     pr #20: 'Call Print.MyFont("Courier New")'
12430 ! pr #20: 'Call Print.MyFontColor("Green")'
12440     fnpa_txt(at$(1),xmargin+6,lyne*1-1+ymargin)
12450     pr #20: 'Call Print.MyFont("Lucida Console")'
12460     pr #20: 'Call Print.MyFontSize(10)'
12470     pr #20: 'Call Print.MyFontBold(False)'
12480     fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.65)
12490     fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
12500     pr #20: 'Call Print.MyFontColor("Black")'
12510     fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
12520     fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
12530     pr #20: 'Call Print.AddText("THIS BILL IS NOW DUE AND",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
12540     pr #20: 'Call Print.AddText("PAYABLE",'&str$(xmargin+2)&','&str$(lyne*9+ymargin)&')'
12550     pr #20: 'Call Print.AddText("Billing Date: ",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
12560     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+30)&','&str$(lyne*11+ymargin)&')'
12570     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&','&str$(linelength)&',0)'
12580     pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
12590     pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
12600     pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
12610 ! ___________________________
12620 PRINTGRID: meter=14
12630     pr #20: 'Call Print.MyFontSize(8)'
12640 ! d(1)=123456789 : d(3)=123456789 : g(1)=123456.89 : g(2)=123456.89 : d(9)=123456789 : d(11)=123456789 : g(4)=123456.89 : g(5)=123456.89 : g(6)=123456.89 : g(8)=123456.89 : g(9)=123456.89 : pB=123456.89
12650     if g(1) then 
12660       pr #20: 'Call Print.AddText("WA",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12670       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
12680       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
12690       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12700     end if 
12710     if g(2) then 
12720       pr #20: 'Call Print.AddText("SW",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12730       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12740     end if 
12750     if g(4)=0 then 
12760       pr #20: 'Call Print.AddText("GS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12770       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*(meter)+ymargin)&')'
12780       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*(meter)+ymargin)&')'
12790       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12800     end if 
12810     if g(5) then 
12820       pr #20: 'Call Print.AddText("WS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12830       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12840     end if 
12850     if g(6) then 
12860       pr #20: 'Call Print.AddText("SS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12870       pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12880     end if 
12890     if g(8) then 
12900       pr #20: 'Call Print.AddText("OC",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12910       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12920     end if 
12930     if g(9) then 
12940       pr #20: 'Call Print.AddText("TX",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12950       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12960     end if 
12970     if pb then 
12980       pr #20: 'Call Print.AddText("PB",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
12990       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
13000     end if 
13010     pr #20: 'Call Print.MyFontSize(10)'
13020 ! ___________________________
13030     pr #20: 'Call Print.AddText("Billing from '&billstart$&' to '&billend$&'",'&str$(xmargin+1)&','&str$(lyne*21.5+ymargin)&')'
13040     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&','&str$(linelength)&',0)'
13050     pr #20: 'Call Print.AddText("Pay Now:",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
13060     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
13070     pr #20: 'Call Print.AddText("Pay After",'&str$(xmargin+1)&','&str$(lyne*25.5+ymargin)&')'
13080     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(xmargin+22)&','&str$(lyne*25.5+ymargin)&')'
13090     if bal>0 then 
13100       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
13110     else 
13120       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
13130     end if 
13140     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*27+1+ymargin)&','&str$(linelength)&',0)'
13150     fnpa_txt(mg$(1),xmargin+1,lyne*29+ymargin)
13160     fnpa_txt(mg$(2),xmargin+1,lyne*30+ymargin)
13170     fnpa_txt(mg$(3),xmargin+1,lyne*31+ymargin)
13180     pr #20: 'Call Print.MyFontSize(7)'
13190     pr #20: 'Call Print.AddText("Springfield",'&str$(xmargin+80)&','&str$(lyne*2-1+ymargin)&')'
13200     pr #20: 'Call Print.AddText("     IL    ",'&str$(xmargin+80)&','&str$(lyne*3-1+ymargin)&')'
13210     pr #20: 'Call Print.AddText("    62702  ",'&str$(xmargin+80)&','&str$(lyne*4-1+ymargin)&')'
13220     pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*4+2)&',True)'
13230     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
13240     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
13250     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
13260     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
13270     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
13280     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
13290     pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
13300     pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
13310     pr #20: 'Call Print.AddText(" Paid One Ounce ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
13320     pr #20: 'Call Print.AddText("  Permit No.916 ",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
13330     pr #20: 'Call Print.MyFontSize(10)'
13340     pr #20: 'Call Print.AddText("Please return this side with",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
13350     pr #20: 'Call Print.AddText("payment to:  ",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
13355     pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*10+ymargin)&')'
13360     pr #20: 'Call Print.AddText("Pay Now:",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
13370     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
13380     pr #20: 'Call Print.AddText("After",'&str$(xmargin+68)&','&str$(lyne*13+ymargin)&')'
13390     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+80)&','&str$(lyne*13+ymargin)&')'
13400     if bal>0 then 
13410       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*13+ymargin)&')'
13420     end if 
13430     if bal<=0 then 
13440       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*13+ymargin)&')'
13450     end if 
13460     fnpa_txt('#'&trim$(z$),xmargin+68,lyne*16+ymargin)
13470     addy=17
13480     fnpa_txt(e$(2),xmargin+68,lyne*(addy+=1)+ymargin)
13490     fnpa_txt(e$(3),xmargin+68,lyne*(addy+=1)+ymargin)
13500     fnpa_txt(e$(4),xmargin+68,lyne*(addy+=1)+ymargin)
13510     if checkcounter=0 then 
13520       fnpa_newpage
13530     end if 
13540   fnend 
13550 ! ______________________________________________________________________
