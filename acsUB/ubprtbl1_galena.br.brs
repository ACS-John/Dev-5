10000 ! Replace S:\acsUB\ubprtbl1
10010 ! pr bills (new format)
10020 ! ______________________________________________________________________
10030   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_txt,fnpa_newpage
10040   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
10050   on error goto ERTN
10060 ! ______________________________________________________________________
10070   dim resp$(10)*80,txt$*40,mg$(3)*30,rw(22,13),cap$*128
10080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
10090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
10100 ! ______________________________________________________________________
10110   fncno(cno,cnam$)
10120   fnLastBillingDate(d1)
10130   open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input 
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
10280   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
10290   open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence
10300 ! ______________________________________________________________________
10310 SCREEN1: ! 
10320   a$="" : prtbkno=0
10330   fnTos(sn$="UBPrtBl1-1")
10340   pf=26 : ll=24 : respc=0
10350 ! fnLbl(1,1,"Service From:",LL,1)
10360 !  fnTxt(1,PF,8,8,1,"1",0,TT$)
10370 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
10380 ! fnLbl(2,1,"Service To:",LL,1)
10390 !  fnTxt(2,PF,8,8,1,"1")
10400 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
10410   fnLbl(1,1,"Penalty Due Date:",ll,1)
10420   fnTxt(1,pf,8,8,1,"1",0,tt$)
10430   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
10440   fnLbl(2,1,"Meter Read:",ll,1)
10450   fnTxt(2,pf,8,8,1,"1",0,tt$)
10460   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
10470   fnLbl(4,1,"Message on Bill:",ll,1)
10480   fnTxt(4,pf,30,30)
10490   resp$(respc+=1)=mg$(1)
10500   fnTxt(5,pf,30,30)
10510   resp$(respc+=1)=mg$(2)
10520   fnTxt(6,pf,30,30)
10530   resp$(respc+=1)=mg$(3)
10540   fnLbl(7,1,"Date of Billing:",ll,1)
10550   fnTxt(7,pf,8,8,1,"1")
10560   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
10570   fnLbl(8,1,"Starting Route/Sequence:",ll,1)
10580   fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
10590   resp$(respc+=1)="[All]"
10600   fnLbl(9,1,"Route Number:",ll,1)
10610   fncmbrt2(9,pf)
10620   resp$(respc+=1)="[All]"
10630   fnChk(10,pf,"Select Accounts to Print",1)
10640   resp$(respc+=1)="False"
10650   fnCmdSet(3)
10660   fnAcs(sn$,0,mat resp$,ck)
10670   if ck=5 then goto ENDSCR
10680   d1=val(resp$(6))
10690   d3=val(resp$(2))
10700   d4=val(resp$(1))
10710   mg$(1)=resp$(3)
10720   mg$(2)=resp$(4)
10730   mg$(3)=resp$(5)
10740   if resp$(7)="[All]" then 
10750     a$=""
10760   else 
10770     a$=lpad$(trim$(resp$(7)(1:10)),10)
10780   end if 
10790   if resp$(8)="[All]" then 
10800     prtbkno=0
10810   else 
10820     prtbkno=val(resp$(8))
10830   end if 
10840   if resp$(9)="True" then sl1=1 else sl1=0
10850   if trim$(a$)<>"" then 
10860     read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
10870     st1=1
10880   end if 
10890 L480: form pos 1,c 10,pos 1741,n 2,n 7
10900   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
10910   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
10920   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
10930 ! ______________________________________________________________________
10940   open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed 
10950   fnPa_open("Landscape")
10970 ! ______________________________________________________________________
10980 ! IF SL1=0 THEN GOSUB SORT1
10990 L570: if sl1=1 then goto SCREEN3
11000   if s5=0 then goto L640
11010 L590: read #7,using L600: r6 eof F5_CANCEL
11020 L600: form pos 1,pd 3
11030   read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ noRec L590
11040   read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey L590
11050   goto L650
11060 L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route eof F5_CANCEL
11070 L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2
11080   if prtbkno=0 then goto L680
11090   if prtbkno><route then goto F5_CANCEL
11100 L680: if f><d1 then goto L570
11110   if st1=0 then goto HERE
11120   if st1$=z$ then st1=0 else goto L570
11130 HERE: ! 
11140 ! read alternate billing address
11150   read #3,using L740,key=z$: mat ba$ nokey L810
11160 L740: form pos 11,4*c 30
11170   e1=0 : mat pe$=("")
11180   for j=1 to 4
11190     if rtrm$(ba$(j))<>"" then 
11200       e1=e1+1
11210       pe$(e1)=ba$(j)
11220     end if 
11230   next j
11240   goto L950
11250 ! ______________________________________________________________________
11260 L810: e1=0 : mat pe$=("")
11270   for j=2 to 4
11280     if rtrm$(e$(j))<>"" then 
11290       e1=e1+1
11300       pe$(e1)=e$(j)
11310     end if 
11320   next j
11330   goto L950
11340 ! ______________________________________________________________________
11350 F5_CANCEL: ! 
11360   close #1: ioerr L890
11370 L890: close #3: ioerr L900
11380 L900: ! 
11390 ! close #20: ioerr L920
11400 L920: fnpa_finis
11410   goto ENDSCR
11420 ! ______________________________________________________________________
11430 L950: ! 
11440   pb=bal-g(11)
11450 ! ______________print bill routine______________________________________
11460   fn_vbprint
11470 ! _____________end of pr routine______________________________________
11480   bct(2)=bct(2)+1
11490 ! accumulate totals
11500   goto L570
11510 ! ______________________________________________________________________
11520 SCREEN3: ! 
11530   sn$="UBPrtBl1-2"
11540   fnTos(sn$)
11550   txt$="Account (blank to stop)"
11560   fnLbl(1,1,txt$,31,1)
11570   if trim$(a$)="" then goto L1070 else goto L1080
11580 L1070: if z$<>"" then 
11590     txt$="Last Account entered was "&z$
11600     fnLbl(3,1,txt$,44,1)
11610   else 
11620     txt$=""
11630     fnLbl(3,1,txt$,44,1)
11640   end if 
11650 L1080: fncmbact(1,17) ! 
11660   resp$(1)=a$
11670   fnCmdSet(11): fnAcs(sn$,0,mat resp$,ck)
11680   if ck=5 then goto F5_CANCEL
11690   a$=lpad$(trim$(resp$(1)(1:10)),10)
11700   if trim$(a$)="" then goto F5_CANCEL
11710   read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey SCREEN3
11720   goto HERE
11730 ! ______________________________________________________________________
11740 SORT1: ! SELECT & SORT
11750   open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1410
11760   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
11770   s5=1
11780   if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
11790   restore #2,search>=routekey$: 
11800 L1210: read #2,using L1220: z$,f,route eof END5
11810 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
11820   if prtbkno=0 then goto L1250
11830   if prtbkno><route then goto END5
11840 L1250: if f><d1 then goto L1210
11850   zip5$=cr$=""
11860   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
11870 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
11880   goto L1210
11890 ! ______________________________________________________________________
11900 END5: close #6: 
11910   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
11920 L1330: form pos 1,c 128
11930   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
11940   write #9,using L1330: "Mask 1,19,C,A"
11950   close #9: 
11960   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
11970 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
11980   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
11990   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
12000 L1410: return 
12010 ! ______________________________________________________________________
12020 ENDSCR: ! pr totals screen
12030   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
12040   fnTos(sn$="Bills-Total")
12050   mylen=23 : mypos=mylen+2
12060   respc=0
12070   fnLbl(1,1,"Total Bills Printed:",mylen,1)
12080   fnTxt(1,mypos,8,0,1,"",1)
12090   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
12120   fnCmdSet(52)
12130   fnAcs(sn$,0,mat resp$,ck)
12140 XIT: fnxit
12150 ! ______________________________________________________________________
12160 ERTN: fnerror(program$,err,line,act$,"xit")
12170   if uprc$(act$)<>"PAUSE" then goto L1550
12180   execute "List -"&str$(line) : pause : goto L1550
12190   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
12200 L1550: execute act$
12210   goto ERTN
12220 ! ______________________________________________________________________
12230   def fn_vbprint
12240 ! -- Printer Program for New Laser Utility Bills
12250     checkcounter+=1
12260     if checkcounter=1 then xmargin=0 : ymargin=0
12270     if checkcounter=2 then xmargin=141.2 : ymargin=0
12280     if checkcounter=3 then xmargin=0 : ymargin=108
12290     if checkcounter=4 then xmargin=141.2 : ymargin=108 : checkcounter=0
12300 ! ___________________________
12310 ! - CONSTANTS
12320     lyne=3
12330     character=1.5
12340     pr #20: 'Call Print.MyOrientation("Landscape")'
12350     pr #20: 'Call Print.MyFontSize(12)'
12360     pr #20: 'Call Print.MyFontSize(10)'
12370     pr #20: 'Call Print.MyFontBold(False)'
12380     pr #20: 'Call Print.MyFontColor("Black")'
12390     fnpa_txt('#'&trim$(z$),xmargin,lyne*6+ymargin)
12400     fnpa_txt(e$(1),xmargin+26,lyne*6+ymargin)
12410 ! pr #20: 'Call Print.AddText("Billing Date: ",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
12420 ! pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+30)&','&str$(lyne*11+ymargin)&')'
12430 ! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&','&str$(linelength)&',0)'
12440 ! ___________________________
12450 PRINTGRID: meter=9
12460     pr #20: 'Call Print.MyFontSize(8)'
12470 ! d(1)=123456789 : d(3)=123456789 : g(1)=123456.89 : g(2)=123456.89 : d(9)=123456789 : d(11)=123456789 : g(4)=123456.89 : g(5)=123456.89 : g(6)=123456.89 : g(8)=123456.89 : g(9)=123456.89 : pB=123456.89
12480     if g(1) then 
12490       pr #20: 'Call Print.AddText("WA",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12500       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+1)&','&str$(lyne*meter+ymargin)&')'
12510       pr #20: 'Call Print.AddText("'&fnformnumb$(d(2),0,9)&'",'&str$(xmargin+18)&','&str$(lyne*meter+ymargin)&')'
12520       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+35)&','&str$(lyne*meter+ymargin)&')'
12530       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12540     end if 
12550     if g(2) then 
12560       pr #20: 'Call Print.AddText("SW",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12570       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12580     end if 
12590     if g(4) then 
12600       pr #20: 'Call Print.AddText("PS",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12640       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12650     end if 
12660     if g(5) then 
12670       pr #20: 'Call Print.AddText("TR",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12680       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12690     end if 
12700     if g(6) then 
12710       pr #20: 'Call Print.AddText("PW",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12720       pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12730     end if 
12740     if g(8) then 
12750       pr #20: 'Call Print.AddText("OC",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12760       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12770     end if 
12780     if g(9) then 
12790       pr #20: 'Call Print.AddText("TX",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12800       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12810     end if 
12820     if pb then 
12830       pr #20: 'Call Print.AddText("PB",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
12840       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
12850     end if 
12860 ! ___________________________
12870     pr #20: 'Call Print.AddText("'&date$(days(d3,"mmddyy"),"m")&'",'&str$(xmargin)&','&str$(lyne*23+ymargin)&')'
12880     pr #20: 'Call Print.AddText("'&date$(days(d3,"mmddyy"),"D")&'",'&str$(xmargin+6)&','&str$(lyne*23+ymargin)&')'
12890     if bal>0 then 
12900       pr #20: 'Call Print.AddText("'&fnformnumb$(bal-g(9),2,9)&'",'&str$(xmargin+18)&','&str$(lyne*23+ymargin)&')'
12910       if g(10)>0 then pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+31)&','&str$(lyne*23+ymargin)&')'
12920       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10)-g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*23+ymargin)&')'
12930       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+18)&','&str$(lyne*29.2+ymargin)&')'
12940       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*29.2+ymargin)&')'
12950     else 
12960       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+18)&','&str$(lyne*23+ymargin)&')'
12970       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+52)&','&str$(lyne*23+ymargin)&')'
12980     end if 
12990     if g(9)>0 and bal>0 then 
13000       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+18)&','&str$(lyne*25.4+ymargin)&')'
13010       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*25.4+ymargin)&')'
13020     end if 
13030     if bal>0 then 
13040     end if 
13050 ! pr #20: 'Call Print.AddText("Springfield",'&str$(xmargin+80)&','&str$(lyne*2-1+ymargin)&')'
13060 ! pr #20: 'Call Print.AddText("     IL    ",'&str$(xmargin+80)&','&str$(lyne*3-1+ymargin)&')'
13070 ! pr #20: 'Call Print.AddText("    62702  ",'&str$(xmargin+80)&','&str$(lyne*4-1+ymargin)&')'
13080 ! pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*4+2)&',True)'
13090 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
13100 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
13110 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
13120 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
13130 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
13140 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
13150 ! pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
13160 ! pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
13170 ! pr #20: 'Call Print.AddText(" Paid One Ounce ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
13180 ! pr #20: 'Call Print.AddText("  Permit No.916 ",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
13190     pr #20: 'Call Print.MyFontSize(8)'
13200     pr #20: 'Call Print.AddText("Please return this side with",'&str$(xmargin+75)&','&str$(lyne*6+ymargin)&')'
13210     pr #20: 'Call Print.AddText("payment to:  '&cnam$&'",'&str$(xmargin+75)&','&str$(lyne*7+ymargin)&')'
13220     addy=9
13230     fnpa_txt(e$(2),xmargin+75,lyne*(addy+=1)+ymargin)
13240     fnpa_txt(e$(3),xmargin+75,lyne*(addy+=1)+ymargin)
13250     fnpa_txt(e$(4),xmargin+75,lyne*(addy+=1)+ymargin)
13260     fnpa_txt(mg$(1),xmargin+75,lyne*(addy+=2)+ymargin)
13270     fnpa_txt(mg$(2),xmargin+75,lyne*(addy+=1)+ymargin)
13280     fnpa_txt(mg$(3),xmargin+75,lyne*(addy+=1)+ymargin)
13290     pr #20: 'Call Print.MyFontSize(9)'
13300     fnpa_txt(z$,xmargin+80,lyne*(addy+=5)+ymargin)
13320     fnpa_txt(cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),xmargin+107,lyne*addy+ymargin)
13330     fnpa_txt(fnformnumb$(bal,2,9),xmargin+75,lyne*(addy+=8.5)+ymargin)
13340     if bal>0 then 
13350       fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*addy+ymargin)
13360     else 
13370       fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*addy+ymargin)
13380     end if 
13390     if checkcounter=0 then 
13400       fnpa_newpage
13410     end if 
13420   fnend 
13430 ! ______________________________________________________________________
