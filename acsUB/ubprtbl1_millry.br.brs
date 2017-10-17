10000 ! Replace S:\acsUB\ubprtbl1_millry
10100 ! pr bills (new format)
10200 ! ______________________________________________________________________
10300   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt
10400   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
10500   on error goto ERTN
10600 ! ______________________________________________________________________
10700   dim resp$(10)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128
10800   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
10900   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
11000 ! ______________________________________________________________________
11100   fncno(cno,cnam$)
11200   fnLastBillingDate(d1)
11300   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
11400   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
11500   close #21: 
11600   at$(1)=cnam$
11700   z=21
11800   at$(1)=trim$(at$(1))(1:z)
11900   x=len(at$(1)) : y=z-x
12000   at$(1)=rpt$(" ",int(y/2))&at$(1)
12100   z=26
12200   for j=2 to udim(at$)
12300     at$(j)=trim$(at$(j))(1:z)
12400     x=len(at$(j)) : y=z-x
12500     at$(j)=rpt$(" ",int(y/2))&at$(j)
12600   next j
12700   linelength=62
12800   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in account order
12900   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence
13000 ! ______________________________________________________________________
13100   mg$(1)='To avoid disconnection bill'
13200   mg$(2)='must be paid in full by'
13300   mg$(3)=''
13400 SCREEN1: ! 
13500   a$="" : prtbkno=0
13600   fntos(sn$="UBPrtBl1-1")
13700   pf=26 : ll=24 : respc=0
13800 ! fnLBL(1,1,"Service From:",LL,1)
13900 !  fnTXT(1,PF,8,8,1,"1",0,TT$)
14000 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
14100 ! fnLBL(2,1,"Service To:",LL,1)
14200 !  fnTXT(2,PF,8,8,1,"1")
14300 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
14400   fnlbl(1,1,"Penalty Due Date:",ll,1)
14500   fntxt(1,pf,8,8,1,"1",0,tt$)
14600   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
14700   fnlbl(4,1,"Message on Bill:",ll,1)
14800   fntxt(4,pf,30,30)
14900   resp$(respc+=1)=mg$(1)
15000   fntxt(5,pf,30,30)
15100   resp$(respc+=1)=mg$(2)
15200   fntxt(6,pf,30,30)
15300   resp$(respc+=1)=mg$(3)
15400   fnlbl(7,1,"Date of Billing:",ll,1)
15500   fntxt(7,pf,8,8,1,"1")
15600   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
15700   fnlbl(8,1,"Starting Route/Sequence:",ll,1)
15800   fncombof("ubm-act-nam",8,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2)
15900   resp$(respc+=1)="[All]"
16000   fnlbl(9,1,"Route Number:",ll,1)
16100   fncmbrt2(9,pf)
16200   resp$(respc+=1)="[All]"
16300   fnchk(10,pf,"Select Accounts to Print",1)
16400   resp$(respc+=1)="False"
16500   fncmdset(3)
16600   fnacs(sn$,0,mat resp$,ck)
16700   if ck=5 then goto ENDSCR
16800   d1=val(resp$(5))
16900   d4=val(resp$(1))
17000   mg$(1)=resp$(2)
17100   mg$(2)=resp$(3)
17200   mg$(3)=resp$(4)
17300   if resp$(6)="[All]" then 
17400     a$=""
17500   else 
17600     a$=lpad$(trim$(resp$(6)(1:10)),10)
17700   end if 
17800   if resp$(7)="[All]" then 
17900     prtbkno=0
18000   else 
18100     prtbkno=val(resp$(7))
18200   end if 
18300   if resp$(8)="True" then sl1=1 else sl1=0
18400   if trim$(a$)<>"" then 
18500     read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
18600     st1=1
18700   end if 
18800 L480: form pos 1,c 10,pos 1741,n 2,n 7
18900   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
19000   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
19100   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
19200 ! ______________________________________________________________________
19300   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
19400   fnPa_open("Landscape")
19600 ! ______________________________________________________________________
19700 ! IF SL1=0 THEN GOSUB SORT1
19800 L570: if sl1=1 then goto SCREEN3
19900   if s5=0 then goto L640
20000 L590: read #7,using L600: r6 eof F5_CANCEL
20100 L600: form pos 1,pd 3
20200   read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ norec L590
20300   read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,use_alt nokey L590
20400   goto L650
20500 L640: !
20520 mat ba$=('') 
20530  read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,use_alt eof F5_CANCEL
20600 L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1854,pd 5.2
20700   if prtbkno=0 then goto L680
20800   if prtbkno><route then goto F5_CANCEL
20900 L680: if f><d1 then goto L570
21000   if st1=0 then goto HERE
21100   if st1$=z$ then st1=0 else goto L570
21200 HERE: ! 
21300 ! read alternate billing address
21400   read #3,using L740,key=z$: mat ba$ nokey L810
21500 L740: form pos 11,4*c 30
21600   e1=0 : mat pe$=("")
21700   for j=1 to 4
21800     if rtrm$(ba$(j))<>"" then 
21900       e1=e1+1
22000       pe$(e1)=ba$(j)
22100     end if 
22200   next j
22300   goto L950
22400 ! ______________________________________________________________________
22500 L810: e1=0 : mat pe$=("")
22600   for j=2 to 4
22700     if rtrm$(e$(j))<>"" then 
22800       e1=e1+1
22900       pe$(e1)=e$(j)
23000     end if 
23100   next j
23200   goto L950
23300 ! ______________________________________________________________________
23400 F5_CANCEL: ! 
23500   close #1: ioerr L890
23600 L890: close #3: ioerr L900
23700 L900: ! 
23800 ! close #20: ioerr L920
23900 L920: fnpa_finis
24000   goto ENDSCR
24100 ! ______________________________________________________________________
24200 L950: ! 
24300   pb=bal-g(11)
24400 ! ______________print bill routine______________________________________
24500   fn_vbprint
24600 ! _____________end of pr routine______________________________________
24700   bct(2)=bct(2)+1
24800 ! accumulate totals
24900   goto L570
25000 ! ______________________________________________________________________
25100 SCREEN3: ! 
25200   sn$="UBPrtBl1-2"
25300   fntos(sn$)
25400   txt$="Account (blank to stop)"
25500   fnlbl(1,1,txt$,31,1)
25600   if trim$(a$)="" then goto L1070 else goto L1080
25700 L1070: if z$<>"" then 
25800     txt$="Last Account entered was "&z$
25900     fnlbl(3,1,txt$,44,1)
26000   else 
26100     txt$=""
26200     fnlbl(3,1,txt$,44,1)
26300   end if 
26400 L1080: fncmbact(1,17) ! 
26500   resp$(1)=a$
26600   fncmdset(11): fnacs(sn$,0,mat resp$,ck)
26700   if ck=5 then goto F5_CANCEL
26800   a$=lpad$(trim$(resp$(1)(1:10)),10)
26900   if trim$(a$)="" then goto F5_CANCEL
27000   read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,use_alt nokey SCREEN3
27100   goto HERE
27200 ! ______________________________________________________________________
27300 SORT1: ! SELECT & SORT
27400   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1410
27500   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
27600   s5=1
27700   if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
27800   restore #2,search>=routekey$: 
27900 L1210: read #2,using L1220: z$,f,route eof END5
28000 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
28100   if prtbkno=0 then goto L1250
28200   if prtbkno><route then goto END5
28300 L1250: if f><d1 then goto L1210
28400   zip5$=cr$=""
28500   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
28600 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
28700   goto L1210
28800 ! ______________________________________________________________________
28900 END5: close #6: 
29000   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
29100 L1330: form pos 1,c 128
29200   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
29300   write #9,using L1330: "Mask 1,19,C,A"
29400   close #9: 
29500   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
29600 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
29700   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
29800   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
29900 L1410: return 
30000 ! ______________________________________________________________________
30100 ENDSCR: ! pr totals screen
30200   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
30300   fntos(sn$="Bills-Total")
30400   mylen=23 : mypos=mylen+2
30500   respc=0
30600   fnlbl(1,1,"Total Bills Printed:",mylen,1)
30700   fntxt(1,mypos,8,0,1,"",1)
30800   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
31100   fncmdset(52)
31200   fnacs(sn$,0,mat resp$,ck)
31300 XIT: fnxit
31400 ! ______________________________________________________________________
31500 ERTN: fnerror(program$,err,line,act$,"xit")
31600   if uprc$(act$)<>"PAUSE" then goto L1550
31700   execute "List -"&str$(line) : pause : goto L1550
31800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
31900 L1550: execute act$
32000   goto ERTN
32100 ! ______________________________________________________________________
32200   def fn_vbprint
32300 ! -- Printer Program for New Laser Utility Bills
32400     checkcounter+=1
32500     if checkcounter=1 then xmargin=1 : ymargin=0
32600     if checkcounter=2 then xmargin=142 : ymargin=0
32700     if checkcounter=3 then xmargin=1 : ymargin=110
32800     if checkcounter=4 then xmargin=142 : ymargin=110 : checkcounter=0
32900     col2_adj=65
33000 ! ___________________________
33100 ! - CONSTANTS
33200     lyne=3
33300     character=1.5
33400 ! pr #20: 'Call Print.MyOrientation("Landscape")'
33500     pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',55,'&str$(lyne*3+3)&',True)'
33600     pr #20: "Call Print.MyFontBold(True)"
33700     pr #20: 'Call Print.MyFontSize(12)'
33800     pr #20: 'Call Print.MyFont("Courier New")'
33900 ! pr #20: 'Call Print.MyFontColor("Green")'
34000     fnpa_txt(at$(1),xmargin+6,lyne*1-1+ymargin)
34100     pr #20: 'Call Print.MyFont("Lucida Console")'
34200     pr #20: 'Call Print.MyFontSize(10)'
34300     pr #20: 'Call Print.MyFontBold(False)'
34400     fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.65)
34500     fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
34600     pr #20: 'Call Print.MyFontColor("Black")'
34700     fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
34800     fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
34900     pr #20: 'Call Print.AddText("THIS BILL IS NOW DUE AND",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
35000     pr #20: 'Call Print.AddText("PAYABLE",'&str$(xmargin+2)&','&str$(lyne*9+ymargin)&')'
35100     pr #20: 'Call Print.AddText("Billing Date: ",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
35200     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+30)&','&str$(lyne*11+ymargin)&')'
35300     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&','&str$(linelength)&',0)'
35400     pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
35500     pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
35600     pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
35700 ! ___________________________
35800 PRINTGRID: meter=14
35900     pr #20: 'Call Print.MyFontSize(8)'
36000 ! d(1)=123456789 : d(3)=123456789 : g(1)=123456.89 : g(2)=123456.89 : d(9)=123456789 : d(11)=123456789 : g(4)=123456.89 : g(5)=123456.89 : g(6)=123456.89 : g(8)=123456.89 : g(9)=123456.89 : pB=123456.89
36100     if g(1) then 
36200       pr #20: 'Call Print.AddText("WA",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
36300       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
36400       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
36500       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
36600     end if 
36700     if g(2) then 
36800       pr #20: 'Call Print.AddText("SW",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
36900       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
37000     end if 
37100     if g(4)=0 then 
37200       pr #20: 'Call Print.AddText("GS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
37300       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*(meter)+ymargin)&')'
37400       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*(meter)+ymargin)&')'
37500       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
37600     end if 
37700     if g(5) then 
37800       pr #20: 'Call Print.AddText("WS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
37900       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
38000     end if 
38100     if g(6) then 
38200       pr #20: 'Call Print.AddText("SS",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
38300       pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
38400     end if 
38500     if g(8) then 
38600       pr #20: 'Call Print.AddText("OC",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
38700       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
38800     end if 
38900     if g(9) then 
39000       pr #20: 'Call Print.AddText("TX",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
39100       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
39200     end if 
39300     if pb then 
39400       pr #20: 'Call Print.AddText("PB",'&str$(xmargin+2)&','&str$(lyne*(meter+=1)+ymargin)&')'
39500       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
39600     end if 
39700     pr #20: 'Call Print.MyFontSize(10)'
39800 ! ___________________________
39900     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&','&str$(linelength)&',0)'
40000     pr #20: 'Call Print.MyFontSize(9)'
40100     pr #20: 'Call Print.AddText("Pay Now:",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
40200     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
40300     pr #20: 'Call Print.AddText("Pay After",'&str$(xmargin+1)&','&str$(lyne*25.5+ymargin)&')'
40400     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(xmargin+22)&','&str$(lyne*25.5+ymargin)&')'
40500     if bal>0 then 
40600       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
40700     else 
40800       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25.5+ymargin)&')'
40900     end if 
41000     pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*27+1+ymargin)&','&str$(linelength)&',0)'
41100     fnpa_txt(mg$(1),xmargin+1,lyne*29+ymargin)
41200     fnpa_txt(mg$(2),xmargin+1,lyne*30+ymargin)
41300     fnpa_txt(mg$(3),xmargin+1,lyne*31+ymargin)
41400     pr #20: 'Call Print.MyFontSize(7)'
41500     pr #20: 'Call Print.AddText("Springfield",'&str$(xmargin+80)&','&str$(lyne*2-1+ymargin)&')'
41600     pr #20: 'Call Print.AddText("     IL    ",'&str$(xmargin+80)&','&str$(lyne*3-1+ymargin)&')'
41700     pr #20: 'Call Print.AddText("    62702  ",'&str$(xmargin+80)&','&str$(lyne*4-1+ymargin)&')'
41800     pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*4+2)&',True)'
41900     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
42000     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
42100     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
42200     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
42300     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
42400     pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
42500     pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
42600     pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
42700     pr #20: 'Call Print.AddText(" Paid One Ounce ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
42800     pr #20: 'Call Print.AddText("  Permit No.916 ",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
42900     pr #20: 'Call Print.MyFontSize(9)'
43000     pr #20: 'Call Print.AddText("Please return this side with",'&str$(xmargin+col2_adj)&','&str$(lyne*8+ymargin)&')'
43100     pr #20: 'Call Print.AddText("payment to:  '&cnam$&'",'&str$(xmargin+col2_adj)&','&str$(lyne*9+ymargin)&')'
43200     pr #20: 'Call Print.MyFontSize(10)'
43300     pr #20: 'Call Print.AddText("Pay Now:",'&str$(xmargin+col2_adj)&','&str$(lyne*11+ymargin)&')'
43400     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
43500     pr #20: 'Call Print.AddText("After",'&str$(xmargin+col2_adj)&','&str$(lyne*12+ymargin)&')'
43600     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+80)&','&str$(lyne*12+ymargin)&')'
43700     if bal>0 then 
43800       pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
43900     end if 
44000     if bal<=0 then 
44100       pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
44200     end if 
44300     fnpa_txt('#'&trim$(z$),xmargin+col2_adj,lyne*15+ymargin)
44400     addy=16
44500     if use_alt=1 then 
44600       fnpa_txt(ba$(1),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
44700       fnpa_txt(ba$(2),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
44800       fnpa_txt(ba$(4),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
44900     else 
45000       fnpa_txt(e$(2),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
45100       fnpa_txt(e$(3),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
45200       fnpa_txt(e$(4),xmargin+col2_adj,lyne*(addy+=1)+ymargin)
45300     end if 
45400     if checkcounter=0 then 
45500       fnpa_newpage
45600     end if 
45700   fnend 
45800 ! ______________________________________________________________________
