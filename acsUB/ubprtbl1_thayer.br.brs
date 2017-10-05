10000 ! Replace S:\acsUB\ubprtbl1_thayer
10200 ! pr bills for Village of Thayer  (new 4 per page 10/24/06)
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt,fnpa_line
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
11400   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
11600   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
11800 ! ______________________________________________________________________
12000   fncno(cno,cnam$)
12200   fnd1(d1)
12400   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
12600   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
12800   close #21: 
13000   at$(1)=cnam$
13200   z=21
13400   at$(1)=trim$(at$(1))(1:z)
13600   x=len(at$(1)) : y=z-x
13800   at$(1)=rpt$(" ",int(y/2))&at$(1)
14000   z=26
14200   for j=2 to udim(at$)
14400     at$(j)=trim$(at$(j))(1:z)
14600     x=len(at$(j)) : y=z-x
14800     at$(j)=rpt$(" ",int(y/2))&at$(j)
15000   next j
15200   linelength=62
15400 ! 
15600 ! 
15800   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
16000   fn_bulksort
16200   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
16400   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
16600 ! 
16800 ! ______________________________________________________________________
17000 SCREEN1: ! 
17200   a$="" : prtbkno=0
17400   fntos(sn$="UBPrtBl1-1T")
17600   pf=32 : ll=30
17800   respc=0
18000   fnlbl(2,1,"Penalty Due Date:",ll,1)
18200   fntxt(2,pf,8,8,1,"1",0,tt$)
18400   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
18600   fnlbl(4,1,"Message on Bill:",ll,1)
18800   fntxt(4,pf,30,30)
19000   resp$(respc+=1)=mg$(1)
19200   fntxt(5,pf,30,30)
19400   resp$(respc+=1)=mg$(2)
19600   fntxt(6,pf,30,30)
19800   resp$(respc+=1)=mg$(3)
20000   fnlbl(8,1,"Date of Billing:",ll,1)
20200   fntxt(8,pf,8,8,1,"1")
20400   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
20600   fnlbl(10,1,"Prior Reading Date Override:",ll,1)
20800   fntxt(10,pf,8,8,1,"1")
21000   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_prior_s1)
21200   fnlbl(11,1,"Current Reading Date Override:",ll,1)
21400   fntxt(11,pf,8,8,1,"1")
21600   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_cur_s1)
21800   fncmdset(3)
22000   fnacs(sn$,0,mat resp$,ck)
22200   if ck=5 then goto ENDSCR
22400   d4=val(resp$(1))
22600   mg$(1)=resp$(2)
22800   mg$(2)=resp$(3)
23000   mg$(3)=resp$(4)
23200   d1=val(resp$(5))
23400   reading_date_prior_s1=val(resp$(6))
23600   reading_date_cur_s1=val(resp$(7))
23800   a$=""
24000   prtbkno=0
24200   sl1=0
24400   if trim$(a$)<>"" then 
24600     read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
24800     holdz$=z$: begin=1
25000     st1=1
25200   end if 
25400 L460: form pos 1,c 10,pos 1741,n 2,n 7
25600   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
25800   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
26000   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
26200 ! ______________________________________________________________________
26400   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
26600   gosub VBOPENPRINT ! Open #20: "Name="&env$('Q')&"\UBmstr\Bill"&WSID$&".txt,Replace,RecL=5000",Display,Output
26800 ! .   ! fnOPENPRN
27000 ! ______________________________________________________________________
27200   on fkey 5 goto RELEASE_PRINT
27400 L550: if sl1=1 then goto SCREEN3
27600 L560: read #6,using L590: z$ eof RELEASE_PRINT
27800   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
28000   begin=0 ! cancel starting account
28200 L590: form pos 22,c 10
28400   read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,escrow nokey L560
28600 L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1859,pd 5.2
28800   if prtbkno=0 then goto L640
29000   if prtbkno><route then goto RELEASE_PRINT
29200 L640: if f><d1 then goto L550
29400   if st1=0 then goto READALTADR
29600 ! If ST1$=Z$ Then sT1=0 Else Goto 560
29800 READALTADR: ! 
30000 ! read alternate billing address
30200   read #3,using L700,key=z$: mat ba$ nokey L770
30400 L700: form pos 11,4*c 30
30600   e1=0 : mat pe$=("")
30800   for j=1 to 4
31000     if rtrm$(ba$(j))<>"" then 
31200       e1=e1+1
31400       pe$(e1)=ba$(j)
31600     end if 
31800   next j
32000   goto L920
32200 ! ______________________________________________________________________
32400 L770: e1=0 : mat pe$=("")
32600   for j=2 to 4
32800     if rtrm$(e$(j))<>"" then 
33000       e1=e1+1
33200       pe$(e1)=e$(j)
33400     end if 
33600   next j
33800   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
34000   goto L920
34200 ! ______________________________________________________________________
34400 RELEASE_PRINT: ! 
34600   close #1: ioerr L860
34800 L860: close #3: ioerr L870
35000 L870: ! 
35200 ! 
35400   fnpa_finis
35800   goto ENDSCR
36000 ! ______________________________________________________________________
36200 L920: ! 
36400   pb=bal-g(11)
36600   if bal<=0 then g(5)=g(6)=g(7)=0 ! don't show penalty if balance 0 or less
36800 ! ______________print bill routine______________________________________
37000   fn_vbprint
37200 ! _____________end of pr routine______________________________________
37400   bct(2)=bct(2)+1
37600 ! .   ! accumulate totals
37800   goto L550
38000 ! ______________________________________________________________________
38200 SCREEN3: ! 
38400   sn$="UBPrtBl1-2"
38600   fntos(sn$)
38800   txt$="Account (blank to stop)"
39000   fnlbl(1,1,txt$,31,1)
39200 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
39400   if trim$(z$)<>"" then 
39600     txt$="Last Account entered was "&z$
39800     fnlbl(3,1,txt$,44,1)
40000   else 
40200     txt$=""
40400     fnlbl(3,1,txt$,44,1)
40600   end if 
40800   fncmbact(1,17) ! 
41000   resp$(1)=a$
41200   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
41400   a$=lpad$(trim$(resp$(1)(1:10)),10)
41600   if trim$(a$)="" then goto RELEASE_PRINT
41800   if ck=5 then goto RELEASE_PRINT
42000   read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,escrow nokey SCREEN3
42200   goto READALTADR
42400 ! ______________________________________________________________________
42600 SORT1: ! SELECT & SORT
42800   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1390
43000   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
43200   s5=1
43400   if prtbkno=0 then 
43600     routekey$=""
43800   else 
44000     routekey$=cnvrt$("N 2",prtbkno)&"       "
44200 ! .    ! key off first record in route (route # no longer part of customer #)
44400   end if 
44600   restore #2,search>=routekey$: 
44800 L1190: read #2,using L1200: z$,f,route eof END5
45000 L1200: form pos 1,c 10,pos 296,pd 4,pos 1741
45200   if prtbkno=0 then goto L1230
45400   if prtbkno><route then goto END5
45600 L1230: if f><d1 then goto L1190
45800   zip5$=cr$=""
46000   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1260
46200 L1260: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
46400   goto L1190
46600 ! ______________________________________________________________________
46800 END5: close #6: 
47000   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
47200 L1310: form pos 1,c 128
47400   write #9,using L1310: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
47600   write #9,using L1310: "Mask 1,19,C,A"
47800   close #9: 
48000   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1360
48200 L1360: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
48400   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
48600   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
48800 L1390: return 
49000 ! ______________________________________________________________________
49200 ENDSCR: ! pr totals screen
49400   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
49600   fntos(sn$="Bills-Total")
49800   mylen=23 : mypos=mylen+2
50000   respc=0
50200   fnlbl(1,1,"Total Bills Printed:",mylen,1)
50400   fntxt(1,mypos,8,0,1,"",1)
50600   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
51200 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
51400 ! fnTXT(2,MYPOS,8,0,1,"",1)
51600 ! .   ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
51800 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
52000 ! fnTXT(3,MYPOS,8,0,1,"",1)
52200 ! .   ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
52400 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
52600 ! fnTXT(4,MYPOS,8,0,1,"",1)
52800 ! .   ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
53000   fncmdset(52)
53200   fnacs(sn$,0,mat resp$,ck)
53400 XIT: fnxit
53600 ! ______________________________________________________________________
53800 ERTN: fnerror(program$,err,line,act$,"xit")
54000   if uprc$(act$)<>"PAUSE" then goto L1590
54200   execute "List -"&str$(line) : pause : goto L1590
54400   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
54600 L1590: execute act$
54800   goto ERTN
55000 ! ______________________________________________________________________
55200 VBOPENPRINT: ! 
55600     fnpa_open("Landscape")
56400     lyne=3
57000   return 
57200 ! ______________________________________________________________________
57400   def fn_vbprint
57600 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
57800     billcounter+=1
58000     if billcounter=1 then xmargin=4 : ymargin=5
58200     if billcounter=2 then xmargin=143 : ymargin=5
58400     if billcounter=3 then xmargin=4 : ymargin=113
58600     if billcounter=4 then xmargin=143 : ymargin=113 : billcounter=0
58800 ! ______________________________________________________________________
59000 ! pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
59200     if reading_date_cur_s1=0 then reading_date_cur=d3 else reading_date_cur=reading_date_cur_s1
59400     if reading_date_prior_s1=0 then reading_date_prior=d2 else reading_date_prior=reading_date_prior_s1
59600     fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3, 1)
59800     pr #20: "Call Print.MyFontBold(True)"
60000     pr #20: 'Call Print.MyFontSize(12)'
60200     pr #20: 'Call Print.MyFont("Courier New")'
60400     pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
60600     pr #20: 'Call Print.MyFont("Lucida Console")'
60800     pr #20: 'Call Print.MyFontSize(10)'
61000     pr #20: 'Call Print.MyFontBold(False)'
61200     pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
61400     pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
61600     fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
61800     pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
62000     pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_prior/100))&'  To: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_cur/100))&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
62200     pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
62400     pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
62600 ! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
62800     fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
63000     pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
63200     pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
63400     pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
63600 ! ______________________________________________________________________
63800 PRINTGRID: ! 
64000     meter=14
64200     pr #20: 'Call Print.MyFontSize(8)'
64400     if g(1)<>0 then 
64600       pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
64800       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
65000       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
65200       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
65400     end if 
65600     if g(2)<>0 then 
65800       pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
66000       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
66200     end if 
66400     if g(3)<>0 then 
66600       pr #20: 'Call Print.AddText("Admin",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
66800       pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
67000     end if 
67200     if a4=1 then 
67400       gcode$="RSGS"
67600     else if a4=2 then 
67800       gcode$="CMGS"
68000     else if a4=3 then 
68200       gcode$="INGS"
68400     else 
68600       gcode$="GAS"
68800     end if 
69000     if g(4)<>0 then 
69200       pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
69400       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
69600       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
69800       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
70000     end if 
70200     if g(5)<>0 or g(6)<>0 or g(7)<>0 then 
70400       pr #20: 'Call Print.AddText("PEN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
70600       pr #20: 'Call Print.AddText("'&fnformnumb$(g(5)+g(6)+g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
70800     end if 
71000 ! if g(6)<>0 then
71200 !  pr #20: 'Call Print.AddText("FUR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
71400 !  pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
71600 ! . end if
71800     if g(10)<>0 then 
72000       pr #20: 'Call Print.AddText("Penalty",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
72200       pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
72400     end if 
72600     if g(8)<>0 then 
72800       pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
73000       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
73200     end if 
73400     if g(9)<>0 then 
73600       pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
73800       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
74000     end if 
74200     if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
74400     if pb><0 then 
74600       pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')'
74800       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(10)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
75000     end if 
75200     if pb<>0 then 
75400       pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
75600       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
75800     end if 
76000 ! If ESCROW>0 Then
76200 ! pr #20: 'Call Print.AddText("Escrow CR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
76400 ! pr #20: 'Call Print.AddText("'&fnformnumb$(escrow,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'\
76600 ! end if
76800     pr #20: 'Call Print.MyFontSize(10)'
77000 ! ______________________________________________________________________
77200     if estimatedate=d1 then let fnpa_txt("Bill estimated!",xmargin+1,lyne*27+ymargin)
77400     fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
77600     fnpa_txt("Pay By    "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+1,lyne*24+ymargin)
77800     fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
78000     fnpa_txt("Pay After "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+1,lyne*25+ymargin)
78200     if bal>0 then 
78400       fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+42,lyne*25+ymargin)
78600     else 
78800       fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*25+ymargin)
79000     end if 
79200     fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
79400 ! fnpa_txt("  Office 217-628-3416",xmargin+1,lyne*28.5+ymargin)
79600     fnpa_txt("Address Correction Requested",xmargin+1,lyne*27.9+ymargin)
79610     fnpa_txt(mg$(1),xmargin+1,29*lyne+ymargin)
79612     fnpa_txt(mg$(2),xmargin+1,32*lyne+ymargin)
79614     fnpa_txt(mg$(3),xmargin+1,33*lyne+ymargin)
79800 ! ______________________________________________________________________
80000     special=28
80200 ! ______________________________________________________________________
80400     pr #20: 'Call Print.MyFontSize(7)'
80600     pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
80800 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
81000 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
81200 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
81400 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
81600 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
81800 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
82000 ! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
82200     fnpa_txt("FIRST CLASS MAIL",xmargin+100,lyne*1-1+ymargin)
82400     fnpa_txt("U.S. POSTAGE PAID",xmargin+100,lyne*2-1+ymargin)
82600     fnpa_txt("   ONE OUNCE",xmargin+100,lyne*3-1+ymargin)
82800     fnpa_txt("THAYER, IL 62689",xmargin+100,lyne*4-1+ymargin)
83000     fnpa_txt("  PERMIT NO. 1",xmargin+100,lyne*5-1+ymargin)
83200     pr #20: 'Call Print.MyFontSize(9)'
83400     fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
83600     fnpa_txt("side with payment to",xmargin+68,lyne*8+ymargin)
83800     fnpa_txt("Thayer Water/Sewer Dept.",xmargin+68,lyne*9+ymargin)
84000     pr #20: 'Call Print.MyFontSize(10)'
84200     fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*11+ymargin)
84400     fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
84600     fnpa_txt("After  "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*12+ymargin)
84800     if bal>0 then 
85000 ! fnpa_txt('3Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',csp-2,factor+line_height*11)
85200       fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+106,lyne*12+ymargin)
85400     else 
85600       fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*12+ymargin)
85800     end if 
86000     pr #20: 'Call Print.MyFontSize(9)'
86200     addy=18 ! 14
86400 ! fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
86600 ! fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
86800 ! fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
87000 ! addy+=1
87200     pr #20: 'Call Print.MyFontSize(10)'
87400     if df$="Y" then 
87600       fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
87800     end if 
88000     if final>0 then let fnpa_txt("Final Bill",xmargin+1,lyne*(addy+5)+ymargin)
88200     pr #20: 'Call Print.MyFontSize(12)'
88400     addy+=.5
88600     fnpa_txt('#'&trim$(z$),xmargin+75,lyne*(addy+=1.1)+ymargin)
88800     if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+75,lyne*(addy+=1.1)+ymargin)
89000     if pe$(2)<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+75,lyne*(addy+=1.1)+ymargin)
89200     if pe$(3)<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+75,lyne*(addy+=1.1)+ymargin)
89400     if pe$(4)<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+75,lyne*(addy+=1.1)+ymargin)
89600 ! 
89800     if billcounter=1 then checkx=1.375 : checky=3.6875
90000     if billcounter=2 then checkx=6.75 : checky=3.6875
90200     if billcounter=3 then checkx=1.375 : checky=7.9375
90400     if billcounter=0 then checkx=6.75 : checky=7.9375
90600     bc$=""
90800     if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
91000     if billcounter=0 then 
91200       fnpa_newpage
91400     end if 
91600   fnend 
91800 ! ______________________________________________________________________
92000   def fn_bulksort ! bulk sort order
92200     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
92400     open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
92600 L2790: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2820
92800     write #6,using "Form POS 1,C 12,n 2,n 7,c 10": z$,0,0,z$ ! just do it in account order
93000 ! write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$ ! origional order
93200     goto L2790
93400 L2820: close #1: ioerr L2830
93600 L2830: close #6: ioerr L2840
93800 L2840: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2860
94000     open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
94200 L2860: ! 
94400   fnend 
