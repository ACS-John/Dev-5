00010 ! Replace S:\acsUB\ubPenCa2
00020 ! Additional Penalty Calculation ( calculates a standard dollar amount of penalty of each penalty charge in their system
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnerror,fntos,fnopenprn,fncloseprn,fnmsgbox,fnxit,fndate_mmddyy_to_ccyymmdd,fncno,fndat,fnd1,fncmdset,fntop,fnchk,fncreg_read,fncreg_write,fnget_services
00050   on error goto ERTN
14000 ! ______________________________________________________________________
14020   dim resp$(9)*40,msgline$(1)*80,oldtg(11)
14040   dim z$*10,g(12),dat$*20,e$(4)*30,cnam$*40,cap$*128,transkey$*19
14060   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),tg(11),route(99)
14080   dim servicename$(10)*20,tax_code$(10)*1,pencolumn(10)
14100   dim penalty$(10)*1,subjectto(10),gb(10),extra(23),a(7)
14120   dim columnhead$(10)*13,tmp$*220,coltot(10)
14140 ! ______________________________________________________________________
18000   let fntop(program$,cap$="Additional Penalty Calculation")
18020   let fncno(cno,cnam$)
18040   let fnd1(bildat)
18060   let fndat(dat$)
18080 ! 
18100   let fnget_services(mat servicename$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
18120   for j=1 to 10
18140     if uprc$(penalty$(j))="Y" then 
18160       let pencount=pencount+1 ! count number of penalty columns needed
18180       let column(pencount)=j
18200       let columnhead$(pencount)=lpad$(rtrm$(servicename$(j)(1:10)),10)
18220     end if 
18240   next j
18260   if pencount<1 then let pencount=1
18280   mat pencolumn(pencount)
18300   mat columnhead$(pencount)
18320   mat coltot(pencount)
18340 ! 
18360   let fncreg_read('Second Penalty Calculation Min Balance',minimumbal$) : let minimumbal=val(minimumbal$) conv ignore
18380   let fncreg_read('Second Penalty Calculation Penalty Amount',penaltyamt$) : let penaltyamt=val(penaltyamt$) conv ignore
18400   let fncreg_read('Second Penalty Calculation Skip Service 10 Rate 9 Customers',skip_s10r9$) ! : let penaltyamt=val(penaltyamt$) conv ignore
18420   if minimumbal=0 then 
18440     open #minbal:=5: "Name="&env$('Q')&"\UBmstr\Minbal.H"&str$(cno)&",Shr",internal,outin,relative ioerr ignore
18460     read #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal ioerr ignore
18480     close #minbal: ioerr ignore
18500   end if 
18520 ! 
28000 SCREEN1: ! 
28020   let fntos(sn$="ubPenCa2")
28040   let mylen=29 : let mypos=mylen+2
28060   let fnlbl(1,1,"Penalty Date:",mylen,1)
28080   let fntxt(1,mypos,10,0,1,"1003")
28100   let resp$(1)=str$(pendat)
28120   let fnlbl(2,1,"Last Billing Date:",mylen,1)
28140   let fntxt(2,mypos,10,0,1,"1003")
28160   let resp$(2)=str$(bildat)
28180   let fnlbl(3,1,"Report Heading Date:",mylen,1)
28200   let fntxt(3,mypos,20)
28220   let resp$(3)=dat$
28240   let fnchk(4,31,"Print Meter Address:",1)
28260   let resp$(4)="False"
28280   let fnchk(5,31,"Print Mailing Address:",1)
28300   let resp$(5)="False"
28320   let fnlbl(6,1,"Minimum Balance:",mylen,1)
28340   let fntxt(6,mypos,8,0,1,"10",0,"The customer's balance must be at least this amount before a penalty will be calculated.")
28360   let resp$(6)=str$(minimumbal)
28380   let fnlbl(7,1,"Penalty Amount:",mylen,1)
28400   let fntxt(7,mypos,8,0,1,"10",0,"Amount of penalty.")
28420   let resp$(7)=str$(penaltyamt)
28440   let fnchk(9,31,"Skip Customers with a "&trim$(servicename$(10))&" Rate Code of 9",1)
28460   let resp$(8)=skip_s10r9$
28480   let fncmdset(2)
28500   let fnacs(sn$,0,mat resp$,ck)
32000   if ck=5 then goto XIT
32020   let pendat=val(resp$(1)(5:6)&resp$(1)(7:8)&resp$(1)(3:4))
32040   let bildat=val(resp$(2)(5:6)&resp$(2)(7:8)&resp$(2)(3:4))
32060   let dat$=resp$(3)
32080   if resp$(4)="True" then let printadr=1 ! wants meter address printed
32100   if resp$(5)="True" then let printmail=1 ! wants meter mailing address
32120   let minimumbal=val(resp$(6))
32140   let penaltyamt=val(resp$(7))
32160   let skip_s10r9$=resp$(8)
32180 ! 
34000   if pendat=0 then 
34020     let msgline$(1)="You must enter a valid Penalty Date"
34040     let fnmsgbox(mat msgline$,pause$,cap$,48)
34060     goto SCREEN1
34080   else 
34100     let pendat=fndate_mmddyy_to_ccyymmdd(pendat)
34120   end if 
34140   if bildat=0 then 
34160     let msgline$(1)="You must enter a valid Last Billing Date."
34180     let fnmsgbox(mat msgline$,pause$,cap$,48)
34200     goto SCREEN1
34220   end if 
34240 ! 
36000   let fncreg_write('Second Penalty Calculation Min Balance',str$(minimumbal))
36020   let fncreg_write('Second Penalty Calculation Penalty Amount',str$(penaltyamt))
36040   let fncreg_write('Second Penalty Calculation Skip Service 10 Rate 9 Customers',skip_s10r9$)
36060   let fndat(dat$,2)
36080 ! 
38100   open #customer=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
38120   open #h_trans:=2: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&str$(cno)&",Shr",internal,outin,keyed 
38140   gosub BUD1
38160   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
38180   let fnopenprn
38200   gosub HDR
42000 READ_CUSTOMER: ! r:
42020   read #customer,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2,POS 1741,N 2,N 7,2*N 6,N 9,PD 5.2,N 3,3*N 9,3*N 2,3*N 3,N 1,3*N 9,3*PD 5.2,C 30,7*C 12,3*C 30': z$,mat e$,mat a,bal,f,mat g,mat gb,mat extra eof EO_CUSTOMER
42040 ! If TRIM$(Z$)="100120.06" Then Pause
42060   if skip_s10r9$='True' and a(7)=9 then 
42080 !   pr 'skipping '&z$&' because service 10 rate code is a 9'
42100 !   pause
42120     goto READ_CUSTOMER
42140   end if 
42160   if bud1=1 then gosub BUD2
42180   if totba>0 and bd1>0 and f=bildat and g(10)>0 then goto EO_READ ! Penalize if Budget Bill and Havent Paid Last Bill, Even If BAL <0
42200   if bal=0 or bal<minimumbal then goto READ_CUSTOMER
42220   if totba>0 and bd1=0 then goto READ_CUSTOMER ! have budget billing and have paid last bill
42240 ! If G(10)=0 Then Goto 430
42260   if f<>bildat then goto READ_CUSTOMER
42280 EO_READ: ! 
42300   goto PENCAL ! /r
42320 ! ______________________________________________________________________
46000 PENCAL: ! r: penalty calculation
46020   mat tg=(0)
46040   let column=0
46060   for j=1 to 10
46080     if uprc$(penalty$(j))="Y" then ! place first penalty in first column, column totals, etc
46100       let tg(j)=penaltyamt
46120       let column+=1
46140       let pencolumn(column)=tg(j)
46160       let coltot(column)=coltot(column)+tg(j)
46200     end if 
46220   next j
46240   let bal+=sum(tg)
46260 ! let tot+=sum(tg)
46280   let tcode=2 ! penalty trans code
46300   for j=1 to 10
46320     if tg(j)<>0 then let gb(j)+=tg(j) ! add new penalties into balance breakdown if here is a penalty
46340   next j
46360   let transkey$=z$&cnvrt$("pic(########)",pendat)&cnvrt$("pic(#)",tcode)
46380   let tamount=sum(tg)
46400 ! add all penalties into total transaction amount
46420   read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=transkey$: y$,olddate,oldcode,oldamount,mat oldtg nokey L990 ! check for recalk
46440   let bal=bal-oldamount
46460   for j=1 to 10
46480     let gb(j)=gb(j)-tg(j) ! take off of balance breakdown
46500   next j
46520   rewrite #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
46540   goto L1000
46560 ! __
46580 L990: ! 
46600   write #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
46620 L1000: ! 
46640   let totb+=bal
46660   rewrite #customer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2': bal,mat gb
46680   if extra(1)<0 or extra(1)>99 then let extra(1)=99
46700   let route(extra(1))+=sum(pencolumn)
46720 ! print extra(1)
46740   if printadr<>1 then 
46760     print #255,using F_PRINT_LINE: z$,e$(2),mat pencolumn,bal pageoflow PGOF
46780   end if 
46800   if printadr=1 then 
46820     print #255,using F_PRINT_LINE: z$,e$(2),mat pencolumn,bal,e$(1)(1:25) pageoflow PGOF
46840   end if 
46860   if printmail=1 then 
46880     print #255,using "Form POS 15,C 30": e$(3) pageoflow PGOF
46900     print #255,using "Form POS 15,C 30": e$(4) pageoflow PGOF
46920   end if 
46940 F_PRINT_LINE: form pos 1,c 10,x 4,c 30,pos 52,pencount*pic(---------.##),x 2,pic(-------.##),x 2,c 25
46960   goto READ_CUSTOMER
46980 ! ______________________________________________________________________
48000 EO_CUSTOMER: ! 
48020   let tmp$= rpt$(" ",52)&"{\ul"&rpt$(" ",12)&"}"
48040   for j=1 to udim(columnhead$)
48060     let tmp$=tmp$&" {\ul"&rpt$(" ",12)&"}"
48080   next j
48100   print #255: tmp$
48120   print #255,using "Form POS 17,C 30,x 5,pencount*N 12.2,N 12.2": "Overall Totals",mat coltot,totb
48140   let tmp$= rpt$(" ",52)&"{\ul \strike"&rpt$(" ",12)&"}"
48160   for j=1 to udim(columnhead$)
48180     let tmp$=tmp$&" {\ul \strike"&rpt$(" ",12)&"}"
48200   next j
48220   print #255: tmp$
48240   print #255,using 'form skip 2,c 20': "Totals by Route"
48260   for j=1 to 99
48280     if route(j)<>0 then 
48300       print #255,using "form pos 1,c 10,pic(zzz,zzz.zz)": "Route "&cnvrt$("pic(zz)",j),route(j) pageoflow PGOF_NO_HDR
48320     end if 
48330   next j
48340   close #customer: ioerr ignore
48360   close #2: ioerr ignore
48380   let fncloseprn
48400   goto XIT ! /r
50000 PGOF: ! r:
50020   print #255: newpage
50040   gosub HDR
50060   continue  ! /r
51000 PGOF_NO_HDR: ! r:
51020   print #255: newpage
51060   continue  ! /r
52000 HDR: ! r:
52020 ! let p+=1
52040   print #255: "\qc "&cnam$
52060   print #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&" - Listing}"
52080   print #255: dat$
52100   print #255: "\ql   "
52120   let tmp$="{\ul Account No  } {\ul Customer Name and Address      }         {\ul "
52140   for j=1 to udim(columnhead$)
52160     let tmp$=tmp$&columnhead$(j)&"} { \ul "
52180   next j
52200   if printadr=1 then let tmp$=tmp$&"} {\ul   Balance}  {\ul  Meter Address}" else let tmp$=tmp$&"} {\ul   Balance}"
52220   print #255: tmp$
52240   print #255: ""
52260   return  ! /r
54000 IGNORE: continue 
56000 XIT: let fnxit
58000 BUD1: ! r:
58020   let bud1=0
58040   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr EO_BUD1
58060   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&",Shr",internal,outin,relative 
58080   let bud1=1
58100 EO_BUD1: ! 
58120   return  ! /r
60000 BUD2: ! r:
60020   let totba=bd1=bd2=0
60040   mat bd1(5) : mat bd1=(0) : mat bd2=(0)
60060   if bud1=0 then goto EO_BUD2
60080   read #81,using L1520,key=z$: z$,mat ba,mat badr nokey EO_BUD2
60100   for j=2 to 12: let totba=totba+ba(j): next j
60120 L1520: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
60140   let ta1=badr(1)
60160 L1540: if ta1=0 then goto EO_BUD2
60180   read #82,using L1560,rec=ta1: z$,mat bt1,nba norec EO_BUD2
60200 L1560: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
60220   if bt1(14,1)>0 then goto L1610
60240   if bt1(12,1)=0 then goto L1600 ! don't allow blank records to go thru routine
60260   let bd1=bd1+1
60280 L1600: if bd1>5 then goto EO_BUD2
60300 L1610: let ta1=nba : goto L1540
60320 EO_BUD2: ! 
60340   return  ! /r
62000 ! <Updateable Region: ERTN>
62020 ERTN: let fnerror(program$,err,line,act$,"xit")
62040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
62060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
62100 ERTN_EXEC_ACT: execute act$ : goto ERTN
62120 ! /region
64000 ! r: Sangamon's second screen
64020   let fntos(sn$="ubPenCal22")
64040   let fnlbl(1,1,"First Route #:",27,1)
64060   let fntxt(1,29,2,0,1,"30",0,"Enter the first route number that is subject to a penalty on this penalty date")
64080   let resp$(1)=str$(prtbkno1)
64100   let fnlbl(2,1,"Last Route #:",27,1)
64120   let fntxt(2,29,2,0,1,"30")
64140   let resp$(2)=str$(prtbkno2)
64160   let fncmdset(2)
64180   let fnacs(sn$,0,mat resp$,ck)
64200   if ck=5 then goto XIT
64220   let prtbkno1=val(resp$(1))
64240   let prtbkno2=val(resp$(2))
64260   return  ! /r
66000 ! WORRYABOUT: ! r:
66010 !   dim basepenalty(10)
66020 !   if sum(basepenalty)<>0 then
66040 !     let negativepercent=round((sum(basepenalty)+negatives)/sum(basepenalty),2)
66060 !     for j=1 to 10
66080 !       let basepenalty(j)=round(basepenalty(j)*negativepercent,2)
66100 !     next j
66120 !   end if
66140 !   return  ! /r
68000 ! CHECK_ROUNDING: ! r:  make sure (if they are basing on balance due) that the individual calculations add to the same amount as the rate * the balance
68010 !   dim rt(10,3)
68020 !   if round(bal*rt(1,3),2) = sum(tg) then goto EO_CHECK_ROUNDING
68040 !   let adjustment=0
68060 !   if round(bal*rt(1,3),2)-.01 =sum(tg) then
68080 !     let adjustment=.01 ! add one cent to a penalty breakdown amount
68100 !   else if round(bal*rt(1,3),2)+.01 =sum(tg) then
68120 !     let adjustment=-.01 ! subtract one cent from a penalty breakdown amount
68140 !   else 
68160 !     goto EO_CHECK_ROUNDING
68180 !   end if
68200 !   let penx=0
68220 !   for j=1 to 10 ! put rounding adjustment in first service that has a penalty
68240 !     if basepenalty(j)>0 then
68260 !       let tg(j)=tg(j)+adjustment
68280 !       let penx+=1
68300 !       let coltot(penx)=coltot(penx)+adjustment
68320 !       goto EO_CHECK_ROUNDING
68340 !     end if
68360 !   next j
68380 ! EO_CHECK_ROUNDING: !
68400 !   return  ! /r
