20000 ! formerly S:\acsUB\ubPenCal
20020 ! Penalty Calculation
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnerror,fntos,fnopenprn,fncloseprn,fnmsgbox,fnxit,fndate_mmddyy_to_ccyymmdd,fndat,fnd1,fncmdset,fntop,fnchk,fnfra,fnopt,fnget_services
20080   on error goto ERTN
20100 ! ______________________________________________________________________
20120   dim resp$(8)*40,msgline$(1)*80,oldtg(11)
20140   dim z$*10,g(12),dat$*20,e$(4)*30,rt(10,3),transkey$*19
20160   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),tg(11),route(99)
20180   dim servicename$(10)*20,tax_code$(10)*1,pencolumn(10)
20200   dim d(15)
20220   dim subjectto(10),gb(10),extra(23),a(7)
20240   dim columnhead$(10)*13,tmp$*220,coltot(10),basepenalty(10)
20260 ! ______________________________________________________________________
20280   let right=1
20300   let fntop(program$)
20320   let fnd1(bildat)
20340   let fndat(dat$)
20400 ! r:  get MinimumBal
20420   open #minbal:=5: "Name="&env$('Q')&"\UBmstr\Minbal.H"&env$('cno')&",Use,RecL=10,Shr",internal,outin,relative 
20440   read #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal norec SET_DEFAULT_MINUMUMBAL
20460   goto EO_MINIMUMBAL
20480 SET_DEFAULT_MINUMUMBAL: ! 
20500   let minimumbal=1.00
20520   write #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal
20540 EO_MINIMUMBAL: ! 
20560   close #minbal: 
20580 ! /r
20600   let fn_scr_main
20620   if ck=5 then goto XIT
20640 ! 
20660   if env$('client')="Sangamon" then 
20680     let fn_scr_route_range
20700     if ck=5 then goto XIT
20720   end if 
20740 ! 
20760   open #h_customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
20780   open #h_trans:=2: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno')&",Shr",internal,outin,keyed 
20800 ! 
20820   fnget_services(mat servicename$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
20880 ! 
20900   for j=1 to 10
20920     if uprc$(penalty$(j))="Y" then 
20940       let pencount=pencount+1
20960       let column(pencount)=j
20980       let columnhead$(pencount)=lpad$(rtrm$(servicename$(j)(1:10)),10)
21000 ! count number of penalty columns needed
21020     end if 
21040   next j
21060   if pencount<1 then let pencount=1
21080   mat pencolumn(pencount)
21100   mat columnhead$(pencount)
21120   mat coltot(pencount)
21140   let fn_bud1
21160   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
21180   let fnopenprn
21200   let fn_print_header
21220   do 
21240 READ_CUSTOMER: ! 
21260     read #h_customer,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2,POS 1741,N 2,N 7,2*N 6,N 9,PD 5.2,N 3,3*N 9,3*N 2,3*N 3,N 1,3*N 9,3*PD 5.2,pos 217,15*pd 5': z$,mat e$,mat a,bal,f,mat g,mat gb,mat extra,mat d eof EO_CUSTOMER
21270     ! if env$('acsDeveloper')<>'' and trim$(z$)='100001.00' then let debug_this_account=1 else debug_this_account=0
21272     if debug_this_account then let show_math=1 else let show_math=0
21280     let route_number=extra(1)
21300 !   if env$('client')="Divernon" and bal<0 then goto READ_CUSTOMER
21320     if env$('client')="Sangamon" and (route_number<prtbkno1 or route_number>prtbkno2) then goto READ_CUSTOMER ! bill certain routes at different billing dates
21340     if bud1=1 then 
21360       let fn_bud2
21380       if env$('client')='White Hall' and bd1>0 then goto READ_CUSTOMER ! Never penalize if Budget Billing
21400       if totba>0 and bd1>0 and f=bildat then goto EO_READ ! Penalize if Budget Bill and Havent Paid Last Bill, Even If BAL <0
21420       if totba>0 and bd1=0 then goto READ_CUSTOMER ! have budget billing and have paid last bill
21440     end if 
21460     if bal=0 or bal<minimumbal then goto READ_CUSTOMER
21480     if env$('client')='Sangamon' then goto EO_READ
21500     if f<>bildat then goto READ_CUSTOMER
21520 EO_READ: ! 
21540     let fn_pencal
21560   loop 
21580 ! 
21600 EO_CUSTOMER: ! 
21620   let fn_print_totals
21640   close #h_customer: ioerr ignore
21660   close #h_trans: ioerr ignore
21680   let fncloseprn
21700   goto XIT
21720 ! ______________________________________________________________________
21740 PGOF: ! 
21760   print #255: newpage
21780   let fn_print_header
21800   continue 
21820 ! ______________________________________________________________________
21840 XIT: let fnxit
21860 IGNORE: continue 
21880 ! ______________________________________________________________________
21900 ! <Updateable Region: ERTN>
21920 ERTN: let fnerror(program$,err,line,act$,"xit")
21940   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
21960   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
21980   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
22000 ERTN_EXEC_ACT: execute act$ : goto ERTN
22020 ! /region
22040 ! ______________________________________________________________________
22060 def fn_scr_main
22080   SM_ASK: ! 
22100   let fntos(sn$="ubPenCal")
22120   let mylen=27
22140   let mypos=mylen+2
22160   let fnlbl(1,1,"Penalty Date:",mylen,right)
22180   let fntxt(1,mypos,10,0,1,"1003")
22200   let resp$(1)=str$(pendat)
22220   let fnlbl(2,1,"Last Billing Date:",mylen,right)
22240   let fntxt(2,mypos,10,0,1,"1003")
22260   let resp$(2)=str$(bildat)
22280   let fnlbl(3,1,"Report Heading Date:",mylen,right)
22300   let fntxt(3,mypos,20)
22320   let resp$(3)=dat$
22340   let fnchk(4,31,"Print Meter Address:",right)
22360   let resp$(4)="False"
22380   let fnchk(5,31,"Print Mailing Address:",right)
22400   let resp$(5)="False"
22420   let fnlbl(6,1,"Minimum Balance:",mylen,right)
22440   let fntxt(6,mypos,8,0,1,"10",0,"The customer's balance must be at least this amount before a penalty will be calculated.")
22460   let resp$(6)=str$(minimumbal)
22480   let fnfra(8,1,2,45,"Base for calculating penalty","The penalty can either be calculated on current bill or the total balance owed.",0)
22500   let fnopt(1,2,"Base penalty on current bill",0,1)
22520   let resp$(7)="True"
22540   let fnopt(2,2,"Base penalty on total balance",0,1)
22560   let resp$(8)="False"
22580   if env$('client')="Colyell" or env$('client')="Sangamon" or env$('client')="Cerro Gordo" then ! can change the default on that to: Base penalty on total balance
22600     let resp$(7)="False"
22620     let resp$(8)="True"
22640   end if  ! env$('client')=...
22660   let fncmdset(2)
22680   let fnacs(sn$,0,mat resp$,ck)
22700   if ck<>5 then 
22720     let pendat=val(resp$(1)(5:6)&resp$(1)(7:8)&resp$(1)(3:4))
22740     let bildat=val(resp$(2)(5:6)&resp$(2)(7:8)&resp$(2)(3:4))
22760     let dat$=resp$(3)
22780     let fndat(dat$,2)
22800     if resp$(4)="True" then let printadr=1 ! wants meter address printed
22820     if resp$(5)="True" then let printmail=1 ! wants meter mailing address
22840     let minimumbal=val(resp$(6))
22860     open #minbal:=5: "Name="&env$('Q')&"\UBmstr\Minbal.H"&env$('cno')&",Use,RecL=10,Shr",internal,outin,relative 
22880     rewrite #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal
22900     close #minbal: 
22920     if resp$(7)="True" then let penaltybase$="Bill" ! base penalties on current bill
22940     if resp$(8)="True" then let penaltybase$="Balance" ! base penalties on current balance
22960     if pendat=0 then 
22980       let msgline$(1)="You must enter a valid Penalty Date"
23000       let fnmsgbox(mat msgline$,pause$,'',48)
23020       goto SM_ASK
23040     else if bildat=0 then 
23060       let msgline$(1)="You must enter a valid Last Billing Date."
23080       let fnmsgbox(mat msgline$,pause$,'',48)
23100       goto SM_ASK
23120     end if 
23140     let pendat=fndate_mmddyy_to_ccyymmdd(pendat)
23160   end if 
23180 fnend 
23200 def fn_scr_route_range ! Sangamon's second screen
23220   let fntos(sn$="ubPenCalb")
23240   let fnlbl(1,1,"First Route #:",27,1)
23260   let fntxt(1,29,2,0,1,"30",0,"Enter the first route number that is subject to a penalty on this penalty date")
23280   let resp$(1)=str$(prtbkno1)
23300   let fnlbl(2,1,"Last Route #:",27,1)
23320   let fntxt(2,29,2,0,1,"30")
23340   let resp$(2)=str$(prtbkno2)
23360   let fncmdset(2)
23380   let fnacs(sn$,0,mat resp$,ck)
23400   if ck<>5 then 
23420     let prtbkno1=val(resp$(1))
23440     let prtbkno2=val(resp$(2))
23460   end if 
23480 fnend 
23500 def fn_pencal ! penalty calculation
23520   let negatives=0 ! if breakdown is by mat gb and mat gb has some negatives, then do something special
23540   mat basepenalty=(0)
23545   if show_math then 
23549     print #255: ''
23553     print #255: '*Math for Account: '&z$&'  Penalty Based on '&penaltybase$
23557   end if 
23560   if env$('client')="Galena" then 
23580     let bdiff=sum(mat gb)-bal
23600     let gb(8)=gb(8)-bdiff
23620   end if 
23640   if env$('client')='Sangamon' or env$('client')='Colyell' or env$('client')='White Hall' then 
23650     let basepenalty(10)=bal
23660     goto GOT_BASEPENALTY
23700   else if env$('client')='Lovington' and penaltybase$="Balance" then 
23720     let waterpercent=round(g(1)/(g(1)+g(2)),2) ! let sewerpercent=1-waterpercent ! lovington
23740     let basepenalty(9)=round((bal*waterpercent),2) ! logington
23760     let basepenalty(10)=bal-basepenalty(9) ! lovington allocate water and sewer penalty in ration of water to sewer
23780     goto GOT_BASEPENALTY ! lovington
23800   end if 
23810  ! if debug_this_account then pause
23820   if env$('client')="Franklinton" then let fn_franklinton : goto L1370
23840   if penaltybase$="Bill" and bal<g(11) then ! use the balance to calculate penalty if balance less than last bill
23841     if show_math then print #255: '     *** use the balance ('&str$(bal)&') to calculate penalty if balance less than last bill ('&str$(g(11))&')( let usebalance=1 )'
23843     let usebalance=1
23845   else 
23847     let usebalance=0
23849   end if 
23850   ! If SUM(GB)<>BAL Then Let BASE=0 : Goto 990 ! if mat gb screwed up, just use the balance to calculate the penalty  kj 20110  afraid to leave in
23852   ! if trim$(z$)='100090.00' then pr z$ : pause
23860   for j=1 to 10
23870     ! r: get Base
23880     if totba>0 and bd1>0 and subjectto(j)<>0 then ! have budget and have not paid last payment
23882       if show_math then print #255: '     have budget and have not paid last payment'
23884       if show_math then print #255: '       Base set to '&str$(g(j))
23886       let base=g(j)
23888       !       goto L1060
23900     else if penaltybase$="Bill" and usebalance=0 then ! base on current bill
23905       if show_math then print #255: '     Base on current bill ( if penaltybase$="Bill" and usebalance=0 )'
23906       if show_math then print #255: '       usebalance='&str$(usebalance)
23910       if show_math then print #255: '       Base set to '&str$(g(j))
23915       let base=g(j)
23920       !       goto L1060
23925     else if (penaltybase$="Balance" or usebalance=1) and subjectto(j)<>0 and gb(j)>0 then 
23930       if show_math then print #255: '     (penaltybase$="Balance" or usebalance=1) and subjectto('&str$(j)&')<>0 and gb('&str$(j)&')'
23935       if show_math then print #255: '       Base set to '&str$(gb(j))
23940       let base=gb(j)
23945     else 
23946       if show_math then print #255: '     base set to zero'
23950       let base=0 ! base on balance
23955     end if 
23960     if (penaltybase$="Balance" or usebalance=1) and subjectto(j)<>0 and gb(j)<0 then 
23962       let negatives+=gb(j)
23964     end if 
23980    ! L1060: !
23990    ! if debug_this_account then pr ' mat basepenalty should get set here subjectto('&str$(j)&')=';subjectto(j) : pause
24000     if subjectto(j)>0 then ! accumulate all charges by the penalty they are subject to
24002       if show_math then 
24004         print #255: '     accumulate all charges by the penalty they are subject to'
24006         print #255: '       basepenalty('&str$(subjectto(j))&')=itself + base(which is '&str$(base)&') totalling '&str$(basepenalty(subjectto(j))+base)
24008       end if 
24010       let basepenalty(subjectto(j))=basepenalty(subjectto(j))+base
24012     end if 
24020     if (penaltybase$="Balance" or usebalance=1) and subjectto(j)=0 and penalty$(j)="Y" then ! if the service is any of the penalties, add the previous penalty into the balance
24022       if show_math then 
24024         print #255: '     if the service is any of the penalties, add the previous penalty into the balance'
24026         print #255: '       basepenalty('&str$(j)&')=itself + base(which is '&str$(base)&') totalling '&str$(basepenalty(j)+base)
24028       end if 
24030       let basepenalty(j)=basepenalty(j)+base
24032     end if 
24040   next j
24060   if env$('client')="Kimberling" and g(2)>0 then let basepenalty(10)-=g(1) ! no penalty on water if they have swewer
24062   if show_math and negatives then print #255: '     negatives='&str$(negatives)&' doing fn_worryabout'
24080   if negatives<>0 then let fn_worryabout
24100   GOT_BASEPENALTY: ! 
24102   if show_math then 
24104     for j=1 to 10
24106       if basepenalty(j)<>0 then 
24108         print #255: '     Base Penalty ('&str$(j)&')= '&str$(basepenalty(j))
24110       end if 
24112     next j
24114   end if 
24116   ! /r
24120   mat tg=(0) ! accumulate penalties in mat tg (transaction record)
24140   let column=0
24160   mat pencolumn=(0)
24180   for j=1 to 10
24200     if uprc$(penalty$(j))="Y" then 
24220       let penaltycode$=uprc$(service$(j))
24240       if j<6 then let pencode=a(j) ! rate codes in customer layout are not in order.  The first 5 a( match the services. The next three services are pulled from mat extra. 9 and 10 use a(6)&a(7)
24260       if j=6 then let pencode=extra(11)
24280       if j=7 then let pencode=extra(12)
24300       if j=8 then let pencode=extra(13)
24320       if j=9 then let pencode=a(6)
24340       if j=10 then let pencode=a(7)
24360       if env$('client')="Pennington" and pencode=0 then let pencode=1 ! default to one so codes don't have to be added to old customer records
24380       if env$('client')="Granby" and pencode=0 then let pencode=1 ! default to one so codes don't have to be added to old customer records
24400       read #ratemst,using 'Form POS 55,32*G 10',key=penaltycode$&lpad$(str$(pencode),2): mc1,mu1,mat rt nokey L1230
24420       goto L1240
24440       L1230: ! 
24460       let rt(1,3)=0
24480       let mc1=0 ! nokey but still let amount take up a column and calculate 0
24500       L1240: ! 
24520       if env$('client')="Riverside" and j=10 then ! r:
24540         let g(10)=round(rt(1,3)*min(mc1,g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)),2)
24560         let g(10)=g(10)+rt(2,3)*round(max(0,g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)-mc1),2)
24580         let g(10)=max(0,g(10))
24600         let tg(10)=g(10)
24620       else ! /r
24640         ! if debug_this_account then pause
24642         ! if env$('client')='Exeter' then let basepenalty(9)=basepenalty(10) : basepenalty(10)=0  ! XXX  This is for the first month only!
24645         if show_math and round(basepenalty(j)*rt(1,3),2)<>0 then 
24650           print #255: '     calculated Penalty Base is '&penaltybase$&' '&str$(basepenalty(j))
24655           print #255: '     Penalty: tg('&str$(j)&')=round('&str$(basepenalty(j))&'*'&str$(rt(1,3))&',2)'
24660           let tg(j)=round(basepenalty(j)*rt(1,3),2)
24665           print #255: '     tg('&str$(j)&')='&str$(tg(j))
24670         else 
24675           let tg(j)=round(basepenalty(j)*rt(1,3),2) ! penalty based on base amount that was accumulated for each penalty field * rate for that penalty code
24680         end if 
24685         if show_math then 
24690           print #255: '     let tg('&str$(j)&')=max('&str$(mc1)&','&str$(tg(j))&')'
24695         end if 
24700         let tg(j)=max(mc1,tg(j))
24705         if env$('client')="Pennington" then ! r:
24720           if a(6)=0 then let a(6)=1 ! default tax code to 1
24740           read #ratemst,using 'Form POS 55,32*G 10',key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey L1330 ! read Pennington's sales tax rate
24760           ! let tg(j)=round(tg(j)*(1+rt(1,3)),2) ! Pennington adds sales tax to penalty; but now they say they don't.  Not sure what happened there.
24780         end if  ! /r
24800       end if 
24820       let column+=1
24840       let tg(j)=max(0,tg(j)) ! no negative penalties
24860       let pencolumn(column)=tg(j)
24880       let coltot(column)=coltot(column)+tg(j) ! place first penalty in first column, column totals, etc
24900     end if 
24920   next j
24940   ! if env$('client')="Kimberling" then let fn_flat_percent_on_priorbal(.75) ! add .75% of previous balance
24960   ! if env$('client')="Thayer" or env$('client')='Exeter' then let fn_flat_percent_on_lastbill(10) ! add 10% of last month's bill
24962   if env$('client')="Thayer" then let fn_flat_percent_on_lastbill(10) ! add 10% of last month's bill
24964   if env$('client')='Edison' then let fn_flat_amt(5)
24980   if sum(mat tg)=0 then goto XIT_PENCAL ! skip if no penalty calculated prb 10/13/11
25000   if penaltybase$="Balance" then let fn_check_rounding ! some times the penalty calculated at the time of the bill is off a penny because this program rounds each calculation for each service. Not always same as calculating on total!
25020   L1330: ! 
25040   let bal+=sum(mat tg)
25060   let tot+=sum(mat tg)
25080   let tcode=2 ! penalty trans code
25100   for j=1 to 10
25120     if tg(j)<>0 then let gb(j)+=tg(j) ! add new penalties into balance breakdown if here is a penalty
25140   next j
25160   L1370: ! 
25180   let transkey$=z$&cnvrt$("pic(########)",pendat)&cnvrt$("pic(#)",tcode)
25200   let tamount=sum(mat tg) ! add all penalties into total transaction amount
25220   read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=transkey$: y$,olddate,oldcode,oldamount,mat oldtg nokey WRITE_2 ! check for recalk
25240   let bal=bal-oldamount
25260   for j=1 to 10
25280     let gb(j)=gb(j)-oldtg(j) ! take off of balance breakdown
25300   next j
25320   rewrite #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
25340   goto PAST_WRITE_2
25360   ! __
25380   WRITE_2: ! 
25400   write #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
25420   PAST_WRITE_2: ! 
25440   rewrite #h_customer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2': bal,mat gb
25460   ! 
25480   let totb+=bal
25500   if route_number<0 or route_number>99 then let route_number=99
25520   let route(route_number)+=sum(pencolumn)
25540   let fn_print_record
25560   XIT_PENCAL: ! 
25580 fnend 
25600 def fn_print_header
25620   ! let p+=1
25640   print #255: "\qc "&env$('cnam')
25660   print #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&" - Listing}"
25680   print #255: dat$
25700   print #255: "\ql   "
25720   let tmp$="{\ul Account No  } {\ul Customer Name and Address      }         {\ul "
25740   for j=1 to udim(columnhead$)
25760     let tmp$=tmp$&columnhead$(j)&"} { \ul "
25780   next j
25800   ! if env$('client')="Kimberling" then
25820   !   let tmp$=tmp$&"} {\ul  Interest} {\ul     Balance}"
25840   if printadr=1 then 
25860     let tmp$=tmp$&"} {\ul   Balance}  {\ul  Meter Address}"
25880   else 
25900     let tmp$=tmp$&"} {\ul   Balance}"
25920   end if 
25940   print #255: tmp$
25960   print #255: ""
25980 fnend 
26000 def fn_print_record
26020   ! 
26040   ! if env$('client')="Kimberling" then
26060   !  int_tmp=PENCOLUMN(1)-G(10) ! MAX(0,round((g(12)-g(11))*.0075,2))
26080   !  pen_tmp=G(10) ! MAX(0,round(pencolumn(1)-int_tmp,2))
26100   !  print #255,using F_PRLINE_KIM: z$,e$(2),pen_tmp,int_tmp,bal pageoflow PGOF
26120   !  F_PRLINE_KIM: form pos 1,c 10,x 4,c 30,pos 52,2*pic(---------.##),x 2,pic(-------.##),x 2,c 25
26140   !  pen_accum+=pen_tmp
26160   !  int_accum+=int_tmp
26180   if printadr=1 then 
26200     print #255,using F_PRLINE: z$,e$(2),mat pencolumn,bal,e$(1)(1:25) pageoflow PGOF
26220   else 
26240     print #255,using F_PRLINE: z$,e$(2),mat pencolumn,bal pageoflow PGOF
26260   end if 
26280   if printmail=1 then 
26300     print #255,using "Form POS 15,C 30": e$(3) pageoflow PGOF
26320     print #255,using "Form POS 15,C 30": e$(4) pageoflow PGOF
26340   end if  ! printmail=1
26360   F_PRLINE: form pos 1,c 10,x 4,c 30,pos 52,pencount*pic(---------.##),x 2,pic(-------.##),x 2,c 25
26380 fnend 
26400 def fn_print_totals
26420   let tmp$=rpt$(" ",52)&"{\ul"&rpt$(" ",12)&"}"
26440   let column_count=udim(mat columnhead$)
26460   ! if env$('client')="Kimberling" then column_count+=1
26480   for j=1 to column_count
26500     let tmp$=tmp$&" {\ul"&rpt$(" ",12)&"}"
26520   next j
26540   print #255: tmp$
26560   ! if env$('client')="Kimberling" then
26580   !   print #255,using "Form POS 17,C 30,x 5,3*N 12.2": "Overall Totals",pen_accum,int_accum,totb
26600   ! else 
26620   print #255,using "Form POS 17,C 30,x 5,pencount*N 12.2,N 12.2": "Overall Totals",mat coltot,totb
26640   ! end if
26660   let tmp$=rpt$(" ",52)&"{\ul \strike"&rpt$(" ",12)&"}"
26680   for j=1 to column_count
26700     let tmp$=tmp$&" {\ul \strike"&rpt$(" ",12)&"}"
26720   next j
26740   print #255: tmp$
26760   print #255,using 'form skip 2,c 20': "Totals by Route"
26780   for j=1 to 99
26800     if route(j)<>0 then 
26820       print #255,using "form pos 1,c 10,pic(zzz,zzz.zz)": "Route "&cnvrt$("pic(zz)",j),route(j) pageoflow PGOF
26840     end if 
26860   next j
26880   restore #1: 
26900   do 
26920     read #1,using "Form POS 292,PD 4.2": bal eof PT_EO1
26940     let totalbal=totalbal+bal
26960   loop 
26980   PT_EO1: ! 
27000   print #255: 
27020   let balpos=51+(pencount*12)
27040   print #255,using 'form pos 15,c 30,pos balpos,pic($-,---,---.##)': "Total Due from all Customers",totalbal
27060 fnend 
27080 def fn_bud1
27100   let bud1=0
27120   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr EO_BUD1
27140   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
27160   let bud1=1
27180   EO_BUD1: ! 
27200 fnend 
27220 def fn_bud2
27240   let totba=bd1=bd2=0
27260   mat bd1(5)
27280   mat bd1=(0)
27300   mat bd2=(0)
27320   if bud1=0 then goto EO_BUD2
27340   read #81,using F_BUD_2A,key=z$: z$,mat ba,mat badr nokey EO_BUD2
27360   F_BUD_2A: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
27380   for j=2 to 12
27400     let totba=totba+ba(j)
27420   next j
27440   let ta1=badr(1)
27460   do 
27480     if ta1=0 then goto EO_BUD2
27500     read #82,using "form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3",rec=ta1: z$,mat bt1,nba norec EO_BUD2
27520     if bt1(14,1)<=0 then 
27540       if bt1(12,1)<>0 then ! don't allow blank records to go thru routine
27560         let bd1=bd1+1
27580       end if 
27600       if bd1>5 then goto EO_BUD2
27620     end if 
27640     let ta1=nba
27660   loop 
27680   EO_BUD2: ! 
27700 fnend 
27720 def fn_worryabout
27740   if sum(basepenalty)<>0 then 
27760     let negativepercent=round((sum(basepenalty)+negatives)/sum(basepenalty),2)
27780     for j=1 to 10
27800       let basepenalty(j)=round(basepenalty(j)*negativepercent,2)
27820     next j
27840   end if 
27860 fnend 
27880 def fn_check_rounding
27900   !   make sure (if they are basing on balance due) that the individual calculations add to the same amount as the rate * the balance
27920   if round(bal*rt(1,3),2)=sum(mat tg) then goto CR_XIT
27940   let adjustment=0
27960   if round(bal*rt(1,3),2)-.01=sum(mat tg) then let adjustment=.01 : goto CR_DOIT ! add one cent to a penalty breakdown amount
27980   if round(bal*rt(1,3),2)-.02=sum(mat tg) then let adjustment=.02 : goto CR_DOIT ! add one cent to a penalty breakdown amount
28000   if round(bal*rt(1,3),2)+.01=sum(mat tg) then let adjustment=-.01 : goto CR_DOIT ! subtract one cent from a penalty breakdown amount
28020   if round(bal*rt(1,3),2)+.02=sum(mat tg) then let adjustment=-.02 : goto CR_DOIT ! subtract one cent from a penalty breakdown amount
28040   goto CR_XIT
28060   CR_DOIT: ! 
28080   let penx=0
28100   for j=1 to 10 ! put rounding adjustment in first service that has a penalty
28120     if basepenalty(j)>0 then 
28140       let tg(j)=tg(j)+adjustment
28160       let penx+=1
28180       let coltot(penx)=coltot(penx)+adjustment
28200       goto CR_XIT
28220     end if 
28240   next j
28260   CR_XIT: ! 
28280 fnend  
28300 def fn_franklinton
28320   let pentot=0
28340   let gb(1)=gb(1)+round(g(1)*.1,2)
28360   let gb(2)=gb(2)+round(g(2)*.1,2)
28380   let gb(5)=gb(5)+round(g(5)*.1,2)
28400   let gb(8)=gb(8)+round(g(8)*.1,2)
28420   let pentot=pentot+round(g(1)*.1,2)+round(g(2)*.1,2)+round(g(5)*.1,2)+round(g(8)*.1,2)
28440   if a(4)=3 then 
28460     let gb(4)=gb(4)+round(g(4)*.02,2)
28480     let pentot=pentot+round(g(4)*.02,2)
28500   else 
28520     let gb(4)=gb(4)+round(d(11)*.005,2)
28540     let pentot=pentot+round(d(11)*.005,2)
28560   end if 
28580   mat tg=(0)
28600   let tg(10)=pentot
28620   let bal+=sum(mat tg)
28640   let tot+=sum(mat tg)
28660   let tcode=2 ! penalty trans code
28680   let pencolumn(1)=tg(10)
28700   let coltot(1)=coltot(1)+tg(10)
28720 fnend 
28740 def fn_flat_percent_on_lastbill(percentage)
28760   let percentage=percentage/100
28780   let tg(10)=round(tg(10)+g(11)*percentage,2)
28800   let tg(10)=max(0,tg(10))
28820   let pencolumn(1)=tg(10)
28840   let coltot(1)=coltot(1)+tg(10)
28860 fnend 
28880 ! def fn_flat_percent_on_priorbal(percentage) ! not really the prior balance - but sorta is
28900 !  let percentage=percentage/100
28920 !  let pbal=bal-g(11)
28940 !  let tg(10)=round(tg(10)+pbal*percentage,2)
28960 !  let tg(10)=max(0,tg(10))
28980 !  let pencolumn(1)=tg(10)
29000 !  let coltot(1)=coltot(1)+tg(10)
29020 ! fnend
42000 def fn_flat_amt(penaltyAmount)
42020   if basepenalty>0 then
42040     tg(10)=penaltyAmount
42060   end if
42080 fnend
