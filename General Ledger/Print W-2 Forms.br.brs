10000 ! Replace S:\acsGL\PRW2
10020 ! r: setup
10040   library 'S:\Core\Library': fnxit,fntop, fnerror,fnpa_finis,fnpa_newpage,fnpa_open,fnNameParse,fnw3,fnAcs,fnCmdKey,fngethandle,fnmsgbox,fnTos,fnLbl,fnTxt,fnpa_background
10050   library 'S:\Core\Print\w2.br': fnask_w2_info,fnw2_text
10060   on error goto ERTN
10080 ! ______________________________________________________________________
10090   dim fw2box16$*255,ss$*11,s(13),t(13)
12100   ! dim txt$*80
12120   dim state$*2,a$(3)*40,b$(2)*12,controlNumber$*12,desc$(6)*15
12140   dim m(36),w(13)
12160   dim cap$*128
14120   dim tmpMsgLine$(0)*256
14140   !
14160   dim in4$(30)
14180   dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
14190   dim k$(3)*30
14200 ! ______________________________________________________________________
14220   fntop(program$,cap$="Print W-2 Forms")
14240   fw2box16$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
14260 ! 
14280 ! ______________________________________________________________________
14300   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outIn,relative
14320   read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2',rec=1: ficarate,ficawage,feducrat,feducwag 
14340   close #1: 
14360   ficarate=ficarate/100 
14380   feducrat=feducrat/100
14400   open #hCompany:=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input  
14420   read #hCompany,using 'Form POS 1,3*C 40,2*C 12,POS 618,40*N 1': mat a$,mat b$,mat dedcode,mat dedfed,mat dedfica,mat dedst 
14440   close #hCompany: 
14460   for j=1 to 3 : a$(j)=a$(j)(1:30) : next j
14480   for j=1 to 4 
14500     fl1$(j)=str$(j+7)&",28,N 1,UT,N" 
14520     fl1$(j+4)=str$(j+7)&",37,N 1,UT,N" 
14540   next j
14760   dim w2laser_output_filename$*256
14780   !
14800   dim w2Copy$*68
14940   !
14960   dim W2CopyFile$(6)*128,w2ssnMask(6)
14980   W2CopyFile$(1)='S:\Core\pdf\2016\W-2\Copy A.pdf' : w2ssnMask(1)=0
15000   W2CopyFile$(2)='S:\Core\pdf\2016\W-2\Copy 1.pdf' : w2ssnMask(2)=0
15020   W2CopyFile$(3)='S:\Core\pdf\2016\W-2\Copy B.pdf' : w2ssnMask(3)=0
15040   W2CopyFile$(4)='S:\Core\pdf\2016\W-2\Copy C.pdf' : w2ssnMask(4)=1
15060   W2CopyFile$(5)='S:\Core\pdf\2016\W-2\Copy 2.pdf' : w2ssnMask(5)=1
15080   W2CopyFile$(6)='S:\Core\pdf\2016\W-2\Copy D.pdf' : w2ssnMask(6)=1
15100   !

15300 ! /r
20000   ASK_INFO: !
20040    if ~fnask_w2_info(taxYear$,unusedmaybe_beg_date,unusedmaybe_end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,state$,enableAskCLocality:=1,cLocality$) then goto XIT
22000   ! r: open export files (if appropriate, return to ASK_INFO screen if failure
22020   if exportFormatID then 
22040     w2laser_output_filename$=srep$(w2laser_output_filename$,'[CompanyNumber]',env$('cno'))
22060     w2laser_output_filename$=srep$(w2laser_output_filename$,'[companynumber]',env$('cno'))
22080     w2laser_output_filename$=srep$(w2laser_output_filename$,'[COMPANYNUMBER]',env$('cno'))
22100     w2laser_output_filename$=srep$(w2laser_output_filename$,'[TaxYear]',taxYear$)
22120     w2laser_output_filename$=srep$(w2laser_output_filename$,'[taxyear]',taxYear$)
22140     w2laser_output_filename$=srep$(w2laser_output_filename$,'[TAXYEAR]',taxYear$)
22160     open #hExport:=fngethandle: "Name="&br_filename$(w2laser_output_filename$)&",REPLACE",display,output ioerr ASK_INFO
22180   end if
22200   ! /r

26000   ! r: open files, initialize output, etc
26020   if exportFormatID=0 then 
26040     fnpa_open('',w2Copy$,'PDF') 
26060   end if 
26080   open #hEmployee:=fngethandle: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
26100   box16=0
26120   open #hW2Box16:=fngethandle: "Name="&env$('Q')&"\GLmstr\W2Box16.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\W2INDEX.H"&env$('cno')&",Shr",internal,input,keyed ioerr w2b16openfail
26140   box16=1
26160   w2b16openfail: !
26240   cLocality$="NO"
26260   w2printCount=0
27120 ! /r
30000 READ_EMPLOYEE: ! r:
30010 do
30020   read #hEmployee,using 'form pos 1,n 4,3*c 25,c 11,36*pd 5.2': eno,mat k$,ss$,mat m eof FINIS
30060   if endnumb>0 and eno>endnumb then goto FINIS ! past ending #
30080   ! L870: !
30100   fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
30120   kz$=lpad$(str$(eno),8)
30140   if m(1)=0 then goto READ_EMPLOYEE
30160   dedfed=dedfica=dedst=0
30180   for j=1 to 10
30200     if dedcode(j)=1 then 
30220       if dedfed(j)=1 then dedfed=dedfed+m(j*2+9)
30240       if dedfica(j)=1 then dedfica=dedfica+m(j*2+9)
30260       if dedst(j)=1 then dedst=dedst+m(j*2+9)
30280     end if
30300   next j
30320   if eno=0 then goto READ_EMPLOYEE
30340   controlNumber$=str$(eno)
30360   empId$=b$(1) ! FEDERAL ID #
30380   stcode$=b$(2) ! STATE ID #
30400   w(1)=m(3) ! FED W/H YTD
30420   w(2)=m(1)-dedfed ! TOTAL TAXABLE WAGES
30440   w(3)=m(5) ! FICA W/H YTD
30460   w(4)=m(35) ! EIC TOTAL
30480   w(5)=m(1)-m(31)-dedfica ! TOTAL SS WAGES
30500   if m(5)=0 then w(5)=0 ! no ss wages if no ss wh
30520   w(6)=m(31) ! FICA TIPS YTD
30540   w(11)=w(5)+w(6) ! medicare wages = ss wages + tips
30560   w(7)=m(7) ! STATE WH
30580   w(9)=m(1)-dedst ! STATE WAGES
30600   if m(9)=0 then ! NO LOCAL WH
30610     printLocality$=""  
30612   else
30620     w(8)=m(9) ! LOCAL WITHHOLDING
30640     w(10)=m(1)-dedst ! LOCAL WAGES
30660     if uprc$(cLocality$)="YES" then gosub ASK_EMP_LOCALITY
30680     printLocality$=empLocality$ ! LOCALITY NAME
30700   end if
30720   if box16=1 then gosub BOX16_process
30740   w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
30760   w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
30780   sswh=min(round((w(5)+w(6))*ssrate,2),w(3))
30800   w(12)=w(3)-sswh ! MEDICARE WH
30820   w(3)=sswh ! SOCIAL SECURITY WITHHELD
35180       if exportFormatID=1 then 
35190         nqp=amt(1)+amt(2)
35200         gosub EXPORT_AMS
35210         x$=''
35220     ! else if exportFormatID=2 then 
35240     !   gosub EXPORT_CPS !  ! removed access 01/03/2017
35260       else 
35840         gosub PrintW2
35850       end if
35860   mat s=s+w
35880   mat desc$=("")
35900   box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
35920   mat amt=(0)
35940   wctr=wctr+1
35960   mat w=(0)
35980 loop ! /r
37000 FINIS: ! r:
37020   close #hEmployee: 
37080   close #hW2Box16: 
37100   if ~exportFormatID then
37120     mat t=t+s
37125     box12aCode$=''
37130     box12aAmt$=cnvrt$("Nz 10.2",t(13))
37135     mat w=t
37140     controlNumber$="FINAL TOTAL"
37160     nameFirst$=nameMiddle$=nameLast$=""
37180     mat k$=("")
37182     ss$=stcode$=printLocality$=""
37190     x$=" "
37200     gosub PRINTW2
37280     fnpa_finis
37300   end if
37320   if enableW3$="True" then let fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
37340   if exportFormatID then 
37360       mat tmpMsgLine$(2)
37380       tmpMsgLine$(1)='Export file created:'
37400       tmpMsgLine$(2)=os_filename$(file$(hExport))
37420       close #hExport:
37440       fnmsgbox(mat tmpMsgLine$,resp$,cap$) ! ,16+4)
37460     goto XIT
37500   end if
37520 goto XIT ! /r
38000 XIT: fnxit
52000 ASK_EMP_LOCALITY: ! r:
52020   fnTos(sn$="Prw2-5")
52040   rc=0
52060   mylen=30
52080   mypos=mylen+3
52100   fnLbl(1,1,k$(1),mylen,1,0,0)
52120   fnLbl(2,1,"Locality Name:",mylen,1,0,0)
52140   fnTxt(2,mypos,12,0,1,"",0,"Enter the Locality for this employee.",0)
52160   resp$(rc+=1)=empLocality$
52180   fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
52200   fnCmdKey("E&xit",5,0,1,"Returns to menu")
52220   fnAcs(sn$,0,mat resp$,ckey)
52240   if ckey=5 then goto XIT
52260   empLocality$=resp$(1)
52280   ! controlNumber$=rtrm$(controlNumber$)
52300   ! if controlNumber$="1" then goto L2770
52320   ! empLocality$=controlNumber$
52340   ! L2770: ! 
52360 return ! /r
64000 BOX16_process: ! r: Box 16
64020   ! passed hW2Box16,kz$, mat w, etc
64040 ! if trim$(kz$)='14' then pause
64060   read #hW2Box16,using fw2box16$,key=rpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey b16ReadLPad
64080   goto b16PastRead
64100   b16ReadLPad: !
64120   read #hW2Box16,using fw2box16$,key=lpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey B16Finis
64140   b16PastRead: !
64160   for j=1 to 6
64180     amt(j)=val(in4$(j*5-3))
64200     if in4$(j*5-2)="1" then w(2)+=amt(j)
64220     if in4$(j*5-1)="1" then w(5)+=amt(j)
64240     !   if env$('client')="Washington Parrish" then goto L3760
64260     if in4$(j*5-1)="1" then w(11)+=amt(j)
64280     ! L3760: !
64300     if in4$(j*5-0)="1" then w(9)+=amt(j)
64320     if in4$(j*5-2)="2" then w(2)=w(2)-amt(j)
64340     if in4$(j*5-1)="2" then w(5)=w(5)-amt(j)
64360     !   if env$('client')="Washington Parrish" then goto L3810
64380     if in4$(j*5-1)="2" then w(11)=w(11)-amt(j)
64400     ! L3810: ! 
64420     if in4$(j*5-0)="2" then w(9)=w(9)-amt(j)
64440     if j=1 then 
64460       desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&ltrm$(cnvrt$("Nz 10.2",amt(j))),15)
64480     else if j=2 then
64500       desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&cnvrt$("Nz 10.2",amt(j)),15)
64520     else if j=3 then
64540       box12aCode$=in4$(j*5-4)(1:2)
64560       box12aAmt$=cnvrt$("Nz 10.2",amt(j))
64580     else if j=4 then
64600       box12bCode$=in4$(j*5-4)(1:2)
64620       box12bAmt$=cnvrt$("Nz 10.2",amt(j))
64640     else if j=5 then
64660       box12cCode$=in4$(j*5-4)(1:2)
64680       box12cAmt$=cnvrt$("Nz 10.2",amt(j))
64700     else if j=6 then
64720       box12dCode$=in4$(j*5-4)(1:2)
64740       box12dAmt$=cnvrt$("Nz 10.2",amt(j))
64760     end if
64780     if (j=3 or j=4) and (in4$(j*5-4)(1:1)="D" or in4$(j*5-4)(1:1)="E" or in4$(j*5-4)(1:1)="F" or in4$(j*5-4)(1:1)="H") then w(13)=w(13)+amt(j) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
64800   next j
64820 B16Finis: !
64840 return ! /r
66000 EXPORT_AMS: ! r: LASER W2 FOR ADVANCED MICRO SOLUTIONS
66120   pr #hExport: "ROAN="&controlNumber$
66140   pr #hExport: "FEIN="&empId$
66160   pr #hExport: "WAGES="&str$(w(2))
66180   pr #hExport: "FITW="&str$(w(1))
66200   pr #hExport: "PNAME1="&a$(1)
66220   pr #hExport: "PNAME2="
66240   pr #hExport: "SSWAGES="&str$(w(5))
66260   pr #hExport: "SSWH="&str$(w(3))
66280   pr #hExport: "PADDR1="&a$(2)
66300   pr #hExport: "PADDR2="&a$(3)
66320   pr #hExport: "MCWAGES="&str$(w(11))
66340   pr #hExport: "MCWH="&str$(w(12))
66360   pr #hExport: "SSN="&srep$(ss$,' ','')
66380   pr #hExport: "SSTIPS="&str$(w(6))
66400   pr #hExport: "ALLOCATIP="  ! "ALLOCATIP=";0
66420   pr #hExport: "RNAME1="&(rtrm$(nameLast$)&","&nameFirst$)(1:24)
66440   pr #hExport: "RNAME2="&(k$(2)(1:24))
66460 ! pr #hExport: "AEIC=";w(4)    ! this field is no longer supported 1/4/2017
66480   pr #hExport: "DEPDCARE="&str$(dcb)
66500   pr #hExport: "RADDR1="
66520   pr #hExport: "RADDR2="&(k$(3)(1:24))
66540   pr #hExport: "LAB14A="
66560   pr #hExport: "BOX14A=0"
66580   pr #hExport: "LAB12A="&box12aCode$
66600   pr #hExport: "BOX12A="&box12aAmt$
66620   pr #hExport: "CNTRYCODE="
66640   pr #hExport: "RCOUNTRY="
66660   pr #hExport: "LAB14B="
66680   pr #hExport: "BOX14B=0"
66700   pr #hExport: "LAB12B="&box12bCode$
66720   pr #hExport: "BOX12B="&box12bAmt$
66740   pr #hExport: "LAB14C="
66760   pr #hExport: "BOX14C=0"
66780   pr #hExport: "LAB12C="&box12cCode$
66800   pr #hExport: "BOX12C="&box12cAmt$
66820   pr #hExport: "EESTAT=0"
66840   pr #hExport: "EERETR="&retirementPlanX$
66860   pr #hExport: "LAB14D="
66880   pr #hExport: "BOX14D=0"
66900   pr #hExport: "LAB12D="&box12dCode$
66920   pr #hExport: "BOX12D="&box12dAmt$
66940   pr #hExport: "EESICK=0"
66960   pr #hExport: "BOX11Q="&str$(nqp)
66980   pr #hExport: "NQPLANS="
67000   pr #hExport: "STATE1="&state$
67020   pr #hExport: "SEIN1="&stcode$
67040   pr #hExport: "SWAGES1="&str$(w(9))
67060   pr #hExport: "SITW1="&str$(w(7))
67080   pr #hExport: "LWAGES1="&str$(w(10))
67100   pr #hExport: "LITW1="&str$(w(8))
67120   pr #hExport: "LOCAL1="&printLocality$
67140   pr #hExport: "STATE2="
67160   pr #hExport: "SEIN2="
67180   pr #hExport: "SWAGES2=0"
67200   pr #hExport: "SITW2=0"
67220   pr #hExport: "LWAGES2=0"
67240   pr #hExport: "LITW2=0"
67260   pr #hExport: "LOCAL2="
67280   pr #hExport: "FName="&nameFirst$(1:24)
67300   pr #hExport: "LName="&nameLast$(1:24)
67320   pr #hExport: "TAG="
67340   pr #hExport: "EBAT="
67360   pr #hExport: "PHONE="
67380   pr #hExport: "*"
67400 return ! /r
75000 IGNORE: continue 
76000 ! <updateable region: ertn>
76040 ERTN: fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
77000 PrintW2: ! r:
77020   w2printCount+=1
77040   if  w2printCount/2=int(w2printCount/2) then ! it's the second one on a page
77060     fnw2_text(bottom,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
77080     fnpa_newpage
77100   else
77120     if enableBackground$='True' then let fnpa_background(W2CopyFile$(w2Copy))
77140     fnw2_text(topmargin,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
77180   end if
77200 return  ! /r
