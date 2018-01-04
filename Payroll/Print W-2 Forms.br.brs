10000 !  (formerly) S:\acsPR\newPRW2A 
10020 ! r: setup
10100   library 'S:\Core\Library': fntop
10120   library 'S:\Core\Library': fnxit
10140   library 'S:\Core\Library': fnchain
10160   library 'S:\Core\Library': fnTos
10180   library 'S:\Core\Library': fnLbl
10200   library 'S:\Core\Library': fnTxt
10220   library 'S:\Core\Library': fnCmdKey
10240   library 'S:\Core\Library': fnAcs
10260   library 'S:\Core\Library': fnmsgbox
10280   library 'S:\Core\Library': fnChk
10300   library 'S:\Core\Library': fnpa_finis
10320   library 'S:\Core\Library': fnerror
10340   library 'S:\Core\Library': fnCmdSet
10360   library 'S:\Core\Library': fnpa_open
10380   library 'S:\Core\Library': fnpa_newpage
10400   library 'S:\Core\Library': fnerror
10420   library 'S:\Core\Library': fnCmdSet
10440   library 'S:\Core\Library': fnpa_open
10460   library 'S:\Core\Library': fnpa_newpage
10480   library 'S:\Core\Library': fncreg_read
10500   library 'S:\Core\Library': fncreg_write
10520   library 'S:\Core\Library': fnpa_background
10540   library 'S:\Core\Library': fngethandle
10560   library 'S:\Core\Library': fnDedNames
10580   library 'S:\Core\Library': fncomboa
10600   library 'S:\Core\Library': fnw3
10620   library 'S:\Core\Library': fnNameParse
10640   library 'S:\Core\Library': fnAddOneC
10660   library 'S:\Core\Library': fnask_w2_info
10680   library 'S:\Core\Library': fnw2_text
10700   library 'S:\Core\Library': fnFree
11000   on error goto ERTN
12000 ! ______________________________________________________________________
12090   dim fw2box16$*255,ss$*11,s(13),t(13)
12100   dim cLocality$*8,desc$(6)*15,amt(6)
12220   dim tcp(32),tdc(10),resp$(128)*256
12240   dim w(13),a$(3)*40,empId$*12,controlNumber$*12,d$(10)*8,e$(10)*12,cap$*128
12260   dim newdedfed(20),newdedcode(20)
12280   dim newcalcode(20),dedfica(20),dedst(20),deduc(20),fullname$(20)*20
12300   dim abrevname$(20)*8
12320   dim dedcode$(20)*2,dedyn$(20)*5
12340   dim miscded(20),totalbox12(20)
14120   dim tmpMsgLine$(0)*256
14140   !
14160   dim in4$(30)
14180   dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
14190   dim k$(3)*30
14200 ! ______________________________________________________________________
14220   fntop(program$,cap$="Print W-2 Forms")
14240   fw2box16$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
14260   !
14280 ! ______________________________________________________________________
14446   !
14460   open #hCompany:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
14480   read #hCompany,using fCompany: mat a$,empId$,mat d$,loccode,mat e$
14500   fCompany: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12 
14520   for j=1 to 3: a$(j)=a$(j)(1:30): next j
14540   close #hCompany: 
14560   !
14580   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
14600   !
14620   dim w2box12Opt$(0)*3
14640   mat w2box12Opt$(0)
14660   box_12a=fnAddOneC(mat w2box12Opt$,'12a')
14680   box_12b=fnAddOneC(mat w2box12Opt$,'12b')
14700   box_12c=fnAddOneC(mat w2box12Opt$,'12c')
14720   box_12d=fnAddOneC(mat w2box12Opt$,'12d')
14740   !
14760   dim w2laser_output_filename$*256
14780   !
14800   dim w2Copy$*68
15100   !
15300   ! /r
16000   ASK_INFO: !
16020    if ~fnask_w2_info(taxYear$,beg_date,end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,unused_state$,loccode,cLocality$) then goto XIT
16040   ! pause  !
16060   dim w2ssnMask(6) ! W2CopyFile2pp$(6)*128,W2CopyFile$(6)*128,
16080   w2ssnMask(1)=0 ! W2CopyFile$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A.pdf' :  W2CopyFile2pp$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A - 2pp.pdf'
16100   w2ssnMask(2)=0 ! W2CopyFile$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1.pdf' :  W2CopyFile2pp$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1 - 2pp.pdf'
16120   w2ssnMask(3)=0 ! W2CopyFile$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B.pdf' :  W2CopyFile2pp$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B - 2pp.pdf'
16140   w2ssnMask(4)=1 ! W2CopyFile$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C.pdf' :  W2CopyFile2pp$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C - 2pp.pdf'
16160   w2ssnMask(5)=1 ! W2CopyFile$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2.pdf' :  W2CopyFile2pp$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2 - 2pp.pdf'
16180   w2ssnMask(6)=1 ! W2CopyFile$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D.pdf' :  W2CopyFile2pp$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D - 2pp.pdf'
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
24000 ! ASK_DEDUCTIONS: ! r: ! ask if any misecllaneous deductions should pr in box 12
24020   fnTos(sn$="Prw2-box12")
24040   rc=cf=0 : mylen=20 : mypos=mylen+3
24060   fnLbl(1,1,"Indicate if any of the miscellaneous deductions",50,1,0,0)
24080   fnLbl(2,1,"should appear in box 12 on the W-2.",44,1,0,0)
24100   fnLbl(4,7,"Deduction Name")
24120   fnLbl(4,26,"Yes" )
24140   fnLbl(4,35,"Box" )
24160   fnLbl(4,45,"Code")
24180   for dedItem=1 to 20
24200     if trim$(fullname$(dedItem))<>'' then
24220       fncreg_read('w2 deduction '&str$(dedItem)&' box 12 enable',dedyn$(dedItem),'False')
24240       fncreg_read('w2 deduction '&str$(dedItem)&' box 12 which',tmpBox12x$) : box12which(dedItem)=val(tmpBox12x$)
24260       fncreg_read('w2 deduction '&str$(dedItem)&' box 12 code',dedcode$(dedItem))
24280     end if
24300   nex dedItem
24320   dedItem=0
24340   dim respc_box12opt(20),box12which(20)
24350   tmpLine=5
24360   for dedItem=1 to 20
24420     if trim$(fullname$(dedItem))<>'' then 
24430       fnLbl(tmpLine+=1,1,fullname$(dedItem),mylen,1,0,0)
24440       fnChk(tmpLine,26,"",0,0,0,0)
24460       resp$(rc+=1)=dedyn$(dedItem)
24480       fncomboa('w2Copy',tmpLine,35,mat w2box12Opt$, '',3)
24500       if box12which(dedItem)=0 then
24520         resp$(respc_box12opt(dedItem)=rc+=1)=''
24540       else
24560         resp$(respc_box12opt(dedItem)=rc+=1)=w2box12Opt$(box12which(dedItem))
24580       end if
24660       fnTxt(tmpLine,45,2,0,1,"",0,"Enter the Code that should appear in the box.")
24680       resp$(rc+=1)=dedcode$(dedItem)
24700     end if
24720   next dedItem
24740   fnCmdSet(2)
24780   fnAcs(sn$,0,mat resp$,ckey)
24800   if ckey=5 then 
24820     if exportFormatID then
24840       close #hExport:
24860     end if
24880     goto ASK_INFO
24900   else
24920     x=0
24940     for dedItem=1 to 20
24960       if trim$(fullname$(dedItem))<>'' then
24980         dedyn$(dedItem)=resp$(x+=1)
25000         box12which(dedItem)=srch(mat w2box12Opt$,resp$(respc_box12opt(dedItem)))
25020         x+=1 ! box12(dedItem)=val(resp$(x+=1))
25040         dedcode$(dedItem)=resp$(x+=1)
25060         fncreg_write('w2 deduction '&str$(dedItem)&' box 12 enable',dedyn$(dedItem))
25100         fncreg_write('w2 deduction '&str$(dedItem)&' box 12 which',str$(box12which(dedItem)))
25120         fncreg_write('w2 deduction '&str$(dedItem)&' box 12 code',dedcode$(dedItem))
25140       end if
25160     nex dedItem
25200   end if ! /r
26000   ! r: open files, initialize output, etc
26020   if exportFormatID=0 then 
26040     fnpa_open('',w2Copy$,'PDF') 
26060   end if
26080 ! lyne=topmargin ! starting of 1st line
26100   goproc=0
26120   open #hEmployee:=1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
26140   open #hDepartment:=2: "Name="&env$('Q')&"\PRmstr\department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\deptidx.h"&env$('cno'),internal,outIn,keyed 
26160   open #hChecks:=fngethandle: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outIn,keyed 
26180   open #hAddr:=fngethandle: "Name="&env$('Temp')&"\Addr."&session$&",Replace,RecL=33,NoShr",internal,output 
26200   write #hAddr,using 'form pos 1,n 10.2,n 1': ssmax,w1
26220   open #hW2Box16:=fngethandle: "Name="&env$('Q')&"\PRmstr\W2Box16.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\W2Index.h"&env$('cno')&",Shr",internal,input,keyed ioerr ignore
26260   w2printCount=0
27000   ! if loccode=0 or cLocality$="YES" or cLocality$="NO" then 
27020   !   goto READ_EMPLOYEE 
27040   ! else 
27060   !   empLocality$=cLocality$
27080   !   gosub ASK_EMP_LOCALITY
27100   ! end if
27120 ! /r
30000 READ_EMPLOYEE: ! r:
30010   do
30020     read #hEmployee,using 'form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3': eno,mat k$,ss$,em6,ta eof EO_EMPLOYEE
30040     if endnum>0 and eno>endnum then goto EO_EMPLOYEE ! ending employee number entered
30060     fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
30070     if numb<>0 and eno<empno then goto READ_EMPLOYEE
30080     kz$=lpad$(rtrm$(str$(eno)),8)
30100     retirementPlanX$=""
30120     box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
30140     mat amt=(0)
30160     mat miscded=(0)
30180 !   tdedret=0 ! REMOVE EXPLANATION  FROM LINE 905 TO LIST RETIREMENT IN BOX 13
30260     first=1
30280     ! Read #hDepartment,Using 1190,Rec=TA: TENO,TCD,MAT TY,TA
32000     checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
32020     restore #hChecks,key>=checkkey$: nokey READ_EMPLOYEE
32040     do
32060       READ_CHECK: !
32080       read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof EO_CHECKS_FOR_EMP
32100       if heno<>eno then goto EO_CHECKS_FOR_EMP
32120       if prd<beg_date or prd>end_date then goto READ_CHECK ! not this year
32140       read #hDepartment,using "form pos 48,n 2", key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zzz)",tdn): tcd nokey ignore ! get state code
32160       if tcd<1 or tcd>10 then tcd=1
32180       if exportFormatID then 
32200         stcode=tcd 
32220       else
32240         if first=1 then stcode=tcd
32260         if first=0 and stcode><tcd then 
32280           goto L1300
32300         end if
32320       end if
32340       state$=d$(tcd)(1:2)
32360       stcode$=e$(tcd)
32380       L1300: ! 
32400       dedfica=0
32420       dedret=0
32440       for dedItem=1 to 20
32460         if newdedfed(dedItem)>=1 and newdedcode(dedItem)=1 then dedret=dedret+tcp(dedItem+4)
32480         if dedfica(dedItem)=1 and newdedcode(dedItem)=1 then dedfica=dedfica+tcp(dedItem+4)
32500         miscded(dedItem)=miscded(dedItem)+tcp(dedItem+4)
32520       next dedItem
32540       ! tDEDRET=TDEDRET+DEDRET ! ACCUMULATE BOX 13 RETIREMENT; THIS LINE WILL ONLY WORK IF NO CAFETERIA; REMOVE ! OFF 861 AND 882 FOR RETIREMENT ONLY ! can change 882 only if know specific ded to show in box 13
32560       w(1)+=tcp(1) ! FED W/H YTD
32580       w(2)+=tcp(31)-dedret ! TOTAL TAXABLE WAGES
32600       ytdFica+=tcp(2) ! FICA W/H YTD
32620       w(4)+=tcp(24) ! EIC TOTAL
32640       if em6<>9 then 
32660         w(5)+=tcp(31)-tcp(30)-dedfica ! TOTAL SS WAGES
32680         w(11)+=tcp(31)-dedfica ! TOTAL MC WAGES & TIPS
32700         ! if env$('client')="Washington Parrish" then w(11)+=tcp(6) ! add deferred comp match into medicare wages
32720         if em6=2 then w(5)=0 ! NO SS
32740         if em6=1 then w(11)=0 ! NO MC
32760       end if
32770       ! if env$('client')<>"Washington Parrish" then 
32780         w(6)=w(6)+tcp(30) ! FICA TIPS YTD
32790       ! end if
32800       w(3)+=tcp(2)  
32820       w(12)+=tcp(3) 
32840       if tcd=stcode then 
32860         w(7)+=tcp(4) ! STATE WH
32880         w(9)+=tcp(31)-dedret ! STATE WAGES
32900         if loccode=0 or tcp(loccode+4)=0 then goto L1560
32920         w(8)+=tcp(loccode+4) ! LOCAL WITHHOLDING
32940         w(10)+=tcp(31)-dedret ! LOCAL WAGES
32960         L1560: ! 
32980         if pn1>0 and tcp(pn1+4)>0 then retirementPlanX$="X"
33000         if dc1>0 and dc1<11 then dcb+=tcp(dc1+4)
33020       else
33040         if loccode=0 then lowh=0 else lowh=tcp(loccode+4)
33060         write #hAddr,using 'form pos 1,n 8,n 2,3*pd 5.2,c 8': eno,tcd,tcp(31)-dedret,tcp(3),lowh,empLocality$
33080         goproc=1
33100       end if
33120       first=0
33140     loop ! read next check record
34000     EO_CHECKS_FOR_EMP: ! 
34020     gosub BOX16_process
34040     for dedItem=1 to 20
34060       if trim$(fullname$(dedItem))<>'' then 
34080         if dedyn$(dedItem)="True" and miscded(dedItem)<>0 then 
34100           if box12which(dedItem)=box_12a then
34120             if box12aCode$='' and box12aAmt$='' then 
34140               box12aCode$=lpad$(dedcode$(dedItem),4) 
34160               box12aAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
34180             end if
34200             ! descWhich=3
34220           else if box12which(dedItem)=box_12b then
34240             if box12bCode$='' and box12bAmt$='' then 
34260               box12bCode$=lpad$(dedcode$(dedItem),4)
34280               box12bAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
34300               totalbox12(dedItem)+=miscded(dedItem)
34320             end if
34340             ! descWhich=4
34360           else if box12which(dedItem)=box_12c then
34380             if box12cCode$='' and box12cAmt$='' then 
34400               box12cCode$=lpad$(dedcode$(dedItem),4)
34420               box12cAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
34440               totalbox12(dedItem)+=miscded(dedItem)
34460             end if
34480             ! descWhich=5
34500           else if box12which(dedItem)=box_12d then
34520             if box12dCode$='' and box12dAmt$='' then 
34540               box12dCode$=lpad$(dedcode$(dedItem),4)
34560               box12dAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
34580               totalbox12(dedItem)+=miscded(dedItem)
34600             end if
34620             ! descWhich=6
34640           end if
34660           ! if trim$(desc$(descWhich))="" then 
34680           !   desc$(descWhich)=lpad$(dedcode$(dedItem)&" "&cnvrt$("Nz 10.2",miscded(dedItem)),15)
34700           !   ! totalbox12(dedItem)+=miscded(dedItem)
34720           ! end if
34740         end if 
34760       end if
34780     next dedItem
34800     w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
34820     w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
35000     if em6=9 then w(3)=w(5)=w(11)=w(12)=0 ! NO SS OR MC
35020     if w(8)=0 then 
35040       printLocality$=""
35060     else
35080       if cLocality$="YES" then gosub ASK_EMP_LOCALITY
35100       printLocality$=empLocality$
35120     end if
35140     controlNumber$=str$(eno)
35160     if w(2)<>0 or w(5)<>0 then ! only pr w2 if wages
35180       if exportFormatID=1 then 
35200         gosub EXPORT_AMS
35220     ! else if exportFormatID=2 then 
35240     !   gosub EXPORT_CPS !  ! removed access 01/03/2017
35260       else 
35840         gosub PrintW2
35850       end if
35860       mat s=s+w
35940       wctr=wctr+1
35950     end if
35960     mat w=(0)
35970     nqp=dcb=ytdFica=0
35980   loop ! /r
36000 EO_EMPLOYEE: ! r:
36020   mat t=t+s
36040   misc=3
36060   for dedItem=1 to 20 ! changed from 10 to 20 on 1/4/17
36080     if totalbox12(dedItem)<>0 then 
36100       desc$(misc)=lpad$("  "&cnvrt$("Nz 10.2", totalbox12(dedItem)),15)
36120       misc+=1
36140       if misc>7 then goto FINIS ! only allow 4 different deductions
36160     end if
36180   next dedItem
36200   goto FINIS: ! /r
37000 FINIS: ! r:
37020   close #hEmployee: 
37040   close #hDepartment: 
37060   close #hAddr: 
37080   close #hW2Box16: 
37100   if ~exportFormatID then 
37135     mat w=t 
37140     controlNumber$="Final Total" 
37160     nameFirst$=nameMiddle$=nameLast$=""
37180     mat k$=("")
37190     ! state$=''
37182     ss$=printLocality$="" 
37200     gosub PrintW2
37220     fnpa_finis
37240   end if
37260   if enableW3$="True" then let fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
37280   if exportFormatID then 
37300       mat tmpMsgLine$(2)
37320       tmpMsgLine$(1)='Export file created:'
37340       tmpMsgLine$(2)=os_filename$(file$(hExport))
37360       close #hExport:
37380       fnmsgbox(mat tmpMsgLine$,resp$,cap$) ! ,16+4)
37400     goto XIT
37420   else
37440     if goproc=1 then 
37460       goto PRW2B
37480     end if
37500   end if
37520 goto XIT ! /r
38000 XIT: fnxit
40000 PRW2B: ! r:
40020   open #1: "Name="&env$('Temp')&"\Control."&session$,internal,output 
40040   restore #1: 
40060   write #1,using 'form pos 1,c 128': "FILE "&env$('Temp')&"\Addr."&session$&",,,PRW2ADDR.H"&env$('cno')&","&env$('Q')&"\PRmstr,,"&env$('Q')&"\PRmstr,,A,N"
40080   write #1,using 'form pos 1,c 128': "MASK 9,2,n,a,1,8,n,a"
40100   close #1: 
40120   fnFree(env$('Q')&"\PRmstr\PRW2ADDR.H"&env$('cno'))
40140   execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
40160 fnchain("S:\acsPR\prw2b") ! /r
52000 ASK_EMP_LOCALITY: ! r:
52020   fnTos(sn$="Prw2-5")
52040   rc=cf=0
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
52280   controlNumber$=rtrm$(controlNumber$)
52300   if controlNumber$="1" then goto L2770
52320   empLocality$=controlNumber$
52340 L2770: ! 
52360 return  ! /r
64000 BOX16_process: ! r: Box 16
64020   ! passed hW2Box16,kz$, mat w, etc
64040   read #hW2Box16,using fw2box16$,key=kz$: kz$,mat in4$ nokey B16Finis
64060   for j=1 to 6
64080     amt(j)=val(in4$(j*5-3))
64100     if in4$(j*5-2)="1" then w(2)+=amt(j)
64120     if in4$(j*5-1)="1" then w(5)+=amt(j)
64140     !   if env$('client')="Washington Parrish" then goto L3760
64160     if in4$(j*5-1)="1" then w(11)+=amt(j)
64180     ! L3760: !
64200     if in4$(j*5-0)="1" then w(9)+=amt(j)
64220     if in4$(j*5-2)="2" then w(2)=w(2)-amt(j)
64240     if in4$(j*5-1)="2" then w(5)=w(5)-amt(j)
64260     !   if env$('client')="Washington Parrish" then goto L3810
64280     if in4$(j*5-1)="2" then w(11)=w(11)-amt(j)
64300     ! L3810: ! 
64320     if in4$(j*5-0)="2" then w(9)=w(9)-amt(j)
64340     if j=1 then 
64360       desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&ltrm$(cnvrt$("Nz 10.2",amt(j))),15)
64380     else if j=2 then
64400       desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&cnvrt$("Nz 10.2",amt(j)),15)
64420     else if j=3 then
64440       box12aCode$=in4$(j*5-4)(1:2)
64460       box12aAmt$=cnvrt$("Nz 10.2",amt(j))
64480     else if j=4 then
64500       box12bCode$=in4$(j*5-4)(1:2)
64520       box12bAmt$=cnvrt$("Nz 10.2",amt(j))
64540     else if j=5 then
64560       box12cCode$=in4$(j*5-4)(1:2)
64580       box12cAmt$=cnvrt$("Nz 10.2",amt(j))
64600     else if j=6 then
64620       box12dCode$=in4$(j*5-4)(1:2)
64640       box12dAmt$=cnvrt$("Nz 10.2",amt(j))
64660     end if
64680     ! if (j=3 or j=4) and (in4$(j*5-4)(1:1)="D" or in4$(j*5-4)(1:1)="E" or in4$(j*5-4)(1:1)="F" or in4$(j*5-4)(1:1)="H") then w(13)=w(13)+amt(j) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
64700   next j
64720   B16Finis: ! 
64740 return  ! /r
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
66820   pr #hExport: "EESTAT=" ! 0"
66840   pr #hExport: "EERETR="&retirementPlanX$
66940   pr #hExport: "EESICK=" ! 0"
66660   pr #hExport: "LAB14B="
66680   pr #hExport: "BOX14B=0"
66700   pr #hExport: "LAB12B="&box12bCode$
66720   pr #hExport: "BOX12B="&box12bAmt$
66740   pr #hExport: "LAB14C="
66760   pr #hExport: "BOX14C=0"
66780   pr #hExport: "LAB12C="&box12cCode$
66800   pr #hExport: "BOX12C="&box12cAmt$
66860   pr #hExport: "LAB14D="
66880   pr #hExport: "BOX14D=0"
66900   pr #hExport: "LAB12D="&box12dCode$
66920   pr #hExport: "BOX12D="&box12dAmt$
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
77040   if w2printCount/2=int(w2printCount/2) then ! it's the second one on a page 
77060 !   if enableBackground$='True' then let fnpa_pic('S:\Core\pdf\W-2 Copy 1.png',1,bottom,200,200)
77080 !   if enableBackground$='True' then let fnpa_pic(W2CopyFile$(w2Copy),1,bottom,200,200)
77100     fnw2_text(bottom,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
77120     fnpa_newpage
77140   else
77160     if enableBackground$='True' then 
77240       fnpa_background('S:\Core\pdf\'&taxYear$&'\W-2\Copy '&w2Copy$(1:1)&'.pdf')
77280     end if
77300     fnw2_text(topmargin,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
77340   end if
77360 return  ! /r
84080 def fnQAC(mat qac$,qacText$*256)
84100   qacCount+=1
84120   mat qac$(qacCount)
84140   qac$(qacCount)=qacText$
84160   fnQAC=qacCount
84180 fnend

