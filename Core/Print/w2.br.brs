08000 ! r: testing zone
08020 let fn_setup
08040 library program$: fnNameParse
08050 nameFormat$=optNameFormat$(2)
08060 fncreg_write('Employee Name Format',nameFormat$)
08080 dim n1$*64,n2$*64,n3$*64
08100 pr 'name format: "'&nameFormat$&'"'
08120 dim nFull$(4)*256
08140 nFull$(1)='JOINER,JR JOHNNY C.'
08160 nFull$(2)='CRUZ,  BRYAN EDUARDO DEL'
08180 nFull$(3)='SAN NICOLAS, CHRISTOPHER'
08200 nFull$(4)='GARCIA,JR JESUS'
08220 for nFullItem=1 to udim(mat nFull$)
08240   fnNameParse(nFull$(nFullItem),n1$,n2$,n3$,n4$)
08260   pr 'source name: "'&nFull$(nFullItem)&'"'
08280   pr '     first: "'&n1$&'"'
08300   pr '    middle: "'&n2$&'"'
08320   pr '      last: "'&n3$&'"'
08340   pr '    suffix: "'&n4$&'"'
08360 nex nFullItem
08370 end
08380 ! /r
14000 def library fnask_w2_info(&taxYear$,&beg_date,&end_date,&empStart$,&empEnd$,&ssrate,&ssmax,&mcrate,&mcmax,mat w2destinationOpt$,&enableW3$,&enableBackground$,&w2Copy,&w2Copy$,&exportFormatID,&w2laser_output_filename$,&pn1,&dc1,&topmargin,&bottom,&state$,enableAskCLocality,&cLocality$)
14020   if ~awi_setup then ! r:
14040     awi_setup=1
14050     if ~setup then let fn_setup
14060     ! r: constants
14070     dim resp$(128)*256
14080     dim optW2Copy$(6)*68
14100     optW2Copy$(1)='A - For Social Security Administration' ! Send with W-3
14120     optW2Copy$(2)='1 - For State, City or Local Tax Department'
14140     optW2Copy$(3)='B - To Be Filed with Employee''s Federal Tax Return'
14160     optW2Copy$(4)='C - For Employee''s Records'
14180     optW2Copy$(5)='2 - To Be Filed with Employee''s State, City or Local Tax Department'
14200     optW2Copy$(6)='D - For Employer'
14220     dim cLlocalityToolTip$*256
14300     ! /r
14320     ! r: read or set values for ASK_INFO screen
14340     taxYear$=date$(days(date$)-120,'CCYY')
14360     empStart$='[All]'
14380     empEnd$='[All]'
14400     ssrate=.062
14420     if taxYear$='2016' then
14440       ssmax=118500 ! 2016
14460     else ! if taxYear$='2017' then
14480         ssmax=127200 ! 2017
14500     end if
14520     let mcrate=.0145
14540     let mcmax=999999
14550     fncreg_read('W-2 - cLocality',cLocality$,'NO')
14560     fnreg_read('Print W-2'                          ,w2destinationOpt$(1),'True' )
14580     fnreg_read('Export for Advanced Micro Solutions',w2destinationOpt$(2),'False')
14600     ! fnreg_read('Export for Center Piece Software'   ,w2destinationOpt$(3),'False')  ! removed access 01/03/2017
14620     fnreg_read('Print W-3 also'                     ,enableW3$           ,'True' )
14640     fnreg_read('W-2 - Enable Background'            ,enableBackground$   ,'True' )
14680     fncreg_read('W-2 - Copy Current',w2Copy$,optW2Copy$(1)) : w2Copy=srch(mat optW2Copy$,w2Copy$) : if w2Copy<=0 then let w2Copy=1
14700     fncreg_read('Employee Name Format',nameFormat$,optNameFormat$(1))
14720     w2Copy$=optW2Copy$(w2Copy)
14740     fnureg_read('W-2 - Export Filename',w2laser_output_filename$,os_filename$(env$('userprofile')&'\Desktop\ACS [TaxYear] W-2 Export (Company [CompanyNumber]).txt'))
14760     fncreg_read('Qualified Pension Plan' ,tmp$) : pn1=val(tmp$)
14770     fncreg_read('Dependent Care Benefits',tmp$) : dc1=val(tmp$)
14780     fncreg_read('W-2 - State',state$)
14800     !
14820     fnreg_read('W-2 - Form 1 Y'              ,tmp$   ,'10' ) : topmargin=val(tmp$)
14840     fnreg_read('W-2 - Form 2 Y'              ,tmp$   ,'151') : bottom=val(tmp$)
15000     enableDateRange=0
15020     disableSSMCedit=0
15040     enableEmpRange=0
15060     ! /r
15080   end if ! /r
15100   awiReturn=0 
15500   if env$('cursys')='PR' then
15520     enablePayrollDeductions=1
15540     enableAskState=0
15550     cLlocalityToolTip$="If you have answered that you have local withholdings in the company information file, you must enter the locality name"
15560   else if env$('cursys')='GL' then
15580     enablePayrollDeductions=0
15600     enableAskState=1
15610     cLlocalityToolTip$=""
15620   else
15640     pr 'not configured for anything but GL and PR yet.'
15660     pause
15680   end if
17000   ASK_INFO: !
17020   ! r: build and display the ASK_INFO screen
17040   fntos(sn$="Prw2-2")
17060   let rc=cf=0: let mylen=21: let mypos=mylen+2 
17080    if enableAskCLocality then 
17100      fraWidth=94
17120    else
17140      fraWidth=82 ! 68
17160    end if
17180   !
17200   fraGeneralHeight=2
17220   if enableDateRange then fraGeneralHeight+=2
17240   if enableEmpRange then fraGeneralHeight+=3
17260   if enableAskCLocality then fraGeneralHeight+=5
17280   fnfra(1,1,fraGeneralHeight,fraWidth,"General","Normally this would the first and last day of the calendar year",0)
17300   cf+=1 : let franum=cf : lc=0
17320   fnlbl(lc+=1,1,"Tax Year:",mylen,1,0,franum)
17340   fntxt(lc,mypos,4,0,1,"",1,"Year to pr W-2s for",franum)
17360   resp$(resc_taxYear:=rc+=1)=taxYear$
17380   if enableDateRange then
17400     fnlbl(lc+=1,1,"Starting Date:",mylen,1,0,franum)
17420     fntxt(lc,mypos,10,0,1,"3",0,"First day of calendar year",franum)
17440     resp$(respc_startdate:=rc+=1)='0101'&date$(days(date$)-180,'YY')
17460     fnlbl(lc+=1,1,"Ending Date:",mylen,1,0,franum)
17480     fntxt(lc,mypos,10,0,1,"3",0,"Last day of calendar year",franum)
17500     resp$(respc_enddate:=rc+=1)='1231'&date$(days(date$)-180,'YY')
17520     lc+=1
17540   end if
17560   if enableEmpRange then
17580     fnlbl(lc+=1,1,"Starting Employee:",mylen,1,0,franum)
17600     fncmbemp(lc,mypos,1,franum)
17620     resp$(respc_empStart:=rc+=1)=""
17640     fnlbl(lc+=1,1,"Ending Employee:",mylen,1,0,franum)
17660     fncmbemp(lc,mypos,1,franum)
17680     resp$(respc_empEnd:=rc+=1)=""
17700     lc+=1
17720   end if
17740   if enableAskCLocality then
17760     fnlbl(lc+=1,1,"Locality Name:",mylen,1,0,franum)
17780     fntxt(lc,mypos,12,0,1,"",0,cLocalityToolTip$,franum)
17800     resp$(resp_cLocality:=rc+=1)=cLocality$
17820     fnlbl(lc   ,mypos+12+2,"Enter the locality name if the same on all employees.",57,0,0,franum)
17840     fnlbl(lc+=1,mypos+12+2,"Enter NO (or blank) if it is not applicable.",57,0,0,franum)
17860     fnlbl(lc+=1,mypos+12+2,"Enter YES if applicable, but not he same on all employees",57,0,0,franum)
17880     lc+=1
17900   end if
17920   fnlbl(lc+=1,1,"Employee Name Format:",mylen,1,0,franum)
17940   fncomboa('nameFormat',lc,mypos,mat optNameFormat$, '',20,franum)
17960   let resp$(resp_namcde:=rc+=1)=nameFormat$
17980   !
18000   fra2Height=5 : fra2Y=fraGeneralHeight+3
18020   fnfra(fra2Y,1,fra2Height,fraWidth,"Print W-2s","",0)
18040   cf+=1 : let franum=cf : lc=0
18060   mylen=46: let mypos=mylen+2
18080   fnlbl(1,1,"Social Security Withholding Rate:",mylen,1,0,franum)
18100   fntxt(1,mypos,10,0,1,"34",disableSSMCedit,"Use format such as .062.",franum)
18120   let resp$(respc_ssrate:=rc+=1)=str$(ssrate)
18140   fnlbl(2,1,"Maximum Wage Subject to SS Withholdings:",mylen,1,0,franum)
18160   fntxt(2,mypos,10,0,1,"10",disableSSMCedit,"Enter the maximum wage subject to social security withholdings for the current year just ended.",franum)
18180   let resp$(respc_ssmax:=rc+=1)=str$(ssmax)
18200   fnlbl(4,1,"Medicare Withholding Rate:",mylen,1,0,franum)
18220   fntxt(4,mypos,10,0,1,"34",disableSSMCedit,"Use format such as .0145 .",franum)
18240   let resp$(respc_mcrate:=rc+=1)=str$(mcrate)
18260   fnlbl(5,1,"Maximum Wage Subject to Medicare Withholdings:",mylen,1,0,franum)
18280   fntxt(5,mypos,10,0,1,"10",disableSSMCedit,"At the present time there is no maximum.  Enter a number larger than any one's wages can be. For example, 999999.00",franum)
18300   let resp$(respc_mcmax:=rc+=1)=str$(mcmax)
18320   !
18340   fra3Y=fra2Y+fra2Height+2 : fra3Height=6
18360   fnfra(fra3Y,1,fra3Height,fraWidth,"Printing or Exporting","You have the option to either pr the W-2s or export them to another system for printing.")
18380   cf+=1 : let franum=cf : let mylen=26 : let mypos=mylen+2
18400   fnopt(1,3,"Print W-2",0,franum)
18420   let resp$(respc_PrintW2:=rc+=1)=w2destinationOpt$(1)
18440   fnlbl(1,fraWidth-50,"(2 per page is not yet available with Backgrounds)",50,1,0,franum)
18460   fnlbl(2,5,"Copy:",12,1,0,franum)
18480   fncomboa('w2Copy',2,19,mat optW2Copy$, '',20,franum)
18500   let resp$(respc_w2copy:=rc+=1)=w2Copy$
18520   fnchk(2,68,'W-2 - Enable Background',1,franum)
18540   let resp$(respc_enableBackground:=rc+=1)=enableBackground$
18560   ! fnchk(3,68,'2 Per Page',1,franum)
18600   fnchk(4,68,'Print W-3 also',1,franum)
18620   resp$(respc_w3:=rc+=1)=enableW3$
18640   fnopt(4,3,"Export for Advanced Micro Solutions",0,franum)
18660   resp$(respc_export_ams:=rc+=1)=w2destinationOpt$(2)
18680 ! fnopt(5,3,"Export for Center Piece Software",0,franum)  ! removed access 01/03/2017
18700 ! resp$(respc_export_cps:=rc+=1)=w2destinationOpt$(3)  ! removed access 01/03/2017
18720   fnlbl(5,5,"Export File:",12,1,0,franum)
18740   fntxt(5,19,20,80,0,'72',0,'Choose a destination location for the ACS export.',franum)
18760   resp$(resp_w2_export_file:=rc+=1)=w2laser_output_filename$
18780   fnbutton(5,5+12+20+5,'Default',14,'Choose to set the default for the selected destination software.',0,0,franum)
18800   fnlbl(6,19,"([CompanyNumber] and [TaxYear] will be substituted in filename)",0,0,0,franum)
18820   !
18840   if enablePayrollDeductions then
18860     fra4Y=fra3y+fra3Height+2 ! 25
18880     fnfra(fra4Y,1,2,fraWidth,"Identify the Following Deductions","You have twenty miscellaneous deductions available to you. If you have Qualified Pension or Dependent Care, start with the first deduction and count down to identify the number of the deduction.")
18900     cf+=1 : let franum=cf
18920     fnlbl(1,1,"Qualified Pension Plan:",mylen,1,0,franum)
18940     fntxt(1,mypos,2,0,1,"30",0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
18960     let resp$(respc_qpenplan:=rc+=1)=str$(pn1)
18980     fnlbl(2,1,"Dependent Care Benefits:",mylen,1,0,franum)
19000     fntxt(2,mypos,2,0,1,"30",0,"If you have dependent care benefits that should be identifies on the W-2, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
19020     let resp$(respc_depCareBen:=rc+=1)=str$(dc1)
19040   else if enableAskState then
19060     fra4Y=fra3y+fra3Height+2 ! 25
19080     fnfra(fra4Y,1,2,fraWidth,"State","")
19100     cf+=1 : let franum=cf
19120     fnlbl(1,1,"State Name:",mylen,1,0,franum)
19140     fntxt(1,mypos,2,0,1,"",0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
19160     let resp$(respc_state:=rc+=1)=state$
19180   end if
19200   fncmdkey("&Margins",ckey_margins:=1021,0,0,"Manually adjust margins for hitting forms")
19220   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
19240   fncmdkey("&Cancel",5,0,1,"Returns to menu")
19260   fnacs(sn$,0,mat resp$,ckey)
19280   ! /r
20000   ! r: ASK_INFO screen - respond to FKeys, and get local values from mat resp$
20020   if ckey=5 then 
20040     awiReturn=0 
20060   else 
20080     awiReturn=1
20100     taxYear$=resp$(resc_taxYear)
20120     nameFormat$=resp$(resp_namcde)
20140     if enableDateRange then
20160       beg_date=val(resp$(respc_startdate))
20180       end_date=val(resp$(respc_enddate))
20200     else
20220       beg_date=val(taxYear$&'0101')
20240       end_date=val(taxYear$&'1231')
20260     end if
20280 !   pr beg_date,end_date : pause ! 1154
20300     numb=empno=endnum=0
20320     if enableEmpRange then
20340       empStart$=resp$(respc_empStart)(1:8)
20360       empEnd$  =resp$(respc_empEnd)(1:8)
20380       if empStart$<>'[All]' then 
20400         numb=val(empStart$(1:8))
20420         empno=numb
20440       end if
20460       if enableAskCLocality then
20480         cLocality$=uprc$(rtrm$(resp$(resp_cLocality)))
20490         if cLocality$='' then cLocality$='NO'
20500       end if
20520       if empEnd$<>'[All]' then 
20540          endnum=val(empEnd$(1:8))
20560       end if
20580     end if
20600     ssrate=val(resp$(respc_ssrate))
20620     ssmax=val(resp$(respc_ssmax))
20640     mcrate=val(resp$(respc_mcrate))
20660     mcmax=val(resp$(respc_mcmax))
20680     w2Copy$=resp$(respc_w2copy) : w2Copy=srch(mat optW2Copy$,resp$(respc_w2copy)) 
20700     enableBackground$=resp$(respc_enableBackground)
20740     enableW3$=resp$(respc_w3)
20760     if resp$(respc_PrintW2)="True" then 
20800       exportFormatID=0
20820     else if resp$(respc_export_ams)="True" then 
20860       exportFormatID=1
20880     ! else if resp$(respc_export_cps)="True" then 
20920     !   exportFormatID=2
20940     end if
20960     w2destinationOpt$(1)=resp$(respc_PrintW2)
20980     w2destinationOpt$(2)=resp$(respc_export_ams)
21000 !   w2destinationOpt$(3)=resp$(respc_export_cps)  ! removed access 01/03/2017
21020     w2laser_output_filename$=resp$(resp_w2_export_file)
21040     if enablePayrollDeductions then
21060       pn1=val(resp$(respc_qpenplan))
21080       dc1=val(resp$(respc_depCareBen))
21100     else if enableAskState then
21120       state$=resp$(respc_state)
21140     end if
21160     if ckey=14 then 
21180 !     if exportFormatID=1 then
21200 !       let w2laser_output_filename$=os_filename$("\1099ETC.W"&date$(days(date$)-180,'YY')&"\W2DATA\W2DAT.PRN")
21220 !     else if exportFormatID=2 then  ! removed access 01/03/2017
21240 !       let w2laser_output_filename$=os_filename$("\CPS04\ASCIIW2.TXT")  ! removed access 01/03/2017
21260 !     else 
21280         let w2laser_output_filename$=os_filename$(env$('userprofile')&'\Desktop\ACS [TaxYear] W-2 Export (Company [CompanyNumber]).txt')
21300 !     end if 
21320       goto ASK_INFO
21340     else if ckey=ckey_margins then
21360       fn_ask_margins
21380       goto ASK_INFO
21400     end if
21420     if beg_date=0 or end_date=0 then goto ASK_INFO
21430     fncreg_write('W-2 - cLocality',cLocality$)
21440     fncreg_write('W-2 - Copy Current',w2Copy$)
21460     fnreg_write('Print W-2',w2destinationOpt$(1))
21480     fnreg_write('Print W-3 also',enableW3$)
21500     fnreg_write('W-2 - Enable Background',enableBackground$)
21540     fnreg_write('Export for Advanced Micro Solutions',w2destinationOpt$(2))
21560 !   fnreg_write('Export for Center Piece Software'   ,w2destinationOpt$(3))  ! removed access 01/03/2017
21580     fnureg_write('W-2 - Export Filename',w2laser_output_filename$)
21600     fncreg_write('Qualified Pension Plan',str$(pn1))
21620     fncreg_write('Dependent Care Benefits',str$(dc1))
21640     fncreg_write('Employee Name Format',nameFormat$)
21660     fncreg_write('W-2 - State',state$)
21680     !
21700     ! /r
21710     if w2Copy$=optW2Copy$(1) and enableBackground$='True' then let fn_FormCopyAwithBackgroundWarn
21720   end if 
21730   XIT: !
21740   fnask_w2_info=awiReturn
21760 fnend
33000 def fn_setup
33020   if ~setup then
33040     setup=1
33060     dim w2laser_output_filename$*256
33070     library 'S:\Core\Library': fntos,fnfra,fnlbl,fntxt,fncmdkey,fnacs,fnopt,fnmsgbox,fnchk,fncmbemp,fnpa_finis,fnerror,fnureg_read,fnureg_write,fnbutton,fncmdset,fnpa_open,fnpa_newpage,fnpa_fontsize,fnpa_txt,fncreg_read,fncreg_write,fnpa_background,fngethandle,fnreg_read,fnreg_write,fncomboa,fnw3,fnpa_pic,fnAddOneC
33160     on error goto ERTN
33162     dim optNameFormat$(2)*20,nameFormat$*20
33164     optNameFormat$(1)='First Name First'
33166     optNameFormat$(2)='Last Name First'
33180   end if
33200 fnend
36000 def fn_ask_margins
36020 ! if env$('acsdeveloper')='' then pr bell; : goto am_xit
36040   fnreg_read('W-2 - Form 1 Y',amResp$(1),'10' )
36060   fnreg_read('W-2 - Form 2 Y',amResp$(2),'151')
36080   fnreg_read('W-2 - X'       ,amResp$(3),'12' )
36100   fntos(sn$='w2_ask_margins')
36120   mylen=30 : mypos=mylen+2
36140   fnlbl(lc+=1,1,"Form 1 Distance from Top (mm):",mylen,1)
36160   fntxt(lc,mypos,3,0,1,'30')
36180   fnlbl(lc+=1,1,"Form 2 Distance from Top (mm):",mylen,1)
36200   fntxt(lc,mypos,3,0,1,'30')
36220   fnlbl(lc+=1,1,"Left Margin Size (mm):",mylen,1)
36240   fntxt(lc,mypos,3,0,1,'30')
36260   fncmdset(4)
36280   fnacs(sn$,0,mat amResp$,ckey)
36300   if ckey<>5 then
36320     fnreg_write('W-2 - Form 1 Y' ,amResp$(1))
36340     fnreg_write('W-2 - Form 2 Y' ,amResp$(2))
36360     fnreg_write('W-2 - X'        ,amResp$(3))
36380     topmargin= val(amResp$(1))
36400     bottom=    val(amResp$(2))
36420     left=      val(amResp$(3))
36440   end if
36900 fnend
38000 def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
38020   if ~setup then let fn_setup
38040   ! passed in: fullname$
38060   ! gathered: nameFormat$, mat optNameFormat$
38080   ! returns: nameFirst$,nameMiddle$,nameLast$
38100   ! calling program should include:  dim nameFirst$*15,nameMiddle$*15,nameLast$*20
38120   fncreg_read('Employee Name Format',nameFormat$,optNameFormat$(1))
38140   let fullname$=uprc$(rtrm$(fullname$)): ! Let nameFormat$="s"
38160   let npPosSpace1=pos(fullname$," ",1)
38180   let npPosSpace2=pos(fullname$," ",npPosSpace1+1)
38200   if nameFormat$=optNameFormat$(1) then 
38220     ! r: first name first
38240     let nameFirst$=fullname$(1:max(min(15,npPosSpace1-1),1))
38260     if npPosSpace2>0 then 
38280       let nameMiddle$=fullname$(npPosSpace1+1:npPosSpace2-1)
38300       let nameLast$=fullname$(npPosSpace2+1:len(fullname$))
38320     end if
38340     if npPosSpace2=0 then
38360       let nameLast$=fullname$(npPosSpace1+1:len(fullname$))
38380       let nameMiddle$=""
38400     end if
38420     ! /r
38440   else  ! last name first
38460     ! r: last name first
38480     ! npPosComma=pos(fullname$,',')
38500     ! if npPosComma then
38520     !   ! r: last name, first name
38540     !   dim fullNameCopy$*256
38560     !   fullNameCopy$=fullname$
38580     !   nameLast$=fullNameCopy$(1:npPosComma-1)
38600     !   fullNameCopy$(1:npPosComma)=''
38620     !   fullNameCopy$=trim$(fullNameCopy$)
38640     !   npFncPosSpace1=pos(fullNameCopy$,' ')
38660     !   if npFncPosSpace1<=0 then let npFncPosSpace1=len(fullNameCopy$)
38680     !   nameFirst$=trim$(fullNameCopy$(1:npFncPosSpace1))
38700     !   fullNameCopy$(1:npFncPosSpace1)=''
38720     !   nameMiddle$=trim$(fullNameCopy$)
38740     !   fullNameCopy$=''
38760     !   ! /r
38780     ! else
38800     !   ! r: last name [space] first name
38820     !   ! /r
38840     ! ! end if
38860     if npPosSpace1>0 and fullname$(npPosSpace1-1:npPosSpace1-1)="," then 
38880       let nameLast$=fullname$(1:npPosSpace1-2) 
38900     else 
38920       let nameLast$=fullname$(1:max(npPosSpace1-1,1))
38930     end if
38940     if npPosSpace2>0 then 
38960       let nameFirst$=fullname$(npPosSpace1+1:npPosSpace2-1): let nameMiddle$=fullname$(npPosSpace2+1:len(fullname$))
38980     else ! if npPosSpace2=0 then 
39000       let nameFirst$=fullname$(npPosSpace1+1:len(fullname$)): let nameMiddle$=""
39020     end if
39040     ! /r
39060   end if
39080   nameFirst$=rtrm$(nameFirst$,',')
39100   ! r: nameSuffix$ process
39120   nameSuffix$='' err npNsFinis
39140   if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',JR*','')) then
39160     nameSuffix$='JR'
39180     nameFirst$=rtrm$(nameFirst$) 
39200     nameFirst$=nameFirst$(1:len(nameFirst$)-3)
39220   end if
39240   if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR*','')) then
39260     nameSuffix$='JR'
39280     nameLast$=rtrm$(nameLast$) 
39300     nameLast$=nameLast$(1:len(nameLast$)-3)
39320   end if
39340   if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR.*','')) then
39360     nameSuffix$='JR'
39380     nameLast$=rtrm$(nameLast$) 
39400     nameLast$=nameLast$(1:len(nameLast$)-4)
39420   end if
39440   npNsFinis: !
39460   ! /r
39480   ! pr nameFirst$,nameMiddle$,nameLast$
39500 fnend
76000 ! <updateable region: ertn>
76040 ERTN: let fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
78000 def library fnw2_text(w2Yoffset,maskSsn,mat a$,empId$*12,ss$,controlNumber$,mat w,dcb$,nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$*6)
78010   if ~setup then let fn_setup
78020   ! r: variable definations
78040   ! topmargin       how far down the page (mm) is the top of W-2
78060   ! maskSsn         if 1 than turn all but the last 4 of the SSN into *s
78080   ! ss$             social security number (with dashes)
78100   ! mat a$          company name and address
78120   ! controlNumber$  control number
78140   ! 
78160   ! box12aCode$     box 12a code
78180   ! box12aAmt$      box 12a amount
78200   !               
78220   ! box12bCode$     box 12b code
78240   ! box12bAmt$      box 12b amount
78260   !               
78280   ! box12cCode$     box 12c code
78300   ! box12cAmt$      box 12c amount
78320   !               
78340   ! box12dCode$     box 12d code
78360   ! box12dAmt$      box 12d amount
78380   ! 
78400   ! w(4)            EIC
78420   ! /r
78440   if ~w2setup then
78460     w2setup=1
78470     fnreg_read('W-2 - X',tmp$   ,'12' ) : left=val(tmp$)
78480     fncreg_read('W-2 - cLocality',cLocality$,'NO')
78490     w2Col1=left     
78500     w2Col2=left+117 
78520     w2Col3=left+160 
78540     w2Box12CodePos=w2Col3-14
78560     w2Box12AmtPos=w2Box12CodePos+18
78580   end if
78600   fnpa_fontsize
78640   if maskSsn then
78660     fnpa_txt('***-**-'&ss$(8:11),left+44,fn_line(1))
78680   else
78700     fnpa_txt(ss$,left+44,fn_line(1))
78720   end if
78740   fnpa_txt(empId$,w2Col1,fn_line(2))
78760   fnpa_txt(cnvrt$("pic(--,---,---.##",w(2)),w2Col2,fn_line(2))
78780   fnpa_txt(cnvrt$("pic(--,---,---.##",w(1)),w2Col3,fn_line(2))
78800   fnpa_txt(a$(1),w2Col1,fn_line(3))
78820   fnpa_txt(cnvrt$("pic(--,---,---.##",w(5)),w2Col2,fn_line(3))
78840   fnpa_txt(cnvrt$("pic(--,---,---.##",w(3)),w2Col3,fn_line(3))
78860   fnpa_txt(a$(2),w2Col1,fn_line(4))
78880   fnpa_txt(cnvrt$("pic(--,---,---.##",w(11)),w2Col2,fn_line(4))
78900   fnpa_txt(cnvrt$("pic(--,---,---.##",w(12)),w2Col3,fn_line(4))
78920   fnpa_txt(a$(3),w2Col1,fn_line(5))
78940   fnpa_txt(cnvrt$("pic(--,---,---.##",w(6)),w2Col2,fn_line(5))
78980   fnpa_txt(controlNumber$,w2Col1,fn_line(6))
79000 ! fnpa_txt(cnvrt$("pic(--,---,---.##",w(4)),w2Col2,fn_line(6)) ! EIC - no longer reported - just greyed out
79020   fnpa_txt(cnvrt$("pic(--,---,---.##",dcb),w2Col3,fn_line(6))
79040   fnpa_txt((rtrm$(nameFirst$)&" "&rtrm$(nameMiddle$))(1:17),w2Col1,fn_line(7))
79060   fnpa_txt(rtrm$(nameLast$),left+48,fn_line(7))
79070   fnpa_txt(rtrm$(nameSuffix$),left+92,fn_line(7))
79080   fnpa_txt(box12aCode$,w2Box12CodePos,fn_line(7))
79100   fnpa_txt(box12aAmt$,w2Box12AmtPos,fn_line(7))
79120   fnpa_txt(k$(2),w2Col1,fn_line(8))
79140   fnpa_txt(retirementPlanX$,left+118,fn_line(8))
79160   fnpa_txt(box12bCode$,w2Box12CodePos,fn_line(8))
79180   fnpa_txt(box12bAmt$,w2Box12AmtPos,fn_line(8))
79200   fnpa_txt(k$(3),w2Col1,fn_line(9))
79220   fnpa_txt(box12cCode$,w2Box12CodePos,fn_line(9))
79240   fnpa_txt(box12cAmt$,w2Box12AmtPos,fn_line(9))
79260   fnpa_txt(box12dCode$,w2Box12CodePos,fn_line(10))
79280   fnpa_txt(box12dAmt$,w2Box12AmtPos,fn_line(10))
79290   if env$('client')<>'Zaleski' then ! cLocality$<>'NO' then
79300     fnpa_txt(state$,left,fn_line(11))
79320     fnpa_txt(stcode$,left+10,fn_line(11))
79340     fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(9)),left+51,fn_line(11))
79360     fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(7)),left+79,fn_line(11))
79380     fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(10)),left+109,fn_line(11))
79400     fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(8)),left+137,fn_line(11))
79420     fnpa_txt(printLocality$(1:6),left+164,fn_line(11))
79430   end if
79440 fnend
86000 def fn_line(lineNumber)
86020   lReturn=0
86040   if lineNumber=1 then 
86060     lReturn=w2Yoffset+1
86080   else  ! if lineNumber>=1 and lineNumber<=14 then
86100     lReturn=w2Yoffset+10+(8.5*(lineNumber-2))
86120     if lineNumber>=11 then lReturn+=3.5
86140   end if 
86160   fn_line=lReturn
86900 fnend
88000 def library fnFormCopyAwithBackgroundWarn
88020   if ~setup then let fn_setup
88040   fnFormCopyAwithBackgroundWarn=fn_FormCopyAwithBackgroundWarn
88060 fnend
88080 def fn_FormCopyAwithBackgroundWarn
88100   if ~fcawbwSetup then
88120     fcawbwSetup=1
88140     dim fcawbwText$(0)*128
88160     fnAddOneC(mat fcawbwText$,'The IRS Warns you should not pr Copy A with the background.')
88180     fnAddOneC(mat fcawbwText$,'This form is provided for informational purposes only. Copy A appears')
88200     fnAddOneC(mat fcawbwText$,'in red, similar to the official IRS form. The official printed version')
88220     fnAddOneC(mat fcawbwText$,'of this IRS form is scannable, but the online version of it, printed')
88240     fnAddOneC(mat fcawbwText$,'from this website, is not. Do not pr and file this Copy A.')
88260     fnAddOneC(mat fcawbwText$,'A penalty may be imposed for filing forms that can’t be scanned.')
88280     fnAddOneC(mat fcawbwText$,'')
88300     fnAddOneC(mat fcawbwText$,'To order official IRS information returns such as Forms W-2 and W-3,')
88320     fnAddOneC(mat fcawbwText$,'which include a scannable Copy A for filing, visit www.irs.gov/orderforms')
88340     fnAddOneC(mat fcawbwText$,'and click on Employer and Information returns.')
88360     fnAddOneC(mat fcawbwText$,'The IRS will mail you the scannable forms and any other products you order.')
88380   end if
88400   if ~fcawbwToldAlready then
88420     fcawbwToldAlready=1
88440     fnmsgbox(mat fcawbwText$,response$,'Notification')
88460   end if
88480 fnend
