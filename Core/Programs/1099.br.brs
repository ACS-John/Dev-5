12000 ! Replace S:\Core\programs\1099.br
12020 ! library for all 1099 forms
12040   dim ad$(3)*30,ss$*11,a$(3)*40,box(11),ph$*12
12060 def fn_setup
12080   if ~setup then
12100     setup=1
12120     library 'S:\Core\Library': fnerror,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt,fnpa_FontSize
12122     library 'S:\Core\Library': fnFormCopyAwithBackgroundWarn,fnbutton,fncombof
12123     library 'S:\Core\Library': fnacs,fncmdkey,fncmdset,fncomboa
12125     library 'S:\Core\Library': fncreg_read,fncreg_write,fnlbl,fndednames,fnmsgbox
12126     library 'S:\Core\Library': fnpa_background,fnreg_read,fnreg_write,fnureg_read,fnureg_write,fnopt,fntxt,fntos,fngethandle,fnchk
12140     on error goto ERTN
12160     ! r: constants
12170     dim ml$(0)*128
12180     dim resp$(128)*256
12200     dim optCopy$(5)*72
12220     optCopy$(1)='A - For Internal Revenue Service Center'
12240     optCopy$(2)='1 - For State Tax Department'
12260     optCopy$(3)='B - For Recipient'
12280     optCopy$(4)='C - For Payer'
12300     optCopy$(5)='2 - To be filed with recipient;;s state income tax return, when required'
12320   dim copyCurrent$*68
12340   !
12360   dim copyFile$(5)*128,ssnMask(5)
12380   copyFile$(1)='S:\Core\pdf\2016\1099-Misc\Copy A.pdf' : ssnMask(1)=0
12400   copyFile$(2)='S:\Core\pdf\2016\1099-Misc\Copy 1.pdf' : ssnMask(2)=0
12420   copyFile$(3)='S:\Core\pdf\2016\1099-Misc\Copy B.pdf' : ssnMask(3)=0
12440   copyFile$(4)='S:\Core\pdf\2016\1099-Misc\Copy C.pdf' : ssnMask(4)=1
12460   copyFile$(5)='S:\Core\pdf\2016\1099-Misc\Copy 2.pdf' : ssnMask(5)=1
12480     ! /r
12500   end if
12520 fnend
16000 ! <Updateable Region: ERTN>
16020 ERTN: let fnerror(program$,err,line,act$,"xit")
16040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
16060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
16080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
16100 ERTN_EXEC_ACT: execute act$ : goto ERTN
16120 ! /region
22000 def library fn1099print_close
22020     if ten99Export$='True' then 
22040       close #hExport: 
22060     else 
22080       fnpa_finis
22100     end if
22120     ten99initialized=0
22140 fnend
24000 def library fn1099print(vn$*8,nam$*30,mat ad$,ss$*11,mat box)
24020   if ~setup then let fn_setup
24040   if ~ten99initialized then ! r: initialize output destination (if necessary)
24060     if env$('CurSys')='PR' then
24080       open #hCompany:=fngethandle: "Name="&env$('Q')&"\PRmstr\company.h"&str$(cno)&",Shr", internal,input,relative
24100       read #hCompany,using "Form POS 1,3*C 40,C 12": mat a$,mat fed$
24120       close #hCompany: 
24140     else if env$('CurSys')='GL' then
24160       open #hCompany:=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative
24180       read #hCompany,using ' Form POS 1,3*C 40,C 12': mat a$,fed$ 
24200       close #hCompany: 
24202     else if env$('CurSys')='CL' then
24204       open #hCompany:=fngethandle: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative
24206       read #hCompany,using ' Form POS 1,3*C 40,C 12': mat a$,fed$ 
24208       close #hCompany: 
24220     end if
24240     ten99initialized=1
24260     fnreg_read('1099 - Export 1' ,ten99Export$,'False')
24280     fnreg_read('1099 - Form 1 Y',tmp$,'5'  ) : topmargin=val(tmp$)
24300     fnreg_read('1099 - Form 2 Y',tmp$,'144') : bottom=val(tmp$)
24320     fnreg_read('1099 - X'       ,tmp$,'5'  ) : left=val(tmp$)
24340     fnreg_read('1099 - 2 Per Page',twoPerPage$,'False' )
24360     fnreg_read('1099 - Enable Background',enableBackground$,'True' )
24380     fnureg_read('1099 - Export Filename',output_filename$,os_filename$(env$('userprofile')&'\Desktop\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt'))
24400     fncreg_read('1099 - Your Phone Number',ph$)
24420     fncreg_read('1099 - Copy Current'    ,copyCurrent$,optCopy$(1)) : copyCurrent=max(1,srch(mat optCopy$,copyCurrent$))
24440     !
24460     if ten99Export$='True' then 
24480       dim output_filename$*256
24500       output_filename$=srep$(output_filename$,'[CompanyNumber]',env$('cno'))
24520       output_filename$=srep$(output_filename$,'[companynumber]',env$('cno'))
24540       output_filename$=srep$(output_filename$,'[COMPANYNUMBER]',env$('cno'))
24560       output_filename$=srep$(output_filename$,'[TaxYear]',taxYear$)
24580       output_filename$=srep$(output_filename$,'[taxyear]',taxYear$)
24600       output_filename$=srep$(output_filename$,'[TAXYEAR]',taxYear$)
24620       open #hExport:=fngethandle: "Name="&br_filename$(output_filename$)&",REPLACE",display,output ioerr ASK_INFO
24640     else
24660       fnpa_open('',copyCurrent$,'PDF') 
24680     end if
24700   end if ! /r
24720   !
26000   if ten99Export$='True' then 
26020     ! r: export one
26040     pr #hExport: "01 ";" "
26060     pr #hExport: "02 ";ph$
26080     pr #hExport: "03 ";a$(1)
26100     pr #hExport: "04 ";box(1)
26120     pr #hExport: "05 ";" "
26140     pr #hExport: "06 ";a$(2)
26160     pr #hExport: "07 ";box(2)
26180     pr #hExport: "08 ";a$(3)
26200     pr #hExport: "09 ";box(3)
26220     pr #hExport: "10 ";box(4)
26240     pr #hExport: "11 ";fed$
26260     pr #hExport: "12 ";ss$
26280     pr #hExport: "13 ";box(5)
26300     pr #hExport: "14 ";box(6)
26320     pr #hExport: "15 ";box(7)
26340     pr #hExport: "16 ";box(8)
26360     pr #hExport: "17 ";nam$
26380     pr #hExport: "18 ";" "
26400     pr #hExport: "19 ";" "
26420     pr #hExport: "20 ";box(10)
26440     pr #hExport: "21 ";ad$(1)
26460     pr #hExport: "22 ";ad$(2)
26480     pr #hExport: "23 ";" "
26500     pr #hExport: "24 ";0
26520     pr #hExport: "25 ";vn$
26540     pr #hExport: "26 ";" "
26560     pr #hExport: "27 ";0
26580     pr #hExport: "28 ";" "
26600     pr #hExport: "29 ";0
26620     pr #hExport: "30 ";" " ! 0
26640     ! pr #hExport: "31 ";" "
26660     ! pr #hExport: "32 ";0
26680     pr #hExport: "*"
26700     ! /r
26720   else
27000     ! r: pr one
27020     column1=15 +left 
27040     column2=85+left+17
27060     column3=119 +left+18
27080     let ten99Count+=1
27100     if ten99Count=1 then let yOffset=topmargin 
27120     if ten99Count=2 then let yOffset=bottom
27140     if enableBackground$='True' and ten99Count=1 then
27160       fnpa_background(CopyFile$(copyCurrent))
27180     end if
27500     if debug then
27520       for tmp=1 to 10
27540         fnpa_txt('line '&str$(tmp),1,fn_line(tmp))
27560       nex tmp
27580     end if
28000     fnpa_FontSize
28020     fnpa_txt(a$(1)(1:30),column1,fn_line(1))
28040     fnpa_txt(a$(2)(1:30),column1,fn_line(1)+5)
28060     fnpa_txt(a$(3)(1:30),column1,fn_line(1)+10)
28080     fnpa_txt(ph$,column1,fn_line(1)+20)
28100     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(1)),column2,fn_line(1))
28120     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(2)),column2,fn_line(2))
28140     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(3)),column2,fn_line(3))
28160     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(4)),column3,fn_line(3))
28180     fnpa_txt(fed$,column1,fn_line(4))
28200     fnpa_txt(ss$,column1+45,fn_line(4))
28220     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(5)),column2,fn_line(4))
28240     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(6)),column3,fn_line(4))
28260     fnpa_txt(nam$(1:30),column1,fn_line(5)-8)
28280     fnpa_txt(ad$(1),column1,fn_line(5)+5) ! address line 1
28300     if udim(mat ad$)=2 then
28310       fnpa_txt(ad$(2),column1,fn_line(5)+18) !  CSZ
28320     else if udim(mat ad$)=3 then
28330       fnpa_txt(ad$(2),column1,fn_line(5)+8) ! address line 2
28340       fnpa_txt(ad$(3),column1,fn_line(5)+18) !  CSZ
28440     else 
28460       pr 'udim(mat ad$)=';udim(mat ad$);' this is unexpected by '&program$ : pause
28480     end if
28500     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(7)),column2,fn_line(5))
28520     fnpa_txt(cnvrt$("pic(zz,zzz,zzz.##",box(8)),column3,fn_line(5))
28540     fnpa_txt(vn$,column1,fn_line(8))
29000     if twoPerPage$='False' or ten99Count=2 then 
29020        fnpa_newpage
29040        let ten99Count=0
29060      end if
29080     ! /r
29100   end if
30000   XIT: ! 
30020 fnend 
34000 def fn_line(lineNumber)
34020   if lineNumber=1  then
34040     let lineReturn=yOffset+10
34060   else if lineNumber=2  then
34080     let lineReturn=yOffset+23
34100   else if lineNumber=3 then
34120     let lineReturn=yOffset+32
34140   else if lineNumber=4 then
34160     let lineReturn=yOffset+43+2+4
34180   else if lineNumber=5 then
34200     let lineReturn=yOffset+56+2+8
34220   else if lineNumber>5 then
34240     let lineReturn=yOffset+56+2+((lineNumber-5)*13)
34260   else
34280     pr 'i dunno' : pause
34300   end if
34320   fn_line=lineReturn
34340 fnend
36000 def fn_ask_margins
36020 ! if env$('acsdeveloper')='' then pr bell; : goto am_xit
36040   fnreg_read('1099 - Form 1 Y',amResp$(1),'5' )
36060   fnreg_read('1099 - Form 2 Y',amResp$(2),'144')
36080   fnreg_read('1099 - X'       ,amResp$(3),'5' )
36100   fntos(sn$='1099_ask_margins')
36120   lc=0 : mylen=30 : mypos=mylen+2 
36140   fnlbl(lc+=1,1,"Form 1 Distance from Top (mm):",mylen,1)
36160   fntxt(lc,mypos,3,0,1,'30')
36180   fnlbl(lc+=1,1,"Form 2 Distance from Top (mm):",mylen,1)
36200   fntxt(lc,mypos,3,0,1,'30')
36220   fnlbl(lc+=1,1,"Left Margin Size (mm):",mylen,1)
36240   fntxt(lc,mypos,3,0,1,'30')
36260   fncmdset(4)
36280   fnacs(sn$,0,mat amResp$,ckey)
36300   if ckey<>5 then
36320     fnreg_write('1099 - Form 1 Y' ,amResp$(1))
36340     fnreg_write('1099 - Form 2 Y' ,amResp$(2))
36360     fnreg_write('1099 - X'        ,amResp$(3))
36380     topmargin= val(amResp$(1))
36400     bottom=    val(amResp$(2))
36420     left=      val(amResp$(3))
36440   end if
36900 fnend
40000 def library fnask_1099_info(&seltp,&type,&min1,&beg_date,&end_date)
40020   if ~awi_setup then ! r:
40040     awi_setup=1
40060     if ~setup then let fn_setup
40280     ! r: read or set values for ASK_INFO screen
40300     taxYear$=date$(days(date$)-180,'CCYY')
40310     if env$('CurSys')='PR' then
40320       dim deductionFullName$(20)*20,deductionOption$(20)*20
42020       fnDedNames(mat deductionFullName$)
42040       deductionOptionCount=0
42060       for j=1 to udim(mat deductionFullName$)
42070         deductionFullName$(j)=rtrm$(deductionFullName$(j))
42080         if trim$(deductionFullName$(j))<>'' then
42100           deductionOption$(deductionOptionCount+=1)=deductionFullName$(j)
42120         end if
42140       next j
42160       mat deductionOption$(deductionOptionCount)
42180       for j=1 to 8
42200         let typeOption$(j)=str$(j)
42220       next j
42230     end if
42240     !
42260     fncreg_read('1099 - Filter - Minimum Amount',tmp$,'600') : min1=val(tmp$)
42280     !
42300     fnreg_read('Print 1099'              ,destinationOpt$(1),'True' )
42320     fnreg_read('1099 - Export 1'         ,destinationOpt$(2),'False')
42360     fnreg_read('1099 - Enable Background',enableBackground$  ,'True' )
42380     fnreg_read('1099 - 2 Per Page'       ,twoPerPage$        ,'False' )
42400     fncreg_read('1099 - Copy Current'    ,copyCurrent$,optCopy$(1)) : copyCurrent=max(1,srch(mat optCopy$,copyCurrent$))
42440     fnureg_read('1099 - Export Filename',output_filename$,os_filename$(env$('userprofile')&'\Desktop\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt'))
42450     fncreg_read('1099 - Your Phone Number',ph$)
42452     dim seltp$*256
42454     fncreg_read('1099 - seltp',seltp$) 
42455     if env$('curSys')='PR' then seltp=srch(mat deductionFullName$,seltp$)
42456     if env$('curSys')='GL' or env$('curSys')='CL' then seltp=val(seltp$(1:2))
42460     ! /r
42480   end if ! /r
43000   awiReturn=0 
44000   ASK_INFO: !
44020   fntos(sn$="pr1099-1")
44040   let rc=lc=0 : let mylen=40 : let mypos=mylen+3
44060   fnlbl(lc+=1,1,"Tax Year:",mylen,1)
44080   fntxt(lc,mypos,4,0,1,"",1,"Year to pr 1099s for")
44100   resp$(resc_taxYear:=rc+=1)=taxYear$
44120   if env$('cursys')='PR' then
44140     lc+=1
44160     fnlbl(lc+=1,1,"Miscellaneous Deduction to Print:",mylen,1)
44180     fncomboa("deductions",lc,mypos,mat deductionOption$,"Select the deduction you want printed.") 
44200     let resp$(respc_deduction:=rc+=1)=seltp$
44220     fnlbl(lc+=1,1,"1099 Type to Print:",mylen,1)
44260     fncomboa('type',lc,mypos,mat typeOption$,"Select the code for the 1099 vendor type.") 
44280     let resp$(respc_type:=rc+=1)=""
44300   else if env$('cursys')='GL' then
44310     fnlbl(lc+=1,1,"Payee Type to Print:",mylen,1)
44320     fncombof("PayeeType",lc,mypos,27,env$('Q')&"\GLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.") 
44340     let resp$(respc_deduction:=rc+=1)=seltp$
44342   else if env$('cursys')='CL' then
44344     fnlbl(lc+=1,1,"Payee Type to Print:",mylen,1)
44346     fncombof("Payeetype",lc,mypos,27,env$('Q')&"\CLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.")
44348     let resp$(respc_deduction:=rc+=1)=seltp$
44360 !
44380   end if
44400   lc+=1
44420   fnlbl(lc+=1,1,"Minimum Amount to Print:",mylen,1)
44440   fntxt(lc,mypos,12,0,1,"10",0,'Enter the minimum amount that should be printed."') 
44460   let resp$(respc_min1:=rc+=1)=str$(min1)
44480   fnlbl(lc+=1,1,"Your Telephone Number:",mylen,1)
44500   fntxt(lc,mypos,12,0,1,"",0,'You can use dashes, etc."') 
44520   let resp$(respc_phone:=rc+=1)=ph$
44540   lc+=1
44560   fnopt(lc+=1,3,"Print 1099")
44580   let resp$(respc_Print1099:=rc+=1)=destinationOpt$(1)
44600   fnlbl(lc+=1,5,"Copy:",12,1,0)
44620   fncomboa('Copy',lc,19,mat optCopy$, '',20)
44640   let resp$(respc_copyCurrent:=rc+=1)=copyCurrent$
44660   fnlbl(lc+=1,20,"(2 per page is not yet available with Backgrounds)",50,0)
44680   fnchk(lc+=1,20,'Enable Background',1)
44700   let resp$(respc_enableBackground:=rc+=1)=enableBackground$
44720   fnchk(lc+=1,20,'2 Per Page',1)
44740   let resp$(respc_twoPerPage:=rc+=1)=twoPerPage$
44760   lc+=1
44780   fnopt(lc+=1,3,"Export for Advanced Micro Solutions")
44800   resp$(respc_export_ams:=rc+=1)=destinationOpt$(2)
44820   fnlbl(lc+=1,5,"Export File:",12,1,0,franum)
44840   fntxt(lc,19,20,80,0,'72',0,'Choose a destination location for the ACS export.',franum)
44860   resp$(resp_export_file:=rc+=1)=output_filename$
44880   fnbutton(lc,5+12+20+5,'Default',14,'Choose to set the default for the selected destination software.',0,0,franum)
44900   fnlbl(lc+=1,19,"([CompanyNumber] and [TaxYear] will be substituted in filename)",0,0,0,franum)
44920   !
44940   fncmdkey("&Margins",ckey_margins:=1021,0,0,"Manually adjust margins for hitting forms")
44960   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
44980   fncmdkey("&Cancel",5,0,1,"Returns to menu")
46000   fnacs(sn$,0,mat resp$,ckey)
46020   if ckey<>5 then 
46040     ! r: gather local variables from mat resp$
46060     if env$('cursys')='PR' then
46080       seltp$=resp$(respc_deduction)
46090       seltp=srch(mat deductionFullName$,seltp$)
46100       type=val(resp$(respc_type))
46102     else if env$('cursys')='GL' or env$('cursys')='CL' then
46104       seltp=val(resp$(respc_deduction)(1:2))
46106       seltp$=resp$(respc_deduction)
46120     end if
46140     min1=val(resp$(respc_min1))
46160     ph$=resp$(respc_phone)
46180     beg_date=val(taxYear$&'0101')
46200     end_date=val(taxYear$&'1231')
46220     copyCurrent$=resp$(respc_copyCurrent)
46240     enableBackground$=resp$(respc_enableBackground)
46260     twoPerPage$=resp$(respc_twoPerPage)
46280     destinationOpt$(1)=resp$(respc_Print1099)
46300     destinationOpt$(2)=resp$(respc_export_ams)
46320     output_filename$=resp$(resp_export_file)
46340     ! /r
47000     ! r: validate, respond and/or reject 
47020     if env$('cursys')='PR' and seltp=0 then 
47040       mat ml$(2) 
47060       let ml$(1)="You must indicate which deduction you want printed." 
47080       let ml$(2)="        Click OK to correct." 
47100       fnmsgbox(mat ml$,resp$,cap$,0)
47120       goto ASK_INFO
47140     end if
47160     if env$('cursys')='PR' and (type=0 or type>8) then 
47180       mat ml$(2) 
47200       let ml$(1)="You must enter a valid 1099 type." 
47220       let ml$(2)="        Click OK to correct." 
47240       fnmsgbox(mat ml$,resp$,cap$,0)
47260       goto ASK_INFO
47280     end if
47300     if ckey=14 then 
47320         let output_filename$=os_filename$(env$('userprofile')&'\Desktop\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt')
47340       goto ASK_INFO
47360     else if ckey=ckey_margins then
47380       fn_ask_margins
47400       goto ASK_INFO
47420     end if
47440     ! /r
52000     ! r: save stuff
52020     fncreg_write('1099 - Filter - Minimum Amount',str$(min1))
52030     fncreg_write('1099 - seltp',seltp$)
52040     fncreg_write('1099 - Copy Current',copyCurrent$)
52060     fnreg_write('Print 1099',destinationOpt$(1))
52080     fnreg_write('1099 - Enable Background',enableBackground$)
52100     fnreg_write('1099 - 2 Per Page',twoPerPage$)
52120     fnreg_write('1099 - Export 1',destinationOpt$(2))
52140     fnureg_write('1099 - Export Filename',output_filename$)
52160     fncreg_write('1099 - Your Phone Number',ph$)
52180     ! /r
54000     if copyCurrent$=optCopy$(1) and enableBackground$='True' then let fnFormCopyAwithBackgroundWarn
54020     awiReturn=1
54030   end if 
54060   fnask_1099_info=awiReturn
54080 fnend
