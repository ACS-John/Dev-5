10000 ! r: setup
10020 ! ken: "
10040 ! never could get it work with an RO record if all zeroes;  
10060 ! without RO record i had to put an RE record in front of every RW record and 
10080 ! had to follow with an RT record -  
10100 ! Right now this program will not create an RO record
10120 ! "
10140   library 'S:\Core\Library': fntop,fnxit, fnerror,fnacs,fnlbl,fntxt,fntos,fncmdkey,fnureg_read,fnureg_write,fngethandle,fnDedNames,fncreg_read,fncreg_write,fncomboa
10160   on error goto ERTN
10180   let fntop(program$,cap$="Electronic W-2")
10200 ! ______________________________________________________________________
10220   dim em$(3)*30,ss$*11,tcp(32),cap$*128,tmp$*128
10240   dim tdc(10),newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20)
10260   dim dedst(20),deduc(20),abrevname$(20)*8,fullname$(20)*20
10280   dim a$(3)*40,federal_id$*12,s2(2)
10300   dim w3(2),i2(2),t2(2)
10320   dim emppin$*17 ! Personal ID Number (used in RA Record)
10340   dim tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
10360   dim w2(9),i1(9),t1(9),ct$*20,st$*2
10380   dim terminat$*1,first$*15,mid$*15,last$*20,resp$(40)*256,path$*256
10400   open #hCompany:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
10420   read #hCompany,using fCompany: mat a$,federal_id$,loccode
10440   fCompany: form pos 1,3*c 40,c 12,pos 150,x 80,n 2
10460   close #hCompany: 
10480   !
10500   dim optNameFormat$(2)*20,nameFormat_sf$(2)*1
10520   optNameFormat$(1)='First Name First' : nameFormat_sf$(1)='F'
10540   optNameFormat$(2)='Last Name First'  : nameFormat_sf$(2)='S'
10560   !
10580   disable=1 ! 
10600   let med$="Y"
10620 ! /r
11000   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
11020   open #hEmployee:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
11040   open #hChecks:=fngethandle: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
12000 ! r: initialize variables
12020   let fnureg_read('electronic w-2 file',path$,os_filename$(env$('userprofile')&'\Desktop\w2elec-'&env$('cno')))
12040   beg_date=val('0101'&date$(days(date$)-180,'YY'))
12060   end_date=val('1231'&date$(days(date$)-180,'YY'))
12080   yr=date(days(date$)-180,'ccyy')
12100   ssmax =127200
12120   ssrate=.062
12140   mcmax =.0145
12160   mcrate=999999
12180   fncreg_read('W-2 Company Name',a$(1),a$(1))
12200   fncreg_read('W-2 Company Street Address',a$(2),a$(2))
12220   fncreg_read('W-2 Company City',ct$)
12240   fncreg_read('W-2 Company State',st$)
12260   fncreg_read('W-2 Company Zip',zip$)
12280   if ct$='' or st$='' or zip$='' then
12300     let p1=pos(a$(3),",",1): let comma=1
12320     if p1=0 then let p1=pos(a$(3)," ",1): let comma=0
12340     let ct$=a$(3)(1:p1-1)
12360     if comma=1 then let st$=a$(3)(p1+2:p1+3) else let st$=a$(3)(p1+1:p1+2)
12380     let p2=len(rtrm$(a$(3)))
12400     let p1=p2-4
12420     let zip$=a$(3)(p1:p2)
12440   end if
12460   fncreg_read('Employee Name Format',tmp$,optNameFormat$(1)) : nameFormat=max(1,srch(mat optNameFormat$,tmp$))
12480   fncreg_read('Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins',tmp$) : ins=val(tmp$)
12500   fncreg_read('Miscellaneous Deduction Used For Pension',tmp$) : pen=val(tmp$)
12520   fncreg_read('Miscellaneous Deduction Used For Deferred Compensation',tmp$) : dfc=val(tmp$)
12540   fncreg_read('Miscellaneous Deduction Used For Dependent Care Assistance',tmp$) : dcan=val(tmp$)
12560 !
12580   dim contact$*27
12600   dim email$*40
12620   fncreg_read('W-2 Personal ID Number',emppin$)
12640   fncreg_read('W-2 Resub Indicator',resub$,"0")
12660   fncreg_read('W-2 Resub TLCN',tlcn$)
12680   fncreg_read('W-2 Contact Name',contact$)
12700   fncreg_read('W-2 Contact Phone Number',contactph$)
12720   fncreg_read('W-2 Contact Phone Extension',phoneext$)
12740   fncreg_read('W-2 Contact E-Mail',email$)
12760   fncreg_read('W-2 Terminating Business Indicator',terminat$,"0")
12780 !
12800   fncreg_read('W-2 State Code',tmp$) : sr1=val(tmp$)
12820   fncreg_read('W-2 FIPS',tmp$) : sr2=val(tmp$)
12840 !
12860 !
12880 ! /r
14000 SCREEN1_NEW: ! r:
14020   let fntos(sn$="W2-1") 
14040   let rc=lyne=0: mylen=17 : mypos=mylen+2
14060   let fnlbl(lyne+=1,1,"Starting Date:",mylen,1,0,0)
14080   let fntxt(lyne,mypos,10,0,1,"3",0,"First day of calendar year",0) 
14100   let resp$(respc_dateStart:=rc+=1)=str$(beg_date)
14120   let fnlbl(lyne+=1,1,"Ending Date:",mylen,1,0,0)
14140   let fntxt(lyne,mypos,10,0,1,"3",0,"Last day of calendar year",0) 
14160   let resp$(respc_dateEnd:=rc+=1)=str$(end_date)
14180   lyne+=1
14200   let fnlbl(lyne+=1,1,"Output File Name:",mylen,1,0,0)
14220   let fntxt(lyne,mypos,30,0,0,'70',0,"Destination and file name you wish to use.",0) 
14240   let resp$(respc_path:=rc+=1)=path$
14260   lyne+=1
14280   fnlbl(lyne+=1,1,"Company Name:",mylen,1,0,0)
14300   fntxt(lyne,mypos,40,0,0,"",0,"Enter the name of the company submitting",0) 
14320   resp$(resp_cnam:=rc+=1)=a$(1)
14340   fnlbl(lyne+=1,1,"Street Address:",mylen,1,0,0)
14360   fntxt(lyne,mypos,40,0,0,"",0,"Address of the company submitting",0) 
14380   resp$(resp_cstreet:=rc+=1)=a$(2)
14400   fnlbl(lyne+=1,1,"City:",mylen,1,0,0)
14420   fntxt(lyne,mypos,10,22,0,"",0,"City of the company submitting",0) 
14440   resp$(resp_ccity:=rc+=1)=ct$
14460   fnlbl(lyne,mypos+10+2,"State:",6,1,0,0)
14480   fntxt(lyne,mypos+10+2+6+2,2,0,0,"",0,"State for the company being submitted",0) 
14500   resp$(resp_cstate:=rc+=1)=st$
14520   fnlbl(lyne,mypos+10+2+6+2+2+2,"Zip:",4,1,0,0)
14540   fntxt(lyne,mypos+10+2+6+2+2+2+4+2,5,0,0,"",0,"Zip code for the company being submitted",0) 
14560   resp$(resp_czip:=rc+=1)=zip$
14580   fnlbl(lyne+=1,1,"Federal ID:",mylen,1,0,0)
14600   fntxt(lyne,mypos,12,0,0,"1000",0,"Enter the Federal Id number without slashes or dashes.",0) 
14620   resp$(resp_fid:=rc+=1)=federal_id$ 
14640   !
14660   lyne=0
14680   col3=17+2+30+2 :   mylen=29 : mypos=col3+mylen+2
14700   fnlbl(lyne+=1,col3,"Payment Year:",mylen,1,0,0)
14720   fntxt(lyne,mypos,4,0,0,"1030",0,"Enter the year for which the wages were paid in ccyy format.",0) 
14740   resp$(resp_paymentYear:=rc+=1)=str$(yr)
14760   fnlbl(lyne+=1,col3,"Social Security Maximum Wage:",mylen,1,0,0)
14780   fntxt(lyne,mypos,10,0,0,"10",disable,"Enter the social security maximum wage for the year just completed.",0) 
14800   resp$(resp_ssmax:=rc+=1)=str$(ssmax)
14820   fnlbl(lyne+=1,col3,"Social Security Rate:",mylen,1,0,0)
14840   fntxt(lyne,mypos,6,0,0,"34",disable,"Enter the social security rate for the year just completed.",0) 
14860   resp$(resp_ssrate:=rc+=1)=str$(ssrate)
14880   fnlbl(lyne+=1,col3,"Medicare Maximum Wage:",mylen,1,0,0)
14900   fntxt(lyne,mypos,10,0,0,"10",disable,"Enter the medicare maximum wage for the year just completed.",0) 
14920   resp$(resp_mcmax:=rc+=1)=str$(mcmax)
14940   fnlbl(lyne+=1,col3,"Medicare Rate:",mylen,1,0,0)
14960   fntxt(lyne,mypos,6,0,0,"30",disable,"Enter the medicare rate for the year just completed.",0) 
14980   resp$(resp_mcrate:=rc+=1)=str$(mcrate)
15000   !
15020   lyne=10 : mylen=69 : mypos=mylen+2
15040   fnlbl(lyne+=1,1,"Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:",mylen,1,0,0)
15060   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
15080   resp$(resp_ins:=rc+=1)=str$(ins)
15100   fnlbl(lyne+=1,1,"Miscellaneous Deduction Used For Pension:",mylen,1,0,0)
15120   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
15140   resp$(resp_pen:=rc+=1)=str$(pen)
15160   fnlbl(lyne+=1,1,"Miscellaneous Deduction Used For Deferred Compensation:",mylen,1)
15180   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
15200   resp$(resp_dfc:=rc+=1)=str$(dfc)
15220   fnlbl(lyne+=1,1,"Miscellaneous Deduction Used For Dependent Care Assistance:",mylen,1)
15240   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
15260   resp$(resp_dcan:=rc+=1)=str$(dcan)
15280   lyne+=1
15300   mylen=31 : mypos=mylen+2
15320   fnlbl(lyne+=1,1,"Employee Name Format:",mylen,1)
15340   fncomboa('nameFormat',lyne,mypos,mat optNameFormat$, '',20)
15360   resp$(resp_nameFormat:=rc+=1)=optNameFormat$(nameFormat)
15380   lyne+=1
15400   fnlbl(lyne+=1,1,"Personal ID Number:",mylen,1)
15420   fntxt(lyne,mypos,17,0,0,"",0,"",0) 
15440   resp$(resp_emppin:=rc+=1)=emppin$
15460   fnlbl(lyne+=1,1,"Resub Indicator:",mylen,1)
15480   fntxt(lyne,mypos,1,0,0,"30",0,"",0) 
15500   resp$(resp_resub:=rc+=1)=resub$
15520         fnlbl(lyne+=1,1,"Resub TLCN:",mylen,1)
15540         fntxt(lyne,mypos,6,0,0,"30",0,"",0) 
15560         resp$(resp_tlcn:=rc+=1)=tlcn$
15580   fnlbl(lyne+=1,1,"Contact Name:",mylen,1)
15600   fntxt(lyne,mypos,27,0,0,"30",0,"",0) 
15620   resp$(resp_contact:=rc+=1)=contact$
15640   fnlbl(lyne+=1,1,"Contact Phone Number:",mylen,1)
15660   fntxt(lyne,mypos,15,0,0,"30",0,"",0) 
15680   resp$(resp_contactph:=rc+=1)=contactph$
15700         fnlbl(lyne+=1,1,"Contact Phone Extension:",mylen,1)
15720         fntxt(lyne,mypos,5,0,0,"",0,"",0) 
15740         resp$(resp_phoneext:=rc+=1)=phoneext$
15760   fnlbl(lyne+=1,1,"Contact E-Mail:",mylen,1)
15780   fntxt(lyne,mypos,40,0,0,"",0,"",0) 
15800   resp$(resp_email:=rc+=1)=email$
15820   fnlbl(lyne+=1,1,"Terminating Business Indicator:",mylen,1)
15840   fntxt(lyne,mypos,1,0,0,"30",0,"",0) 
15860   resp$(resp_terminat:=rc+=1)=terminat$
15880   lyne+=1
15900   mylen=62 : mypos=mylen+2
15920   fnlbl(lyne+=1,1,"Some states require filing electronic W-2s.",80,2)
15940   fnlbl(lyne+=1,1,"Answer the following questions if you wish to create 'RS' records during this run.",80,2)
15960   fnlbl(lyne+=1,1,"State code used in your record to identify the selected state:",mylen,1)
15980   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
16000   resp$(resp_state_code:=rc+=1)=str$(sr1)
16020   fnlbl(lyne+=1,1,"Appropriate FIPS postal numeric code:",mylen,1)
16030   fntxt(lyne,mypos,2,0,0,"30",0,"",0) 
16040   resp$(resp_fips:=rc+=1)=str$(sr2)
16060   fnlbl(lyne+=1,1,'(See an appendix in your electronic booklet for the postal code!)',80,2)
16080   !
16100   let fncmdkey("Next",1,1,0,"Creates the export")
16120   let fncmdkey("Cancel",5,0,1,"Returns to menu")
16140   let fnacs(sn$,0,mat resp$,ckey) ! /r
16160   if ckey=5 then goto XIT
18000 ! r: populate local variables from mat resp$
18020   beg_date=val(resp$(respc_dateStart))
18040   end_date=val(resp$(respc_dateEnd))
18060   path$=resp$(respc_path)
18080   !
18100   a$(1)=resp$(resp_cnam)
18120   a$(2)=resp$(resp_cstreet)
18140   ct$=resp$(resp_ccity)
18160   st$=resp$(resp_cstate)
18180   zip$=resp$(resp_czip)
18200   !
18220   federal_id$=resp$(resp_fid) : federal_id_val=val(srep$(federal_id$,'-',''))
18240   yr=val(resp$(resp_paymentYear))
18260   ssmax=val(resp$(resp_ssmax))
18280   ssrate=val(resp$(resp_ssrate))
18300   mcmax=val(resp$(resp_mcmax))
18320   mcrate=val(resp$(resp_mcrate))
18340   !
18360   nameFormat=srch(mat optNameFormat$,resp$(resp_nameFormat))
18380   !
18400   ins=val(resp$(resp_ins))
18420   pen=val(resp$(resp_pen))
18440   dfc=val(resp$(resp_dfc))
18460   dcan=val(resp$(resp_dcan))
18480   !
18500   emppin$=resp$(resp_emppin)
18520   resub$=resp$(resp_resub)
18540   tlcn$=resp$(resp_tlcn)
18560   contact$=resp$(resp_contact)
18580   contactph$=resp$(resp_contactph)
18600   phoneext$=resp$(resp_phoneext)
18620   email$=resp$(resp_email)
18640   terminat$=resp$(resp_terminat)
18660   !
18680   sr1=val(resp$(resp_state_code))
18700   sr2=val(resp$(resp_fips))
18720   !
18740 ! /r
22000 ! r: validate screen1 values
22020   if beg_date=0 then goto SCREEN1_NEW
22040   if end_date=0 then goto SCREEN1_NEW
22060   if nameFormat<=0 or nameFormat>udim(mat nameFormat_sf$) then goto SCREEN1_NEW
22080   if yr<(date('ccyy')-10) then goto SCREEN1_NEW
22100   if ssmax<53400 then goto SCREEN1_NEW
22120   if ins<0 or ins>10 then goto SCREEN1_NEW
22140   if pen<0 or pen>10 then goto SCREEN1_NEW
22160   if dfc<0 or dfc>10 then goto SCREEN1_NEW
22180   if resub$="1" and rtrm$(tlcn$)="" then goto SCREEN1_NEW
22200   if terminat$<>"0" and terminat$<>"1" then goto SCREEN1_NEW
22220   if sr1<0 or sr1>20 then goto SCREEN1_NEW
22240   if sr1>0 and sr2=0 then goto SCREEN1_NEW
22260 ! /r
24000 ! r: save screen1 values
24020   fnureg_write('electronic w-2 file',path$)
24040   fncreg_write('W-2 Company Name',a$(1))
24060   fncreg_write('W-2 Company Street Address',a$(2))
24080   fncreg_write('W-2 Company City',ct$)
24100   fncreg_write('W-2 Company State',st$)
24120   fncreg_write('W-2 Company Zip',zip$)
24140   open #hCompany:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,outin 
24160   rewrite #hCompany,using 'form pos 1,x 120,c 12,pos 150,x 80,n 2',rec=1: federal_id$,loccode
24180   close #hCompany: 
24200   fncreg_write('Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins',str$(ins))
24220   fncreg_write('Miscellaneous Deduction Used For Pension',str$(pen))
24240   fncreg_write('Miscellaneous Deduction Used For Deferred Compensation',str$(dfc))
24260   fncreg_write('Miscellaneous Deduction Used For Dependent Care Assistance',str$(dcan))
24280   !
24300   fncreg_write('W-2 Personal ID Number',emppin$)
24320   fncreg_write('W-2 Resub Indicator',resub$)
24340   fncreg_write('W-2 Resub TLCN',tlcn$)
24360   fncreg_write('W-2 Contact Name',contact$)
24380   fncreg_write('W-2 Contact Phone Number',contactph$)
24400   fncreg_write('W-2 Contact Phone Extension',phoneext$)
24420   fncreg_write('W-2 Contact E-Mail',email$)
24440   fncreg_write('W-2 Terminating Business Indicator',terminat$)
24460   !
24480   fncreg_write('W-2 State Code',str$(sr1))
24500   fncreg_write('W-2 FIPS',str$(sr2))
24520   !
24540 ! /r
32120   open #hOut:=fngethandle: "Name=W2REPORT,RecL=512,eol=crlf,replace",display,output 
32140 !
32160   gosub RecRA
32180   gosub RecRE ! kj 22610  was commented
34000 NEXT_EMPLOYEE: ! r: main loop
34020  ! Print Fields "12,32,N 3,UT,N": readCount+=1/LREC(1)*100
34040   read #hEmployee,using fEmployee: eno,mat em$,ss$,em6,ta eof FINIS
34060   fEmployee: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
34080   gosub NameParse
34100   let p1=pos(em$(3),",",1) : let comma=1
34120   if p1=0 then let p1=pos(em$(3)," ",1): let comma=0
34140   let emct$=em$(3)(1:p1-1)
34160   gosub EXTRACT_STATE : let emst$=holdst$ ! If COMMA=1 Then Let EMST$=EM$(3)(P1+2:P1+3) Else Let EMST$=EM$(3)(P1+1:P1+2)
34180   let p2=len(rtrm$(em$(3)))
34200   let p1=p2-4
34220   let emzip$=em$(3)(p1:p2)
34240 L2070: let p1=pos(ss$,"-",1)
34260   if p1>0 then let ss$(p1:p1)="": goto L2070 else let ssn=val(ss$)
34280   let checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
34300   restore #hChecks,key>=checkkey$: nokey NEXT_EMPLOYEE
34320 L2120: read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L2480
34340   if heno<>eno then goto L2480
34360   if prd<beg_date or prd>end_date then goto L2120 ! not this year
34380 ! form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
34400   if tcd<1 or tcd>10 then let tcd=1
34420 ! FILE_SHUFFLEh with this employee
34440   let dedret=0
34460   let cafded=0
34480   for j=1 to 20
34500     if newdedfed(j)=1 then 
34520       let dedret=dedret+tcp(j+4)
34540     end if
34560     if dedfica(j)=1 then 
34580       let cafded=cafded+tcp(j+4)
34600     end if
34620   next j
34640   let w2(1)=min(w2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
34660   let w3=w3+tcp(2) ! TOTAL FICA WITHHELD
34680   let w3(1)=w3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
34700 ! if client$="Washington Parrish" then let w3(1)=w3(1)+tcp(5) ! add deferred comp match to medicare wages
34720   let w3(1)=min(mcmax,w3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
34740   if uprc$(med$)="Y" then  ! SS WH
34760     let w2=w2+tcp(2) 
34780   else 
34800     let w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2)
34820   end if
34840   if uprc$(med$)="Y" then  ! MEDICARE WITHHELD
34860     let w3(2)=w3(2)+tcp(3) 
34880     else 
34900     let w3(2)=w3-w2
34920   end if
34940   let w2(2)=w2(2)+tcp(30) ! FICA TIPS YTD
34960   let w2(3)=w2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
34980   let w2(4)=w2 ! W2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
35000 ! LET W2(4)=W2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
35020   let w2(5)=w2(5)+tcp(1) ! FED W/H YTD
35040   if ins>0 then let w2(6)=w2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
35060   let w2(7)=w2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
35080   let w2(8)=w2(8)+tcp(24) ! EIC TOTAL
35100   let w2(9)=w2(9)+0 ! ALLOCATED TIPS
35120   if dfc>0 then let dc1=dc1+tcp(4+dfc)*100 ! DEFERRED COMPENSATION
35140   if dcan>0 then let dca=dca+tcp(4+dcan)*100 ! DEPENDENT CARE ASSISTANCE
35160   if sr1><tcd then goto L2470
35180   let s2(1)=s2(1)+((tcp(31)-dedret)*100)
35200   let s2(2)=s2(2)+(tcp(4)*100)
35220 L2470: !
35240   goto L2120
35260 L2480: !
35280   if em6=9 then let w2(1)=w2(4)=w3(1)=w3(2)=0 ! NO SS OR MC
35300   if em6=1 then let w3(1)=w3(2)=0 ! NO MEDICARE
35320   if em6=2 then let w2(1)=w2(4)=0 ! NO SOC-SEC
35340   if w2(3)<>0 or w2(1)<>0 then 
35360     ! Gosub RecRE   kj 22610
35380     gosub RecRW
35400     gosub RecRS
35420     let tw1=tw1+1
35440     let tw2=tw2+1
35460     ! Gosub RecRT   ! KJ 22610
35480     ! Let TW2=0  kj 22610
35500   end if
35520 goto NEXT_EMPLOYEE ! /r
35540 ! ______________________________________________________________________
38000 RecRA: ! r:
38020   print #hOut,using fRecRA: "RA",federal_id_val,emppin$(1:8),"",resub$,tlcn$,"98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L",""
38040   fRecRA: form pos 1,c 2,pic(#########),c 8,c 9,c 1,c 6,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 12
38060 return ! /r
42000 RecRE: ! r:
42020 ! if client$="PiattCO" then let emptype$="S"
42040   print #hOut,using fRecRE: "RE",yr,"",federal_id_val,"",terminat$,"","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"",emptype$,"","","","","R","",0,""
42060   fRecRE: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 1,c 4,c 23,c 15,c 2,c 1,c 1,n 1,c 291
42080 return ! /r
44000 ! r: unused (2E record type??)
44020 ! ! form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
44040 !   print #hOut,using L2710: "2E",ct$,st$,"",zip$,nameFormat_sf$(nameFormat),typemp$(1:1),"","","",""
44060 ! L2710: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
44080 !   return 
44100 ! /r
46000 RecRW: ! r:
46020   for j=1 to 9: let w2(j)=w2(j)*100: next j
46040   for j=1 to 2: let w3(j)=w3(j)*100 : next j
46060   if pen=0 then let pen$="0" else let pen$="1"
46080   if dfc=0 then let dfc$="" else let dfc$="D"
46100   print #hOut,using fRecRW: "RW",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",w2(3),w2(5),w2(1),w2(4),w3(1),w3(2),w2(2),w2(8),dca,dc1,0,0,0,0,0,0,0,0,0,"",w2(6),0,0,0,0,"",0,"",pen$,0,""
46120   fRecRW: form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,19*pic(###########),c 11,5*pic(###########),c 23,n 1,c 1,c 1,n 1,c 23
46140   ! PRINT #hOut,USING 2270: "RO","",W2(9),W2(7),0,0,0,0,0,"","","",0,0,0,0,0,0,0,"",0,0,""
46160   ! form pos 1,c 2,c 9,7*pic(###########),c 176,c 1,c 9,7*pic(###########),c 11,2*pic(###########),c 128
46180 return ! /r
48000 RecRS: ! r: STATE RECORD
48020 ! if sr1=0 then goto 2880 ! NO STATE SELECTED
48040   if s2(1)<>0 or s2(2)<>0 then ! NO STATE WAGES
48060     ! let totrsrecs+=1
48080     print #hOut,using fRecRS: "RS",sr2,"",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","","","",0,0,0,0,0,"","","",sr2,s2(1),s2(2),"","",0,0,"","","",""
48100     fRecRS: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
48120   end if
48140   let t1=t1+1: mat t1=t1+w2
48160   mat i1=i1+w2
48180   mat i2=i2+w3
48200   mat t2=t2+w3
48220   let dc2=dc2+dc1
48240   let dc3=dc3+dc1
48260   let dca2=dca2+dca
48280   let dca3=dca3+dca
48300   let w2=w3=dca=dc1=0
48320   mat w2=(0)
48340   mat w3=(0)
48360 ! let totalstatewages+=s2(1)
48380 ! let totalstatewh+=s2(2)
48400 ! let totalpeople+=1
48420   mat s2=(0)
48440 return ! /r
50000 RecRT: ! r:
50020   print #hOut,using L3050: "RT",tw2,t1(3),t1(5),t1(1),t1(4),t2(1),t2(2),t1(2),t1(8),dca3,dc3,0,0,0,0,0,0,0,0,0,"",t1(6),0,0,0,0,0,""
50040   let dc3=0 ! kj 120805
50060 L3050: form pos 1,c 2,pic(#######),16*pic(###############),3*pic(###############),c 15,6*pic(###############),c 113
50080 ! PRINT #hOut,USING 2520: "RU",TW2,T1(9),T1(7),0,0,0,0,0,"",0,0,0,0,0,0,0,0,0,""
50100 ! form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
50120   let t1=0: mat t1=(0)
50140   mat t2=(0)
50160   return ! /r
52000 ! RecRV: ! r:
52020 !   print #hOut,using L3121: "RV",totalpeople,totalstatewages,totalstatewh," "
52040 !   L3121: form pos 1,c 2,pic(#######),2*pic(###############),c 473
52060 ! return ! /r
54000 RecRF: ! r:
54020   print #hOut,using L3130: "RF"," ",tw1,""
54040   L3130: form pos 1,c 2,c 5,pic(#########),c 496
54060 return ! /r
58000 FINIS: ! r:
58020   gosub RecRT ! kj 22610
58060   gosub RecRF
58080   gosub FILE_SHUFFLE
58100 XIT: let fnxit
58120 ! /r
62000 FILE_SHUFFLE: ! r:
62020   dim a$*512
62040   close #24: ioerr ignore
62060   close #hOut: ioerr ignore
62080   open #24: "Name=X,RecL=513,EOL=NONE,REPLACE",external,output 
62100   open #hOut:=fngethandle: "Name=w2report,RecL=512",display,input 
62120   do 
62140     linput #hOut: a$ eof L3320
62160     if a$(512:512)="X" then let a$(512:512)=""
62180     write #24,using 'form pos 1,c 512,c 1': rpad$(a$,512),chr$(10)
62200   loop 
62220 L3320: close #24: 
62240   close #hOut: 
62260   execute "COPY x "&path$
62280 return ! /r
72000 NameParse: ! r:
72020   dim first$*15,mid$*15,last$*20,em$(3)*30
72040   let em$(1)=uprc$(rtrm$(em$(1))): ! Let nameFormat$="s"
72060   let x1=pos(em$(1)," ",1)
72080   let x2=pos(em$(1)," ",x1+1)
72100   ! let x3=pos(em$(1)," ",x2+1)
72120   if uprc$(nameFormat_sf$(nameFormat))="S" then ! last name first
72140     if x1=0 then let x1=pos(em$(1),",",1)
72160     if x1>0 and em$(1)(x1-1:x1-1)="," then let last$=em$(1)(1:x1-2) else let last$=em$(1)(1:max(x1-1,1))
72180     if x2>0 then let first$=em$(1)(x1+1:x2-1): let mid$=em$(1)(x2+1:len(em$(1)))
72200     if x2=0 then let first$=em$(1)(x1+1:len(em$(1)))(1:15): let mid$=""
72220     let x=pos(first$,",",1): if x>0 then let first$(x:x)=""
72240   else
72260     let first$=em$(1)(1:min(15,max(x1-1,1)))
72280     if x2>0 then let mid$=em$(1)(x1+1:x2-1): let last$=em$(1)(x2+1:len(em$(1)))
72300     if x2=0 then let last$=em$(1)(x1+1:len(em$(1))): let mid$=""
72320   end if
72340   ! Print FIRST$,MID$,LAST$
72360 return ! /r
76000 EXTRACT_STATE: ! r: extract state name
76020   let holdst$="          "
76040   let p3=oldp3=0
76060   let p4=10
76080   for j=1 to 10
76100     let p3=pos(rtrm$(em$(3))," ",p3+1)
76120     if oldp3>p3 then 
76140       goto L5110 ! end of address reached
76160     end if
76180     if p3>0 then 
76200       let oldp3=p3 
76220     end if
76240   next j
76260   L5110: !
76280   for j=1 to 10
76300     if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then
76320       if rtrm$(holdst$)<>"" then 
76340         goto L5150
76360       end if
76380     else 
76400       let p4=p4-1: let holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j)
76420     end if
76440   next j
76460     L5150: !
76480     let holdst$=ltrm$(holdst$)(1:2)
76500   if holdst$="TE" then let holdst$="TX"
76520 return ! /r
78000 ! <Updateable Region: ERTN>
78020 ERTN: let fnerror(program$,err,line,act$,"xit")
78040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
78060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
78080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
78100 ERTN_EXEC_ACT: execute act$ : goto ERTN
78120 ! /region
80000 ! SCREEN1OLD: ! r:
80020 !   dim io1$(18)
80040 !   let io1$(1)="5,25,C 40,UT,N"
80060 !   let io1$(2)="6,25,C 40,UT,N"
80080 !   let io1$(3)="7,25,C 20,UT,N"
80100 !   let io1$(4)="8,25,C 2,UT,N"
80120 !   let io1$(5)="9,25,C 5,UT,N"
80140 !   let io1$(6)="10,25,N 9,UT,N"
80160 !   let io1$(7)="11,25,N 4,UT,N"
80180 !   let io1$(8)="12,25,N 10.2,UT,N"
80200 !   let io1$(9)="13,25,N 10.4,UT,N"
80220 !   let io1$(10)="14,25,N 10.2,UT,N"
80240 !   let io1$(11)="15,25,N 10.4,UT,N"
80260 !   let io1$(12)="16,75,N 2,UT,N"
80280 !   let io1$(13)="17,47,N 2,UT,N"
80300 !   let io1$(14)="18,61,N 2,UT,N"
80320 !   let io1$(15)="19,65,N 2,UT,N"
80340 !   let io1$(16)="20,35,C 8,UT,N"
80360 !   let io1$(17)="21,52,C 1,UT,N"
80380 !   let io1$(18)="22,38,C 1,UT,N"
80400 !   print newpage
80420 !   close #101: ioerr ignore
80440 !   open #101: "SROW=2,SCOL=3,EROW=23,ECOL=80,BORDER=DR,CAPTION=<Create Electronic W2 for I.R.S.",display,outin 
80460 ! ! print #101,fields "3,15,C 51,R,N": "  INSERT DISKETTE FOR ELECTRONIC W2'S IN DRIVE A:"
80480 !   print #101,fields "5,5,C 60": "Company Name:"
80500 !   print #101,fields "6,5,C 60": "Street Address:"
80520 !   print #101,fields "7,5,C 60": "City:"
80540 !   print #101,fields "8,5,C 60": "State:"
80560 !   print #101,fields "9,5,C 60": "Zip Code:"
80580 !   print #101,fields "10,5,C 60": "Federal ID #:"
80600 !   print #101,fields "11,5,C 60": "Payment Year:"
80620 !   print #101,fields "12,5,C 60": "Soc-Sec Maximum:"
80640 !   print #101,fields "13,5,C 60": "Soc-Sec Rate:"
80660 !   print #101,fields "14,5,C 60": "Medicare Maximum:"
80680 !   print #101,fields "15,5,C 60": "Medicare Rate:"
80700 !   print #101,fields "16,5,C 70": "Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:"
80720 !   print #101,fields "17,5,C 70": "Miscellaneous Deduction Used For Pension:"
80740 !   print #101,fields "18,5,C 70": "Miscellaneous Deduction Used For Deferred Compensation:"
80760 !   print #101,fields "19,5,C 70": "Miscellaneous Deduction Used For Dependent Care Assistance:"
80780 !   print #101,fields "20,5,C 60": "Computer Manufacturer's Name:"
80800 !   print #101,fields "21,5,C 60,N": "F=First Name First or S=Surname First on File:"
80820 !   print #101,fields "22,5,C 60": "Type of Business Code R=Regular:"
80840 !   print fields "24,28,C 9,B,1": "Next (F1)"
80860 !   print fields "24,39,C 11,B,5": "Cancel (F5)"
80880 !   print #101,fields mat io1$: a$(1),a$(2),ct$,st$,zip$,federal_id_val,yr,87900,.062,999999,.0145,ins,pen,dfc,dcan,ibm$,nameFormat$,typemp$
80900 ! L1260: rinput #101,fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,federal_id_val,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,nameFormat$,typemp$ conv CONV1
80920 !   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
80940 !   if cmdkey>0 then goto L1350 else let ce=curfld+1
80960 !   if ce>udim(io1$) then let ce=1
80980 ! L1300: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",1)
81000 !   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L1260
81020 ! CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
81040 !   let ce=cnt+1
81060 ! ERR1: print fields "24,78,C 1": bell : goto L1300
81080 ! L1350: ! 
81100 !   if cmdkey=5 then goto XIT
81120 !   if rtrm$(a$(1))="" then let ce=1: goto ERR1
81140 !   if rtrm$(a$(2))="" then let ce=2: goto ERR1
81160 !   if rtrm$(ct$)="" then let ce=3: goto ERR1
81180 !   if rtrm$(st$)="" then let ce=4: goto ERR1
81200 !   if rtrm$(zip$)="" then let ce=5: goto ERR1
81220 !   if federal_id_val=0 then let ce=6: goto ERR1
81240 !   if yr<2001 then let ce=7: goto ERR1
81260 ! ! let ficarate=ssrate+mcrate
81280 !   if ssmax<53400 then let ce=8: goto ERR1
81300 !   if ins<0 or ins>10 then let ce=9: goto ERR1
81320 !   if pen<0 or pen>10 then let ce=10: goto ERR1
81340 !   if dfc<0 or dfc>10 then let ce=11: goto ERR1
81360  !goto screen2 ! /r