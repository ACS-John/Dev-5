00010 ! formerly S:\acsPR\newprFM
00020 ! Payroll Employee File
10000 ! r: setup and open files
10020   let fn_setup
10040   let fntop(program$)
10060   open #1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
10080   open #11: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndx2.h"&env$('cno')&",Shr",internal,outin,keyed 
10100   if ~exists(env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')) then gosub SETUP_PAYROLLCHECKS
10120   if ~exists(env$('Q')&"\PRmstr\checkidx3.h"&env$('cno')&",Shr") then gosub INDEX_CHECKIDX3
10140   open #h_checkhistory:=4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",Shr",internal,outin,keyed 
10160   open #44: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx3.h"&env$('cno')&",Shr",internal,outin,keyed 
10180   if ~exists(env$('Q')&"\PRmstr\Department.h"&env$('cno')) then ! SETUP_DEPARTMENT
10200     open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",RecL=149,kps=1/9,kln=8/3,use",internal,outin,keyed 
10220     close #2: 
10240     execute "Index "&env$('Q')&"\PRmstr\Department.h"&env$('cno')&' '&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&" 1/9 8/3 Replace DupKeys -n"
10260   end if 
10280   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
10300   goto MENU1
10320 ! /r
12000 MENU1: let ndep=0 : goto ASKEMPLOYEE
14000 ASKEMPLOYEE: ! r:
14020   ad1=0 ! add code - used to tell other parts of the program, that I am currently adding an employee record.
14040   let fntos(sn$="Employee-ask")
14060   let respc=0
14080   let fnlbl(1,1,"Employee Number:",16,right)
14100   let fncmbemp(1,18)
14120   if hact$="" then 
14140     let resp$(respc+=1)=""
14160   else 
14180     let resp$(respc+=1)=hact$
14200   end if 
14220   let fncmdkey("&Add",1,0,0,"Add a new employee" )
14240   let fncmdkey("E&dit",2,1,0,"Access the highlighted record")
14260   let fncmdkey("&Next Record",3,0,0,"Access next record in employee # order")
14280   let fncmdkey("&Search",8,0,0,"Search for employee record")
14300   let fncmdkey("&Refresh",7,0,0,"Updates search grids and combo boxes with new employee information")
14320   let fncmdkey("E&xit",6,0,1,"Returns to menu")
14340   let fnacs(sn$,0,mat resp$,ckey) ! ask employee #
14360   let hact$=resp$(1)(1:8)
14380   let eno=ent=val(resp$(1)(1:8))
14400   if ckey=1 then 
14420     let ti1=ad1=1
14440     goto ADDREC
14460   else if ckey=2 then 
14480     goto EDITREC
14500   else if ckey=3 then 
14520     read #1,using L870: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd eof L1120
14540 L870: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
14560     let holdeno=eno
14580     let ent$=lpad$(str$(eno),8)
14600     goto SCR_EMPLOYEE
14620   else if ckey=4 then 
14640     let ent=eno
14660     goto EDITREC
14680   else if ckey=8 then 
14700     let fnemployee_srch(x$,fixgrid)
14720     let ent=val(x$)
14740     goto EDITREC
14760   else if ckey=6 or env$('ExitNow')='yes' then ! Added ExitNow env$ by GSB to ensure program recursively exits when they click the Windows X in a Subwindow
14780     goto XIT
14800   else if ckey=7 then 
14840     goto ASKEMPLOYEE
14860   end if 
14880   goto ADDREC ! /r
16000 ADDREC: ! r:
16020   let fntos(sn$="Employeefm")
16040   let respc=0 : let frac=0
16060   let mylen=25 : let mypos=mylen+2
16080   let fnlbl(1,1,"Employee Number:",mylen,1)
16100   let fntxt(1,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
16120   let resp$(respc+=1)=str$(eno)
16140   let fncmdkey("&Next",1,1,0,"Process employee information.")
16160   let fncmdkey("&Cancel",5,0,1,"Returns to maintenance screem.")
16180   let fnacs(sn$,0,mat resp$,ckey)
16200   if ckey=5 then goto ASKEMPLOYEE
16220   add1=1
16240   let ent=val(resp$(1))
16260   let ent$=lpad$(str$(ent),8)
16280   read #1,using F_RPMSTR,key=ent$: tempeno nokey L1020
16300   mat ml$(2)
16320   let ml$(1)="A record with this number already exists!"
16340   let ml$(2)="Select a different employee number."
16360   let fnmsgbox(mat ml$,resp$,'',48)
16380   goto ADDREC ! /r
18000 L1020: ! r:
18020   mat em$=("")
18040   let ph$=ss$=""
18060   bd=0
18080   mat rs=(0)
18100   mat em=(0)
18120   let lpd=tgp=0
18140   mat ta=(0)
18160   mat ty=(0)
18180   mat tqm=(0)
18200   mat tcp=(0)
18220   mat sc=(0)
18240   let eno=teno=holdeno=ent
18260 ! let holdem$=em$(1)
18280   let dd$="N"
18300 ! clear bank draft
18320   let rtn=0
18340   acc=0
18360   acn=0
18380   goto SCR_EMPLOYEE ! /r
20000 EDITREC: ! r:
20020   if ent=0 then goto ASKEMPLOYEE
20040   let teno=eno=ent ! let hdar=0
20060   let ent$=lpad$(str$(ent),8)
20080   read #1,using F_RPMSTR,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd nokey L1120
20100   let holdeno=eno
20120   goto SCR_EMPLOYEE ! /r
22000 L1120: ! r:
22020   mat ml$(2)
22040   let ml$(1)="A record with this number does not exist!"
22060   let ml$(2)="Select a different employee number."
22080   let fnmsgbox(mat ml$,resp$,'',48)
22100   goto ASKEMPLOYEE ! /r
26000 SCR_EMPLOYEE: ! r:
26020   let fntos(sn$="Employeeedit")
26040   let respc=0 : let frac=0 ! 
26060   let mylen=28 : let mypos=mylen+2
26080   let fnlbl(1,1,"Employee Number:",mylen,1)
26100   let fntxt(1,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
26120   let resp$(respc+=1)=str$(eno)
26140   let fnlbl(2,1,"Name:",mylen,1)
26160   let fntxt(2,mylen+3,30,30,0,"",0,"Name can be entered first name first or last name first.")
26180   let resp$(respc+=1)=em$(1)
26200   let fnlbl(3,1,"Address:",mylen,1)
26220   let fntxt(3,mylen+3,30,30,0,"",0,"")
26240   let resp$(respc+=1)=em$(2)
26260   let fnlbl(4,1,"City, State Zip:",mylen,1)
26280   let fntxt(4,mylen+3,30,30,0,"",0,"")
26300   let resp$(respc+=1)=em$(3)
26320   let fnlbl(5,1,"Social Security #:",mylen,1)
26340   let fntxt(5,mylen+3,11,11,0,"",0,"")
26360   let resp$(respc+=1)=ss$
26380   let fnlbl(6,1,"Race:",mylen,1)
26400   let respc+=1: for j=1 to udim(race_option$)
26420     if rs(1)=val(race_option$(j)(1:1)) then let resp$(respc)=race_option$(j)
26440   next j
26460   let fncomboa("Race",6,mylen+3,mat race_option$,"",16)
26480   let fnlbl(7,1,"Sex:",mylen,1)
26500   let respc+=1: for j=1 to udim(gender_option$)
26520     if rs(2)=val(gender_option$(j)(1:1)) then let resp$(respc)=gender_option$(j)
26540   next j
26560   let fncomboa("Sex",7,mylen+3,mat gender_option$,"",10)
26580   let fnlbl(8,1,"Marital Status:",mylen,1)
26600   let respc+=1: for j=1 to udim(married_option$)
26620     if em(1)=val(married_option$(j)(1:1)) then let resp$(respc)=married_option$(j)
26640   next j
26660   let fncomboa("Marital",8,mylen+3,mat married_option$) ! ,"",11)
26680   let fnlbl(9,1,"Federal Exemptions:",mylen,1)
26700   let respc+=1
26720   for j=1 to udim(fed_exemption_option$)
26740     if em(2)=val(fed_exemption_option$(j)(1:2)) then let resp$(respc)=fed_exemption_option$(j)
26760   next j
26780   let fncomboa("FedEx",9,mylen+3,mat fed_exemption_option$,"",3)
26800   let fnlbl(10,1,"State Exemptions:",mylen,1)
26820   let respc+=1
26840   for j=1 to udim(fed_exemption_option$)
26860     if em(3)=val(fed_exemption_option$(j)(1:2)) then let resp$(respc)=fed_exemption_option$(j)
26880   next j
26900   let fncomboa("StateEx",10,mylen+3,mat fed_exemption_option$,"",3)
26920   let fnlbl(11,1,"Employment Status:",mylen,1)
26940   let fncombof("EmpStatus",11,mylen+3,25,env$('Q')&"\PRmstr\EmpStatus.dat",1,2,3,25,env$('Q')&"\PRmstr\EmpStatus.idx",0,0, " ",fracustinfo,0)
26960   let resp$(respc+=1)=str$(em(4))
26980   let fnlbl(12,1,"Pay Code:",mylen,1)
27000   let respc+=1
27020   for j=1 to udim(payperiod_option$)
27040     if em(5)=val(payperiod_option$(j)(1:1)) then let resp$(respc)=payperiod_option$(j)
27060   next j
27080   let fncomboa("PayCode",12,mylen+3,mat payperiod_option$,"",16)
27100   let fnlbl(13,1,"FICA Code:",mylen,1)
27120   let respc+=1: for j=1 to udim(code6$)
27140     if em(6)=val(code6$(j)(1:1)) then let resp$(respc)=code6$(j)
27160   next j
27180   let fncomboa("FICACode",13,mylen+3,mat code6$,"",32)
27200   let fnlbl(14,1,"EIC Code:",mylen,1)
27220   let fncomboa("EICCode",14,mylen+3,mat code7$,"",31)
27240   let resp$(respc+=1)=code7$(em(7)+1)
27260   let fnlbl(15,1,"Sick Pay Code:",mylen,1)
27280   let fntxt(15,mylen+3,6,6,0,"33",0,"Normally is number of sick hours you want accrued each pay period.")
27300   let resp$(respc+=1)=str$(em(8))
27320   let fnlbl(16,1,"Vacation Pay Code:",mylen,1)
27340   let fntxt(16,mylen+3,6,6,0,"33",0,"Normally is number of vacation hours you want accrued each pay period.")
27360   let resp$(respc+=1)=str$(em(9))
27380   let fnlbl(17,1,"Sick Hours Accrued:",mylen,1)
27400   let fntxt(17,mylen+3,10,10,0,"32",0,"This should be the balance of sick hours available at this time.")
27420   let resp$(respc+=1)=str$(em(10))
27440   let fnlbl(18,1,"Vacation Hours Accrued:",mylen,1)
27460   let fntxt(18,mylen+3,10,10,0,"32",0,"This should be the balance of vacation hours available at this time.")
27480   let resp$(respc+=1)=str$(em(11))
27500   let fnlbl(19,1,"Standard Federal W/H:",mylen,1)
27520   let fntxt(19,mylen+3,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of Federal withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip Federal withholdings on this employee.")
27540   let resp$(respc+=1)=str$(em(12))
27560   col3_pos=51 : col3_len=20
27580   let fnlbl(19,col3_pos,"Federal Tax Add-On:",col3_len,1)
27600   let fntxt(19,73,10,10,0,"32",0,"If you wish for the system to add additional Federal withholdings, enter that amount here.")
27620   let resp$(respc+=1)=str$(em(13))
27640   let fnlbl(20,1,"Standard State W/H:",mylen,1)
27660   let fntxt(20,mylen+3,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of State withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip state withholdings on this employee.")
27680   let resp$(respc+=1)=str$(em(14))
27700   let fnlbl(20,col3_pos,"State Tax Add-On:",col3_len,1)
27720   let fntxt(20,73,10,10,0,"32",0,"If you wish for the system to add additional state withholdings, enter that amount here.")
27740   let resp$(respc+=1)=str$(em(15))
27760   let fnlbl(21,1,"Date Hired:",mylen,1)
27780   let fntxt(21,mylen+3,10,10,0,"1",0,"The date hired is only used for information purposes only.")
27800   let resp$(respc+=1)=str$(em(16))
27820   let fnlbl(21,col3_pos,"Last Payroll Date:",col3_len,1)
27840   let fntxt(21,73,10,10,0,"1",0,"This will always be the last time pay was calculated on this employee.")
27860   let resp$(respc+=1)=str$(lpd)
27880   let fnlbl(22,1,"Birth Date:",mylen,1)
27900   let fntxt(22,mylen+3,10,10,0,"1",0,"The birth date is not required.")
27920   let resp$(respc+=1)=str$(bd)
27940   let fnlbl(22,col3_pos,"Phone Number:",col3_len,1)
27960   let fntxt(22,73,12,12,0,"",0,"")
27980   let resp$(respc+=1)=ph$
28000 ! let picture=0
28020   let fncmdkey("&Departments",2,0,0,"Allows you to review departmental information.")
28040   let fncmdkey("Direct D&eposit",7,0,0,"Review direct deposit information.")
28060   let fncmdkey("Re&view Special Hrs",8,0,0,"Review miscellaneous breakdown of hours.")
28080   let fncmdkey("&Review Checks",10,0,0,"Review check information.")
28100 ! let fncmdkey("&Picture",6,0,0,"Place any picture in share\images.")
28120   if ad1=0 then 
28140     let fncmdkey("De&lete",4,0,0,"Deletes this record")
28160   end if 
28180   let fncmdkey("&Save",1,1,0,"Saves all changes.")
28200   let fncmdkey("&Cancel",5,0,1,"Stops without applying any changes.")
28220   let fnacs(sn$,0,mat resp$,ckey)
29000   if ckey=5 then goto ASKEMPLOYEE
29020   let eno=val(resp$(1)(1:8))
29040   let em$(1)=resp$(2) ! name
29060   let em$(2)=resp$(3)
29080   let em$(3)=resp$(4)
29100   let ss$=resp$(5)
29120   let rs(1)=val(resp$(6)(1:1))
29140   let rs(2)=val(resp$(7)(1:1)) ! sex
29160   let em(1)=val(resp$(8)(1:1)) ! marital status
29180   let em(2)=val(resp$(9)(1:2)) ! fed ex
29200   let em(3)=val(resp$(10)(1:2)) ! state ex
29220   let em(4)=val(resp$(11)(1:2)) ! emp status
29240   let em(5)=val(resp$(12)(1:2)) ! pay code
29260   let em(6)=val(resp$(13)(1:2)) ! fica code
29280   let em(7)=val(resp$(14)(1:2)) ! eic code
29300   let em(8)=val(resp$(15)(1:5)) ! sick pay
29320   let em(9)=val(resp$(16)) ! vacation Pay code
29340   let em(10)=val(resp$(17)) ! sick accrued
29360   let em(11)=val(resp$(18)) ! vac accrued
29380   let em(12)=val(resp$(19)) ! std fed
29400   let em(13)=val(resp$(20)) ! fed addon
29420   let em(14)=val(resp$(21)) ! std state
29440   let em(15)=val(resp$(22)) ! state addon
29460   let em(16)=val(resp$(23)) ! date hired
29480   let lpd=val(resp$(24)) ! last payroll date
29500   bd=val(resp$(25)) ! birth date
29520   let ph$=resp$(26) ! phone
29540   ! if ckey=6 then goto PICTURE
29560   if ckey=8 then let fnhours(eno): goto SCR_EMPLOYEE
29580   if ckey=7 then gosub DD: goto EDITREC
29600   if ckey=10 then goto CHECK_INFORMATION ! check information
29620   if ckey=1 or ckey=2 then 
29640     if em(5)=0 then ! pay code not selected
29660       mat ml$(1) : let ml$(1)='Pay Code is required.'
29680       let fnmsgbox(mat ml$,resp$)
29700       goto SCR_EMPLOYEE
29720     end if 
29740     goto REWRITE_MAIN_FILE
29760   end if 
29780   if ckey=4 then goto DELETE_EMPLOYEE
29800   goto REVIEW_DEPARTMENT
29820 ! /r
30000 REVIEW_DEPARTMENT: ! r:
30020   let firstread=1
30040   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey DEPARTMENT_ADD
30060 L2400: ! 
30080   read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof SCR_EMPLOYEE
30100   if firstread=1 and teno<>eno then goto DEPARTMENT_ADD
30120   if teno<>eno then goto SCR_EMPLOYEE
30140 SCR_DEPARTMENT: ! 
30160   let fntos(sn$="EmployeeDep")
30180   let respc=0 : let fram1=1
30200   let mylen=20 : let mypos=mylen+2 : mat resp$=("")
30220   let fnfra(1,1,6,97,"Departmental Information - "&trim$(em$(1)))
30240   let fnlbl(1,1,"Employee Number:",mylen,1,0,fram1)
30260   let fntxt(1,mylen+3,8,8,1,"1030",0,"Employee numbers must be numeric.",fram1)
30280   let resp$(respc+=1)=str$(eno)
30300   let fnlbl(2,1,"Department Number:",mylen,1,0,fram1)
30320   let fntxt(2,mylen+3,3,3,1,"30",0,"Department numbers must be numeric and no department # can be used twice on the same employee.",fram1)
30340   let resp$(respc+=1)=str$(tdn)
30360   let fnlbl(2,35,"General Ledger #:",mylen,1,0,fram1)
30380   let fnqgl(2,58,fram1)
30400   let resp$(respc+=1)=fnrgl$(gl$)
30420   let fnlbl(3,1,"Last Review Date:",mylen,1,0,fram1)
30440   let fntxt(3,mylen+3,8,8,1,"1",0,"Last review is only used for information purposes.  Use MMDDYY format.",fram1)
30460   let resp$(respc+=1)=str$(tdt(1))
30480   let fnlbl(3,35,"Next Review Date:",mylen,1,0,fram1)
30500   let fntxt(3,58,8,8,1,"1",0,"Next review date is only used for information purposes.  Use MMDDYY format.",fram1)
30520   let resp$(respc+=1)=str$(tdt(2))
30540   let fnlbl(4,1,"Last Increase Date:",mylen,1,0,fram1)
30560   let fntxt(4,mylen+3,8,8,1,"1",0,"Last increase date is only used for information purposes.  Use MMDDYY format.",fram1)
30580   let resp$(respc+=1)=str$(tdt(3))
30600   let fnlbl(4,35,"Last Increase Amt:",mylen,1,0,fram1)
30620   let fntxt(4,58,12,12,1,"10",0,"Just a method of storing the amount of the last pay increase.  You must enter by hand.",fram1)
30640   let resp$(respc+=1)=str$(tli)
30660   let fnlbl(5,1,"Last Payroll Date:",mylen,1,0,fram1)
30680   let fntxt(5,mylen+3,8,8,1,"1",0,"Last payroll date is updated each time pay is calculated.  Use MMDDYY format.  Do not change this date.",fram1)
30700   let resp$(respc+=1)=str$(tdt(4))
30720   let fnlbl(5,35,"State Code:",mylen,1,0,fram1)
30740 ! Let FNTXT(5,58,2,2,1,"30",0,"You must enter a state code, even if you have no state withholdings.",FRAM1)
30760   let fncomboa("StateCode",5,58,mat state_option$,"",11,fram1)
30780   if tcd(1)=0 or tcd(1)>10 then let tcd(1)=1 ! default state code to 1
30800   let resp$(respc+=1)=state_option$(tcd(1))
30820   let fnlbl(6,1,"Workmans Code:",mylen,1,0,fram1)
30840   let fntxt(6,mylen+3,2,2,1,"30",0,"You workmans comp code is used for grouping certain types of work on the workmans comp report.",fram1)
30860   let resp$(respc+=1)=str$(tcd(2))
30880   let fnlbl(6,35,"Union Code:",mylen,1,0,fram1)
30900   let fntxt(6,58,2,2,1,"30",0,"You union code is used for grouping employees for the union report.",fram1)
30920   let resp$(respc+=1)=str$(tcd(3))
30940   let fram2=2: let fnfra(9,1,3,97,"Salary and Pay Rates")
30960   let fnlbl(1,1,"Salary:",mylen,1,0,fram2)
30980   let fntxt(1,mylen+3,12,12,1,"10",0,"Enter the salary for the pay period.",fram2)
31000   let resp$(respc+=1)=str$(tdet(1))
31020   let fnlbl(2,1,"Regular Hourly Rate:",mylen,1,0,fram2)
31040   let fntxt(2,mylen+3,12,12,1,"10",0,"Enter the regular hourly rate.",fram2)
31060   let resp$(respc+=1)=str$(tdet(2))
31080   let fnlbl(2,35,"O/T Hourly Rate:",mylen,1,0,fram2)
31100   let fntxt(2,58,12,12,1,"10",0,"Enter the overtime hourly rate.",fram2)
31120   let resp$(respc+=1)=str$(tdet(3))
31140   let fram3=3: let fnfra(14,1,10,97,"Deductions and Additions")
31160   for j=1 to 10
31180     let fnlbl(j,1,dednames$(j*2-1),mylen,1,0,fram3)
31200     let fntxt(j,mylen+3,12,12,1,"10",0,"Enter the standard amount or the percent.",fram3)
31220     let resp$(respc+=1)=str$(tdet(j*2-1+3))
31240     let fnlbl(j,35,dednames$(j*2),mylen,1,0,fram3)
31260     let fntxt(j,58,12,12,1,"10",0,"Enter the standard amount or the percent.",fram3)
31280     let resp$(respc+=1)=str$(tdet(j*2+3))
31300   next j
31320   let fncmdkey("&Next Record",3,1,0,"Save any changes and access next departmental record.")
31340   let fncmdkey("&Add Department",4,0,0,"Add an additional department record.")
31360   let fncmdkey("&Review Checks",10,0,0,"Review check information.")
31380   let fncmdkey("&Delete",9,0,0,"Deletes the department record.")
31400   let fncmdkey("C&omplete",1,0,0,"Saves any changes and returns to main screen.")
31420   let fncmdkey("&Cancel",5,0,1,"Exit departmental record without saving changes.")
31440   let fnacs(sn$,0,mat resp$,ckey)
32000   if ckey=5 then goto ASKEMPLOYEE
32020   let teno=val(resp$(1)) ! employee # in dept record
32040   let tdn=val(resp$(2)) ! department #
32060   if ckey=9 then delete #2,key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn): : goto EDITREC
32080   if resp$(3)="combos" then let resp$(3)=""
32100   let gl$=fnagl$(resp$(3))
32120   let tdt(1)=val(resp$(4)) ! last review date
32140   let tdt(2)=val(resp$(5)) ! next review date
32160   let tdt(3)=val(resp$(6)) ! last increase date
32180   let tli=val(resp$(7)) ! last increase amount
32200   let tdt(4)=val(resp$(8)) ! last payroll date
32220   for j=1 to 10
32240     if uprc$(trim$(resp$(9)(1:2)))=uprc$(trim$(state_option$(j)(1:2))) then let tcd(1)=j ! state code
32260   next j
32280   let tcd(2)=val(resp$(10)) ! w/c code
32300   let tcd(3)=val(resp$(11)) ! union code
32320   let tdet(1)=val(resp$(12)) ! salary
32340   let tdet(2)=val(resp$(13)) ! hourly rate
32360   let tdet(3)=val(resp$(14)) ! overtime rate
32380   for j=4 to 23
32400     let tdet(j)=val(resp$(14+j-3)) ! standard deductions
32420   next j
32440   if tdn=0 then 
32460     mat ml$(2)
32480     let ml$(1)="The department # can not be 0 (zero)."
32500     let ml$(2)="Enter a valid department number!"
32520     let fnmsgbox(mat ml$,resp$)
32540     goto REVIEW_DEPARTMENT
32560   end if 
32580   if ndep<>0 then ! if ndep=0 then goto L3160
32600     write #2,using 'Form POS 1,N 8,N 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',reserve: eno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet ! Duprec 3140
32620   else 
32640     if eno<>ent then goto CHGENO
32660     rewrite #2,using "Form POS 1,N 8,N 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2": teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet
32680   end if 
32700   let firstread=0
32720   let ndep=0
32740   if ckey=4 then goto DEPARTMENT_ADD ! add new department
32760   if ckey=3 then goto L2400 ! move to next departmental record
32780   if ckey=1 then goto EDITREC
32800   if ckey=10 then goto CHECK_INFORMATION
32820   goto ASKEMPLOYEE
32840 ! /r
34000 DEPARTMENT_ADD: ! r: new department
34020   let ndep=1
34040   let tdn=0
34060   let gl$=""
34080   mat tdt=(0)
34100   mat tcd=(0)
34120   let tli=0
34140   mat tdet=(0)
34160   let firstread=0
34180   goto SCR_DEPARTMENT ! /r
36000 ! r: unreferenced code
36020   if ti1=2 then goto L3370
36040   write #1,using F_RPMSTR: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
36060   let r1=r2=1 ! r4=
36080   goto MENU1
36100 ! /r
38000 REWRITE_MAIN_FILE: ! r:
38020   if add1=1 then goto L3390
38040   if holdeno<>eno then goto CHGENO
38060 L3370: ! 
38080   rewrite #1,using F_RPMSTR,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
38100   goto L3420
38120 L3390: ! 
38140   write #1,using F_RPMSTR: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
38160   add1=0
38200 L3420: ! 
38220   if ckey=2 then goto REVIEW_DEPARTMENT
38240   goto MENU1 ! /r
38260 ! DONE: ! r:
38280   close #1: 
38300   close #2: 
38320   if r1=1 then 
38340     execute "Index "&env$('Q')&"\PRmstr\RPMSTR.H"&env$('cno')&' '&env$('Q')&"\PRmstr\RPINDEX.H"&env$('cno')&" 1,8 Replace DupKeys -n"
38360     execute "Index "&env$('Q')&"\PRmstr\dd.H"&env$('cno')&' '&env$('Q')&"\PRmstr\ddidx1.H"&env$('cno')&" 1,10 Replace DupKeys -n"
38380   end if 
38400   if r2=1 then 
38420     execute "Index "&env$('Q')&"\PRmstr\RPMSTR.H"&env$('cno')&' '&env$('Q')&"\PRmstr\RPINDX2.H"&env$('cno')&" 9 30 Replace DupKeys -n"
38440   end if 
38460   goto XIT ! /r
42000 DELETE_EMPLOYEE: ! r:
42020   mat ml$(2)
42040   let ml$(1)="Employee Number "&ltrm$(ent$)&" will be Deleted."
42060   let ml$(2)="Do you wish to continue?"
42080   let fnmsgbox(mat ml$,resp$,'',52)
42100   if resp$="Yes" then goto L3540 else goto MENU1
42120 L3540: ! delete direct deposit
42140   gosub DDDEL ! uses ent$
42160   delete #1,key=ent$: 
42200 ! delete departmental records
42220   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": 
42240 L3580: read #2,using 'Form POS 1,N 8': teno eof ASKEMPLOYEE
42260   if teno<>eno then goto L3620
42280   delete #2: 
42300   goto L3580
42320 L3620: ! delete check transactions
42340   let heno$=lpad$(str$(eno),8)
42360   restore #h_checkhistory,key>=heno$&"         ": nokey L3700
42380 L3650: read #h_checkhistory,using 'form pos 1,n 8': histeno eof L3700
42400 !   form pos 1,n 8
42420   if histeno<>eno then goto L3700
42440   delete #h_checkhistory: 
42460   goto L3650
42480 L3700: ! 
42500   goto MENU1 ! /r
44000 ! r: unused: employee number will be changed (old) messagebox
44020   let msgline$(1)="Employee Number "&ltrm$(ent$)&" will be changed "
44040   let msgline$(1)=msgline$(1)&"to "&str$(eno)&"."
44060   let msgline$(2)="Do you wish to continue?"
44080   let fnoldmsgbox(mat response$,'',mat msgline$,2)
44100   if response$(1)="Y" then 
44120     goto CHGENO
44140   else 
44160     goto MENU1
44180   end if  ! /r
46000 CHGENO: ! r:
46020   mat ml$(3)
46040   let ml$(1)="You have chosen to change the employee number"
46060   let ml$(2)="from "&str$(holdeno)&" to "&str$(eno)&"."
46080   let ml$(3)="Do you wish to continue?"
46100   let fnmsgbox(mat ml$,resp$,'',52)
46120   if resp$<>"Yes" then 
46180     goto CHGENO_XIT
46200   end if 
46240   read #1,using 'form pos 1,n 8',key=lpad$(str$(eno),8): teno nokey L3790
46260   mat ml$(2)
46280   let ml$(1)="Employee Number "&ltrm$(ent$)&" already exists."
46300   let ml$(2)="You cannot change to this number."
46320   let fnmsgbox(mat ml$,resp$)
46340   goto CHGENO_XIT
46360   L3790: ! change direct deposit
46380   gosub DDCHGKEY ! from ent$ to eno
46400   ! CHANGE DEPARTMENTS NUMBERS
46420   let heno$=lpad$(str$(holdeno),8) ! &"   "
46440   restore #2,key>=rpad$(heno$,kln(2)): nokey L3890
46460   do 
46480     read #2,using 'Form POS 1,N 8': deno eof L3890
46500     if deno<>holdeno then goto L3890
46520     rewrite #2,using 'form pos 1,n 8',rec=rec(2): eno
46540   loop 
46560   L3890: ! pause
46580   let fnkey_change(h_checkhistory,'form pos 1,n 8',heno$,lpad$(str$(eno),8)) ! change employee number in check history
46720   ! r: change employee number in any and all rpwork files.
46740   for wsid_item=1 to 99
46760     let wsid_item$=cnvrt$('pic(##)',wsid_item)
46780     if exists(env$('Q')&'\PRmstr\rpwork'&wsid_item$&'.h'&env$('cno')) then 
46800       open #h_rpwork:=fngethandle: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid_item$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid_item$&"idx.H"&env$('cno')&',shr',internal,outin,keyed ioerr RPWORK_OPEN_ERR
46820       let fnkey_change(h_rpwork,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
46840       close #h_rpwork: 
46860       RPWORK_OPEN_ERR: ! 
46880     end if 
46900   next wsid_item
46920   ! /r
46940   ! L3980: ! change main employee record
46960   delete #1,key=ent$: 
46980   write #1,using F_RPMSTR: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
47000   let ent$=lpad$(str$(eno),8)
47020   let hact$=ent$
47060   CHGENO_XIT: ! 
47080 goto MENU1 ! /r
48000 def fn_setup
48020   library 'S:\Core\Library': fntop,fnxit, fnoldmsgbox,fnerror,fnhours,fntos,fnlbl,fncmbemp,fncmdkey,fnacs,fncombof,fntxt,fnmsgbox,fncomboa,fnpic,fnfra,fnrgl$,fnqgl,fnagl$,fncheckfile,fnemployee_srch,fngethandle,fnkey_change,fnDedNames,fnaddonec
48040   on error goto ERTN
48060   ! on fkey 5 goto MENU1
48080   ! ______________________________________________________________________
48100   dim ph$*12
48120   dim response$(5)*1,msgline$(2)*60,resp$(50)*128
48140   dim ty(21),tqm(17),tcp(22),em(16),ta(2)
48160   dim tdt(4),tcd(3),tdet(23),ss$*11,rs(2)
48180   dim em$(3)*30
48200   dim code8$(2)*22
48220   dim code9$(2)*33,ml$(2)*80
48240   ! 
48260   dim race_option$(7)*15
48280   let race_option$(1)="0 - Unknown"
48300   let race_option$(2)="1 - Caucasian"
48320   let race_option$(3)="2 - Hispanic"
48340   let race_option$(4)="3 - Black"
48360   let race_option$(5)="4 - Oriental"
48380   let race_option$(6)="5 - AmIndian"
48400   let race_option$(7)="6 - Indochines"
48420   ! 
48440   dim gender_option$(3)*11
48460   let gender_option$(1)="0 - Unknown"
48480   let gender_option$(2)="1 - Male"
48500   let gender_option$(3)="2 - Female"
48520   ! 
48530   dim married_option$(0)*58
48540   mat married_option$(0)
48542   fnaddonec(mat married_option$,"0 - Single")
48544   fnaddonec(mat married_option$,"1 - Married")
48545   fnaddonec(mat married_option$,'2 - Single - Head of Household')
48546   fnaddonec(mat married_option$,'3 - Married - filing joint - only one working')
48548   fnaddonec(mat married_option$,'4 - Married - filing joint - both working')
48550   fnaddonec(mat married_option$,'5 - Married - filing seperate - both working')
48600  ! 
48620   dim fed_exemption_option$(22)
48640   for j=1 to 21
48660     let fed_exemption_option$(j)=str$(j-1)
48680   next j
48700   let fed_exemption_option$(22)="99"
48720   ! 
48740   dim payperiod_option$(4)
48760   let payperiod_option$(1)="1 - Monthly"
48780   let payperiod_option$(2)="2 - Semi-monthly"
48800   let payperiod_option$(3)="3 - Bi-weekly"
48820   let payperiod_option$(4)="4 - Weekly"
48840   ! 
48860   dim code6$(4)*28
48880   code6$(1)="0 - Subject to SS and Med WH"
48900   code6$(2)="1 - SS only"
48920   code6$(3)="2 - Medicare Only"
48940   code6$(4)="9 - Neither SS nor Medicare"
48960   ! 
48980   dim code7$(3)*29
49000   code7$(1)="0 - Not qualified for EIC"       !  em(7)=1
49020   code7$(2)="1 - Single or Spouse not file"   !  em(7)=2
49040   code7$(3)="2 - Married both filing"         !  em(7)=3
49060   ! 
49120   dim statenames$(10)*8
49140   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,outin,relative 
49160   read #1,using "form pos 150,10*c 8",rec=1: mat statenames$
49180   close #1: 
49200   dim state_option$(10)*11
49220   for j=1 to 10: let state_option$(j)=cnvrt$("Pic(z#)",j)&" "&statenames$(j): next j
49240   ! 
49260   dim dednames$(20)*20
49280   fnDedNames(mat dednames$)
49340   L430: ! 
49360   for j=1 to 20
49380     if trim$(dednames$(j))<>"" then 
49400       let dednames$(j)=trim$(dednames$(j))&":"
49420     end if 
49440   next j
49460 fnend 
50000 F_RPMSTR: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
52000 XIT: let fnxit
54000 IGNORE: continue 
56000 ! <Updateable Region: ERTN>
56020 ERTN: let fnerror(program$,err,line,act$,"xit")
56040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
56060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
56080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56100 ERTN_EXEC_ACT: execute act$ : goto ERTN
56120 ! /region
58000 DDREADNOKEY: ! r:
58020   let dd$='N' : let rtn=acc=acn=0 ! defaults
58040   write #30,using "Form pos 1,C 10,C 1,N 9,N 2,N 17": key$,dd$,rtn,acc,acn nokey DDREADNOKEY
58060   goto ASKDD ! /r
60000 DDKEY: ! r:
60020   let key$=rpad$(str$(eno),10)
60040   return  ! /r
62000 DD: ! r:
62020   gosub DDKEY
62040   gosub DDOPEN
62060   read #30,using "Form pos 11,C 1,N 9,N 2,N 17",key=key$: dd$,rtn,acc,acn nokey DDREADNOKEY
62080 ASKDD: ! 
62100   let fntos(sn$="DirectDeposit")
62120   let respc=0: let mylen=35 : let right=1
62140   let fnlbl(1,1,"Employee #:",mylen,right)
62160   let fntxt(1,mylen+3,8,8,1,"",0,"")
62180   let resp$(respc+=1)=str$(eno)
62200   let fnlbl(2,1,"Direct Deposit:",mylen,right)
62220   code9$(1)="Y = Activate Direct Deposit"
62240   code9$(2)="N = Direct Deposit not activated."
62260   let respc+=1: for j=1 to udim(code9$)
62280     if dd$=code9$(j)(1:1) then let resp$(respc)=code9$(j)
62300   next j
62320   let fncomboa("Directd",2,mylen+3,mat code9$,"",35)
62340   let fnlbl(3,1,"Routing Number:",mylen,right)
62360   let fntxt(3,mylen+3,9,9,1,"",0,"Employee's bank's routing #. The bank account and the routing # can be found at the bottom of the employees personal check.")
62380   let resp$(respc+=1)=str$(rtn)
62400   code8$(1)="27 = Regular Checking"
62420   code8$(2)="37 = Savings Account"
62440   let respc+=1: for j=1 to udim(code8$)
62460     if acc=val(code8$(j)(1:2)) then let resp$(respc)=code8$(j)
62480   next j
62500   let fnlbl(4,1,"Account Type:",mylen,right)
62520   let fncomboa("AccType",4,mylen+3,mat code8$,"",35)
62540   let fnlbl(5,1,"Employee Bank Account #:",mylen,right)
62560   let fntxt(5,mylen+3,17,17,1,"30",0,"Enter the employee's bank account #. ")
62580   let resp$(respc+=1)=str$(acn)
62600   let fncmdkey("&Save",1,1,0,"Saves the information on the screen." )
62620   let fncmdkey("&Delete",4,0,0,"Deletes the direct deposit information on this employee.You can stop direct deposits simply by changing the direct deposit question to no.")
62640   let fncmdkey("&Cancel",5,0,1,"Cancels without recording any chnages to the screen.")
62660   let fnacs(sn$,0,mat resp$,ckey)
62680   if ckey=5 then goto DDDONE
62700   let key$=resp$(1)
62720   let dd$=resp$(2)(1:1)
62740   let rtn=val(resp$(3)) !  banks routing #
62760   acc=val(resp$(4)(1:2)) ! checking or savings
62780   acn=val(resp$(5)) ! employee bank acct #
62800   if ckey=4 then 
62820     let dd$="N"
62840     let rtn=acc=acn=0
62860     let key$=rpad$(key$,10)
62880     delete #30,key=key$: nokey L4660
62900     goto DDDONE
62920   end if 
62940 L4660: ! 
62960   if dd$="Y" and (rtn=0 or acc=0 or acn=0) then 
62980     goto L4670
63000   else 
63020     goto L4680
63040   end if 
63060 L4670: ! 
63080   mat ml$(2)
63100   let ml$(1)="You must have valid answers in the routing #, account"
63120   let ml$(2)="type, and bank account before you can answer yes."
63140   let fnmsgbox(mat ml$,resp$)
63160   goto ASKDD
63180 L4680: ! 
63200   if ckey=1 or ckey=4 then goto SAVEDD
63220   goto ASKDD ! /r
64000 SAVEDD: ! r:
64020   let key$=rpad$(str$(eno),10)
64040   rewrite #30,using "Form pos 11,C 1,N 9,N 2,N 17",key=key$: dd$,rtn,acc,acn
64060   goto DDDONE ! /r
66000 DDDONE: ! r:
66020   close #30: 
66040   return  ! /r
68000 DDOPEN: ! r:
68020   close #30: ioerr ignore
68040   open #30: "Name="&env$('Q')&"\PRmstr\dd.h"&env$('cno')&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",kps=1,kln=10,Use",internal,outin,keyed 
68060   return  ! /r
70000 DDDEL: ! r:
70020   gosub DDOPEN
70040 ! set the key with ENT$
70060 ! delete the record
70080   goto DDDONE ! /r
72000 DDCHGKEY: ! r: update the dd file when you change an employee number
72020   gosub DDOPEN
72040   gosub DDKEY
72060   rewrite #30,using "Form Pos 1,C 10",key=rpad$(trim$(ent$),10): key$ nokey ignore
72080   goto DDDONE  ! /r
74100 ! close #2: ioerr ignore
74120 ! execute "Index "&env$('Q')&"\PRmstr\Department.h"&env$('cno')&' '&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&" 1/9 8/3 Replace DupKeys -n"
74140 ! open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
74160 ! return
76000 ! PICTURE: ! r:
76020 !   let fntos(sn$="Employeepic")
76040 !   let fnpic(1,10,24,60,str$(eno)&".bmp")
76060 !   let fnlbl(25,80,"")
76080 !   let fncmdkey("O&K",7,1,0,"Returns to customer record.")
76100 !   let fnacs(sn$,0,mat resp$,ckey)
76120 !   goto SCR_EMPLOYEE ! /r
80000 SETUP_PAYROLLCHECKS: ! r:
80020   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",RecL=224,use",internal,outin 
80040   close #4: 
80060   execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&' '&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&" 1 17 Replace DupKeys"
80070   gosub INDEX_CHECKIDX3
80072   return  ! /r
80080 INDEX_CHECKIDX3: ! r:
80100   execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&' '&env$('Q')&"\PRmstr\checkidx3.h"&env$('cno')&" 1/12/9 8/6/3 Replace DupKeys"
80120   return  ! /r
82000 CHECK_INFORMATION: ! r:
82020   let hact$=str$(eno)
82040   let filnum=44 ! 44 for date sequence
82060   let fncheckfile(hact$,filnum)
82080   goto EDITREC ! /r
