00010 ! formerly S:\acsPR\NewpRInput
00020 ! enter time sheets
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnDedNames,fnopenprn,fncloseprn,fnchain,fnhours,fntos,fnfra,fnopt,fnlbl,fntxt,fncmdkey,fnacs,fncombof,fnchk,fnmsgbox,fnemployee_srch,fncmbemp,fnerror,fndate_mmddyy_to_ccyymmdd,fngethandle,fnindex_it,fnstatus_close,fncreg_write,fncreg_read
00050   on error goto ERTN
00052   fntop(program$,cap$="Enter Time Sheets")
00060 ! r: dims and constants
00070   dim cap$*128
00080   dim inp(29),em$*30,sc1$(31)*20,hr(2)
00090   dim n1$(9),n2$(9)
00100   dim en$*8,tdet(23) ! ,shd$*78
00110   dim tinp(29),f1$*400,f2$*400,pr(9,35)
00120   dim dednames$(20)*20,resp$(50)*40,ml$(2)*100,skipit$(20)*1,skipit(20)
00130   dim name$(20)*21,d1$*20,deptname$*20,h(7)
00140   dim tdt(4),tcd(3)
00150 ! 
00155   fnDedNames(mat dednames$)
00170   sc1$(1)="Regular Hours "
00175   sc1$(2)="Overtime Hours"
00180   sc1$(3)="Sick Hours    "
00185   sc1$(4)="Vacation Hours"
00190   sc1$(5)="Holiday Hours "
00195   sc1$(6)="Salary        "
00200   sc1$(7)="Other Compensation"
00205   sc1$(8)="Meals"
00210   sc1$(9)="Tips "
00215   for j=1 to 20
00220     sc1$(j+9)=dednames$(j)
00225   next j
00230   sc1$(30)="Reg Hourly Rate"
00235   sc1$(31)="O/T Hourly Rate"
00240 ! 
00265   f1$="Form POS 1,C 20" ! need this for edit list
00270   let f2$=f1$
00275   for j=1 to 9
00280     f1$=rtrm$(f1$)&",PIC(------------)"
00285     let f2$=rtrm$(f2$)&",PIC(---------.--)"
00290   next j
00295   pathtotimecard$="C:\progra~1\acs\"
00300 ! /r
00428   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00430   read #1,using 'form pos 726,pd 3.2': mhw
00440   close #1: 
00450   open #11: "Name="&env$('Q')&"\PRmstr\Dates.h"&env$('cno')&",USE,RecL=76",internal,outin,relative 
00460   read #11,using "form pos 49,n 8,c 20",rec=1: d1,d1$ norec ignore
00470   close #11: 
00471 ! 
00472   open #9: "Name="&env$('Q')&"\PRmstr\DeptName.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptNameIdx.h"&env$('cno')&",Shr",internal,input,keyed ioerr ignore
00480 ! 
00490   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\prwork"&wsid$&"idx.H"&env$('cno'),internal,outin,keyed ioerr ignore
00500 F_RPWORK: form pos 1,n 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
00502 ! ______________________________________________________________________
00510 SCREEN_1: ! 
00520   fntos(sn$="Prinput-1")
00522   rc=cf=0
00530   fnfra(1,1,3,50,"Payroll Time Sheet Entry","You would only add to previous entries if the last batch was not calculated.",0)
00532   cf+=1 : let franum=cf
00540   fnopt(1,3,"Regular Time Sheet Entry",0,franum)
00542   resp$(rc+=1)="True"
00550   fnopt(2,3,"Additions to Previous Input",0,franum)
00552   resp$(rc+=1)="False"
00560   fnfra(6,1,2,50,"Pay Period Ending Date","You must enter the pay perod ending date.  You can not have more than one payroll with the same date.")
00562   cf+=1 : let franum=cf : mylen=26 : mypos=mylen+2
00570   fnlbl(1,1,"Pay Period Ending Date:",mylen,1,0,franum)
00580   fntxt(1,mypos,10,0,1,"1",0,"Use mmddyy.",franum)
00582   resp$(rc+=1)=str$(prd)
00590   fnfra(10,1,6,60,"Method of Entry","You can select specific employees to pay; you can automatically calculate salaried persons; or you can pull from a another system.")
00592   cf+=1 : let franum=cf
00600   fnopt(1,3,"Select employees to pay",0,franum)
00602   resp$(rc+=1)="True"
00610   fnopt(2,3,"Automatically pay salaried employees",0,franum)
00612   resp$(rc+=1)="False"
00620   fnopt(3,3,"Pull time from time card system",0,franum)
00622   resp$(rc+=1)="False"
00630   fnopt(4,3,"Pull time from job cost system",0,franum)
00632   resp$(rc+=1)="False"
00640   fnlbl(6,1,"Employment Status:",mylen,1,0,franum)
00650   fncombof("EmpStatus",6,mylen+3,25,env$('Q')&"\PRmstr\EmpStatus.dat",1,2,3,25,env$('Q')&"\PRmstr\EmpStatus.idx",0,0, "Only necessary if automatically paying salaried people. ",franum,0)
00652   resp$(rc+=1)=""
00660   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
00670   fncmdkey("&Cancel",5,0,1,"Returns to customer record")
00680   fnacs(sn$,0,mat resp$,ckey)
00682   if ckey=5 then goto XIT
00690   if resp$(1)="True" then ! Regular Time Sheet Entry
00691     noauto=ti1=1
00692     additional=1
00693   else if resp$(2)="True" then ! Additions to Previous Input
00694     noauto=ti1=2
00695     additional=2
00696   end if 
00700   prd=val(resp$(3))
00710   if prd=0 then 
00712     mat ml$(2)
00714     ml$(1)="You must enter a valid payroll date!"
00716     ml$(2)="Click OK to return to previous screen. "
00718     fnmsgbox(mat ml$,resp$,cap$,0)
00720     goto SCREEN_1
00722   end if 
00724   if resp$(4)="True" then 
00726     noauto=ti1=1
00728   else if resp$(5)="True" then 
00730     noauto=ti1=2
00732   else if resp$(6)="True" then 
00734     estat=99
00736   else if resp$(7)="True" then 
00738     jobcost=1
00740   end if 
00742   em4=val(resp$(8)(1:2))
00750 ! SKIPDEDUCTIONS: !
00760   fntos(sn$="Prinput-2")
00762   rc=cf=linecnt=0
00770   fnfra(1,1,10,50,"Skip Deductions This Pay Period","You can skip any deduction this pay period by checking the deduction below.")
00772   cf+=1 : let franum=cf
00780   for j=1 to 19 step 2
00790     if trim$(dednames$(j))<>"" then let x$=":" else let x$=""
00800     fnchk(linecnt+=1,20,trim$(dednames$(j))&x$,1,franum)
00802     resp$(rc+=1)="False"
00810     if trim$(dednames$(j+1))<>"" then let x$=":" else let x$=""
00820     fnchk(linecnt,45,trim$(dednames$(j+1))&x$,1,franum)
00822     resp$(rc+=1)="False"
00830   next j
00840   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
00850   fncmdkey("&Cancel",5,0,1,"Returns to customer record")
00860   fnacs(sn$,0,mat resp$,ckey)
00862   if ckey=5 then goto L900
00870   for j=1 to 20
00880     if resp$(j)="True" then skipit$(j)="Y" else skipit$(j)="N"
00890   next j
00900 L900: if noauto<>2 then em4=0 ! don't allow any employment status code if not selecting to automatically pay salaried
00902   if (~exists(env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')) and additional=2) or additional<>2 then 
00904     close #h_rpwork:=3,free: ioerr ignore
00906     open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",RecL=167,Replace",internal,output 
00907     close #h_rpwork: 
00908   end if 
00909   fnindex_it(env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno'),env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno'),"1,11")
00910   fnstatus_close
00911   if additional=2 then 
00912 !   if exists(env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')) then
00914     gosub OFILE
00916     gosub PRINT_LISTING
00918 !    end if
00920     goto PROOF_TOTALS
00922   end if 
00928 ! close #h_rpwork:=3,free: ioerr ignore
00930 ! open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",RecL=167,Replace",internal,output
00940 ! close #h_rpwork:
00950 ! execute "Index "&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&' '&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno')&" 1,11 replace,DupKeys -N"
00960   gosub OFILE
01050   if jobcost=1 then goto PULL_FROM_JOBCOST
01060 L1060: ! 
01062   if estat>0 then 
01064     goto L4180
01066   else 
01068     goto ASK_EMPLOYEE
01070   end if 
01072 ! ______________________________________________________________________
01220 ENTER_TIME: ! 
01230   en$=lpad$(str$(eno),8)
01240   read #h_rpmstr,using F_RPMSTR_1,key=en$: em$,em4,em8,em9,lpd,tgp nokey L1060
01270   if editmode=1 then goto READ_DEPARTMENTS
01280   if prd=lpd then goto EMP_PREV_ENTERED_WARN
01290 L1290: ! 
01292   goto DUPLICATE_DATE_TEST
01300 ! ______________________________________________________________________
01310 READ_DEPARTMENTS: ! 
01320   tgp=0
01322   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": 
01324 L1340: ! 
01326   if goprev=0 then 
01328     read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ASK_EMPLOYEE
01330   else if goprev=1 then 
01332     semp-=1
01334     let goprev=0
01336     read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',prior: teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ASK_EMPLOYEE
01338   end if 
01342   if teno=eno and goprev=0 then semp+=1
01343   if teno<>eno then semp=0
01350   if teno<>eno then goto ASK_EMPLOYEE
01360   hr(1)=tdet(2) : hr(2)=tdet(3) ! set hourly rates from dept rec
01370   simplekey$=en$&cnvrt$("n 3",dep)&cnvrt$("n 5",cno) ! timecard
01380   reghrs=othrs=vachrs=sickhrs=holhrs=othercomp=0 ! timecard
01390   if timecard=1 then 
01400     read #4,using 'form pos 17,6*pd 5.2',key=simplekey$: reghrs,othrs,vachrs,sickhrs,holhrs,othercomp nokey ignore ! timecard
01410   end if 
01420 ! shd$="Employee # "&ltrm$(en$)&"   Name "&rtrm$(em$)&"    Department # "&str$(dep)
01430   if editmode=1 then goto L1450
01440   goto L1490
01450 L1450: ! 
01452   tdet(2)=hr(1)
01460   tdet(3)=hr(2)
01470   tgp=tgp-gpd
01480   goto ASK_TIME ! L1720
01490 L1490: ! 
01492   mat inp=(0)
01500   inp(1)=reghrs
01502   inp(2)=othrs
01503   inp(3)=vachrs
01504   inp(4)=sickhrs
01505   inp(5)=holhrs
01506   inp(7)=othercomp ! timecard
01510   inp(6)=tdet(1)
01520   for j=1 to 20
01530     inp(j+9)=tdet(j+3)
01540     if skipit(j)=1 then inp(j+9)=0
01550   next j
01560 ! if env$('client')="Washington Parrish" and adr=ta(1) and em4=5 then inp(13)=212.50 ! if employment status=5 and first dept then set tips to $212.50
01570 ! ____________
01580 ASK_TIME: ! 
01590   deptname$=""
01592   if foundept=1 then 
01594     read #9,using "form pos 4,c 20",key=rpad$(ltrm$(str$(dep)),3): deptname$ nokey ignore
01596   end if 
01602   fntos(sn$="prinput-4")
01604   respc=0: mylen=20: let franum=0: rc=0
01610   fnlbl(1,1,"Employee Number: "&str$(eno),60,2,0,franum)
01620   fnlbl(2,1,"Employee Name: "&rtrm$(em$),60,2,0,franum)
01630   fnlbl(3,1,"Department Number: "&str$(dep)&" "&trim$(deptname$),60,2,0,franum)
01640   fnlbl(5,1,"Regular Hours:",mylen,1,0,franum)
01650   fntxt(5,mylen+2,12,0,1,"10",0,".",franum)
01652   resp$(rc+=1)=str$(inp(1))
01660   fnlbl(6,1,"Overtime Hours:",mylen,1,0,franum)
01670   fntxt(6,mylen+2,12,0,1,"10",0,".",franum)
01672   resp$(rc+=1)=str$(inp(2))
01680   fnlbl(7,1,"Sick Hours:",mylen,1,0,franum)
01690   fntxt(7,mylen+2,12,0,1,"10",0,".",franum)
01692   resp$(rc+=1)=str$(inp(3))
01700   fnlbl(8,1,"Vacation Hours:",mylen,1,0,franum)
01710   fntxt(8,mylen+2,12,0,1,"10",0,".",franum)
01712   resp$(rc+=1)=str$(inp(4))
01720 L1720: ! note: the goto that points to this line just can't be right...
01722   fnlbl(9,1,"Holiday Hours:",mylen,1,0,franum)
01730   fntxt(9,mylen+2,12,0,1,"10",0,".",franum)
01732   resp$(rc+=1)=str$(inp(5))
01740   fnlbl(10,1,"Salary:",mylen,1,0,franum)
01750   fntxt(10,mylen+2,12,0,1,"10",0,".",franum)
01752   resp$(rc+=1)=str$(inp(6))
01760   fnlbl(11,1,"Other Compensation:",mylen,1,0,franum)
01770   fntxt(11,mylen+2,12,0,1,"10",0,".",franum)
01772   resp$(rc+=1)=str$(inp(7))
01780   fnlbl(12,1,"Meals:",mylen,1,0,franum)
01790   fntxt(12,mylen+2,12,0,1,"10",0,".",franum)
01792   resp$(rc+=1)=str$(inp(8))
01800   fnlbl(13,1,"Tips:",mylen,1,0,franum)
01810   fntxt(13,mylen+2,12,0,1,"10",0,".",franum)
01812   resp$(rc+=1)=str$(inp(9))
01820   fnlbl(15,1,"Reg Hourly Rate:",mylen,1,0,franum)
01830   fntxt(15,mylen+2,12,0,1,"10",0,".",franum)
01832   resp$(rc+=1)=str$(hr(1))
01840   fnlbl(16,1,"O/T Hourly Rate:",mylen,1,0,franum)
01850   fntxt(16,mylen+2,12,0,1,"10",0,".",franum)
01852   resp$(rc+=1)=str$(hr(2))
01860   for j=1 to 20
01870 ! If TRIM$(DEDNAMES$)="" Then Goto 1450
01880     if trim$(dednames$(j))="" then name$(j)="" else name$(j)=trim$(dednames$(j))&":"
01890     if skipit$(j)="Y" then inp(j+9)=0
01892     disable_deduction=0 : if trim$(name$(j))='' then disable_deduction=1
01900     fnlbl(j+4,25,trim$(name$(j)),mylen,1,0,franum)
01910     fntxt(j+4,47,12,0,1,"10",disable_deduction,".",franum)
01912     resp$(rc+=1)=str$(inp(j+9))
01920   next j
01930   fncmdkey("&Next",1,1,0,"Record this time" )
01940   if editmode=0 then let fncmdkey("&Skip Department F2",2,0,0,"Skips this department.")
01941   if editmode=0 and semp>=1 then let fncmdkey("&Prev Department",12,0,0,"Go back to last department.")
01950   if editmode=1 then let fncmdkey("&Delete Department",10,0,0,"Deletes the hours, etc for this department.")
01960   if editmode=0 then let fncmdkey("&Track Hours",8,0,0,"Track hours other than those entered above.")
01970   fncmdkey("&Make Changes Permanent",3,0,0,"Makes any rate changes or other deductions changes permanent in the employee record.")
01980   if editmode=0 then let fncmdkey("E&xit",5,0,1,"Returns to menu")
01990   if editmode=1 then let fncmdkey("&Finish",7,0,1,"Finished making corrections")
02000   fnacs(sn$,0,mat resp$,ckey) ! ask time
02010   if ckey=5 and editmode=0 then goto FINISH
02020   for j=1 to 9
02030     inp(j)=val(resp$(j))
02040   next j
02050   hr(1)=val(resp$(10))
02060   hr(2)=val(resp$(11))
02070   for j=12 to 31
02080     inp(j-2)=val(resp$(j))
02090   next j
02100   if ckey=8 then let fnhours(eno) : goto ASK_TIME !  breakdown=1 : goto ASK_TIME
02110   if ckey=5 and editmode=1 then goto L2290
02120   if ckey=10 and editmode=1 then goto DELETE_IT
02130   if ckey=2 then goto L2430
02131   if ckey=12 then let goprev=1 : goto L1340
02140   if ckey<>3 then goto L2220
02150   tdet(1)=inp(6)
02160   for j=1 to 20
02170     tdet(j+3)=inp(j+9)
02180   next j
02190   tdet(2)=hr(1)
02200   tdet(3)=hr(2)
02210   rewrite #2,using 'form pos 9,n 3,pos 58,23*pd 4.2',key=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",dep): dep,mat tdet
02220 L2220: ! 
02222   let gpd=0
02230   if em8><-2 then goto L2260
02240   if inp(3)=0 then goto L2260
02250   mat ml$(2)
02252   ml$(1)="This employee is not eligible for Sick Leave!": ml$(2)="Click OK to return to previous screen. "
02254   fnmsgbox(mat ml$,resp$,cap$,0)
02256   goto ASK_TIME
02260 L2260: if em9><-2 then goto L2290
02270   if inp(4)=0 then goto L2290
02280   mat ml$(2)
02282   ml$(1)="This employee is not eligible for Vacation!": ml$(2)="Click OK to return to previous screen. "
02284   fnmsgbox(mat ml$,resp$,cap$,0)
02286   goto ASK_TIME
02290 L2290: ! 
02292   for j=1 to 5
02300     ! if env$('client')="West Rest Haven" and sickhrs>0 then inp(4)=0 ! if sickhrs come from time clock, set the sick hours in entry as 0
02310     ! if env$('client')="West Rest Haven" and j=5 then !:
          !   let gpd=gpd+inp(j)*(hr(1)*1.50) : goto L2330 ! pay time and 1/2 on holiday pay
02320     if j=2 then let gpd=gpd+inp(j)*hr(2) else let gpd=gpd+inp(j)*hr(1)
02330 L2330: ! 
02332   next j
02340   ! if env$('client')="West Rest Haven" then 
02342   !   inp(7)=inp(7)+round(sickhrs*(hr(1)*.50),2)
02344   !   sickhrs=0 ! place  double time portion of holiday overtime hours in other compensation, then clear the sick hours
02346   ! end if 
02350   if inp(9)>0 and gpd+inp(6)+inp(7)+inp(8)+inp(9)<round((inp(1)*mhw+inp(2)*mhw*1.5),2) then inp(7)=inp(7)+round((inp(1)*mhw+inp(2)*mhw*1.5),2)-(gpd+inp(6)+inp(7)+inp(8)+inp(9))
02360   let gpd=gpd+inp(6)+inp(7) +inp(8)+inp(9) ! inp(8) (meals) and inp(9) tips both need to be added in for taxing purposes  they will be taken back out in S:\Payroll\Calc
02370   if ckey=5 and editmode=1 then goto L3960 ! just add proof totals back in
02380   if editmode=1 then goto REWRITE_WORK
02390   write #h_rpwork,using F_RPWORK: eno,dep,mat inp,gpd,mat hr
02410   mat tinp=tinp+inp
02420   tgp=tgp+gpd
02430 L2430: ! 
02432   if estat>0 then goto L4300 ! pulling from time card system
02440   if tgp=0 then ped=0 else ped=prd
02450   rewrite #h_rpmstr,using F_RPMSTR_2,key=en$: ped,tgp
02470   if heno=eno then goto L2490
02480   if tgp>0 then ent1=ent1+1
02490 L2490: ! 
02492   heno=eno
02500   goto L1340 ! If ADR>0 Then Goto 1050 Else Goto 820
02510 ! ______________________________________________________________________
02520 FINISH: ! 
02530   close #h_rpmstr: ioerr ignore
02540   close #2: ioerr ignore
02550   close #h_rpwork: ioerr ignore
02560   close #11: ioerr ignore
02570   close #109: ioerr ignore
02580   close #108: ioerr ignore
02590 PROOF_TOTALS: ! 
02592   fn_add_proof_totals(teno,count_employees_entered,mat tinp)
02595   fntos(sn$="ProofTotal")
02600   respc=0 : mylen=20 : let franum=0 : rc=0
02605   fnlbl(1,1,"P R O O F  T O T A L S",60,2,0,franum)
02610   fnlbl(2,1,"Total Employees/Departments Entered: "&str$(count_employees_entered),60,2,0,franum)
02612   fnlbl(3,1,"Total Employee Numbers Entered: "&str$(teno),60,2,0,franum)
02615   fnlbl(5,1,"Regular Hours:",mylen,1,0,franum)
02620   fntxt(5,mylen+2,12,0,1,"10",1,".",franum)
02625   resp$(rc+=1)=str$(tinp(1))
02630   fnlbl(6,1,"Overtime Hours:",mylen,1,0,franum)
02635   fntxt(6,mylen+2,12,0,1,"10",1,".",franum)
02640   resp$(rc+=1)=str$(tinp(2))
02645   fnlbl(7,1,"Sick Hours:",mylen,1,0,franum)
02650   fntxt(7,mylen+2,12,0,1,"10",1,".",franum)
02655   resp$(rc+=1)=str$(tinp(3))
02660   fnlbl(8,1,"Vacation Hours:",mylen,1,0,franum)
02665   fntxt(8,mylen+2,12,0,1,"10",1,".",franum)
02670   resp$(rc+=1)=str$(tinp(4))
02675   fnlbl(9,1,"Holiday Hours:",mylen,1,0,franum)
02680   fntxt(9,mylen+2,12,0,1,"10",1,".",franum)
02685   resp$(rc+=1)=str$(tinp(5))
02690   fnlbl(10,1,"Salary:",mylen,1,0,franum)
02695   fntxt(10,mylen+2,12,0,1,"10",1,".",franum)
02700   resp$(rc+=1)=str$(tinp(6))
02705   fnlbl(11,1,"Other Compensation:",mylen,1,0,franum)
02710   fntxt(11,mylen+2,12,0,1,"10",1,".",franum)
02715   resp$(rc+=1)=str$(tinp(7))
02720   fnlbl(12,1,"Meals:",mylen,1,0,franum)
02725   fntxt(12,mylen+2,12,0,1,"10",1,".",franum)
02730   resp$(rc+=1)=str$(tinp(8))
02735   fnlbl(13,1,"Tips:",mylen,1,0,franum)
02740   fntxt(13,mylen+2,12,0,1,"10",1,".",franum)
02745   resp$(rc+=1)=str$(tinp(9))
02750   fnlbl(15,1,"Reg Hourly Rate:",mylen,1,0,franum)
02755   fntxt(15,mylen+2,12,0,1,"10",1,".",franum)
02760   resp$(rc+=1)=str$(hr(1))
02765   fnlbl(16,1,"O/T Hourly Rate:",mylen,1,0,franum)
02770   fntxt(16,mylen+2,12,0,1,"10",1,".",franum)
02775   resp$(rc+=1)=str$(hr(2))
02780   for j=1 to 20
02785     if trim$(dednames$(j))="" then name$(j)="" else name$(j)=trim$(dednames$(j))&":"
02790     fnlbl(j+4,25,trim$(name$(j)),mylen,1,0,franum)
02795     fntxt(j+4,47,12,0,1,"10",1,".",franum)
02800     resp$(rc+=1)=str$(tinp(j+9))
02805   next j
02900   fncmdkey("Co&rrections",1,0,0,"Correct any entries.")
02910   fncmdkey("&Listing",2,0,0,"Prints a listing of the entries you have made.")
02920   fncmdkey("&Calculate",3,1,0,"Calculates the pay.")
02930   fncmdkey("&Add",4,0,0,"Add additional time. (If you missed a department, you should delete the original entries on that employee and completely re-enter the employee time.")
02940   fncmdkey("E&xit",5,0,1,"Exit without calculating")
02950   fnacs(sn$,0,mat resp$,ckey) ! proof totals
02960   estat=0
02970   cor=ckey
02980   if ckey=5 then goto XITWOCAL
02990   if ckey=1 or ckey=2 or ckey=4 then gosub OFILE
03000   on ckey goto CORRECTIONS,PRINT_LISTING,GOCALK,ASK_EMPLOYEE none PROOF_TOTALS
03002 ! ______________________________________________________________________
03870 DELETE_IT: ! 
03880 ! eNO=0
03890 ! dEP=0
03900 ! Mat INP=(0)
03910 ! Let GPD=0
03920   delete #h_rpwork,rec=rec(h_rpwork): norec L3990
03930   goto L4000
03940 REWRITE_WORK: ! 
03950   rewrite #h_rpwork,using F_RPWORK,rec=rec(h_rpwork): eno,dep,mat inp,gpd,mat hr
03960 L3960: ! 
03962   tgp=tgp+gpd
03970   teno=teno+eno
03980   mat tinp=tinp+inp
03990 L3990: ! 
03992   if tgp=0 then ped=0 else ped=prd
04000 L4000: ! 
04002   rewrite #h_rpmstr,using F_RPMSTR_2,key=en$: ped,tgp
04010   goto READ_NEXT_DEPARTMENT
04020 ! rp1=1
04030   cor=editmode=0
04040   goto READ_NEXT_DEPARTMENT
04050 ! ______________________________________________________________________
04930 ! r: unaccessed lines... delete them someday
04940 !       12/23/2015  these lines seem totally unaccessed     for j=1 to 20
04950 !       12/23/2015  these lines seem totally unaccessed       if skipit$(j)="Y" then skipit(j)=1 else skipit(j)=0
04960 !       12/23/2015  these lines seem totally unaccessed   ! If SKIPIT$(J)<>"Y" AND SKIPIT$(J)<>"N" Then cE=J : Goto ERR4
04970 !       12/23/2015  these lines seem totally unaccessed     next j
04980 !       12/23/2015  these lines seem totally unaccessed     close #win:
04990 !       12/23/2015  these lines seem totally unaccessed     return
05000 ! /r
35000 GOCALK: ! r:
35020   close #11: ioerr ignore
35040   open #11: "Name="&env$('Q')&"\PRmstr\Dates.h"&env$('cno')&",USE,RecL=76",internal,outin,relative 
35060   rewrite #11,using "form pos 49,n 8,c 20",rec=1: fndate_mmddyy_to_ccyymmdd(prd),d1$ norec ignore
35080   close #11: 
35100   if jobcost=1 then close #5,free: 
35120   fnchain("S:\Payroll\Calc") ! /r
35140 ! ______________________________________________________________________
35160 EMP_PREV_ENTERED_WARN: ! r:
35162   mat ml$(2)
35180   ml$(1)="Employee number "&str$(eno)&" has been previously entered."
35200   ml$(2)="Do you wish to continue anyway? "
35220   fnmsgbox(mat ml$,resp$,cap$,52)
35240   if resp$(1:1)="Y" then goto L1290 ! IN1=2
35260   if resp$(1:1)="N" then goto L1060 ! in1=1
35280 ! /r
35300 L4180: ! 
35320   open #4: "Name="&pathtotimecard$&"timecard\simplesummary,KFName="&pathtotimecard$&"timecard\ssindex,Shr",internal,outin,keyed ioerr L4200 ! timecard
35340   timecard=1 ! timecard files exist
35360 L4200: ! 
35362   read #h_rpmstr,using F_RPMSTR_3: en$,em$,em4,em8,em9,lpd,tgp,mat ta eof FINISH
35380   if em4=9 then goto L4200 ! must use employment status code = 9 for terminated
35400   ! if env$('client')="West Rest Haven" and em4=2 then goto L4200 ! wrh uses code 2 for terminated
35420   tgp=0
35460 ! If ESTAT=99 AND EM4=1 Then Goto 3590 ! employment status on salaries people must be 1
35480 ! If EM4><ESTAT Then Goto 3552
35500   eno=val(en$)
35520   pr f "16,20,C 60": en$&"  "&em$
35540 L4290: ! 
35560   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": 
35580 L4300: ! 
35600   read #2,using 'FORM POS 1,n 8,n 3,POS 58,23*PD 4.2': depeno,dep,mat tdet
35620   if depeno<>eno then goto L1060
35640   simplekey$=en$&cnvrt$("n 3",dep)&cnvrt$("n 5",cno) ! timecard
35660   reghrs=othrs=vachrs=sickhrs=holhrs=othercomp=0 ! timecard
35680   if timecard=1 then 
35700     read #4,using 'form pos 17,6*pd 5.2',key=simplekey$: reghrs,othrs,vachrs,sickhrs,holhrs,othercomp nokey L4380 ! timecard
35720   end if 
35740   ! if env$('client')="West Rest Haven" then gosub WRH_SIMPLE_OFFSET_HOLIDAY
35780   goto L4400
35800 L4380: ! 
35820   if em4=1 and tdet(1)>0 then goto L4400 ! calculate salaries even if no time in time card file; skip any hourly people who do not have any entries from the time card system
35840   goto L4290 ! If NTA>0 Then aDR=NTA: Goto 3870 Else Goto 2010 ! circle if more than one department on hourly people; else skip if no time and no more departments
35860 L4400: ! 
35880   hr(1)=tdet(2)
35900   hr(2)=tdet(3)
35920   mat inp=(0)
35940   inp(1)=reghrs
35960   inp(2)=othrs
35980   inp(3)=vachrs
36000   inp(4)=sickhrs
36020   inp(5)=holhrs
36040   inp(7)=othercomp ! timecard
36060   inp(6)=tdet(1)
36080   for j=1 to 20
36100     inp(j+7)=tdet(j+3)
36120     if skipit(j)=1 then inp(j+7)=0
36140   next j
36160   goto L2220
38000 CORRECTIONS: ! r:
38020   editmode=1
38040   fntos(sn$="Employee-ask2")
38060   respc=0
38080   fnlbl(1,1,"Employee to Correct:",22,right)
38100   fncmbemp(1,24)
38120   resp$(respc+=1)=""
38140   fncmdkey("&Next",1,1,0,"Make corrections to this employee's time." )
38160   fncmdkey("&Finish",6,0,1,"Finished making corrections")
38180   fnacs(sn$,0,mat resp$,ckey) ! ask employee #
38200   if ckey=6 then editmode=0: goto PROOF_TOTALS ! finished corretions
38220   eno=ent=val(resp$(1)(1:8))
38240   read #h_rpwork,using F_RPWORK,key>=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",0),release: depeno,dep2,mat inp,gpd,mat hr nokey L4790
38260   if eno<>depeno then goto L4790
38280   if eno=0 then goto CORRECTIONS
38300   goto L4840
38320 L4790: ! 
38330   mat ml$(2)
38340   ml$(1)="No time has been entered on employee number "&str$(eno)&'.'
38360   ml$(2)="Do you wish to enter new time on this employee? "
38370   fnmsgbox(mat ml$,resp$,cap$,52)
38380   if resp$(1:1)="Y" then 
38390     goto ENTER_TIME ! ASK_EMPLOYEE
38400   else 
38410     goto CORRECTIONS
38420   end if 
38440 READ_NEXT_DEPARTMENT: ! 
38460   read #h_rpwork,using F_RPWORK,release: depeno,dep2,mat inp,gpd,mat hr nokey CORRECTIONS eof CORRECTIONS
38480 L4840: ! 
38500   if depeno<>eno then goto CORRECTIONS
38520   em$=""
38540   en$=lpad$(str$(eno),8)
38560   read #h_rpmstr,using F_RPMSTR_1,key=en$: em$,em4,em8,em9,lpd,tgp nokey ignore
38580   teno=teno-eno ! remove from proof totals
38620   mat tinp=tinp-inp
38640   dep=dep2 ! fix dept # on correction screen
38660   tgp=tgp-gpd
38680   goto ASK_TIME
38700 ! /r
42000 OFILE: ! r: OPEN FILES
42020   open #h_rpmstr:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,outin,keyed 
42040 F_RPMSTR_1: form pos 9,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2,2*pd 3
42060 F_RPMSTR_2: form pos 162,n 6,pd 5.2
42070 F_RPMSTR_3: form pos 1,c 8,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2,2*pd 3
42080   close #11: ioerr ignore
42100   open #11: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDX2.h"&env$('cno')&",Shr",internal,outin,keyed 
42120   close #2: ioerr ignore
42140   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
42160   close #h_rpwork:=3: ioerr ignore
42180   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno')&',shr',internal,outin,keyed 
42200   close #4: ioerr ignore
42220   open #4: "Name="&pathtotimecard$&"timecard\simplesummary,KFName="&pathtotimecard$&"timecard\ssindex,Shr",internal,outin,keyed ioerr L4630 ! timecard
42240   timecard=1 ! timecard files exist
42260 L4630: ! 
42280   return  ! /r
44000 DUPLICATE_DATE_TEST: ! r: ! dont allow to calculate if reversing calculation needs to be run
44020   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey DUPLICATE_DATE_TEST_XIT
44040   do 
44060     read #2,using 'FORM POS 1,n 8,POS 42,n 6': depeno,tdt4 eof DUPLICATE_DATE_TEST_XIT
44080     if depeno<>eno then goto DUPLICATE_DATE_TEST_XIT
44100     if tdt4=prd then 
44160       mat ml$(4)
44180       ml$(1)="You have previously calculated pay using this same payroll date on employee # "&x$
44200       ml$(2)="You must either use a different date or reverse the previous calculation. "
44220       ml$(3)="Click OK to return to previous screen. "
44240       fnmsgbox(mat ml$,resp$,cap$,0)
44260       goto L1060
44280     end if 
44300   loop 
44320 DUPLICATE_DATE_TEST_XIT: ! 
44340   goto READ_DEPARTMENTS
44360 ! /r
46000 ! <Updateable Region: ERTN>
46020 ERTN: fnerror(program$,err,line,act$,"xit")
46040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
46060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
46080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
46100 ERTN_EXEC_ACT: execute act$ : goto ERTN
46120 ! /region
47000 XIT: fnxit
47500 IGNORE: continue 
48000 XITWOCAL: ! r:
48020   mat ml$(2)
48040   ml$(1)="To save your changes, next time you choose to 'Enter Time Sheets'"
48060   ml$(2)="you must select 'Additions to Previous Input'"
48080   fnmsgbox(mat ml$,resp$,cap$,0)
48100   goto XIT
48120 ! /r
50000 ! DUPLICATE_DEPARTMENTS: ! r:
50020 ! L5250: !
50040 !  read #h_rpwork,using L5260,key=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",dep),release: transeno,transdep nokey L5280
50060 ! L5260: form pos 1,n 8,n 3
50080 !  goto L5250
50100 ! L5280: mat ml$(4)
50120 !  ml$(1)="You can not enter time to the same department"
50140 !  ml$(2)="on the same employee. Choose a different department "
50160 !  ml$(3)="or choose to make corrections to fix the previous entry. "
50180 !  ml$(4)="Click OK to return to previous screen. "
50200 !  fnmsgbox(mat ml$,resp$,cap$,0)
50220 ! /r goto L1060
52000 ! WRH_SIMPLE_OFFSET_HOLIDAY: ! r: offset holiday hours for West Rest Haven
52020 !   if sickhrs>0 then 
52040 !     othrs=othrs-sickhrs
52060 !     reghrs=reghrs-(max(0,holhrs-sickhrs)) ! wrh places any holiday hours considered overtime in the sick hours column.  The holiday hours are duplicated either in the reg hours or the ot hours.  this is how we decide which
52080 !   end if 
52100 !   if sickhrs=0 then reghrs=reghrs-holhrs ! their timeclock puts holiday hours in reghrs column or othrs as well holiday column  (if no part belongs to othrs, then take all from the regular hrs
52120 ! return  ! /r
56000 PULL_FROM_JOBCOST: ! r:
56020 ! h(1)=emp#,h(2)=method,h(3)=dept#,h(4)=reghrs,h(5)=ot hrs,h(6)=salary,h(7)=ded #
56040   gosub SORTIT
56060   open #5: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&env$('cno'),internal,input,relative 
56080   open #6: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
56100   close #h_rpwork:=3: ioerr ignore
56120   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",RecL=167,Replace",internal,output 
56140   close #h_rpwork: 
56160   execute "Index "&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&' '&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno')&" 1,11 replace,DupKeys -N"
56180   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno'),internal,outin,keyed 
56200 ! Restore #h_rpmstr:
56220 ! Read #h_rpmstr,Using 5480: EN$ Eof 5520
56240 ! Form POS 1,C 8
56260 ! Rewrite #h_rpmstr,Using 5500,Key=EN$: 0
56280 ! Form POS 168,PD 5.2
56300 ! Goto 5470
56320   holdeno=eno=holddep=dep=0
56340   mat h=(0)
56360 L5520: ! 
56380   holdeno=h(1): holddep=h(3)
56400   read #6,using 'form pos 1,pd 3': jci eof L5990
56420   read #5,using 'form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6',rec=jci: mat h,dt2,jn$ norec L5520
56440   if h(1)=0 then goto L5520 ! don't allow entry without employee #
56460   if rec(6)>1 and (h(1)><eno or holddep><h(3)) then goto L5820 ! first record or not same emp # or not same dept#
56480 L5590: ! 
56500   h2=h(2)
56520   if h2=1 then goto L5630 ! salary only
56540   inp(1)=inp(1)+h(4) ! accumulate hours
56560   inp(2)=inp(2)+h(5)
56580 L5630: ! 
56600   eno=h(1)
56620   dep=h(3)
56640   if h(7)=0 then goto L5660 else inp(h(7)+7)=inp(h(7)+7)+tdet(h(7)-3)
56660 L5660: ! 
56680   if h2=1 or h2=3 then inp(6)=inp(6)+h(6)
56700   if h(7)=21 and h(6)>0 then inp(7)=inp(7)+h(6)
56720   if eno=0 then goto L5520
56740   en$=lpad$(str$(eno),8)
56760   read #h_rpmstr,using 'form pos 9,c 30,pos 126,2*pd 3.3,pos 168,pd 5.2',key=en$: em$,em8,em9,tgp nokey L5710
56780   goto L5720
56800 L5710: ! 
56820   mat ml$(2)
56840   ml$(1)="Can't find an employee record for employee # "&trim$(em$)&"!"
56860   ml$(2)="Time was entered on "&cnvrt$("pic(zz/zz/zz",dte)
56880   ml$(2)="Time for this employee will be skipped."
56900   fnmsgbox(mat ml$,resp$,cap$,0)
56920   goto L5520
56940 L5720: ! 
56960   if eno=holdeno then goto L5750
56980   tgp=0
57000 L5750: ! 
57020   read #2,using "form pos 58,24*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep): mat tdet
57040   if h2=1 or h2=3 then inp(6)=tdet(1)
57060   for j=1 to 20
57080     inp(j+9)=inp(j+9)+tdet(j+3)
57100   next j
57120   if (holdeno=0 and eno>0) or ( holdeno=h(1) and holddep=h(3)) then goto L5520 ! read another record to see if same employee and department or to handle first record
57140 L5820: ! 
57160   for j=1 to 5
57180     if j=2 then 
57200       let gpd=gpd+(inp(j)*tdet(3))
57220     else 
57240       let gpd=gpd+(inp(j)*tdet(2))
57260     end if 
57280   next j
57300   let gpd=gpd+inp(6)+inp(7)+inp(8)+inp(9)
57320   hr(1)=tdet(2)
57340   hr(2)=tdet(3)
57360   write #h_rpwork,using F_RPWORK: holdeno,holddep,mat inp,gpd,mat hr
57400   mat tinp=tinp+inp
57420   tgp=tgp+gpd
57440   let gpd=0
57460   if tgp=0 then ped=0 else ped=prd
57480   rewrite #h_rpmstr,using 'form pos 162,n 6,pd 5.2',key=en$: ped,tgp
57500   if holdeno=eno then goto L5960
57520   if tgp>0 then ent1=ent1+1
57540 L5960: ! 
57560   holdeno=h(1) : holddep=h(3) : mat inp=(0)
57580   if eofcode=1 then goto L6000
57600   goto L5590
57620 L5990: ! 
57640   eofcode=1: goto L5820 ! allow last entry to post
57660 L6000: ! 
57680   goto PROOF_TOTALS
57700 ! /r
60000 SORTIT: ! r:
60020   open #15: "Name="&env$('Temp')&"\Sort"&session$&".tmp,RecL=128,Replace",internal,output 
60040   write #15,using 'form pos 1,c 128': "FILE "&env$('Q')&"\PRmstr\JCPRH1.H"&env$('cno')&",,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N"
60060   write #15,using 'form pos 1,c 128': "MASK 1,8,N,A,10,2,PD,A"
60080   close #15: 
60100   close #6: ioerr ignore
60120   execute "FREE "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
60140   execute "SORT "&env$('Temp')&"\Sort"&session$&".tmp -n"
60160   return  ! /r
62000 ASK_EMPLOYEE: ! r:
62020   editmode=0
62040   fntos(sn$="Employee-ask")
62060   respc=0
62080   fnlbl(1,1,"Employee:",11,right)
62100   fncmbemp(1,13)
62120   resp$(respc+=1)=""
62140   fncmdkey("&Next",1,1,0,"Enter time on this employee" )
62160   fncmdkey("&Search",2,0,0,"Search for employee record")
62180   fncmdkey("&Finish",6,0,1,"Finished entering hours")
62200 !                     fnCMDKEY("E&xit",5,0,1,"Returns to menu") !   fix kj
62220   fnacs(sn$,0,mat resp$,ckey) ! ask employee #
62240   eno=ent=val(resp$(1)(1:8))
62260   if ckey=1 then 
62280     goto ENTER_TIME
62300   else if ckey=2 then 
62320     fnemployee_srch(x$,fixgrid)
62340     eno=val(x$)
62360     goto ENTER_TIME
62380   else if ckey=5 or ckey=6 then 
62400     goto FINISH
62420   else 
62440     goto FINISH
62460   end if 
62480 ! /r
64000   def fn_add_proof_totals(&apt_total_employee_numbers,&apt_count_employees_entered,mat tinp)
64020     open #apt_h_rpwork:=fngethandle: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno'),internal,input,keyed ioerr APT_FINIS
64040     apt_heno=0 ! temp variable for internal comparison
64060     apt_total_employee_numbers=0 ! total of all (unique) employee numbers entered
64080     apt_count_employees_entered=0 ! total unique employees entered
64100     mat tinp=(0)
64120 ! restore #apt_h_rpwork:
64140     do 
64160       read #apt_h_rpwork,using F_RPWORK: eno,dep,mat inp,gpd,mat hr eof APT_FINIS
64180       if apt_heno<>eno then 
64200         apt_total_employee_numbers=apt_total_employee_numbers+eno
64220         apt_count_employees_entered+=1
64240       end if 
64260       mat tinp=tinp+inp
64280       apt_heno=eno
64300     loop 
64320 APT_FINIS: ! 
64340     close #apt_h_rpwork: ioerr ignore
64360   fnend 
66000 PRINT_LISTING: ! 
66020   heno=r=pc=teno=ent1=0
66040   mat tinp=(0)
66060 ! on fkey 5 goto PL_XIT
66080   if additional=2 then goto L3160
66110   fncreg_read('enter time sheets proof sequence',printorder$) : printorder=val(printorder$) conv ignore
66120 L3080: ! 
66140   fntos(sn$="Print-order")
66170   fnlbl(1,1,"Sequence:",11,right)
66180   fnopt(1,13,"Account Order",0)
66200   if printorder<>2 then resp$(1)="True" else resp$(1)='False'
66220   fnopt(2,13,"Order Entered",0)
66240   if printorder=2 then resp$(2)='True' else resp$(2)="False"
66260   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
66280   fnacs(sn$,0,mat resp$,ckey)
66300   if resp$(1)="False" and resp$(2)="False" then goto L3080
66320   if resp$(1)="True" then printorder=1 else printorder=2
66330   fncreg_write('enter time sheets proof sequence',str$(printorder))
66340   fnopenprn
66360 L3160: ! 
66380   restore #h_rpwork: : record=0
66390   pc2=0
66400 PL_READ: ! 
66420   if printorder=2 then 
66440 L3200: ! 
66460     record+=1 : if record>lrec(3) then goto PL_FINIS
66480     read #h_rpwork,using F_RPWORK,rec=record,release: eno,dep,mat inp,gpd,mat hr eof PL_FINIS norec L3200
66500   else 
66520     read #h_rpwork,using F_RPWORK,release: eno,dep,mat inp,gpd,mat hr eof PL_FINIS
66540   end if 
66560   if heno=eno then goto L3290
66580   read #2,using "form pos 42,n 6",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep): lastprdate
66600   if lastprdate=prd then ! make sure pay hasn't been calculated on this person on this date
66620     mat ml$(4)
66640     ml$(1)="You have previously calculated pay using this same payroll date on employee # "&str$(eno)
66660     ml$(2)="You must delete this person's time for now and either reverse the previous calculation "
66680     ml$(3)="or enter the time using a differen payroll date. "
66700     ml$(4)="                         Click OK to continue. "
66720     fnmsgbox(mat ml$,resp$,cap$,0)
66740     if additional=2 then 
66760       delete #h_rpwork,rec=rec(h_rpwork): norec L3270
66780       goto PL_READ
66800     end if 
66820   end if 
66840 L3270: ! 
66860   teno=teno+eno
66880   ent1=ent1+1
66900 L3290: ! 
66920   mat tinp=tinp+inp
66940   heno=eno
66960   if additional=2 then goto PL_READ
66980   if pc=9 then gosub PL_PRINT_EMP_BLOCK
67000   pc=pc+1
67020   read #h_rpmstr,using F_RPMSTR_1,key=lpad$(str$(eno),8),release: em$ nokey L3440
67040   em$=rtrm$(em$)
67060   for j1=len(em$) to 1 step -1
67080     if em$(j1:j1)=" " then goto L3410
67100   next j1
67120   n1$(pc)=em$(1:10) : n2$=em$(12:22) : goto L3440
67140 L3410: ! 
67160   j2=min(j1,10)
67180   n1$(pc)=lpad$(rtrm$(em$(1:j2)),12)
67200   n2$(pc)=lpad$(rtrm$(em$(j1+1:j1+10)),12)
67220 L3440: ! 
67240   pr(pc,1)=eno
67260   pr(pc,2)=dep
67280   pr(pc,32)=gpd
67300   pr(pc,33)=r
67320   pr(pc,34)=hr(1)
67340   pr(pc,35)=hr(2)
67360   for j=1 to 29
67380     pr(pc,j+2)=inp(j)
67400   next j
67420   goto PL_READ
67900 ! ______________________________________________________________________
70000 PL_PRINT_EMP_BLOCK: ! r:
70010   if pc2=3 then pc2=0
70020   pc2=pc2+1
70040   if pc2>1 then goto L3590
70060   pr #255: ''
70080   pr #255,using 'form pos 1,c 25,cc 82,skip 1,c 25': date$,env$('cnam'),time$
70100 L3590: ! 
70120   pr #255,using L3610: mat n1$
70140   pr #255,using L3610: mat n2$
70160 L3610: form pos 21,9*c 12,skip 1
70180 ! pr #255,Using F1$: "Record #     ",PR(1,23),PR(2,23),PR(3,23),PR(4,23),PR(5,23),PR(6,23),PR(7,23),PR(8,23),PR(9,23)
70200   pr #255,using f1$: "Employee  ",pr(1,1),pr(2,1),pr(3,1),pr(4,1),pr(5,1),pr(6,1),pr(7,1),pr(8,1),pr(9,1)
70220   pr #255,using f1$: "Department  ",pr(1,2),pr(2,2),pr(3,2),pr(4,2),pr(5,2),pr(6,2),pr(7,2),pr(8,2),pr(9,2)
70240   for j=1 to 29
70260     if trim$(sc1$(j))<>"" then 
70280       pr #255,using f2$: sc1$(j),pr(1,j+2),pr(2,j+2),pr(3,j+2),pr(4,j+2),pr(5,j+2),pr(6,j+2),pr(7,j+2),pr(8,j+2),pr(9,j+2)
70300     end if 
70320   next j
70340   pr #255,using f2$: "Dept Gross Pay ",pr(1,32),pr(2,32),pr(3,32),pr(4,32),pr(5,32),pr(6,32),pr(7,32),pr(8,32),pr(9,32)
70360   pr #255,using f2$: "Reg Hourly Rate",pr(1,34),pr(2,34),pr(3,34),pr(4,34),pr(5,34),pr(6,34),pr(7,34),pr(8,34),pr(9,34)
70380 ! pr #255,Using F2$: "O/T Hourly Rate",PR(1,35),PR(2,35),PR(3,35),PR(4,35),PR(5,35),PR(6,35),PR(7,35),PR(8,35),PR(9,35)
70400   if pc2=2 then pr #255: newpage else pr #255,using L3730: " "
70420 L3730: form c 1,skip 2
70440   if pc2>1 then pc2=0
70460   mat pr=(0)
70480   mat n1$=("")
70500   mat n2$=("")
70520   pc=0
70540   return  ! /r
70900 ! ______________________________________________________________________
72000 PL_FINIS: ! r:
72020   if additional=2 then 
72040     additional=1
72060     close #h_rpwork: 
72080   else 
72100     gosub PL_PRINT_EMP_BLOCK
72120     fncloseprn
72140   end if 
72160   goto PROOF_TOTALS ! /r
