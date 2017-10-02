10000 ! Replace S:\acsPR\newprWkMCmp
10200 ! Workmans Compensation Report
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit, fnwait,fncloseprn,fnopenprn,fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs,fnGetPayrollDates
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim ename$*30,subtot(2),tottot(2),tqm(17),cap$*128,message$*40
11400   dim tdet(17),tdc(6),tcp(31),tdc(10)
11600 ! ______________________________________________________________________
11800   let fntop(program$,cap$="Workmans Compensation Report")
12400   fnGetPayrollDates(beg_date,end_date)
13000 ! ______________________________________________________________________
13200 MENU1: ! 
13400   let fntos(sn$="prwkmcmp")
13600   let respc=0
13800   let fnlbl(1,47," ",1,1)
14000   let fnlbl(1,1,"Beginning Date to Analyze:",30,1)
14200   let fntxt(1,34,12,0,0,"3",0,"")
14400   let resp$(respc+=1)=str$(beg_date)
14600   let fnlbl(2,1,"Ending Date to Analyze:",30,1)
14800   let fntxt(2,34,12,0,0,"3",0,"")
15000   let resp$(respc+=1)=str$(end_date)
15200   let fncmdset(2)
15400   let fnacs(sn$,0,mat resp$,ck)
15600   if ck=5 then goto XIT
15800   beg_date=val(resp$(1)) ! beginning of year
16000   let end_date=val(resp$(2)) ! ending day of year
16200   fnGetPayrollDates(beg_date,end_date)
16800   open #h_rpmstr:=1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
17000   execute "Index "&env$('Q')&"\PRmstr\Department.h"&env$('cno')&' '&env$('Q')&"\PRmstr\DeptIdx4.h"&env$('cno')&" 50/1/9 2/8/3,Replace,DupKeys,Shr -n" ! index in workmans comp code,department #,employee # sequence
17200   open #h_department:=2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx4.h"&env$('cno')&",Shr",internal,input,keyed  ! open in workmans comp code sequence
17400   open #h_payrollchecks:=4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CheckIdx.h"&env$('cno')&",Shr",internal,input,keyed 
17600   let fnopenprn
17800   let fn_hdr
18000   do 
18200     read #h_department,using 'form pos 1,n 8,n 3,pos 50,n 2': teno,tdept,workman_comp eof EO_DEPARTMENT
18300 ! pr #255: '     *Read h_department: teno='&str$(teno)&', workman_comp='&str$(workman_comp)
18302 !  if teno=13 and tdept=21 then pause
18400     let is_first=1
18600     if workman_comp=0 then let workman_comp=99 ! default to 99 if no code entered
18650     if prev_comp<>0 and prev_comp<>workman_comp then let fn_sub_total : let prev_comp=workman_comp : let heno=teno ! subtotal any time codes change !
18800     checkkey$=cnvrt$("pic(zzzzzzz#)",teno)&cnvrt$("pic(zz#)",tdept)&cnvrt$("pd 6",beg_date) ! index employee#,department# and payroll date
18810 ! checkkey$=cnvrt$("pic(zzzzzzz#)",teno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
19000     restore #h_payrollchecks,key>=checkkey$: nokey NEXT_DEPARTMENT
19020 ! pr #255: '     *restore h_payrollchecks: '&checkkey$
19100     do 
19200       read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof NEXT_DEPARTMENT
19204 !   if heno=teno and tdept><tdn then let fn_print_accumulated : goto NEXT_DEPARTMENT
19300       if heno<>teno or tdept<>tdn then 
19350 !         pr #255: '(A)'
19400         let fn_print_accumulated
19600         goto NEXT_DEPARTMENT
19700       end if  ! heno<>teno
19800       if prd=>beg_date and prd<=end_date then ! this year
20000 !     if prev_comp<>0 and prev_comp<>workman_comp then let fn_sub_total ! subtotal any time codes change !
20200         if is_first=0 then 
20400           if prev_comp=workman_comp and holdeno=teno then 
20520 !         pr #255: ' *(c) read h_payrollchecks: heno='&str$(heno)&',tdn='&str$(tdn)&',prd='&str$(prd)&',ckno='&str$(ckno)
20600             let fn_add_wages
20800 !       else 
20900 !         pr #255: '(B)'
21000 !         let fn_print_accumulated ! goto NEXT_DEPARTMENT ! same employee and same code else pr it
21200           end if 
21400         else 
21600           let is_first=0
21800           let prev_comp=workman_comp
22000           let holdeno=teno
22020 !        pr #255: ' *(d) read h_payrollchecks: heno='&str$(heno)&',tdn='&str$(tdn)&',prd='&str$(prd)&',ckno='&str$(ckno)
22200           let fn_add_wages
22400         end if  ! is_first=0   /   else 
22600       end if 
22800     loop 
23000 NEXT_DEPARTMENT: ! 
23020     let fn_print_accumulated
23400   loop 
23600 EO_DEPARTMENT: ! 
23700   let fn_print_accumulated
23800   if sum(subtot)>(0) then let fn_sub_total
24000   pr #255,using F_TOTAL: "Total",tottot(1),tottot(2)
24200 FINIS: ! 
24400   close #h_rpmstr: ioerr L900
24600 L900: close #h_department: ioerr L910
24800 L910: close #3: ioerr L920
25000 L920: pr #255: newpage
25100   close #h_payrollchecks: 
25200   let fncloseprn
25400 XIT: ! 
25600   let fnxit
25800 PGOFLOW: ! 
26000   pr #255: newpage
26200   let fn_hdr
26400   continue 
26600   def fn_add_wages
26800     let totwage=totwage+tcp(31) ! TOTAL WAGES FOR period
27000     let wcwage=wcwage+tdc(6) ! TOTAL W/C WAGES FOR period
27200   fnend 
27400   def fn_print_accumulated
27600     let pe_eno$=lpad$(str$(holdeno),8)
27800     form pos 1,n 8
27802     if totwage=0 and wcwage=0 then goto SET_NEXT
28004     let ename$=''
28020     read #h_rpmstr,using 'form pos 9,c 30',key=pe_eno$: ename$ nokey PA_CONTINUE
28200 PA_CONTINUE: ! 
28220     pr #255,using L620: ename$,tdept,prev_comp,totwage,wcwage pageoflow PGOFLOW
28400 L620: form pos 1,c 30,pos 31,pic(zz#),pos 36,pic(zz),pos 38,pic(--,---,---.##),pos 52,pic(--,---,---.##)
28600     let subtot(1)=subtot(1)+totwage
28800     let subtot(2)=subtot(2)+wcwage
29000 SET_NEXT: let totwage=0
29200     let wcwage=0
29400     let prev_comp=workman_comp
29600     let holdeno=teno
29800   fnend 
30000 ! ______________________________________________________________________
30200   def fn_sub_total
30400     pr #255,using F_TOTAL: "Subtotal",subtot(1),subtot(2)
30600 F_TOTAL: form pos 6,c 8,pos 37,pic(---,---,---.##),pos 51,pic(---,---,---.##),skip 2
30800     mat tottot=tottot+subtot
31000     mat subtot=(0)
31200   fnend 
31400   def fn_hdr
31600     pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
31800     pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
32000     pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
32200     pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
32400     pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
32600     pr #255: "\ql   "
32800     pr #255,using L1040: "Workmans                Workmans"
33000 L1040: form pos 33,c 32
33200     pr #255,using L1060: "Comp       Total        Comp"
33400 L1060: form pos 35,c 28
33600     pr #255,using L1080: "Employee Name","Dept","Code","Wages","Wages"
33800 L1080: form pos 1,c 13,pos 30,c 4,pos 35,c 4,pos 46,c 5,pos 59,c 5,skip 2
34000   fnend 
81140 ! ______________________________________________________________________
81150 ! <Updateable Region: ERTN>
81160 ERTN: let fnerror(program$,err,line,act$,"xit")
81170   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
81180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
81190   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
81200 ERTN_EXEC_ACT: execute act$ : goto ERTN
81210 ! /region
81220 ! ______________________________________________________________________
