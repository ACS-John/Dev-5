! Replace S:\acsPR\newprWkMCmp
! Workmans Compensation Report
!
autoLibrary
  on error goto Ertn
!
  dim ename$*30,subtot(2),tottot(2),tqm(17),message$*40
  dim tdet(17),tdc(6),tcp(31),tdc(10)
!
  fnTop(program$)
  fnGetPayrollDates(beg_date,end_date)
 
MENU1: !
  fnTos
  respc=0
  fnLbl(1,47," ",1,1)
  fnLbl(1,1,"Beginning Date to Analyze:",30,1)
  fnTxt(1,34,12,0,0,"3",0,"")
  resp$(respc+=1)=str$(beg_date)
  fnLbl(2,1,"Ending Date to Analyze:",30,1)
  fnTxt(2,34,12,0,0,"3",0,"")
  resp$(respc+=1)=str$(end_date)
  fnCmdSet(2)
  fnAcs(mat resp$,ckey)
  if ckey=5 then goto Xit
  beg_date=val(resp$(1)) ! beginning of year
  end_date=val(resp$(2)) ! ending day of year
  fnGetPayrollDates(beg_date,end_date)
  open #hEmployee:=1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
  execute "Index [Q]\PRmstr\Department.h[cno]"&' '&"[Q]\PRmstr\DeptIdx4.h[cno] 50/1/9 2/8/3,Replace,DupKeys,Shr -n" ! index in workmans comp code,department #,employee # sequence
  open #h_department:=2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx4.h[cno],Shr",internal,input,keyed  ! open in workmans comp code sequence
  open #h_payrollchecks:=4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\CheckIdx.h[cno],Shr",internal,input,keyed
  fnopenprn
  fn_hdr
  do
    read #h_department,using 'form pos 1,n 8,n 3,pos 50,n 2': teno,tdept,workman_comp eof EO_DEPARTMENT
! pr #255: '     *Read h_department: teno='&str$(teno)&', workman_comp='&str$(workman_comp)
!  if teno=13 and tdept=21 then pause
    is_first=1
    if workman_comp=0 then workman_comp=99 ! default to 99 if no code entered
    if prev_comp<>0 and prev_comp<>workman_comp then let fn_sub_total : prev_comp=workman_comp : heno=teno ! subtotal any time codes change !
    checkkey$=cnvrt$("pic(zzzzzzz#)",teno)&cnvrt$("pic(zz#)",tdept)&cnvrt$("pd 6",beg_date) ! index employee#,department# and payroll date
! checkkey$=cnvrt$("pic(zzzzzzz#)",teno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
    restore #h_payrollchecks,key>=checkkey$: nokey NEXT_DEPARTMENT
! pr #255: '     *restore h_payrollchecks: '&checkkey$
    do
      read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof NEXT_DEPARTMENT
!   if heno=teno and tdept><tdn then let fn_print_accumulated : goto NEXT_DEPARTMENT
      if heno<>teno or tdept<>tdn then
!         pr #255: '(A)'
        fn_print_accumulated
        goto NEXT_DEPARTMENT
      end if  ! heno<>teno
      if prd=>beg_date and prd<=end_date then ! this year
!     if prev_comp<>0 and prev_comp<>workman_comp then let fn_sub_total ! subtotal any time codes change !
        if is_first=0 then
          if prev_comp=workman_comp and holdeno=teno then
!         pr #255: ' *(c) read h_payrollchecks: heno='&str$(heno)&',tdn='&str$(tdn)&',prd='&str$(prd)&',ckno='&str$(ckno)
            fn_add_wages
!       else
!         pr #255: '(B)'
!         fn_print_accumulated ! goto NEXT_DEPARTMENT ! same employee and same code else pr it
          end if
        else
          is_first=0
          prev_comp=workman_comp
          holdeno=teno
!        pr #255: ' *(d) read h_payrollchecks: heno='&str$(heno)&',tdn='&str$(tdn)&',prd='&str$(prd)&',ckno='&str$(ckno)
          fn_add_wages
        end if  ! is_first=0   /   else
      end if
    loop
NEXT_DEPARTMENT: !
    fn_print_accumulated
  loop
EO_DEPARTMENT: !
  fn_print_accumulated
  if sum(subtot)>(0) then let fn_sub_total
  pr #255,using F_TOTAL: "Total",tottot(1),tottot(2)
FINIS: !
  close #hEmployee: ioerr ignore
	close #h_department: ioerr ignore
	close #3: ioerr ignore
	pr #255: newpage
  close #h_payrollchecks:
  fncloseprn
Xit: !
  fnXit
PGOFLOW: !
  pr #255: newpage
  fn_hdr
  continue
  def fn_add_wages
    totwage=totwage+tcp(31) ! TOTAL WAGES FOR period
    wcwage=wcwage+tdc(6) ! TOTAL W/C WAGES FOR period
  fnend
  def fn_print_accumulated
    pe_eno$=lpad$(str$(holdeno),8)
    form pos 1,n 8
    if totwage=0 and wcwage=0 then goto SET_NEXT
    ename$=''
    read #hEmployee,using 'form pos 9,c 30',key=pe_eno$: ename$ nokey PA_CONTINUE
PA_CONTINUE: !
    pr #255,using L620: ename$,tdept,prev_comp,totwage,wcwage pageoflow PGOFLOW
L620: form pos 1,c 30,pos 31,pic(zz#),pos 36,pic(zz),pos 38,pic(--,---,---.##),pos 52,pic(--,---,---.##)
    subtot(1)=subtot(1)+totwage
    subtot(2)=subtot(2)+wcwage
SET_NEXT: totwage=0
    wcwage=0
    prev_comp=workman_comp
    holdeno=teno
  fnend
 
def fn_sub_total
	pr #255,using F_TOTAL: "Subtotal",subtot(1),subtot(2)
	F_TOTAL: form pos 6,c 8,pos 37,pic(---,---,---.##),pos 51,pic(---,---,---.##),skip 2
	mat tottot=tottot+subtot
	mat subtot=(0)
fnend
def fn_hdr
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	pr #255,using L1040: "Workmans                Workmans"
	L1040: form pos 33,c 32
	pr #255,using L1060: "Comp       Total        Comp"
	L1060: form pos 35,c 28
	pr #255,using L1080: "Employee Name","Dept","Code","Wages","Wages"
	L1080: form pos 1,c 13,pos 30,c 4,pos 35,c 4,pos 46,c 5,pos 59,c 5,skip 2
fnend
 
include: Ertn
 
