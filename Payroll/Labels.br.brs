! Replace S:\acsPR\newLabel
! Payroll Labels
! r: setup
	library 'S:\Core\Library': fntop,fnxit,fnwait,fnaddlabel,fnlabel,fncomboa,fnTos,fnLbl,fnChk,fncombof,fnCmdSet,fnAcs,fnTxt,fndate_mmddyy_to_ccyymmdd,fnCmdKey
	library 'S:\Core\Library': fnPayPeriodEndingDate
	on error goto ERTN
	! ______________________________________________________________________
	dim in1(9),io1$(8),lb$(6,6)*40,ln$*260,msgline$(2)*60,response$(5)*1
	dim resp$(10)*40
	dim em$(3)*30,ss$*11,in$*1,cap$*128,log$*128
	dim labeltext$(5)*120,pt$(5),message$*40,tcp(32),tdc(10)
	dim item1$(4)*40
	dim rs(2) ! RS(1)=Race RS(2)=Sex
	dim em(17) ! see PR Master File Layout  em(4)=employment status
	! ______________________________________________________________________
	fntop(program$,cap$="Labels")
	d1=fnPayPeriodEndingDate
! /r
	gosub SCR1
	gosub OPEN_KEYED
	goto GET_STARTED
! ______________________________________________________________________
OPEN_KEYED: ! r:
	open #1: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
	L270: form pos 1,n 8,3*c 30,c 11,pos 110,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
return ! /r
GET_STARTED: ! r:
	lb1=0
	mat lb$=("")
	eno$=lpad$(str$(eno),8)
L360: if sey$="Y" then gosub ASK_EMP : goto JUST_AFTER_READ else goto READ_SEQUENTIAL
! /r
READ_SEQUENTIAL: ! r:
	foundone=0
L400: read #1,using L270: eno,mat em$,ss$,mat rs,mat em eof FINIS
	if prtall=2 and fndate_mmddyy_to_ccyymmdd(em(17))=d1 then goto JUST_AFTER_READ
	if prtall=2 and fndate_mmddyy_to_ccyymmdd(em(17))<>d1 then goto READ_SEQUENTIAL
	if date_to_select>0 and date_to_select<>d1 then gosub CHECK_FOR_OLD_DATE
	if date_to_select >0 and em(17)=0 then goto L400 ! pr new people also
	if foundone=1 then goto L470 ! found an old matching date
	if date_to_select >0 and fndate_mmddyy_to_ccyymmdd(em(17))<>date_to_select then goto L400 ! only last payroll date
L470: if empstatuse>0 and em(4)<>empstatuse then goto L400 ! based on employment status
	if starting_employee<>0 and eno< starting_employee then goto L400
JUST_AFTER_READ: ! 
	if lb1=>1 then gosub PRINT_LABEL
	lb1=lb1+1
	if empyn$="Y" then lb$(lb1,1)=str$(eno)&"  "
	if ssyn$="Y" then lb$(lb1,1)=lb$(lb1,1)&ss$&"  "
	if date_to_print>0 and ssyn$="N" then 
		lb$(lb1,1)=lb$(lb1,1)&cnvrt$("PIC(ZZ/ZZ/ZZ)",date_to_print)
	else if date_to_print>0 and ssyn$="Y" then 
		lb$(lb1,5)=lb$(lb1,5)&cnvrt$("PIC(ZZ/ZZ/ZZ)",date_to_print)
	end if
	lb$(lb1,2)=em$(1)
	if empadryn$="Y" then lb$(lb1,3)=em$(2)
	if empadryn$="Y" then lb$(lb1,4)=em$(3)
	goto L360
! /r
CHECK_FOR_OLD_DATE: ! r:
	foundone=0
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey L690
L650: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L690
	if heno<>eno then goto L690
	if prd=date_to_select then foundone=1 : goto L690 ! FOUND A MATCHING OLD PAYROLL DATE
	goto L650
L690: return ! /r
FINIS: ! r:
	close #1: ioerr ignore
	if lb1>0 then gosub PRINT_LABEL
	close #2: ioerr ignore
	fnlabel(mat pt$) 
goto XIT ! /r
XIT: fnxit
ASK_EMP: ! r: 
	respc=0
	fnTos(sn$="prlabel-2")
	fnLbl(1,1,"Employee Number to Print:",25,1)
	fncombof("Employee",1,28,20,"[Q]\PRmstr\rpmstr.h[cno]",1,8,9,20,"[Q]\PRmstr\Rpindex.h[cno]",1,0, "Select any employee number you wish printed") 
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Add this employee to list of labels to be printed.")
	fnCmdKey("&Complete",2,0,0,"Print selected labels.")
	fnAcs(sn$,0,mat resp$,ck) 
	if ck=2 then goto FINIS
	eno=val(resp$(1)(1:8))
	if eno=0 then goto FINIS
	read #1,using L270,key=lpad$(str$(eno),8): eno,mat em$,ss$,mat rs,mat em nokey ASK_EMP
	return ! /r
SCR1: ! r:
	fnTos(sn$="prlabel-1")
	respc=0 : mylen=50 : mypos=mylen+3 : right=1
	fnLbl(1,1,"Print Labels For:",mylen,right)
	fi$="cllabels" 
	item1$(print_all=1)="[All]" : all=1 
	item1$(2)="Employees from last payroll only": last_payroll=2 
	item1$(3)="Select employees to print": select_employee=3 
	item1$(4)="Select employment status to print": emp_status=4 
	fncomboa(fi$,1,mypos,mat item1$,"Print labels based on certain employment status code. (Eg. all full time).") 
	resp$(respc+=1)=item1$(1)
	fnChk(5,mypos+2,'Print Employee Number on Label:',right) 
	resp$(respc+=1)='False'
	fnChk(6,mypos+2,'Print Social Security Number on Label:',right) 
	resp$(respc+=1)='False'
	fnChk(7,mypos+2,'Print Employee Address on Label:',right) 
	resp$(respc+=1)='True'
	fnLbl(9,1,"Payroll Date to Use (if applicable):",mylen,1)
	fnTxt(9,mypos,12,0,1,"3",0,'You can pr labels for any payroll period. Only applicable if printing labels for those employees who got paid."') ! 
	resp$(respc+=1)=""
	fnLbl(10,1,"Payroll Date to pr on Label:",mylen,1)
	fnTxt(10,mypos,12,0,1,"1",0,'Used for placing a date on the label. Leave blank if not applilcable."') ! 
	resp$(respc+=1)=""
	fnLbl(11,1,"Employment Status to pr (if applicable):",mylen,1)
	fnTxt(11,mypos,2,0,1,"30",0,'Used for selectiing a specific employment status code. Leave blank if not applilcable."') ! 
	resp$(respc+=1)=""
	fnLbl(13,1,"Starting Employee Number (if applicable):",mylen,1)
	fncombof("Employee",13,mypos,20,"[Q]\PRmstr\rpmstr.h[cno]",1,8,9,20,"[Q]\PRmstr\Rpindex.h[cno]",1,0, "Select starting employee record for printing. Only applicable if not starting with first employee.") 
	resp$(respc+=1)=""
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck) 
	if ck=5 then 
		goto XIT 
	else if resp$(1)=item1$(1) then
		prtall=all 
	else if resp$(1)=item1$(2) then
		prtall=last_payroll 
	else if resp$(1)=item1$(3) then
		prtall=select_employee 
	else if resp$(1)=item1$(4) then 
		prtall=emp_status
	end if
	if resp$(2) ="True" then empyn$="Y" else empyn$="N"
	if resp$(3) ="True" then ssyn$="Y" else ssn$="N"
	if resp$(4) ="True" then empadryn$="Y" else empadryn$="N"
	date_to_select=val(resp$(5)) ! payroll date to use in selecting employees
	date_to_print=val(resp$(6)) ! payroll date to pr on label
	empstatuse=val(resp$(7)) ! employment status used as criteria
	starting_employee=val(resp$(8)(1:8)) ! starting employee #
	if prtall=3 then sey$="Y" else sey$="N" ! CHOSEN TO SELECT EMPLOYEE TO PRINT
return ! /r
PRINT_LABEL: ! r:
	mat labeltext$=("")
	for j1=1 to 5
		ln$="       "
		for j2=1 to lb1 : ln$=ln$&rpad$(lb$(j2,j1)(1:40),42) : next j2
		labeltext$(j1)=ln$
	next j1
	fnaddlabel(mat labeltext$)
	lb1=0 : mat lb$=("")
return ! /r
include: ertn