! Replace S:\acsGL\AcPrReg
! -- PAYROLL REGISTER

autoLibrary
on error goto Ertn

dim k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2)
dim fa$(2),sa$(2)*40,adr(2)

fnTop(program$,"Fix Payroll Dates")

fnTos
rc=cf=0: mylen=22: mypos=mylen+3: frameno=1
fnFra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included.")
fnLbl(1,1,"Bad Date:",mylen,1,0,frameno)
fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno)
resp$(rc+=1)=str$(date_bad)
fnLbl(2,1,"Good Date:",mylen,1,0,frameno)
fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno)
resp$(rc+=1)=str$(date_good)
fnCmdKey("Next",1,1,0,"Calculate tax deposit.")
fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit

date_bad=val(resp$(1))
date_good=val(resp$(2))

open #h_prmstr=fnH: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed
open #h_acprcks=fnH: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Shr",internal,outIn,relative
fnopenprn
fn_hdr1
L350: if d(1)>0 then goto L360 else goto L390
L360: !
L390: read #h_prmstr,using 'Form POS 1,N 4,3*C 25,POS 271,2*N 5': eno,mat k$,mat adr eof FINIS
if adr(1)=0 then goto L390
ca=adr(1)
do
	read #h_acprcks,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L350
	if fndate_mmddyy_to_ccyymmdd(d(2))=date_bad then
		d(2)=date(days(date_good,'ccyymmdd'),'mmddyy')
		rewrite #h_acprcks,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca
		fn_print_1
	end if
	if nca=0 then goto L350
	ca=nca
loop
goto FINIS

def fn_header
	nametab=66-int(len(rtrm$(env$('cnam')))/2)
	pr #255,using L530: date$,time$,env$('cnam')
	L530: form pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
	p1=66-int(len(rtrm$(env$('program_caption')))/2)
	pr #255,using L560: env$('program_caption')
	L560: form pos p1,c 50
fnend
def fn_hdr1
	fn_header
	pr #255,using L630: "EmpNo    Employee Name         Gross   Fed W/H  FICA W/H    St W/H  Misc W/H   Loc W/H       Net     Tips    EIC   Date    Chk#"
	L630: form skip 1,c 132,skip 2
fnend
def fn_print_1
	pr #255,using L870: eno,k$(1)(1:19),d(4),d(5),d(6),d(7),d9,d(8),d(22),d(19),d(21),d(2),d(3) pageoflow PGOF1
	L870: form pos 1,pic(zzzz),pos 8,c 19,7*n 10.2,pic(------.##),pic(----.##),pic(zzz/zz/zz),pic(zzzzzzz),skip 1
fnend
PGOF1: !
	pr #255: newpage
	fn_hdr1
continue
FINIS: !
	close #h_acprcks: ioerr ignore
	fncloseprn
goto Xit
Xit: fnXit

include: Ertn
