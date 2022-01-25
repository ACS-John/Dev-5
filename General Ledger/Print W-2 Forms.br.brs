! r: setup
autoLibrary
on error goto Ertn

dim ss$*11,xs(13),xt(13)
dim state$*2,a$(3)*40,b$(2)*12,controlNumber$*12,desc$(6)*15
dim m(36),w(13)
dim tmpMsgLine$(0)*256

dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
dim k$(3)*30

fnTop(program$)

open #1: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,outi,r
read #1,using 'form pos 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2',rec=1: ficarate,ficawage,feducrat,feducwag
close #1:
ficarate=ficarate/100
feducrat=feducrat/100
open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i
read #hCompany,using 'form pos 1,3*C 40,2*C 12,pos 618,40*N 1': mat a$,mat b$,mat dedcode,mat dedfed,mat dedfica,mat dedst
close #hCompany:
for j=1 to 3 : a$(j)=a$(j)(1:30) : next j
for j=1 to 4
	fl1$(j)=str$(j+7)&',28,N 1,UT,N'
	fl1$(j+4)=str$(j+7)&',37,N 1,UT,N'
next j
dim w2laser_output_filename$*256

dim w2Copy$*68

dim W2CopyFile$(6)*128,w2ssnMask(6)
W2CopyFile$(1)='xs:\Core\pdf\2016\W-2\Copy A.pdf' : w2ssnMask(1)=0
W2CopyFile$(2)='xs:\Core\pdf\2016\W-2\Copy 1.pdf' : w2ssnMask(2)=0
W2CopyFile$(3)='xs:\Core\pdf\2016\W-2\Copy B.pdf' : w2ssnMask(3)=0
W2CopyFile$(4)='xs:\Core\pdf\2016\W-2\Copy C.pdf' : w2ssnMask(4)=1
W2CopyFile$(5)='xs:\Core\pdf\2016\W-2\Copy 2.pdf' : w2ssnMask(5)=1
W2CopyFile$(6)='xs:\Core\pdf\2016\W-2\Copy D.pdf' : w2ssnMask(6)=1


! /r
AskInfo: !
	if ~fnask_w2_info(taxYear$,unusedmaybe_beg_date,unusedmaybe_end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,state$,enableAskCLocality:=1,cLocality$) then goto Xit
! r: open export files (if appropriate, return to AskInfo screen if failure
if exportFormatID then
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[CompanyNumber]',env$('cno'))
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[companynumber]',env$('cno'))
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[COMPANYNUMBER]',env$('cno'))
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[TaxYear]',taxYear$)
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[taxyear]',taxYear$)
	w2laser_output_filename$=srep$(w2laser_output_filename$,'[TAXYEAR]',taxYear$)
	open #hExport=fnH: 'Name='&br_filename$(w2laser_output_filename$)&',REPLACE',d,o ioerr AskInfo
end if
! /r
! r: open files, initialize output, etc
if exportFormatID=0 then
	fnpa_open('',w2Copy$,'PDF')
end if
open #hEmployee=fnH: 'Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr',i,i,k
box16=0
open #hW2Box16=fnH: 'Name=[Q]\GLmstr\W2Box16.h[cno],KFName=[Q]\GLmstr\W2Index.h[cno],Shr',i,i,k ioerr w2b16openfail
box16=1
w2b16openfail: !
cLocality$='NO'
w2printCount=0
! /r
ReadEmployee: ! r:
	do
		read #hEmployee,using 'form pos 1,n 4,3*c 25,c 11,36*pd 5.2': eno,mat k$,ss$,mat m eof Finis
		if endnumb>0 and eno>endnumb then goto Finis ! past ending #
		! L870: !
		fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
		kz$=lpad$(str$(eno),8)
		if m(1)=0 then goto ReadEmployee
		dedfed=dedfica=dedst=0
		for j=1 to 10
			if dedcode(j)=1 then
				if dedfed(j)=1 then dedfed=dedfed+m(j*2+9)
				if dedfica(j)=1 then dedfica=dedfica+m(j*2+9)
				if dedst(j)=1 then dedst=dedst+m(j*2+9)
			end if
		next j
		if eno=0 then goto ReadEmployee
		controlNumber$=str$(eno)
		empId$=b$(1) ! FEDERAL ID #
		stcode$=b$(2) ! STATE ID #
		w(1)=m(3) ! FED W/H YTD
		w(2)=m(1)-dedfed ! TOTAL TAXABLE WAGES
		w(3)=m(5) ! FICA W/H YTD
		w(4)=m(35) ! EIC TOTAL
		w(5)=m(1)-m(31)-dedfica ! TOTAL SS WAGES
		if m(5)=0 then w(5)=0 ! no ss wages if no ss wh
		w(6)=m(31) ! FICA TIPS YTD
		w(11)=w(5)+w(6) ! medicare wages = ss wages + tips
		w(7)=m(7) ! STATE WH
		w(9)=m(1)-dedst ! STATE WAGES
		if m(9)=0 then ! NO LOCAL WH
			printLocality$=''
		else
			w(8)=m(9) ! LOCAL WITHHOLDING
			w(10)=m(1)-dedst ! LOCAL WAGES
			if uprc$(cLocality$)='YES' then gosub AskEmpLocality
			printLocality$=empLocality$ ! LOCALITY NAME
		end if
		if box16=1 then gosub Box16process
		w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
		w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
		sswh=min(round((w(5)+w(6))*ssrate,2),w(3))
		w(12)=w(3)-sswh ! MEDICARE WH
		w(3)=sswh ! SOCIAL SECURITY WITHHELD
		if exportFormatID=1 then
			nqp=amt(1)+amt(2)
			gosub ExportAms
			x$=''
			! else if exportFormatID=2 then
			!   gosub EXPORT_CPS ! removed access 01/03/2017
		else
			gosub PrintW2
		end if
		mat xs=xs+w
		mat desc$=('')
		box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
		mat amt=(0)
		wctr=wctr+1
		mat w=(0)
	loop ! /r
Finis: ! r:
close #hEmployee: ioerr ignore
close #hW2Box16: ioerr ignore
if ~exportFormatID then
	mat xt=xt+xs
	box12aCode$=''
	box12aAmt$=cnvrt$('Nz 10.2',xt(13))
	mat w=xt
	controlNumber$='FINAL TOTAL'
	nameFirst$=nameMiddle$=nameLast$=''
	mat k$=('')
	ss$=stcode$=printLocality$=''
	x$=' '
	gosub PRINTW2
	fnpa_finis
end if
if enableW3$='True' then let fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
if exportFormatID then
	mat tmpMsgLine$(2)
	tmpMsgLine$(1)='Export file created:'
	tmpMsgLine$(2)=os_filename$(file$(hExport))
	close #hExport:
	fnmsgbox(mat tmpMsgLine$,resp$) ! ,16+4)
	goto Xit
end if
goto Xit ! /r
Xit: fnXit
AskEmpLocality: ! r:
	fnTos
	rc=0
	mylen=30
	mypos=mylen+3
	fnLbl(1,1,k$(1),mylen,1,0,0)
	fnLbl(2,1,'Locality Name:',mylen,1,0,0)
	fnTxt(2,mypos,12,0,1,'',0,'Enter the Locality for this employee.',0)
	resp$(rc+=1)=empLocality$
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('E&xit',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	empLocality$=resp$(1)
	! controlNumber$=rtrm$(controlNumber$)
	! if controlNumber$='1' then goto L2770
	! empLocality$=controlNumber$
	! L2770: !
return ! /r
Box16process: ! r: Box 16
	! passed hW2Box16,kz$, mat w, etc
	! if trim$(kz$)='14' then pause
	dim in4$(30)
	dim fw2box16$*255
	fw2box16$='form  pos 1,C 8'&rpt$(',C 12,G 10.2,3*G 1',6)
	read #hW2Box16,using fw2box16$,key=rpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey b16ReadLPad
	goto b16PastRead
	b16ReadLPad: !
	read #hW2Box16,using fw2box16$,key=lpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey B16Finis
	b16PastRead: !
	for j=1 to 6
		amt(j)=val(in4$(j*5-3))
		if in4$(j*5-2)='1' then w(2)+=amt(j)
		if in4$(j*5-1)='1' then w(5)+=amt(j)
		!   if env$('client')='Washington Parrish' then goto L3760
		if in4$(j*5-1)='1' then w(11)+=amt(j)
		! L3760: !
		if in4$(j*5-0)='1' then w(9)+=amt(j)
		if in4$(j*5-2)='2' then w(2)=w(2)-amt(j)
		if in4$(j*5-1)='2' then w(5)=w(5)-amt(j)
		!   if env$('client')='Washington Parrish' then goto L3810
		if in4$(j*5-1)='2' then w(11)=w(11)-amt(j)
		! L3810: !
		if in4$(j*5-0)='2' then w(9)=w(9)-amt(j)
		if j=1 then
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&ltrm$(cnvrt$('Nz 10.2',amt(j))),15)
		else if j=2 then
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&cnvrt$('Nz 10.2',amt(j)),15)
		else if j=3 then
			box12aCode$=in4$(j*5-4)(1:2)
			box12aAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=4 then
			box12bCode$=in4$(j*5-4)(1:2)
			box12bAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=5 then
			box12cCode$=in4$(j*5-4)(1:2)
			box12cAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=6 then
			box12dCode$=in4$(j*5-4)(1:2)
			box12dAmt$=cnvrt$('Nz 10.2',amt(j))
		end if
		if (j=3 or j=4) and (in4$(j*5-4)(1:1)='D' or in4$(j*5-4)(1:1)='E' or in4$(j*5-4)(1:1)='F' or in4$(j*5-4)(1:1)='H') then w(13)=w(13)+amt(j) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
	next j
	B16Finis: !
return ! /r
ExportAms: ! r: LASER W2 FOR ADVANCED MICRO SOLUTIONS
	pr #hExport: 'ROAN='&controlNumber$
	pr #hExport: 'FEIN='&empId$
	pr #hExport: 'WAGES='&str$(w(2))
	pr #hExport: 'FITW='&str$(w(1))
	pr #hExport: 'PNAME1='&a$(1)
	pr #hExport: 'PNAME2='
	pr #hExport: 'SSWAGES='&str$(w(5))
	pr #hExport: 'SSWH='&str$(w(3))
	pr #hExport: 'PADDR1='&a$(2)
	pr #hExport: 'PADDR2='&a$(3)
	pr #hExport: 'MCWAGES='&str$(w(11))
	pr #hExport: 'MCWH='&str$(w(12))
	pr #hExport: 'SSN='&srep$(ss$,' ','')
	pr #hExport: 'SSTIPS='&str$(w(6))
	pr #hExport: 'ALLOCATIP='  ! 'ALLOCATIP=';0
	pr #hExport: 'RNAME1='&(rtrm$(nameLast$)&','&nameFirst$)(1:24)
	pr #hExport: 'RNAME2='&(k$(2)(1:24))
	! pr #hExport: 'AEIC=';w(4)    ! this field is no longer supported 1/4/2017
	pr #hExport: 'DEPDCARE='&str$(dcb)
	pr #hExport: 'RADDR1='
	pr #hExport: 'RADDR2='&(k$(3)(1:24))
	pr #hExport: 'LAB14A='
	pr #hExport: 'BOX14A=0'
	pr #hExport: 'LAB12A='&box12aCode$
	pr #hExport: 'BOX12A='&box12aAmt$
	pr #hExport: 'CNTRYCODE='
	pr #hExport: 'RCOUNTRY='
	pr #hExport: 'LAB14B='
	pr #hExport: 'BOX14B=0'
	pr #hExport: 'LAB12B='&box12bCode$
	pr #hExport: 'BOX12B='&box12bAmt$
	pr #hExport: 'LAB14C='
	pr #hExport: 'BOX14C=0'
	pr #hExport: 'LAB12C='&box12cCode$
	pr #hExport: 'BOX12C='&box12cAmt$
	pr #hExport: 'EESTAT=0'
	pr #hExport: 'EERETR='&retirementPlanX$
	pr #hExport: 'LAB14D='
	pr #hExport: 'BOX14D=0'
	pr #hExport: 'LAB12D='&box12dCode$
	pr #hExport: 'BOX12D='&box12dAmt$
	pr #hExport: 'EESICK=0'
	pr #hExport: 'BOX11Q='&str$(nqp)
	pr #hExport: 'NQPLANS='
	pr #hExport: 'STATE1='&state$
	pr #hExport: 'SEIN1='&stcode$
	pr #hExport: 'SWAGES1='&str$(w(9))
	pr #hExport: 'SITW1='&str$(w(7))
	pr #hExport: 'LWAGES1='&str$(w(10))
	pr #hExport: 'LITW1='&str$(w(8))
	pr #hExport: 'LOCAL1='&printLocality$
	pr #hExport: 'STATE2='
	pr #hExport: 'SEIN2='
	pr #hExport: 'SWAGES2=0'
	pr #hExport: 'SITW2=0'
	pr #hExport: 'LWAGES2=0'
	pr #hExport: 'LITW2=0'
	pr #hExport: 'LOCAL2='
	pr #hExport: 'FName='&nameFirst$(1:24)
	pr #hExport: 'LName='&nameLast$(1:24)
	pr #hExport: 'TAG='
	pr #hExport: 'EBAT='
	pr #hExport: 'PHONE='
	pr #hExport: '*'
return ! /r
PrintW2: ! r:
	w2printCount+=1
	if  w2printCount/2=int(w2printCount/2) then ! it'xs the second one on a page
		fnw2_text(bottom,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
		fnpa_newpage
	else
		if enableBackground$='True' then let fnpa_background(W2CopyFile$(w2Copy))
		fnw2_text(topmargin,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6))
	end if
return  ! /r
include: ertn
