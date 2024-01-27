! r: setup
autoLibrary
on error goto Ertn
fnTop(program$)

open #1: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,outi,r
read #1,using 'form pos 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2',rec=1: ficarate,ficawage,feducrat,feducwag
close #1:
ficarate=ficarate/100
feducrat=feducrat/100
open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i
dim b$(2)*12
read #hCompany,using 'form pos 1,x 120,2*C 12,pos 618,40*N 1': mat b$,mat dedcode,mat dedFed,mat dedFica,mat dedSt
close #hCompany:

! /r
AskInfo: !
	dim state$*2
	if ~fnask_w2_info(taxYear$,ssrate,ssmax, _
		mcrate,mcmax,pn1,dc1,state$,enableAskCLocality:=1,cLocality$) _
		then goto Xit
	! r: open files, initialize output, etc

		open #hEmployee=fnH: 'Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr',i,i,k

		box16=0
		open #hW2Box16=fnH: 'Name=[Q]\GLmstr\W2Box16.h[cno],KFName=[Q]\GLmstr\W2Index.h[cno],Shr',i,i,k ioerr w2b16openfail
		Fw2box16: form  pos 1,C 8,6*(C 12,G 10.2,3*G 1)
		box16=1
		w2b16openfail: !

		cLocality$='NO'

	! /r

ReadEmployee: ! r:
	do
		dim k$(3)*30
		dim ss$*11
		dim xm(36)
		read #hEmployee,using 'form pos 1,n 4,3*c 25,c 11,36*pd 5.2': eno,mat k$,ss$,mat xm eof Finis
		! pr 'read eno '&str$(eno) : pause
		! if endnumb>0 and eno>endnumb then goto Finis ! past ending #

		dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
		fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)

		if xm(1)=0 then goto ReadEmployee
		dedFed=dedFica=dedSt=0
		for j=1 to 10
			if dedcode(j)=1 then
				if dedFed(j)=1 then dedFed+=xm(j*2+9)
				if dedFica(j)=1 then dedFica+=xm(j*2+9)
				if dedSt(j)=1 then dedSt+=xm(j*2+9)
			end if
		next j
		if eno=0 then goto ReadEmployee

		! pr 'before call to fnW2Text('&empId$&')' : pause

		dim controlNumber$*12
		controlNumber$=str$(eno)
		empId$=b$(1)              	! Federal ID #
		stcode$=b$(2)             	! State ID #
		dim w(13)
		w(1)=xm(3)                	! Fed W/H YTD
		w(2)=xm(1)-dedFed         	! Total Taxable Wages
		w(3)=xm(5)                 	! FICA W/H YTD
		w(4)=xm(35)                	! EIC Total
		w(5)=xm(1)-xm(31)-dedFica 	! Total SS Wages
		if xm(5)=0 then w(5)=0    	! no ss Wages if no ss wh
		w(6)=xm(31)               	! FICA Tips YTD
		w(11)=w(5)+w(6)           	! Medicare Wages = ss Wages + Tips
		w(7)=xm(7)                	! State WH
		w(9)=xm(1)-dedSt          	! State Wages
		if xm(9)=0 then           	! No Local WH
			printLocality$=''
		else
			w(8)=xm(9) ! Local Withholding
			w(10)=xm(1)-dedSt ! Local Wages
			if uprc$(cLocality$)='YES' then gosub AskEmpLocality
			printLocality$=empLocality$(1:6) ! Locality Name
		end if

		if box16 then gosub Box16process
		w(5)=min(ssmax-w(6),w(5)) ! SS Wages cannot exceed Maximum
		w(11)=min(mcmax,w(11)) ! MC Wages cannot exceed Maximum
		sswh=min(round((w(5)+w(6))*ssrate,2),w(3))
		w(12)=w(3)-sswh ! Medicare WH
		w(3)=sswh ! Social Security Withheld
		! pr 'calling fnW2Text('&empId$&')' : pause
		fnW2Text(ss$,controlNumber$,mat w,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$)

		! dim xs(13)
		! mat xs=xs+w
		! dim desc$(6)*15
		! mat desc$=('')
		box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
		mat amt=(0)
		! wctr+=1
		mat w=(0)
	loop ! /r
Finis: ! r:
	close #hEmployee: ioerr ignore
	close #hW2Box16: ioerr ignore
	fnW2PrintClose
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
	! passed hW2Box16,eno, mat w, etc
	! if trim$(kz$)='14' then pause
	dim in4$(30)
	kz$=lpad$(str$(eno),8)
	read #hW2Box16,using Fw2box16,key=rpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey b16ReadLPad
	goto b16PastRead
	b16ReadLPad: !
	read #hW2Box16,using Fw2box16,key=lpad$(trim$(kz$),kln(hW2Box16)): kz$,mat in4$ nokey B16Finis
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
			! desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&ltrm$(cnvrt$('Nz 10.2',amt(j))),15)
		else if j=2 then
			! desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&cnvrt$('Nz 10.2',amt(j)),15)
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
		if (j=3 or j=4) and (in4$(j*5-4)(1:1)='D' or in4$(j*5-4)(1:1)='E' or in4$(j*5-4)(1:1)='F' or in4$(j*5-4)(1:1)='H') then
			! subtotal box 17 IF D,E,F,or H codes
			w(13)+=amt(j)
		end if
	next j
	B16Finis: !
return ! /r
include: ertn
