! formerly S:\acsGL\PriorPeriodAdj
! -- Enter Prior Period Adjustments
 
autoLibrary
on error goto Ertn
fnTop(program$)
dim dat$*20
fndat(dat$)
open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative
read #20,using "Form pos 384,N 2",rec=1: nap
close #20:
open #hGlmstr:=fngethandle:  "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed
open #hAcTrans:=fngethandle: "Name=[Q]\GLmstr\ACTRANS.h[cno],KFName=[Q]\GLmstr\ACTRIDX.h[cno],Shr",internal,outIn,keyed
 
do ! r: main loop
	dim resp$(10)*80
	resp$(2)=""
	fnTos
	lc=0
	mylen=40 : mypos=mylen+2
	fnLbl(lc+=1,1,"General Ledger Number:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(1)=""
	fnLbl(lc+=1,1,"Adjustment Amount:",mylen,1)
	fnTxt(lc,mypos,12,0,0,'pointtwo')
	fnLbl(lc+=1,1,"Date:",mylen,1)
	fnTxt(lc,mypos,0,0,0,'1')
	lc+=1
	fnLbl(lc+=1,1,"First Period Affected:",mylen,1)
	fnComboF('Period',lc,mypos,0,"[Q]\GLmstr\Period.h[cno]",1,2,3,25,"[Q]\GLmstr\Period-Idx.h[cno]",1)
	fnLbl(lc+=1,1,"First Year Affected:",mylen,1)
	fnComboF('Year',lc,mypos,0,"[Q]\GLmstr\Year.h[cno]",1,1,2,7,"[Q]\GLmstr\Year-Idx.h[cno]",1)
	lc+=1
	fnLbl(lc+=1,1,"Last Period Affected:",mylen,1)
	fnComboF('Period',lc,mypos,0,"[Q]\GLmstr\Period.h[cno]",1,2,3,25,"[Q]\GLmstr\Period-Idx.h[cno]",1)
	fnLbl(lc+=1,1,"Last Year Affected:",mylen,1)
	fnComboF('Year',lc,mypos,0,"[Q]\GLmstr\Year.h[cno]",1,1,2,7,"[Q]\GLmstr\Year-Idx.h[cno]",1)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then
		goto Finis
	else
		k$=fnagl$(resp$(1))
		am=val(resp$(2))
		d1=val(resp$(3))
		dim fm(4)
		fm(1)=val(resp$(4)(1:2))
		fm(2)=val(resp$(5)(1:2))
		fm(3)=val(resp$(6)(1:2))
		fm(4)=val(resp$(7)(1:2))
		if val(k$)=0 and am=0 then
			goto Finis
		end if
	end if
	if fm(2)=1 then fm2$="C" else fm2$="P"
	if fm(4)=1 then fm4$="C" else fm4$="P"
	
	if ~prnOpen then
		fnopenprn
		prnOpen=1
	end if
	gosub HDR
	dim d$*50
	dim bc(13)
	dim bp(13)
	read #hGlmstr,using 'Form POS 13,C 50,POS 81,41*PD 6.2',key=k$: d$,bb,cb,mat bc,mat bp
	ce=0
	pr #255,using 'Form POS 9,PIC(ZZZ),X 6,PIC(ZZZZZZ),X 9,PIC(ZZZ),X 4,C 35,X 1,N 2,X 1,C 1,X 11,N 2,X 1,C 1,X 4,N 11.2': val(k$(1:3)),val(k$(4:9)),val(k$(10:12)),d$(1:35),fm(1),fm2$,fm(3),fm4$,am pageoflow PGOF
	if am>0 then am1=am1+am else am2=am2+am
	if fm(2)=1 then
		first=fm(1)
	else
		if fm(4)=1 then last=nap else last=fm(3)
		for j=fm(1) to last
			bp(j)=bp(j)+am
		next j
		if fm(4)=2 then goto DoWriting
		first=1
	end if
	for j=first to fm(3)
	 bc(j)=bc(j)+am
	next j
	bb=bb+am
	cb=cb+am
	DoWriting: !
	rewrite #hGlmstr,using 'Form POS 81,41*PD 6.2',key=k$: bb,cb,mat bc,mat bp
	if fm(2)=1 then  ! CURRENT YEAR ONLY
		write #hAcTrans,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': k$,d1,am,3,0,"PPAJ"&date$,"Prior Period Adjustment",fm(1)
	end if
loop ! /r
Finis: ! r:
	close #hGlmstr:
	close #hAcTrans:
	pr #255,using 'Form pos 5,c 18,n 12.2': '   Total Debits: ',am1
	pr #255,using 'Form pos 5,c 18,n 12.2': '  Total Credits: ',am2
	pr #255,using 'Form pos 5,c 18,n 12.2': 'Net Adjustments: ',am1+am2
	fncloseprn : prnOpen=0
	fnIndex('[Q]\GLmstr\AcTrans.h[cno]','[Q]\GLmstr\AcTrIdx.h[cno]','1/71/17/13 12/2/2/4')
goto Xit ! /r
HDR: ! r:
	pr #255,using 'Form Pos 20,Cc 40': env$('cnam')
	pr #255,using 'Form Pos 20,Cc 40': env$('program_caption')
	pr #255,using 'Form Pos 20,Cc 40': dat$
	pr #255: ""
	dim scr$(8)*20
	pr #255,using 'Form POS 4,C 12,X 2,C 12,C 13,POS 43,C 12,POS 73,C 60': scr$(1),scr$(2),scr$(3),"Description","1st Month/Yr   Last Month/Yr   Amount"
return ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
Xit: !
fnXit
include: Ertn
