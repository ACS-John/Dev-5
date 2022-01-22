! r: setup
	autoLibrary
	on error goto Ertn
 
	dim z$*10,txt$*40,resp$(10)*80
	fnTop(program$)
! /r
SCREEN1: ! r:
	fnTos
	fnLbl(1,1,'Bad Billing Date:',24,1)
	fnTxt(1,26,8,0,0,"1")
	resp$(1)=str$(d1)
	fnLbl(2,1,'Good Billing Date:',24,1)
	fnTxt(2,26,8,0,0,"1")
	resp$(2)=str$(d2)
	fnLbl(4,1,"Starting Account:",24,1)
	fncmbact(4,26)
	resp$(3)="[All]"
	fnLbl(5,1,"Ending Account:",24,1)
	fncmbact(5,26)
	resp$(4)="[All]"
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1))
	d2=val(resp$(2))
	z_start$=resp$(3)(1:10) : if trim$(z_start$)='[All]' then z_start$=''
	z_end$=resp$(4)(1:10) : if trim$(z_end$)='[All]' then z_end$=''
	if d1=0 or d2=0 then goto SCREEN1 ! require a date in both fields
	hd1=d1: d1=fndate_mmddyy_to_ccyymmdd(d1)
	hd2=d2: d2=fndate_mmddyy_to_ccyymmdd(d2)
goto Initialize ! /r
Initialize: ! r:
	fnAutomatedSavePoint('before')
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k
	open #hTrans1=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,outIn,k
	open #hTrans2=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",i,outIn,k
	F_UBTRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	gosub BUD1
goto READ_UBTRANS ! /r
READ_UBTRANS: ! r: main loop
	dim tg(11)
	read #hTrans1,using F_UBTRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FINIS
	if z_start$<>'' and p$<z_start$ then goto READ_UBTRANS
	if z_end$<>'' and p$>z_end$ then goto READ_UBTRANS
	if tdate=d1 and tcode=1 then
		read #1,using "form pos 296,PD 4",key=p$: f
		if f><hd1 then goto READ_UBTRANS ! skip if not Last Billing Date
		rewrite #hTrans1,using "form pos 11,n 8": d2
		rewrite #1,using "form pos 296,PD 4": hd2
		recordUpdateCount+=1
		if bud1 then gosub BUD2
	end if
	goto READ_UBTRANS
! /r
FINIS: ! r:
	close #1: ioerr ignore
	close #hTrans1: ioerr ignore
	close #hTrans2: ioerr ignore
	dim mg$(1)*128
	mat mg$(1)
	mg$(1)=str$(recordUpdateCount)&' records updated.'
	fnmsgbox(mat mg$)
goto Xit ! /r
Xit: ! r:
	if bud1 then 
		close #hBudM: ioerr ignore
		close #hBudT: ioerr ignore
	end if
fnXit ! /r
BUD1: ! r:
	bud1=0
	open #hBudM=fnH: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",i,outIn,k ioerr EoBud1
	FbudM: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	dim ba(13)
	dim badr(2)
	open #hBudT=fnH: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
	FbudT: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
	dim bt1(14,2)
	bud1=1
	EoBud1:  !
return ! /r
BUD2: ! r:
	bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
	if bud1 then
		read #hBudM,using FbudM,key=p$: x$,mat ba,mat badr nokey EoBud2
		ta1=badr(1)
		do until ta1=0
			read #hBudT,using L590,rec=ta1: x$,mat bt1,nba noRec EoBud2
			L590: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
			if bt1(1,1)=d1 then
				bt1(1,1)=bt1(1,2)=d2
				rewrite #hBudT,using FbudT,rec=ta1: mat bt1
				goto EoBud2
			end if
			ta1=nba
		loop
	end if
	EoBud2: !
return ! /r
include: ertn
