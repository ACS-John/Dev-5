! formerly S:\acsGL\bldstdaJ
! Standard Adjustments - Hamster (will need a conversion pgm. now only one adjustment per entry - was 10!)

autoLibrary
on error goto Ertn

dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4),sp(4),c$(4,8)*40

fnTop(program$)
! r: Build Layout
	ic=0 ! temporary Item Counter
	! ** Field Labels                 ** Storage Positions **
	lbl$(ic+=1)="Reference"      	: sp(ic)=1
	lbl$(ic+=1)="Description"    	: sp(ic)=13
	lbl$(ic+=1)="G/L Number"     	: sp(ic)=43
	lbl$(ic+=1)="Amount"         	: sp(ic)=55

	ic=0
	! ** Field Types   ** Storage Lengths ** Display Len ** Field Masks **
	fltyp$(ic+=1)='C'  : sln(ic)=12    	: tln(ic)=12  	: mask(ic)=0
	fltyp$(ic+=1)='C'  : sln(ic)=30    	: tln(ic)=30  	: mask(ic)=0
	fltyp$(ic+=1)='C'  : sln(ic)=12    	: tln(ic)=12  	: mask(ic)=0
	fltyp$(ic+=1)='PD' : sln(ic)=5.2   	: tln(ic)=10.2	: mask(ic)=32

	! ** Combo Boxes **
	clx=3 : c$(clx,1)='ComboF'
	c$(clx,2)="[Q]\GLmstr\GLmstr.h[cno]"
	c$(clx,3)="1" : c$(clx,4)="12"
	c$(clx,5)="13" : c$(clx,6)="30"
	c$(clx,7)="[Q]\GLmstr\glindex.h[cno]"
! /r
gosub OpenFile : gosub CloseFile    : gosub OpenFile
fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
gosub FixGlAccounts : gosub CloseFile : gosub Index
goto Xit

OpenFile: !
	hFile=1 ! this value is used in the CloseFile sub routine
	if exists("[Q]\GLmstr\glStdAd.h[cno]")=0 then 
		open #hFile: "Name=[Q]\GLmstr\glStdAd.h[cno],Version=1,Replace,RecL=59",internal,outIn
		gosub CloseFile
		gosub Index
	else
		if exists("[Q]\GLmstr\glstdidx.h[cno]")=0 then gosub Index
		open #hFile: "Name=[Q]\GLmstr\glStdAd.h[cno],KFName=[Q]\GLmstr\glstdidx.h[cno],Version=1,Shr",internal,outIn,keyed
	end if
return

Index: !
	fnIndex('[Q]\GLmstr\glStdAd.h[cno]','[Q]\GLmstr\glstdidx.h[cno]','1 12')
	! execute "Index [Q]\GLmstr\glStdAd.h[cno]"&' '&"[Q]\GLmstr\glstdidx.h[cno]" &" 1 12 Replace,DupKeys"
return

FixGlAccounts: ! r: left pad general ledger number and reference number
	restore #hFile:
	do
		read #hFile, using "form pos 43,c 12": gl$ eof L340
		gl$=lpad$(rtrm$(gl$),12)
		rewrite #hFile, using "form pos 43,c 12,": gl$
	loop
	L340: !
return ! /r

CloseFile: ! r:
	for j=1 to hFile
		close #j: ioerr ignore
	next j
return ! /r

Xit: fnXit

include: ertn
