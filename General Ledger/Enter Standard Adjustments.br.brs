! formerly S:\acsGL\bldstdaJ
! Standard Adjustments - Hamster (will need a conversion pgm. now only one adjustment per entry - was 10!)

autoLibrary
on error goto Ertn

fnTop(program$)
! r: Build Layout
	dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4),sp(4),c$(4,8)*40
	ic=0 ! temporary Item Counter
	! ** Field Labels     ** Storage Positions **		! ** Field Types   ** Storage Lengths ** Display Len ** Field Masks **
	lbl$(ic+=1)="Reference"    	: sp(ic)= 1	:	fltyp$(ic)='C' 	: sln(ic)=12 	: tln(ic)=12  	: mask(ic)= 0
	lbl$(ic+=1)="Description"  	: sp(ic)=13	:	fltyp$(ic)='C' 	: sln(ic)=30 	: tln(ic)=30  	: mask(ic)= 0
	lbl$(ic+=1)="G/L Number"   	: sp(ic)=43	:	fltyp$(ic)='C' 	: sln(ic)=12 	: tln(ic)=12  	: mask(ic)= 0
	lbl$(ic+=1)="Amount"       	: sp(ic)=55	:	fltyp$(ic)='PD'	: sln(ic)=5.2	: tln(ic)=10.2	: mask(ic)=32

	! ** Combo Box **
	clx=3 : c$(clx,1)='ComboF'
	c$(clx,2)="[Q]\GLmstr\GLmstr.h[cno]"
	c$(clx,3)="1" : c$(clx,4)="12"
	c$(clx,5)="13" : c$(clx,6)="30"
	c$(clx,7)="[Q]\GLmstr\glindex.h[cno]"
! /r
if ~exists("[Q]\GLmstr\glStdIdx.h[cno]") then 
	fnIndex('[Q]\GLmstr\glStdAd.h[cno]','[Q]\GLmstr\glStdIdx.h[cno]','1 12')
end if
! open #hFile=fnH: "Name=[Q]\GLmstr\glStdAd.h[cno],KFName=[Q]\GLmstr\glStdIdx.h[cno],Version=1,RecL=59,Shr",internal,outIn,keyed
open #hFile=fnH: "Name=[Q]\GLmstr\glStdAd.h[cno],Version=1,KFName=[Q]\GLmstr\glStdIdx.h[cno],Use,RecL=59,KPs=1,KLn=12,Shr",internal,outIn,keyed


fnHamster("TrAlloc",mat lbl$,mat tln,hFile,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
! r: Fix Gl Accounts  
	! left pad general ledger number and reference number
	restore #hFile:
	do
		read #hFile, using "form pos 43,c 12": gl$ eof FgaEoF
		gl$=lpad$(rtrm$(gl$),12)
		rewrite #hFile, using "form pos 43,c 12,": gl$
	loop
	FgaEoF: !
! /r

close #hFile: ioerr ignore
hFile=0
Xit: fnXit

include: ertn
