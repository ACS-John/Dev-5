autoLibrary
on error goto Ertn
fnTop(program$)
 
! r: build_layout
	ic=0 ! temporary Item Counter      
	number=30
	dim lbl$(6)*38,tln(6),p$(6)*160,fltyp$(6),sln(6),mask(6)
  !  Field Labels                                          	: Display Len : Field Type     : Storage Len : mask
	lbl$(ic+=1)='Fund Number'                              	: tln(ic)=3  : fltyp$(ic)='N' : sln(ic)= 3 : mask(ic)=1000+number
	lbl$(ic+=1)='Description'                              	: tln(ic)=30 : fltyp$(ic)='C' : sln(ic)=30 : mask(ic)=0
	lbl$(ic+=1)='General Ledger Number Due To'           	: tln(ic)=9  : fltyp$(ic)='C' : sln(ic)= 9 : mask(ic)=0
	lbl$(ic+=1)='General Ledger Number Due From'         	: tln(ic)=9  : fltyp$(ic)='C' : sln(ic)= 9 : mask(ic)=0
	lbl$(ic+=1)='General Ledger Number Accounts Payable'	: tln(ic)=12 : fltyp$(ic)='C' : sln(ic)=12 : mask(ic)=0
	lbl$(ic+=1)='General Ledger Number for Discounts'    	: tln(ic)=12 : fltyp$(ic)='C' : sln(ic)=12 : mask(ic)=0

! ** Combo Boxes **
	dim c$(6,8)*40
	! CL=Field Number  : C$(CL,1)='ComboF'
	! C$(CL,2)=Linked File Name
	! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
	! C$(CL,5)=Description Position : C$(CL,6)=Description Length
	! C$(CL,7)=Index File
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
	cl=3: c$(cl,1)='ComboF'
	c$(cl,2)='[Q]\CLmstr\GLmstr.h[cno]'
	c$(cl,3)='4' : c$(cl,4)='9'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)='[Q]\CLmstr\GLIndex.h[cno]' : c$(cl,8)=limit_to_list$
	cl=4: c$(cl,1)='ComboF'
	c$(cl,2)='[Q]\CLmstr\GLmstr.h[cno]'
	c$(cl,3)='4' : c$(cl,4)='9'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)='[Q]\CLmstr\GLIndex.h[cno]' : c$(cl,8)='1'
	cl=5: c$(cl,1)='ComboF'
	c$(cl,2)='[Q]\CLmstr\GLmstr.h[cno]'
	c$(cl,3)='1' : c$(cl,4)='12'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)='[Q]\CLmstr\GLIndex.h[cno]' : c$(cl,8)='1'
	cl=6: c$(cl,1)='ComboF'
	c$(cl,2)='[Q]\CLmstr\GLmstr.h[cno]'
	c$(cl,3)='1' : c$(cl,4)='12'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)='[Q]\CLmstr\GLIndex.h[cno]' : c$(cl,8)='1'
! /r
open #hGc=fnH: 'Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Use,RecL=75,KPs=1,KLn=3,Shr',i,outIn,k
fnHamster('GLControl',mat lbl$,mat tln,hGc,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)

! r: Fix Gl Accounts  
	! left pad general ledger number and reference number
	restore #hGc:
	do
		read #hGc, using 'form pos 1,n 3,pos 34,2*c 9,2*c 12': fund,glf$,glT$,gl1$,gl2$ eof FgaEoF
		! pr 'glF$ read:'&glf$
		! pause
		glF$=lpad$(str$(fund),3)&lpad$(rtrm$(glF$),9)
		! pr 'glF$+fund:'&glf$
		glT$=lpad$(str$(fund),3)&lpad$(rtrm$(glT$),9)
		! glT$=lpad$(str$(fund),3)&glT$
		glF$=fncleangl$(glF$)(4:12)
		! pr 'glF$cln 4+:'&glf$
		! pause
		glT$=fncleangl$(glT$)(4:12)
		gl1$=fncleangl$(gl1$)
		gl2$=fncleangl$(gl2$)
		rewrite #hGc, using 'form pos 1,n 3,pos 34,2*c 9,2*c 12': fund,glf$,glT$,gl1$,gl2$
	loop
	FgaEoF: !
! /r
close #hGc:
goto Xit

 
Xit: fnXit
 
include: ertn
