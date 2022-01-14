! Checkbook PaidInvoice File
autoLibrary
fnTop(program$)
on error goto Ertn
! r: build layout
	dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),mask(4),sln(4)
	dim c$(4,8)*256
	! ** Field Labels            	: Text Box Length  	: Field Type     	: store len  	: field mask      
	lbl$(tc=1)='Vendor Key'   		: tln(tc)=8        	: fltyp$(tc)='C'	: sln(tc)= 8	: mask(tc)=0
	lbl$(tc=2)='Invoice Key'  	: tln(tc)=12       	: fltyp$(tc)='C'	: sln(tc)=12	: mask(tc)=0
	lbl$(tc=3)='Date Paid'     	: tln(tc)=mmddyy  	: fltyp$(tc)='G'	: sln(tc)= 6	: mask(tc)=mmddyy=1
	lbl$(tc=4)='Check Number' 	: tln(tc)=8        	: fltyp$(tc)='G'	: sln(tc)= 8	: mask(tc)=number=30


	! ** Let's Make Some Combo Boxes **
	! CL = Item you want a ComboBox on
	! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len
	! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File
	! C$(cl,8)=limit to list option '1'=yes '0'=no
	limit_to_list$='1'
	cl=1
	c$(cl,1)='ComboF'
	c$(cl,2)='[Q]\CLmstr\PayMstr.h[cno]'
	c$(cl,3)='1' : c$(cl,4)=str$(sln(cl))
	c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='30'
	c$(cl,7)='[Q]\CLmstr\PayIdx1.h[cno]' : c$(cl,8)=limit_to_list$
	! cL=2
	! c$(CL,1)='ComboF'
	! c$(CL,2)='[Q]\CLmstr\BankMstr.h[cno]'
	! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL))
	! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30'
	! c$(CL,7)='[Q]\CLmstr\BankIdx1.h[cno]' : c$(CL,8)=LIMIT_TO_LIST$
	! cL=4
	! c$(CL,1)='ComboF'
	! c$(CL,2)='[Q]\CLmstr\TrMstr.h[cno]'
	! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL))
	! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30'
	! c$(CL,7)='[Q]\CLmstr\BankIdx1.h[cno]' : c$(CL,8)=LIMIT_TO_LIST$
! /r
open #h=fnH: 'Name=[Q]\CLmstr\IvPaid.h[cno],KFName=[Q]\CLmstr\IVIndex.h[cno],Use,RecL=34,KPs=1,KLn=20,Shr',internal,outIn,keyed
fnHamster('PaidInvoice',mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
goto Xit

Xit: fnXit
include: ertn
