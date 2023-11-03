autoLibrary
on error goto Ertn
fnTop(program$)
open #hPrimary=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx1.h[cno],Use,RecL=84,KPs=1,KLn=11,Shr',i,outIn,k
open #2: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx2.h[cno],Use,RecL=84,KPs=28/1,KLn=8/11,Shr',i,outIn,k
open #3: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx3.h[cno],Use,RecL=84,KPs=16/12/4,KLn=2/4/8,Shr',i,outIn,k
! r: Layout
	dim lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),mask(11),sln(11),c$(11,8)*256
	pointtwo=32 : number=30 : mmddyy=1 ! for masks only
	ic=0 ! temporary Item Counter
	! fields:                                            length             typ      storage length    storage pos      mask
	lbl$(ic+=1)='Bank'                         	: tln(ic)= 2 : fltyp$(ic)='C' 	: sln(ic)= 2  	: p$(ic)= '1'	: mask(ic)=0
	lbl$(ic+=1)='Transaction Type'            	: tln(ic)= 1 : fltyp$(ic)='C' 	: sln(ic)= 1  	: p$(ic)= '3'	: mask(ic)=0
	lbl$(ic+=1)='Check/Reference Number'     	: tln(ic)= 8 : fltyp$(ic)='Cr'	: sln(ic)= 8  	: p$(ic)= '4'	: mask(ic)=0
	lbl$(ic+=1)='Check Date'                  	: tln(ic)= 8 : fltyp$(ic)='G' 	: sln(ic)= 6  	: p$(ic)='12'	: mask(ic)=mmddyy
	lbl$(ic+=1)='Amount'                       	: tln(ic)=19 : fltyp$(ic)='PD'	: sln(ic)=10.2 	: p$(ic)='18' 	: mask(ic)=pointtwo
	lbl$(ic+=1)='Payee'                        	: tln(ic)= 8 : fltyp$(ic)='Cr'	: sln(ic)= 8  	: p$(ic)='28'	: mask(ic)=0
	lbl$(ic+=1)='Name/Description'            	: tln(ic)=35 : fltyp$(ic)='C' 	: sln(ic)=35  	: p$(ic)='36'	: mask(ic)=0
	lbl$(icPost=ic+=1)='Posting Code'         	: tln(ic)= 1 : fltyp$(ic)='N' 	: sln(ic)= 1  	: p$(ic)='71'	: mask(ic)=number
	lbl$(ic+=1)='Statement Date Cleared'     	: tln(ic)= 8 : fltyp$(ic)='N' 	: sln(ic)= 6  	: p$(ic)='72'	: mask(ic)=mmddyy
	lbl$(icSource=ic+=1)='Source Code'       	: tln(ic)= 1 : fltyp$(ic)='N' 	: sln(ic)= 1  	: p$(ic)='78'	: mask(ic)=number
	lbl$(ic+=1)='(unknown)'                    	: tln(ic)= 6 : fltyp$(ic)='C' 	: sln(ic)= 6  	: p$(ic)='79'	: mask(ic)=0

	! ** Combo Boxes **
		! xf=Field Number  : C$(xf,1)='ComboF'
		! C$(xf,2)=Linked File Name
		! C$(xf,3)=Key Position         : C$(xf,4)=Key Length
		! C$(xf,5)=Description Position : C$(xf,6)=Description Length
		! C$(xf,7)=Index File
		! C$(xf,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
	xf=1 : c$(xf,1)='ComboF'
	c$(xf,2)='[Q]\CLmstr\BankMstr.h[cno]'
	c$(xf,3)='1' : c$(xf,4)='2'
	c$(xf,5)='3' : c$(xf,6)='30'
	c$(xf,7)='[Q]\CLmstr\BankIdx1.h[cno]'
	c$(xf,8)=limit_to_list$
	xf=2 : c$(xf,1)='ComboF'
	c$(xf,2)='S:\Core\Data\TransactionType.dat'
	c$(xf,3)='1' : c$(xf,4)='1'
	c$(xf,5)='2' : c$(xf,6)='25'
	c$(xf,7)='S:\Core\Data\TransactionType.idx'
	c$(xf,8)=limit_to_list$
	xf=6 : c$(xf,1)='ComboF'
	c$(xf,2)='[Q]\CLmstr\PayMstr.h[cno]'
	c$(xf,3)='1' : c$(xf,4)='8'
	c$(xf,5)='9' : c$(xf,6)='30'
	c$(xf,7)='[Q]\CLmstr\PayIdx1.h[cno]'
	c$(xf,8)=limit_to_list$
	xf=icPost ! 9-1
		c$(xf,1)='ComboF'
	c$(xf,2)='S:\Core\Data\Checkbook\PostingCode.dat'
	c$(xf,3)='1' : c$(xf,4)='1'
	c$(xf,5)='2' : c$(xf,6)='25'
	c$(xf,7)='S:\Core\Data\Checkbook\PostingCode.idx'
	c$(xf,8)=limit_to_list$
	xf=icSource ! 11-1
		c$(xf,1)='ComboF'
	c$(xf,2)='S:\Core\Data\Checkbook\SourceCode.dat'
	c$(xf,3)='1' : c$(xf,4)='1'
	c$(xf,5)='2' : c$(xf,6)='25'
	c$(xf,7)='S:\Core\Data\Checkbook\SourceCode.idx'
	c$(xf,8)=limit_to_list$
! /r

fnHamster('Transaction',mat lbl$,mat tln,hPrimary,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
goto Xit


Xit: fnXit
include: ertn
