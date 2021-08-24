! formerly S:\acsGL\bankreconciliation
! Access Bank Reconciliation File
! Bank Reconciliation File - Hamster
autoLibrary
on error goto Ertn

fnTop(program$)
! r: build_layout

	dim lbl$(10)*38
	dim fltyp$(10)
	dim sln(10)
	dim tln(10)
	dim mask(10)
	dim sp(10)
	dim c$(10,8)*40
	dim p$(10)*160
	
	ic=0
	! Label                   	: Field Types     	: Storage Len 	: Display Len 	: Mask       	: SPos     
	lbl$(ic+=1)='Bank G/L'   	: fltyp$(ic)='CR'	: sln(ic)=12  	: tln(ic)=12  	: mask(ic)= 0	: sp(ic)=79
	lbl$(ic+=1)='T/C'        	: fltyp$(ic)= 'N'	: sln(ic)= 1  	: tln(ic)= 1  	: mask(ic)=30	: sp(ic)= 3
	lbl$(ic+=1)='Ref'        	: fltyp$(ic)= 'C'	: sln(ic)= 8  	: tln(ic)= 8  	: mask(ic)= 0	: sp(ic)= 4
	lbl$(ic+=1)='Date'       	: fltyp$(ic)= 'N'	: sln(ic)= 6  	: tln(ic)= 8  	: mask(ic)= 1	: sp(ic)=12
	lbl$(ic+=1)='Amount'     	: fltyp$(ic)='PD'	: sln(ic)=10.2	: tln(ic)=10.2	: mask(ic)=10	: sp(ic)=18
	lbl$(ic+=1)='Payee'      	: fltyp$(ic)= 'C'	: sln(ic)= 8  	: tln(ic)= 8  	: mask(ic)= 0	: sp(ic)=28
	lbl$(ic+=1)='Description'	: fltyp$(ic)= 'C'	: sln(ic)=35  	: tln(ic)=35  	: mask(ic)= 0	: sp(ic)=36
	lbl$(ic+=1)='P/C'        	: fltyp$(ic)= 'N'	: sln(ic)= 1  	: tln(ic)= 1  	: mask(ic)=30	: sp(ic)=71
	lbl$(ic+=1)='Cleared'    	: fltyp$(ic)= 'N'	: sln(ic)= 6  	: tln(ic)= 8  	: mask(ic)= 1	: sp(ic)=72
	lbl$(ic+=1)='S/C'        	: fltyp$(ic)= 'N'	: sln(ic)= 1  	: tln(ic)= 1  	: mask(ic)=30	: sp(ic)=78

	! ** Combo Box **
	cl=1 : c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]"
	c$(cl,3)="1" : c$(cl,4)="12"
	c$(cl,5)="13" : c$(cl,6)="30"
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]"
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)

	! ** Combo Box **
	cl=2 : c$(cl,1)='ComboF'
	c$(cl,2)='S:\Core\Data\TransactionCode.dat' ! "[Q]\GLmstr\transcode.h[cno]"
	! c$(cl,3)="1" : c$(cl,4)="2"
	c$(cl,3)="1" : c$(cl,4)="1"
	! c$(cl,5)="3" : c$(cl,6)="30"
	c$(cl,5)="2" : c$(cl,6)="18"
	c$(cl,7)='S:\Core\Data\TransactionCode.idx' ! "[Q]\GLmstr\transcode-idx.h[cno]"
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)

! /r
! r: open_file
	if ~exists("[Q]\GLmstr\bankrec.h[cno]") then 
		open #h=fnH: "Name=[Q]\GLmstr\bankrec.h[cno],Version=1,Replace,RecL=91",internal,outIn
		close #h: ioerr ignore
		gosub Index
	end if
	if ~exists("[Q]\GLmstr\glstdidx.h[cno]") then 
		gosub Index
	end if
	open #h=fnH: "Name=[Q]\GLmstr\bankrec.h[cno],KFName=[Q]\GLmstr\bankrec-idx.h[cno],Version=1,Shr",internal,outIn,keyed
! /r
fnHamster("bankrec",mat lbl$,mat tln,h,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
! FixLeftPads: ! r: left pad general ledger number and reference number
	restore #h:
	do
		read #h, using "form pos 43,c 12,pos 4,c 8": gl$,tr$ eof EoH
		gl$=lpad$(rtrm$(gl$),12)
		tr$=lpad$(rtrm$(tr$),8)
		rewrite #h, using "form pos 43,c 12,pos 4,c 8": gl$,tr$
	loop
EoH: ! /r
close #h: ioerr ignore
gosub Index
goto Xit
Index: ! r:
	fnIndex('[Q]\GLmstr\bankrec.h[cno]','[Q]\GLmstr\bankrec-idx.h[cno]','79/3/4 12/1/8')
return ! /r
Xit: fnXit
include: ertn


