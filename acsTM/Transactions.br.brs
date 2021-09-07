autoLibrary
on error goto Ertn
fnTop(program$)
! r: setup layout for hamster2
	mask_pointtwo=32
	mask_number=30
	mask_mmddyy=1

	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)

	fnH2Init
	fnH2AddText("Client ID"					    	, 5)
	fnH2AddText("Invoice"  					    	,12)
	fnH2AddText("Date"     					    	, 6,'N' ,0  ,mask_mmddyy) ! 30 (mask_number) =no decimals, no commas
	fnH2AddText("Origional Amount"     	,10,'PD',5.2,mask_pointtwo)
	fnH2AddText("Amount"                	,10,'PD',5.2,mask_pointtwo)
	fnH2AddText("Salesman Number"       	, 3,'PD',2  ,mask_pointtwo)
	itemTCode=fnH2AddText("Trans Code" 	, 1,'N' ,0  ,mask_number)
	!   fnH2AddComboF(fnH2AddText("Trans Code",1,'N',0,mask_number),'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
	fnH2AddText("Posting Code"          	, 1,'N' ,0  ,mask_number)
	fnH2AddText("Invoice Description"  	,20,'C' )
	fnH2AddText("Next Trans Addr"       	, 5,'PD',3  )
	fnH2AddComboF(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
! /r
open #h=fnH: "Name=S:\Core\Data\acsllc\Transactions.h[cno],Version=0,Use,RecL=60,Shr",i,outi,r
fnHamster2("Transactions", h)
close #h: 
goto Xit
Xit: fnXit
include: ertn
