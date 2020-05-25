autoLibrary
 
on error goto Ertn
 
fnTop(program$)
fn_setup_hamster
gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
fnHamster2("ARTrans")
gosub CLOSE_FILE
goto Xit
 
OPEN_FILE: ! r:
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Version=0,Use,RecL=60,Shr",internal,outIn,relative
return ! /r
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
 
Xit: fnXit
 
def fn_setup_hamster
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8
 
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8
	fnH2Init
	fnH2AddText("Client ID",5)
	fnH2AddText("Invoice Number",12)
	fnH2AddText("Date",6,'N',0,mask_date) ! 30 (mask_number) =no decimals, no commas
	fnH2AddText("Origional Amount",10,'PD',5.2,mask_pointtwo)
	fnH2AddText("Amount",10,'PD',5.2,mask_pointtwo)
	fnH2AddText("Salesman Number",3,'PD',2,mask_pointtwo)
	itemTCode=fnH2AddText("Trans Code",1,'N',0,mask_number)
	!   fnH2AddComboF(fnH2AddText("Trans Code",1,'N',0,mask_number),'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
	fnH2AddText("Posting Code",1,'N',0,mask_number)
	fnH2AddText("Invoice Description",20,'C')
	fnH2AddText("Next Trans Addr",5,'PD',3)
	fnH2AddComboF(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
fnend
include: Ertn
