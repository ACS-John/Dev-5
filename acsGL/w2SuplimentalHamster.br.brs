 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128
 
	cap$=srep$(program$(pos(program$,'\',-1)+1:pos(program$,'.',-1)-1),'Hamster','')
	fnTop(program$,cap$)
	fn_setup_hamster
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
	fnHamster2(cap$)
	gosub CLOSE_FILE
	goto Xit
OPEN_FILE: ! r:
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: 'Name=[Q]\'&env$('cursys')&'mstr\W2Box16.h[cno],KFName=[Q]\'&env$('cursys')&'mstr\W2INDEX.h[cno],Use,RecL=158,Version=0,KPs=1,KLn=8,Shr',internal,outIn,keyed
return ! /r
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
Xit: fnXit
include: ertn
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
		fnH2AddText("Client ID",8)
		boxid$(1)="Box 11:"
		boxid$(2)="Unused:"
		boxid$(3)="Box 12a:"
		boxid$(4)="Box 12b:"
		boxid$(5)="Box 12c:"
		boxid$(6)="Box 12d:"
		for tmp=1 to 6
			fnH2AddText(boxid$(tmp)&" Desc",12)
			fnH2AddText(boxid$(tmp)&" Amount",10.2,'N',10.2,mask_pointtwo)
			fnH2AddText(boxid$(tmp)&" Fed",1)
			fnH2AddText(boxid$(tmp)&" FICA",1)
			fnH2AddText(boxid$(tmp)&" State",1)
		nex tmp
!   fnH2AddComboF(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
fnend  ! fn_setup_hamster
