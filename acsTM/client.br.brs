! Replace S:\acsTM\Client
! TM Client - Hamster
 
autoLibrary
on error goto Ertn
 
fnTop(program$,'Client 420')
 
fn_hamster_setup_1
fn_open_file : fn_close_file : fn_open_file
fnHamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
fn_close_file
goto Xit
 
def fn_open_file
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=S:\Core\Data\acsllc\CLmstr.h420,Version=0,KFName=S:\Core\Data\acsllc\CLIndex.h420,Use,RecL=534,KPs=1,KLn=5,Shr",internal,outIn,keyed
	open #open_file_count+=1: "Name=S:\Core\Data\acsllc\CLmstr.h420,Version=0,KFName=S:\Core\Data\acsllc\CLIndx2-Idx.h420,Use,RecL=534,KPs=6,KLn=30,Shr",internal,outIn,keyed
fnend
def fn_close_file
	for cf_h_item=1 to open_file_count
		close #cf_h_item:
	next cf_h_item
fnend  ! fn_close_file
 
Xit: fnXit
 
def fn_hamster_setup_1
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8
 
	fn_hamster_field_reset
 
	fn_hamster_field_add("Client ID" ,5,'N',0,mask_number)
	fn_hamster_field_add("Name",30,'C')
	fn_hamster_field_add("Balance",12,'pd',5.2,mask_pointtwo,283) ! ,disable:=1) ! ar(1)
	fn_hamster_field_add("Address" ,30,'C',0,0,36)
	fn_hamster_field_add("CSZ" ,30,'C',0,0,66)
	fn_hamster_field_add("Contact",30,'C')
	fn_hamster_field_add("Business Type",30,'C')
	fn_hamster_field_add("Phone",12,'C') ! ph$
	fn_hamster_field_add("Federal ID",11,'c') ! ss$
 
	fn_hamster_field_add("Home Phone",12,'c',0,0,260) ! ph2$
fnend  ! fn_hamster_setup_1
def fn_hamster_field_reset
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
fnend  ! fn_hamster_field_reset
def fn_hamster_field_add(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position) ! ,disable) <- didn't quite work as easily as i was hoping for it to.
	if field_type$='' then field_type$='C'
	if storage_length=0 then storage_length=textbox_len
	add_rec_item=udim(mat lbl$)+1
	mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
	mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
	mat p$(add_rec_item)
	mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
	mat sln(add_rec_item) : sln(add_rec_item)=storage_length
	mat mask(add_rec_item) : mask(add_rec_item)=ar_mask
	mat sp(add_rec_item) : sp(add_rec_item)=storage_position
	if storage_position=0 then
		if add_rec_item=1 then
			sp(add_rec_item)=1
			auto_storage_position=1
		else
			sp(add_rec_item)=sp(add_rec_item-1)+sln(add_rec_item-1)
		end if
	end if
	mat c$(add_rec_item,8)
	! c$(add_rec_item,7)=str$(disable)
fnend  ! fn_hamster_field_add
include: Ertn
