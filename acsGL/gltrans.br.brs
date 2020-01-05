
library 'S:\Core\Library': fntop,fnxit,fnHamster
on error goto Ertn
fntop(program$)
fn_setup_hamster

! gosub OPEN_FILE : gosub CLOSE_FILE : 
gosub OPEN_FILE 
fnHamster("GLTrans",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
gosub CLOSE_FILE

goto XIT

OPEN_FILE: !
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\GLmstr\GLTrans.h[cno],Version=0,Use,RecL=73,Shr",internal,outIn,relative
return

CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return

XIT: fnxit

def fn_setup_hamster
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8

	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)

	! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask)
	fn_add_rec("Dept",3,'N',0,mask_number)
	fn_add_rec("Acct",6,'N',0,mask_number)
	fn_add_rec("Sub",3,'N',0,mask_number)
	fn_add_rec("Date",6,'N',0,mask_mmddyy)
	fn_add_rec("Amount",12.2,'PD',6.2,mask_pointtwo)
	fn_add_rec("Trans Code",2,'N',0,mask_pointtwo)
	fn_add_rec("Posting Code",2,'N',0,mask_pointtwo)
	fn_add_rec("Reference #",12,'C')
	fn_add_rec("Description",30,'C')
	fn_add_rec("Next Tran Addr",5,'PD',3,mask_number)
fnend
def fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask)
	if storage_length=0 then storage_length=textbox_len
	add_rec_item=udim(mat lbl$)+1
	mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
	mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
	mat p$(add_rec_item)
	mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
	mat sln(add_rec_item) : sln(add_rec_item)=storage_length
	mat mask(add_rec_item) : mask(add_rec_item)=ar_mask
	mat c$(add_rec_item,8)
fnend


include: ertn
