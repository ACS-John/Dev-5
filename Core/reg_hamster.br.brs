 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128
 
	fnTop(program$,cap$='Reg Hamster')
	fncno(cno)
	fn_setup_hamster
	fn_open_file : fn_close_file : fn_open_file
	gosub HAMSTER
	fn_close_file
	goto Xit
 
def fn_open_file
		open_file_count=0 ! this value is used in the close_file sub routine
		open #open_file_count+=1: 'Name=[Q]\Data\reg.dat,Version=1,KFName=[Q]\Data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
! open #open_file_count+=1: 'Name=data\reg.dat,Version=1,KFName=data\reg.idx,Shr',internal,outIn,keyed
! open #open_file_count+=1: 'Name=data\reg.dat,Shr',internal,outIn,relative
fnend
def fn_close_file
		for j=1 to open_file_count : close #j: : next j
fnend  ! fn_close_file
 
HAMSTER: !
	fnHamster("Reg_Hamster",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: ertn
 
def fn_add_rec(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
		if field_type$='' then field_type$='C'
		if storage_length=0 then storage_length=textbox_len
! storage_length_prior=storage_length
		add_rec_item=udim(mat lbl$)+1
		mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
		mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
		mat p$(add_rec_item)
		mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
		mat sln(add_rec_item) : sln(add_rec_item)=storage_length
		mat mask(add_rec_item) : mask(add_rec_item)=ar_mask
! if storage_position=0 then
!   storage_position=1
!   auto_storage_position=1
! else if auto_storage_position and storage_position=0 then
!   storage_position=storage_position_prior+storage_length_prior
! end if
		mat sp(add_rec_item) : sp(add_rec_item)=storage_position
! storage_length_prior=storage_position
		mat c$(add_rec_item,8)
fnend  ! fn_add_rec
def fn_setup_hamster
		mask_pointtwo=32 : mask_number=30
		mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
		textlen_mmddyy=8 : textlen_ccyymmdd=10
		storage_len_mmddyy=6 : storage_len_ccyymmdd=8
 
		dim lbl$(1)*38,tln(1),p$(1)*256,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
		mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
 
! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
		fn_add_rec("Key",30,'C',128,0,1)
		fn_add_rec("Value",40,'C',256,0,129)
fnend  ! fn_setup_hamster
