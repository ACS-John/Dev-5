 
autoLibrary
on error goto Ertn
 
fnTop(program$)
dim srvnam$(10)*20,srv$(10)*2
fnget_services(mat srvnam$, mat srv$)
fn_setup_hamster
fn_open_file : fn_close_file : fn_open_file
fnHamster("Customer_Hamster",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
fn_close_file
goto Xit
 
def fn_open_file
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed
	open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed
	open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed
	open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed
fnend
def fn_close_file
	for j=1 to open_file_count : close #j: : next j
fnend
Xit: fnXit
 
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
 
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
 
	! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
	fn_add_rec("Account No",10,'C',10,0,1)
	fn_add_rec("Meter Address",30,'C',30,0,11)
	fn_add_rec("Date of Charge",8,'PD',4,mask_mmddyy,292)
	fn_add_rec("Last Billing",8,'PD',4,mask_mmddyy,296)
	fn_add_rec("Bulk",7,'C',7,0,354)
	fn_add_rec("Route",2,'N',2,mask_number,1741)
	fn_add_rec("Sequence",7,'N',7,mask_number,1743)
	fn_add_rec("Final",10,'C',10,0,1)
	fn_add_rec("G(1) ",12,'PD',4.2,mask_pointtwo,300)
	fn_add_rec("G(2) ",12,'PD',4.2,mask_pointtwo,304)
	fn_add_rec("G(3) ",12,'PD',4.2,mask_pointtwo,308)
	fn_add_rec("G(4) ",12,'PD',4.2,mask_pointtwo,312)
	fn_add_rec("G(5) ",12,'PD',4.2,mask_pointtwo,316)
	fn_add_rec("G(6) ",12,'PD',4.2,mask_pointtwo,320)
	fn_add_rec("G(7) ",12,'PD',4.2,mask_pointtwo,324)
	fn_add_rec("G(8) ",12,'PD',4.2,mask_pointtwo,328)
	fn_add_rec("G(9) ",12,'PD',4.2,mask_pointtwo,332)
	fn_add_rec("G(10)",12,'PD',4.2,mask_pointtwo,336)
	fn_add_rec("G(11)",12,'PD',4.2,mask_pointtwo,340)
	fn_add_rec("G(12)",12,'PD',4.2,mask_pointtwo,344)
	fn_add_rec(srv$(1)&' Reading - Current',12,'PD',5,mask_number,217)
	fn_add_rec(srv$(1)&' Reading - Prior  ',12,'PD',5,mask_number,222)
	fn_add_rec(srv$(1)&' Usage - Current  ',12,'PD',5,mask_number,227)
	fn_add_rec(srv$(1)&' Usage - YTD      ',12,'PD',5,mask_number,232)
	fn_add_rec(srv$(3)&' Reading - Current',12,'PD',5,mask_number,237)
	fn_add_rec(srv$(3)&' Reading - Prior  ',12,'PD',5,mask_number,242)
	fn_add_rec(srv$(3)&' Usage - Current  ',12,'PD',5,mask_number,247)
	fn_add_rec(srv$(3)&' Usage - YTD      ',12,'PD',5,mask_number,252)
	fn_add_rec(srv$(4)&' Reading - Current',12,'PD',5,mask_number,257)
	fn_add_rec(srv$(4)&' Reading - Prior  ',12,'PD',5,mask_number,262)
	fn_add_rec(srv$(4)&' Usage - Current  ',12,'PD',5,mask_number,267)
	fn_add_rec(srv$(4)&' Usage - YTD      ',12,'PD',5,mask_number,272)
	fn_add_rec('Units Per Meter',12,'PD',5,mask_number,277)
	fn_add_rec('Demand Multiplier',12,'PD',5,mask_number,282)
	fn_add_rec("Demand Reading",12,'PD',5,mask_number,287)
fnend  ! fn_setup_hamster
include: Ertn
