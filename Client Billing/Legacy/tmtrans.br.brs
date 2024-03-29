! Replace S:\Client Billing\Legacy\Client
! TM Client - Hamster

autoLibrary
on error goto Ertn

fnTop(program$)
! r: setup_hamster
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8

	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask$(1),c$(1,8)*40 ! SP(1) - not used
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask$(0) : mat c$(0,8) : mat sp(0)

	fn_add_rec('Client ID',5,'C')
	fn_add_rec('Employee',9,'C')
	fn_add_rec('Hours',6,'PD',3.2,mask_pointtwo)
	fn_add_rec('Hourly Rate',6,'PD',3.2,mask_pointtwo)
	fn_add_rec('Amount',8,'PD',4.2,mask_pointtwo)
	fn_add_rec('Trans Date',6,'N',0,mask_number)
	fn_add_rec('Category Code',2,'N',0,mask_number)
	fn_add_rec('Department Code',3,'PD',2)
	fn_add_rec('Trans Code',2,'PD',1)
	fn_add_rec('System Code',2,'N',0,mask_number)
	fn_add_rec('Service Code',4,'C') ! ph$
	fn_add_rec('Invoice Number',12,'C') ! ph$
	fn_add_rec('Next Trans Addr',5,'PD',3) ! ph$
	fn_add_rec('Description',30,'C') ! ph$
! /r
open #h=fnH: 'Name=S:\Core\Data\acsllc\TMTrans.h[cno],Version=0,Use,RecL=86,Shr',i,outi,r
fnHamster2b('tmtrans',mat lbl$,mat tln,h,mat p$,mat fltyp$,mat sln,mat mask$,mat sp,mat c$)
close #h:
goto Xit

Xit: fnXit

def fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,___,add_rec_item)
	if storage_length=0 then storage_length=textbox_len
	add_rec_item=udim(mat lbl$)+1
	mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
	mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
	mat p$(add_rec_item)
	mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
	mat sln(add_rec_item) : sln(add_rec_item)=storage_length
	mat mask$(add_rec_item) : mask$(add_rec_item)=str$(ar_mask)
	mat c$(add_rec_item,8)
fnend

include: ertn
