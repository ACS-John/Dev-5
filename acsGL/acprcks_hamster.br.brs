! "Replace "&program$
! another Hamster

autoLibrary
on error goto Ertn

fnTop(program$,'AcPrCks Hamster')
mask_pointtwo=32 : mask_number=30
mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
textlen_mmddyy=8 : textlen_ccyymmdd=10
storage_len_mmddyy=6 : storage_len_ccyymmdd=8

dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)

! fn_add_rec(label$*38    ,textbox_len,field_type$*2; storage_length,ar_mask         ,storage_position)
fn_add_rec("ENo "         ,12         ,'N' ,               4  ,       mask_number    )
fn_add_rec("Date "        ,12         ,'PD',               4  ,       mask_number    )
fn_add_rec("Check Number ",12         ,'PD',               4  ,       mask_number    )
fn_add_rec("D( 4))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D( 5))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D( 6))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D( 7))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D( 8))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D( 9))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(10))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(11))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(12))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(13))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(14))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(15))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(16))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(17))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(18))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(19))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(20))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(21))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("D(22))"       ,12         ,'PD',               5.2,       mask_pointtwo  )
fn_add_rec("NTA"          ,12         ,'PD',               3  ,       mask_number    )

open #1: "Name=[Q]\GLmstr\AcPrCks.h[cno],Shr",i,outi,r
if sum(mat sp)=0 then mat sp(0)
fnHamster("AcPrCks",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
close #1:
goto Xit


def fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
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
fnend
Xit: fnXit
include: ertn
