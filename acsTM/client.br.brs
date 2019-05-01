! Replace S:\acsTM\Client
! TM Client - Hamster

	library 'S:\Core\Library': fntop,fnxit, fnHamster
	on error goto ERTN

	fntop(program$,'Client 420')

	fn_hamster_setup_1
	fn_hamster_setup_2
	fn_open_file : fn_close_file : fn_open_file
	gosub HAMSTER
	fn_close_file
goto XIT

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
def fn_hamster_setup_2
! 
! ** Combo Boxes **
! c_x=Field Number  : C$(c_x,1)='ComboF'
! C$(c_x,2)=Linked File Name
! C$(c_x,3)=Key Position         : C$(c_x,4)=Key Length
! C$(c_x,5)=Description Position : C$(c_x,6)=Description Length
! C$(c_x,7)=Index File
! C$(c_x,8)=limit to list option ('1'=Yes; '0'=No)
		limit_to_list$='1'
! 
! cl=1 : c$(cl,1)='ComboF'
! c$(cl,2)='S:\Core\Data\acsllc\Client.h420'
! c$(cl,3)='1' : c$(cl,4)='6'
! c$(cl,5)='7' : c$(cl,6)='50'
! c$(cl,7)='S:\Core\Data\acsllc\Client-Idx.h420'
! c$(cl,8)=limit_to_list$
! cl=3 : c$(cl,1)='ComboF'
! c$(cl,2)='S:\Core\Data\acsllc\Systems.h420'
! c$(cl,3)='1' : c$(cl,4)='2'
! c$(cl,5)='3' : c$(cl,6)='50'
! c$(cl,7)='S:\Core\Data\acsllc\Systems-Idx.h420'
! c$(cl,8)=limit_to_list$
! cl=5 : c$(cl,1)='ComboF'
! c$(cl,2)='S:\Core\Data\acsllc\TimeFrame.h420'
! c$(cl,3)='1' : c$(cl,4)='2'
! c$(cl,5)='3' : c$(cl,6)='50'
! c$(cl,7)='S:\Core\Data\acsllc\TimeFrame-Idx.h420'
! c$(cl,8)=limit_to_list$
! 
! c_x=1 : c$(c_x,1)='ComboF'
! c$(c_x,2)="[Q]\UBmstr\Customer.h420"
! c$(c_x,3)='1' : c$(c_x,4)='10' ! Key
! c$(c_x,5)='41' : c$(c_x,6)='30' ! Description
! c$(c_x,7)="[Q]\UBmstr\ubIndex.h420"
! c$(c_x,8)='1'
! 
! c_x=2 : c_y=1
! c$(c_x,c_y)='ComboA'
! for srv_item=1 to 10
!  if (srv_item=1 and trim$(srv$(srv_item))<>'') or (srvnam$(srv_item)="GAS" or srv$(srv_item)="GA") or srv$(srv_item)='EL' or srvnam$(srv_item)="Lawn Meter" then ! if it is a metered service
!   c$(c_x,c_y+=1)=srv$(srv_item)
!  end if
! next srv_item
	fnend 

HAMSTER: ! 
	fnHamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return 

XIT: fnxit

	def fn_hamster_setup_1
		mask_pointtwo=32 : mask_number=30
		mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
		textlen_mmddyy=8 : textlen_ccyymmdd=10
		storage_len_mmddyy=6 : storage_len_ccyymmdd=8
! 
		fn_hamster_field_reset

	! fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)

		fn_hamster_field_add("Client ID" ,5,'N',0,mask_number)
		fn_hamster_field_add("Name",30,'C')
		fn_hamster_field_add("Balance",12,'pd',5.2,mask_pointtwo,283) ! ,disable:=1) ! ar(1)
		fn_hamster_field_add("Address" ,30,'C',0,0,36)
		fn_hamster_field_add("CSZ" ,30,'C',0,0,66)
		fn_hamster_field_add("Contact",30,'C')
		fn_hamster_field_add("Business Type",30,'C')
		fn_hamster_field_add("Phone",12,'C') ! ph$
		fn_hamster_field_add("Federal ID",11,'c') ! ss$
	! fn_hamster_field_add("Partner #",9,'n',0,mask_number) ! pno
	! fn_hamster_field_add("Month of Year-End",2,'n',0,mask_number) ! mye
	! fn_hamster_field_add("dd(01)",4,"PD", 3,mask_number) ! due date
	! fn_hamster_field_add("dd(02)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(03)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(04)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(05)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(06)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(07)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(08)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(09)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("dd(10)",4,"PD", 3,mask_number)
	! fn_hamster_field_add("sc(01)",1,"N") ! status
	! fn_hamster_field_add("sc(02)",1,"N")
	! fn_hamster_field_add("sc(03)",1,"N")
	! fn_hamster_field_add("sc(04)",1,"N")
	! fn_hamster_field_add("sc(05)",1,"N")
	! fn_hamster_field_add("sc(06)",1,"N")
	! fn_hamster_field_add("sc(07)",1,"N")
	! fn_hamster_field_add("sc(08)",1,"N")
	! fn_hamster_field_add("sc(09)",1,"N")
	! fn_hamster_field_add("sc(10)",1,"N")
	! fn_hamster_field_add("ca(01)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(02)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(03)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(04)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(05)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(06)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(07)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(08)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(09)" ,5,"PD", 3)
	! fn_hamster_field_add("ca(10)" ,5,"PD", 3)
		fn_hamster_field_add("Home Phone",12,'c',0,0,260) ! ph2$
	! fn_hamster_field_add("Spouse SSN",11,'c') ! ss2$
	! fn_hamster_field_add("Balance",12,'pd',5.2,mask_pointtwo,283) ! ar(1)
	! fn_hamster_field_add("ar(2)",12,'pd',5.2,mask_pointtwo)
	! fn_hamster_field_add("ar(3)",12,'pd',4.3,mask_pointtwo)
	! fn_hamster_field_add("ar(4)",1,'n')
	! fn_hamster_field_add("ar(5)",1,'n')
	! fn_hamster_field_add("arta(1)",5,'pd',3)
	! fn_hamster_field_add("arta(2)",5,'pd',3)
	! fn_hamster_field_add("cm$",70,'c')
	! fn_hamster_field_add("app(01)",1,'N')
	! fn_hamster_field_add("app(02)",1,'N')
	! fn_hamster_field_add("app(03)",1,'N')
	! fn_hamster_field_add("app(04)",1,'N')
	! fn_hamster_field_add("app(05)",1,'N')
	! fn_hamster_field_add("app(06)",1,'N')
	! fn_hamster_field_add("app(07)",1,'N')
	! fn_hamster_field_add("app(08)",1,'N')
	! fn_hamster_field_add("app(09)",1,'N')
	! fn_hamster_field_add("app(10)",1,'N')
	! fn_hamster_field_add("app(11)",1,'N')
	! fn_hamster_field_add("app(12)",1,'N')
	! fn_hamster_field_add("app(13)",1,'N')
	! fn_hamster_field_add("app(14)",1,'N')
	! fn_hamster_field_add("app(15)",1,'N')
	! fn_hamster_field_add("app(16)",1,'N')
	! fn_hamster_field_add("app(17)",1,'N')
	! fn_hamster_field_add("app(18)",1,'N')
	! fn_hamster_field_add("app(19)",1,'N')
	! fn_hamster_field_add("app(20)",1,'N')
	! fn_hamster_field_add("ma(01)",6,'pd',3.2)
	! fn_hamster_field_add("ma(02)",6,'pd',3.2)
	! fn_hamster_field_add("ma(03)",6,'pd',3.2)
	! fn_hamster_field_add("ma(04)",6,'pd',3.2)
	! fn_hamster_field_add("ma(05)",6,'pd',3.2)
	! fn_hamster_field_add("ma(06)",6,'pd',3.2)
	! fn_hamster_field_add("ma(07)",6,'pd',3.2)
	! fn_hamster_field_add("ma(08)",6,'pd',3.2)
	! fn_hamster_field_add("ma(09)",6,'pd',3.2)
	! fn_hamster_field_add("ma(10)",6,'pd',3.2)
	! fn_hamster_field_add("ma(11)",6,'pd',3.2)
	! fn_hamster_field_add("ma(12)",6,'pd',3.2)
	! fn_hamster_field_add("ma(13)",6,'pd',3.2)
	! fn_hamster_field_add("ma(14)",6,'pd',3.2)
	! fn_hamster_field_add("ma(15)",6,'pd',3.2)
	! fn_hamster_field_add("ma(16)",6,'pd',3.2)
	! fn_hamster_field_add("ma(17)",6,'pd',3.2)
	! fn_hamster_field_add("ma(18)",6,'pd',3.2)
	! fn_hamster_field_add("ma(19)",6,'pd',3.2)
	! fn_hamster_field_add("ma(20)",6,'pd',3.2)
	! fn_hamster_field_add("app2(01)",1,'n')
	! fn_hamster_field_add("app2(02)",1,'n')
	! fn_hamster_field_add("app2(03)",1,'n')
	! fn_hamster_field_add("app2(04)",1,'n')
	! fn_hamster_field_add("app2(05)",1,'n')
	! fn_hamster_field_add("app2(06)",1,'n')
	! fn_hamster_field_add("app2(07)",1,'n')
	! fn_hamster_field_add("app2(08)",1,'n')
	! fn_hamster_field_add("app2(09)",1,'n')
	! fn_hamster_field_add("app2(10)",1,'n')
	! fn_hamster_field_add("app2(11)",1,'n')
	! fn_hamster_field_add("app2(12)",1,'n')
	! fn_hamster_field_add("app2(13)",1,'n')
	! fn_hamster_field_add("app2(14)",1,'n')
	! fn_hamster_field_add("app2(15)",1,'n')
	! fn_hamster_field_add("app2(16)",1,'n')
	! fn_hamster_field_add("app2(17)",1,'n')
	! fn_hamster_field_add("app2(18)",1,'n')
	! fn_hamster_field_add("app2(19)",1,'n')
	! fn_hamster_field_add("app2(20)",1,'n')
	! fn_hamster_field_add("ma2(01)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(02)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(03)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(04)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(05)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(06)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(07)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(08)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(09)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(10)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(11)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(12)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(13)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(14)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(15)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(16)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(17)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(18)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(19)",6,'pd',3.2)
	! fn_hamster_field_add("ma2(20)",6,'pd',3.2)
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
include: ertn
