def library fnH2Init
	autoLibrary
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*256,sp(1)
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
fnend  ! fn_hamster_field_reset
def library fnH2AddText(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
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
			sp(add_rec_item)=sp(add_rec_item-1)+int(sln(add_rec_item-1))
		end if 
	end if 
	mat c$(add_rec_item,8)
	fnH2AddText=add_rec_item
fnend  ! fn_hamster_field_add
def library fnH2AddComboF(hac_screen_item,hac_data_file$*256,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$*256,hac_limit_to_list)
	c$(hac_screen_item,1)='ComboF'
	c$(hac_screen_item,2)=hac_data_file$
	c$(hac_screen_item,3)=str$(hac_key_pos)
	c$(hac_screen_item,4)=str$(hac_key_len) ! Key
	c$(hac_screen_item,5)=str$(hac_desc_pos)
	c$(hac_screen_item,6)=str$(hac_desc_len) ! Description
	c$(hac_screen_item,7)=hac_index_file$
	c$(hac_screen_item,8)=str$(hac_limit_to_list)
fnend 
def library fnH2AddComboA(hac_screen_item,mat hac_option$)
	c_y=1
	c$(hac_screen_item,c_y)='ComboA'
	! if udim(mat c$,2)<udim(mat hac_option$) then mat c$(udim(mat c$,1),udim(mat hac_option$)+c_y)
	for hac_item=1 to udim(mat hac_option$)
		c$(hac_screen_item,c_y+=1)=hac_option$(hac_item)
	next hac_item
fnend 
def library fnHamster2(h2_name$*20; h2_file)
	if h2_file=0 then h2_file=1
	fnHamster(h2_name$,mat lbl$,mat tln,h2_file,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
! fnHamster(uw$*20,mat lbl$,mat fln,fin,mat p$; mat fltyp$,mat sln,mat mask,mat startpos,mat incontrol$,mat mxl)
fnend 

