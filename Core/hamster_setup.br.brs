00010   def library fnhamster_field_reset
00020     library 'S:\Core\Library': fnhamster
00030     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*256,sp(1)
00040     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
00050   fnend  ! fn_hamster_field_reset
00060   def library fnhamster_field_add(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
00070     if field_type$='' then let field_type$='C'
00080     if storage_length=0 then let storage_length=textbox_len
00090     let add_rec_item=udim(mat lbl$)+1
00100     mat lbl$(add_rec_item) : let lbl$(add_rec_item)=label$
00110     mat tln(add_rec_item) : let tln(add_rec_item)=textbox_len
00120     mat p$(add_rec_item)
00130     mat fltyp$(add_rec_item) : let fltyp$(add_rec_item)=field_type$
00140     mat sln(add_rec_item) : let sln(add_rec_item)=storage_length
00150     mat mask(add_rec_item) : let mask(add_rec_item)=ar_mask
00160     mat sp(add_rec_item) : let sp(add_rec_item)=storage_position
00170     if storage_position=0 then 
00180       if add_rec_item=1 then 
00190         let sp(add_rec_item)=1
00200         let auto_storage_position=1
00210       else 
00220         let sp(add_rec_item)=sp(add_rec_item-1)+int(sln(add_rec_item-1))
00230       end if 
00240     end if 
00250     mat c$(add_rec_item,8)
00252     let fnhamster_field_add=add_rec_item
00260   fnend  ! fn_hamster_field_add
00270   def library fnhamster_add_combof(hac_screen_item,hac_data_file$*256,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$*256,hac_limit_to_list)
00280     let c$(hac_screen_item,1)='ComboF'
00290     let c$(hac_screen_item,2)=hac_data_file$
00300     let c$(hac_screen_item,3)=str$(hac_key_pos)
00310     let c$(hac_screen_item,4)=str$(hac_key_len) ! Key
00320     let c$(hac_screen_item,5)=str$(hac_desc_pos)
00330     let c$(hac_screen_item,6)=str$(hac_desc_len) ! Description
00340     let c$(hac_screen_item,7)=hac_index_file$
00350     let c$(hac_screen_item,8)=str$(hac_limit_to_list)
00360   fnend 
00370   def library fnhamster_add_comboa(hac_screen_item,mat hac_option$)
00380     let c_y=1
00390     let c$(hac_screen_item,c_y)='ComboA'
00400 ! if udim(mat c$,2)<udim(mat hac_option$) then mat c$(udim(mat c$,1),udim(mat hac_option$)+c_y)
00410     for hac_item=1 to udim(mat hac_option$)
00420       let c$(hac_screen_item,c_y+=1)=hac_option$(hac_item)
00430     next hac_item
00440   fnend 
00450   def library fnhamster_2(h2_name$*20; h2_file)
00452     if h2_file=0 then let h2_file=1
00460     let fnhamster(h2_name$,mat lbl$,mat tln,h2_file,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00470 ! fnhamster(uw$*20,mat lbl$,mat fln,fin,mat p$; mat fltyp$,mat sln,mat mask,mat startpos,mat incontrol$,mat mxl)
00480   fnend 
00490 ! 
