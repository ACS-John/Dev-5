def library fnmsgbox(&mat mg$; &response$,cap$*128,mt)
	library 'S:\Core\Library': fnerror,fnTos,fnLbl,fnCmdKey,fnAcs
	on error goto ERTN
	if env$('exitnow')='yes' then let setenv('exitnow','cancelled by fnmsgbox')
	if cap$='' then cap$=env$('Program_Caption')
	!   if env$('ACSDeveloper')<>'' then
	fn_br_messagebox(mat mg$,response$, cap$,mt)
	!   else 
	!     fn_ace_messagebox(mat mg$,response$, cap$,mt)
	!   end if
	XIT: ! 
fnend 
IGNORE: continue 
def fn_br_messagebox(&mat mg$, &response$; cap$*128, mt)
	dim bm_text$*2048
	bm_text$=''
	bm_type=mt

	for mg_item=1 to udim(mat mg$)
		bm_text$(inf:inf)=mg$(mg_item)&chr$(13)
	next mg_item
	bm_text$=rtrm$(bm_text$,chr$(13))

	bm_btn_default=1
	if bm_type=>768 then bm_type-=768 : bm_btn_default=4
	if bm_type=>512 then bm_type-=512 : bm_btn_default=3
	if bm_type=>256 then bm_type-=256 : bm_btn_default=2

	bm_icon$=''
	if bm_type=>064 then bm_type-=064 : bm_icon$='Inf'
	if bm_type=>048 then bm_type-=048 : bm_icon$='Excl'
	if bm_type=>032 then bm_type-=032 : bm_icon$='Qst'
	if bm_type=>016 then bm_type-=016 : bm_icon$='Err'

	if bm_type=5 then ! retry/cancel
		fn_ace_messagebox(mat mg$,response$, cap$,mt)
		goto BM_XIT
	else if bm_type=4 then ! yes/no
		bm_button$='yn'
	else if bm_type=3 then ! yes/no/cancel
		bm_button$='ync'
	else if bm_type=2 then ! abort/retry/ignore
		fn_ace_messagebox(mat mg$,response$, cap$,mt)
		goto BM_XIT
	else if bm_type=1 then ! ok/cancel
		bm_button$='okc'
	else if bm_type=0 then ! ok
		bm_button$='ok'
	end if 
	if bm_btn_default=2 and bm_type=1 then 
		bm_button$='okC'
	else if bm_btn_default>1 and bm_type>0 then 
		bm_button$(bm_btn_default:bm_btn_default)=uprc$(bm_button$(bm_btn_default:bm_btn_default))
	end if 

	BM_ASK: ! 
	if bm_icon$='' then 
		bm_response=msgbox(bm_text$,cap$,bm_button$)
	else 
		bm_response=msgbox(bm_text$,cap$,bm_button$,bm_icon$)
	end if 

	if bm_response=0 then 
		pr 'msgbox returned an error'
		pr 'please CALL SUPPORT or type GO and press Enter (Cancel will attempt to be selected)'
		pause 
		bm_response=4
	end if 
	if bm_response=1 then response$="OK"
	if bm_response=2 then response$="Yes"
	if bm_response=3 then response$="No"
	if bm_response=4 then response$="Cancel"
	if response$="Cancel" and bm_type<>5 and bm_type<>3 and bm_type<>1 then ! only allow cancel on messageboxes that have a cancel option
		if bm_type=0 then ! If there was only an OK button, just assume they meant that
			response$="OK"
		else 
			goto BM_ASK
		end if 
	end if 
	BM_XIT: ! 
fnend 
def fn_ace_messagebox(&mat mg$, &response$; cap$*128, mt)
	mat_mg_len=0 : for j=1 to udim(mat mg$) : mat_mg_len+=len(mg$(j)) : next j
	fnTos(sn$="mb"&str$(udim(mat mg$))&'-'&str$(mat_mg_len))
	for mg_item=1 to udim(mat mg$)
		fnLbl(mg_item,1,mg$(mg_item))
	next mg_item
	mat btn_default=(0)
	if mt=>768 then mt-=768 : btn_default(4)=1
	if mt=>512 then mt-=512 : btn_default(3)=1
	if mt=>256 then mt-=256 : btn_default(2)=1
	! if sum(mat btn_default)=0 then btn_default(1)=1
	if mt=>064 then mt-=064
	if mt=>048 then mt-=048 ! 
	if mt=>032 then mt-=032 ! 
	if mt=>016 then mt-=016 ! (X) Critical
	if mt=5 then ! retry/cancel
		fnCmdKey("&Retry",4)
		fnCmdKey("&Cancel",99,btn_default(2),1)
	else if mt=4 then ! yes/no
		fnCmdKey("&Yes",6)
		fnCmdKey("&No",7,btn_default(2))
	else if mt=3 then ! yes/no/cancel
		fnCmdKey("&Yes",6)
		fnCmdKey("&No",7,btn_default(2))
		fnCmdKey("&Cancel",99,btn_default(3),1)
	else if mt=2 then ! abort/retry/ignore
		fnCmdKey("&Abort",3)
		fnCmdKey("&Retry",4,btn_default(2))
		fnCmdKey("&Ignore",5,btn_default(3))
	else if mt=1 then ! ok/cancel
		fnCmdKey("&Ok",1)
		fnCmdKey("&Cancel",99,btn_default(2),1)
	else if mt=0 then ! ok
		fnCmdKey("&Ok",1,1,1)
	end if 
	fnAcs(sn$,0,mat resp$,ckey,0,0,1,1)
	if ckey=1 then response$="OK"
	if ckey=99 then response$="Cancel"
	if ckey=3 then response$="Abort"
	if ckey=4 then response$="Retry"
	if ckey=5 then response$="Ignore"
	if ckey=6 then response$="Yes"
	if ckey=7 then response$="No"
fnend 

include: ertn