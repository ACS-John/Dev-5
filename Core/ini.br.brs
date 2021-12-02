! r: test zone
	! on error goto Ertn
	fn_setup
	library program$: fnIniOpen,fnIniRead$,fnIniSet,fnIniWrite
	fnIniOpen('acs.ini')
	fnIniSet('core','version',wbversion$)
	fnIniSet('core','tom','tomset')
	fnIniSet('acsPR','dave','daveset')
	fnIniWrite
	pr fnIniRead$('core','version')
	pause  ! fnIniOpen('acs.ini')
! fnIniSet('core','
	end
 
! ERTN: ! r:
!   execute 'list -'&str$(line)
!   pr 'Error '&str$(err)&' here';bell
!   pause
! ! ! /r
! /r
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnH,fnarray_item_insert$,fnAddOneC,fnsrch_case_insensitive,fnCopy
	end if
fnend
def library fnIniOpen(ii_file$*256) ! ,MAT INI_SECTION$, MAT INI_FIELD$, MAT INI_VALUE$)
	if ~setup then fn_setup
	if ~setup_fnIniOpen then
		dim ii_line$(1)*2048,ii_section$(1)*1024,ii_section$*1024,ii_value$*1024,ini_file$*256,ini_field$*256
		dim ini_section$(1)*1024, ini_field$(1)*256, ini_value$(1)*256
		setup_fnIniOpen=1
	end if
	ini_file$=trim$(ii_file$)
	ini_field_len_longest=0
	mat ii_line$(0)
	mat ini_section$(0) : mat ini_field$(0) : mat ini_value$(0)
	open #h_ini=fnH: 'Name='&ini_file$,display,input ioerr INI_OPEN_FAIL
	do
		mat ii_line$(udim(mat ii_line$)+1)
		linput #h_ini: ii_line$(udim(mat ii_line$)) eof INI_OPEN_EOF
	loop until file(h_ini)
	INI_OPEN_EOF: !
	close #h_ini:
	mat ii_line$(udim(mat ii_line$)-1) ! if udim(mat ii_line$)=1 and ii_line$(1)="" then mat ii_line$(0)
	mat ii_section$(udim(ii_line$))=("")
	for ii_item=1 to udim(mat ii_line$)
		if ii_line$(ii_item)(1:1)='[' then
			ii_line$(ii_item)=trim$(ii_line$(ii_item))
			ii_section$=uprc$(trim$(trim$(trim$(ii_line$(ii_item)),"["),"]"))
			ii_section$(ii_item)=ii_section$
		else if ii_line$(ii_item)(1:1)='!' or ii_line$(ii_item)(1:1)=';' or ii_line$(ii_item)(1:1)='#' then
			ii_section$(ii_item)=ii_section$
		else
			ii_section$(ii_item)=ii_section$
			fnAddOneC(mat ini_section$,ii_section$)
			ii_pos_equal=pos(ii_line$(ii_item),"=")
			if ii_pos_equal=-1 then ii_pos_equal=pos(ii_line$(ii_item),tab$) ! if no equals sign than use a tab instead
			ini_field$=trim$(trim$(trim$(trim$(ii_line$(ii_item)(1:ii_pos_equal-1),tab$),'"')),tab$)
			fnAddOneC(mat ini_field$,ini_field$)
			ini_field_len_longest=max(ini_field_len_longest,ii_pos_equal-1)
			ii_value$=trim$(trim$(ii_line$(ii_item)(ii_pos_equal+1:len(ii_line$(ii_item)))),tab$)
			if lwrc$(ii_value$)="blank" then ii_value$=''
			fnAddOneC(mat ini_value$,ii_value$)
		end if  ! II_Line$(II_Item)(1:1)=     '['     /  '!'   /     else
	next ii_item
	INI_OPEN_FAIL: !
fnend
def library fnIniRead$*256(il_section$*256,il_field$*256) ! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$ with fnIniOpen
	if ~setup then fn_setup
	tab$=chr$(9)
	mat empty$(0)
	fnIniRead$=fn_iniRead$(il_section$,il_field$)
fnend  ! fnIniRead$
def fn_iniRead$*256(il_section$*256,il_field$*256) ! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$ with fnIniOpen
	dim set_fn_iniRead$*256
	set_fn_iniRead$=""
	il_field$=trim$(il_field$)
	if il_field$="" then il_field$='blank'
	il_section$=trim$(trim$(uprc$(il_section$),"]"),"[")
	il_section_first=fnsrch_case_insensitive(mat ini_section$, il_section$)
	if il_section_first>0 then
		match=fnsrch_case_insensitive(mat ini_field$,il_field$,il_section_first)
		if match>0 then
			if ini_section$(match)=il_section$ then
				set_fn_iniRead$=trim$(ini_value$(match),tab$)
			end if  ! INI_Section$(Match)=IL_Section$
		else
			set_fn_iniRead$=""
		end if  ! Match>0   /   else
	else
		il_field$=""
	end if  ! IL_Section_First>0   /   else
	fn_iniRead$=set_fn_iniRead$
fnend
def library fnIniSet(inis_section$*256,inis_field$*256,inis_value$*256)
	if ~setup then fn_setup
	fnIniSet=fn_iniSet(inis_section$,inis_field$,inis_value$)
fnend
def fn_iniSet(inis_section$*256,inis_field$*256,inis_value$*256)
	! shares MAT INI_Section$, MAT INI_Field$, MAT INI_Value$, Mat II_Section$, INI_Field_Len_Longest with fnIniOpen
	inis_field$=trim$(inis_field$)
	if inis_field$="" then inis_field$=''
	inis_section$=trim$(trim$(inis_section$,"]"),"[")
	inis_section_first=fnsrch_case_insensitive(mat ini_section$, inis_section$)
	if inis_section_first>0 then
		inis_field_item=fnsrch_case_insensitive(mat ini_field$,inis_field$,inis_section_first)
		if inis_field_item>0 then
			if ini_section$(inis_field_item)=uprc$(inis_section$) then
				ini_value$(inis_field_item)=inis_value$
			end if  ! INI_Section$(INIS_Field_Item)=INIS_Section$
		else
		!     pr ' ** need a new field (existing section) **' : pr 'Section='&INIS_SECTION$ : pr '  Field='&INIS_FIELD$ : pr '  Value='&INIS_Value$ : pr 'now go add it!!!' : pause ! insert into Mat II_Section$, Mat II_Line$, Mat INI_Section$, Mat INI_Field$, Mat INI_Value$
			insert_item_number=inis_section_first
			do  ! set Insert_Item_Number to be the last of each section
				insert_item_number+=1
			loop until insert_item_number>udim(ini_section$) or uprc$(inis_section$)<>ini_section$(insert_item_number)
			ini_field_len_longest=max(ini_field_len_longest,len(inis_field$))
			fnarray_item_insert$(mat ii_section$,inis_section$,insert_item_number)
			fnarray_item_insert$(mat ii_line$,rpad$(inis_field$,ini_field_len_longest)&" = "&inis_value$,insert_item_number)
			fnarray_item_insert$(mat ini_section$,uprc$(inis_section$),insert_item_number)
			fnarray_item_insert$(mat ini_field$,inis_field$,insert_item_number)
			fnarray_item_insert$(mat ini_value$,inis_value$,insert_item_number)
		end if  ! INIS_Field_Item>0   /   else
	else
		!      pr ' ** need a new section and a new field in it **' : pr 'Section='&INIS_SECTION$ : pr '  Field='&INIS_FIELD$ : pr '  Value='&INIS_Value$ ! insert into Mat II_Section$, Mat II_Line$, Mat INI_Section$, Mat INI_Field$, Mat INI_Value$
		ini_field_len_longest=max(ini_field_len_longest,len(inis_field$))
		fnAddOneC(mat ii_section$,inis_section$)
		fnAddOneC(mat ii_section$,inis_section$)
		fnAddOneC(mat ii_line$,"["&inis_section$&"]")
		fnAddOneC(mat ii_line$,rpad$(inis_field$,ini_field_len_longest)&" = "&inis_value$)
		fnAddOneC(mat ini_section$,uprc$(inis_section$))
		fnAddOneC(mat ini_field$,inis_field$)
		fnAddOneC(mat ini_value$,inis_value$)
	end if  ! INIS_Section_First>0   /   else
fnend
def library fnIniWrite
	if ~setup then fn_setup
	!  uses (built by fnIniOpen): mat ii_line$, mat ini_section$, mat ini_field$, mat ini_value$, tab$, ini_file$
	dim inir_line$*2048
	dim inir_field$*256,inir_value$*256
	dim inir_section$*1024
	dim inir_temp_file$*1024
	open #inir_temp_handle=fnH: "Name=[Temp]\inir-[Session].ini,RecL=2048,Replace",d,o
	inir_temp_file$=file$(inir_temp_handle)
	if ini_file$="" then
		if env$('ACSDeveloper')<>'' then
			pr "INI_File$ is empty.  fn_ini_write will abort.  You must specifying a file via fnIniOpen, before calling this function."
			pause
		end if
		goto inirXIT
	end if  ! INI_File$=""
	for ii_item=1 to udim(mat ii_line$)
		if ii_line$(ii_item)(1:1)='[' then
			inir_section$=uprc$(trim$(trim$(trim$(ii_line$(ii_item)),"["),"]"))
			inir_line$=ii_line$(ii_item)
			gosub INIR_WRITE
		else if ii_line$(ii_item)(1:1)='!' or ii_line$(ii_item)(1:1)=';' or ii_line$(ii_item)(1:1)='#' or trim$(ii_line$(ii_item))="" then
			inir_line$=ii_line$(ii_item)
			gosub INIR_WRITE
		else
			fnAddOneC(mat ini_section$,ii_section$)
			ii_pos_equal=pos(ii_line$(ii_item),"=")
			if ii_pos_equal=-1 then ii_pos_equal=pos(ii_line$(ii_item),tab$) ! if no equals sign than use a tab instead
			inir_field$=trim$(trim$(ii_line$(ii_item)(1:ii_pos_equal-1),'"'))
			inir_value$=fn_iniRead$(inir_section$,inir_field$)
			if inir_value$='' then inir_value$=""
			inir_line$=ii_line$(ii_item)(1:ii_pos_equal)&' '&inir_value$
			gosub INIR_WRITE
			fnAddOneC(mat ini_value$,ii_value$)
		end if  ! II_Line$(II_Item)(1:1)=     '['     /  '!'...   /     else
	next ii_item
	close #inir_temp_handle:
	fnCopy(inir_temp_file$,ini_file$)
	goto inirXIT
 
	INIR_WRITE: ! r:
		if trim$(inir_line$)<>'' then
			pr #inir_temp_handle: inir_line$
		end if
return ! /r
	inirXIT: !
fnend
 
