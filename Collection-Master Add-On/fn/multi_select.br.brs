print newpage
execute "*con gui on"
library "": fngridio
dim grid$(1)*80
do
	nmax+=1 
	mat grid$(nmax)=("")
	print fields '24,1,cc 80,[S]': "Important Stuff Goes Here"
	for ngrid=1 to nmax 
		! GRID$(NGRID)=CNVRT$('n 5',NGRID)&"³ --- ³"&STR$(NGRID): NEXT NGRID 
		grid$(ngrid)=cnvrt$('n 5',ngrid)&"³ Sample Grid --------------------------³"&str$(ngrid)
	next ngrid
	! 	PRINT "Starting:";TIME$
	choice=fngridio(mat grid$,nmax,"Sample","S[U]","","#;Description;Count","1;3;4;5","One;Three - Is Nick;Four;Five") ! ,0,5,0,19) 
	print fields '21,10,C': "Choice:"&str$(choice) 
	print fields '22,10,C': "  FKey:"&str$(fkey)
loop
pause 
end 
def library fnis_val(amt$*512)
	fnis_val=fn_is_val(amt$)
fnend  ! Fnis_Val
def fn_is_val(amt$*512)
	amt$=srep$(amt$,"-","") 
	amt$=srep$(amt$,"$","") 
	amt$=srep$(amt$,"(","") 
	amt$=srep$(amt$,")","")
	is_val_amt=val(amt$) conv L1090 
	fn_is_val=1 
	L1090: !
fnend 
def fn_setup ! Setup_  #AutoNumber# 30000,20
	if ~setup then 
		setup=1
		gosub SETUP_LIBRARY
		gosub SETUP_CONSTANTS
	end if  ! ~setup
	goto SETUP_XIT
	SETUP_LIBRARY: ! 
		if ~setup_library then 
			setup_library=1
			library 'Collection-Master Add-On\fn\Library.br': fnsession_size_setup
			library "CLSUtil/library": fngethandle,fnhtml$,fnlist,fnhelp,fnstime,fnkey_after$,fngenerate_buttons_for_window
			library "CLSUtil/library": fngenerate_buttons,fnerase_buttons,fnopen_parent$,fnarray_add$
			library 'CLSUtil/Library': fnmessagebox,fnadd_one$,fnadd_one,fnwinscroll,fndate10$,fnsymbol_xlate$,fnspin$
			library 'CLSUtil/Library': fnremove_arrayitem$,fnremove_arrayitem,fndisplay_top,fndate_dis10$,fnsecurity,fncheck_time
			library "CLSUtil/Library": fnmat_1d_to_2d,fnchr_count,fnuser_init$,fnmenu_autonumber
			library "ToolBar/Library": fnpk_cleanup$
			library "Spell/Library": fnsoundex$
			library program$: fngeneric_search_ask
		end if  ! ~setup_library
	return  ! SETUP_LIBRARY
	SETUP_CONSTANTS: ! 
		if ~setup_constants then 
			setup_constants=1
			gosub Enum
			fnsession_size_setup(session_rows,session_cols)
			if lwrc$(login_name$)='jbowman' then jbowman=1 else jbowman=0
		end if  ! ~Setup_Constants
	return  ! SETUP_CONSTANTS
	SETUP_XIT: ! 
fnend  ! fn_setup
!
GRID_METRIX: ! r: Figure out the Grid Metrix
	if max_width<sum(max_field) then max_width=sum(max_field)
	if grid_rows=0 then grid_rows=udim(grid_data$)
	if grid_srow=0 then grid_srow=int((session_rows/2+1)-(grid_rows/2))
	if grid_srow<1 then grid_srow=1
	if grid_srow=1 and trim$(grid_title$)<>"" then grid_srow+=1
	if grid_srow+grid_rows>(session_rows-1) then grid_rows=(session_rows-1)-grid_srow           ! ** 22 leaves a 2 line gap at the bottom
	grid_window_rows=max(grid_rows+1,3) 
	if grid_window_rows>(session_rows-1) then grid_window_rows=(session_rows-1)
	if grid_cols=0 then 
		grid_cols=int(grid_scol+max_width+1)
	end if
	grid_cols+=1
	if grid_scol=0 then 
		grid_scol=int(40-(grid_cols/2))-1
	end if
	if grid_scol<1 then grid_scol=1
	if trim$(grid_title$)<>"" and grid_scol=1 then grid_scol+=1
	! 
	grid_max=80-grid_scol 
	if trim$(grid_title$)<>"" then grid_max=79-grid_scol
	if grid_cols>grid_max then grid_cols=grid_max
	for nghead=1 to max_fields
		if max_field(nghead)<len(grid_heading$(nghead)) then 
			max_field(nghead)=len(grid_heading$(nghead))
		end if
		grid_form$="C" 
		if grid_type$(nghead)="N" then grid_form$="CR"
		grid_form$(999:0)=" "&str$(max_field(nghead))&",[T]T"
		grid_form$(nghead)=grid_form$
	next nghead
	mat grid_cache_data$(1,max_fields)=("")
	if trim$(grid_border$)="" then grid_border$="S"
return  ! /r
!
GRID_SEARCH: ! r: Search Grid
	dim search_type$*20,search_form$(4)*40,search$*80,search_direction$(2)*30,search_seek$*30,list_data$(1)*80,search_match$*256,prior_search$*80,searching$*80
	search_seek=search_find=0 
	if cmdkey=2 then 
		search_seek=1 : search_find=1 
		search_type$="Seek" 
	else if cmdkey=10 then 
		search_seek=0 : search_find=1 
		search_type$="Find" 
	else if cmdkey=20 then 
		search_seek=0 : search_find=-1 
		search_type$="Rev. Find"
	end if
	search_form$(1)="1,10,CU 30/80,[D]S" 
	search_form$(2)="1,41,CHECK 16/18,[W],1001" 
	search_form$(3)="1,58,RADIO 6/6,0,1001" 
	search_form$(4)="1,65,RADIO 5/6,0,1001"
	grid_search_handle=fngethandle 
	open #grid_search_handle: "SCOL=1,SROW=24,COLS=80,ROWS=1",display,outin  
	! OPEN #GRID_SEARCH_HANDLE: "SCOL=2,SROW=2,COLS=78,ROWS=1,BORDER=S[M],N=[X],CAPTION="&SEARCH_TYPE$,DISPLAY,OUTIN  
	print #grid_search_handle: newpage
	if trim$(searching$)<>"" and gridio_footer_handle<>0 then 
		print #gridio_footer_handle,fields "1,1,C "&str$(grid_footer_len)&",[P]": searching$
	end if
	L59310: !
	if search_seek=1 then 
		search_seek$="Extended search" 
	else 
		search_seek$="^Extended search"
	end if
	if search_find=1 then 
		search_direction$(1)="^Down" 
		search_direction$(2)="Up" 
	else 
		search_direction$(1)="Down" 
		search_direction$(2)="^Up"
	end if
	print #grid_search_handle,fields "1,1,C 9/15,[W];1,71,CC 7,[W],B0": search_type$,"Search"
	rinput #grid_search_handle,fields mat search_form$,attr '[A]': search$,search_seek$,mat search_direction$
	if search_seek$(1:1)<>"^" then 
		search_seek=1 
	else 
		search_seek=0
	end if
	if search_direction$(1)(1:1)="^" then 
		search_find=1 
	else 
		search_find=-1
	end if
	if fkey=1001 then goto L59310       ! ** After user selects a choice, allow them to type more
	close #grid_search_handle: 
	if trim$(searching$)<>"" and gridio_footer_handle<>0 then 
		print #gridio_footer_handle,fields "1,1,C "&str$(grid_footer_len)&",[P]": grid_footer$
	end if
	if search_find=1 then 
		nsearch_start=1 
		else 
		nsearch_start=max_body
	end if
	search$=uprc$(rtrm$(search$)) 
	search_len=len(search$) 
	mat list_data$(max_fields)=("") 
	if cmdkey=99 or search$="" then goto L59391
	nsearch=nsearch_start 
	nsearch_match=default_start
	if prior_search$<>search$ then 
		prior_search$=search$ 
		goto SEARCH_LOOP 
		! If it's a new Search, just search
	end if
	nsearch=nsearch_start=prior_match 
	goto L59385    ! ** Skip the last match & start searching again!
	SEARCH_LOOP: !
	search_pass=0 
	do while nsearch<>0 and search_pass<2 
		! ** Search through the list, looking for a match 
		! * For Repeat Searches, start at "Last Match" - Skipping the actual match 
		! ** End search is defined by "Looping" back to nsearch_Start (the 2nd time)
		! CURFLD(1,NSEARCH) 
		! INPUT #GRIDIO_HANDLE,FIELDS GRID_HANDLE$&",ROW,CUR,NOWAIT": MAT LIST_DATA$ 
		! Adding This feature scrolls the search & Allows Unlimited Matches
		search_match$=uprc$(grid_data$(nsearch)) 
		! FOR NMATCH=1 TO UDIM(LIST_DATA$) 
		! SEARCH_MATCH$(9999:0)=UPRC$(LIST_DATA$(NMATCH)&"³") 
		! NEXT NMATCH
		search_match=0 
		if search_seek=1 then 
			search_match=(search$==search_match$(1:search_len)) 
		else 
			search_match=pos(search_match$,search$)
		end if
		if search_match>0 then 
			nsearch_match=prior_match=nsearch 
			nsearch=0 
		end if
			goto L59390
		! PAUSE
		L59385: !
		nsearch+=search_find
		if nsearch>max_body then 
			nsearch=1: search_pass+=1 
		else if nsearch<1 then 
			nsearch=max_body : search_pass+=1
		end if
		if nsearch=nsearch_start then nsearch=0
		L59390: !
	loop 
	L59391: !
	if nsearch_match<>0 then default_start=nsearch_match
return  ! /r
!
def library fngridio(mat grid_data$;default_start,grid_title$*999,grid_border$*20,grid_footer$*999,grid_heading$*999,grid_keys$*999,grid_labels$*999,grid_scol,grid_srow,grid_cols,grid_rows,free,display_only) 
	! (Grid_Title$*1024,Grid_heading$*1024,MAT Grid_Data$)
	on soflow ignore 
	! IF TRIM$(LOGIN_NAME$)="siul" THEN PAUSE
	msg$("Building Grid") 
	if ~setup then fn_setup
	max_body=udim(grid_data$) 
	search$=prior_search$="": prior_match=0 
	grid_title$=srep$(grid_title$,",",".")
	dim grid_heading$(1)*80,xml_line$(1)*80,max_field(1),grid_type$(1)*15,grid_keys$(1)*80,grid_labels$(1)*80,grid_form$(1)*32,grid_form$*32,grid_handle$*200,grid_cache_data$(1,1)*80
	gridio_handle=gridio_footer_handle=0 
	searching$=""
	max_width=10 
	mat max_field(1)=(0) 
	mat grid_type$(1)=("") 
	max_fields=0 
	for nfield=1 to max_body
		if len(grid_data$(nfield))>max_width then 
			max_width=len(grid_data$(nfield))
		end if
		this_field=1 
		last_pos=0
		L59710: !
		xpos=pos(grid_data$(nfield),"³",last_pos+1) 
		xxpos=xpos 
		if xpos<=0 then xxpos=9999
		if udim(max_field)<this_field then 
			mat max_field(this_field): mat grid_type$(this_field)
		end if
		xlen=max(1,len(rtrm$(grid_data$(nfield)(last_pos+1:xxpos-1))))
		if xlen>max_field(this_field) then max_field(this_field)=xlen

		if grid_type$(this_field)="C" then goto L59760
		if ~fn_is_val(grid_data$(nfield)(last_pos+1:xxpos-1)) then 
			goto L59755
		end if
		if trim$(grid_type$(this_field))="" or trim$(grid_type$(this_field))="N" then 
			grid_type$(this_field)="N" 
			goto L59760
		end if
		L59755: !
		dval$=fndate10$(grid_data$(nfield)(last_pos+1:xxpos-1)(1:18)) 
		if trim$(dval$)="" then goto L59757
		if trim$(grid_type$(this_field))="" or trim$(grid_type$(this_field))="D" then 
			grid_type$(this_field)="D" 
			goto L59760
		end if
		L59757: !
		grid_type$(this_field)="C"
		L59760: !
		if xpos<=0 then goto L59770
		last_pos=xpos 
		this_field+=1 
		goto L59710
		L59770: !
		if this_field>max_fields then max_fields=this_field
	next nfield
	fnlist(":"&grid_heading$,mat grid_heading$,";") 
	if udim(grid_heading$)<max_fields then 
		mat grid_heading$(max_fields) 
		mat grid_type$(max_fields)
	end if
	if udim(grid_heading$)>udim(mat max_field) then 
		mat max_field(udim(grid_heading$))
	end if
	max_fields=udim(grid_heading$)
	mat grid_type$(max_fields) 
	mat grid_form$(max_fields) 
	mat grid_heading$(max_fields)
	gosub GRID_METRIX
	gridio_handle=fngethandle 
	open #gridio_handle: "SCOL="&str$(grid_scol)&",SROW="&str$(grid_srow)&",COLS="&str$(grid_cols)&",ROWS="&str$(grid_window_rows)&",BORDER="&grid_border$&",N=[T],TAB="&grid_title$,display,outin  
	print #gridio_handle: newpage
	if trim$(grid_footer$)<>"" then goto L59855
	if trim$(grid_border$)<>"" and grid_cols>=20 then 
		grid_footer$="[F2/F10 to Search]" 
	else if trim$(grid_border$)<>"" then 
		grid_footer$="[F2/F10]"(1:grid_cols-1)
	end if
	L59855: !
	grid_footer_len=len(grid_footer$) 
	if grid_footer_len>grid_cols then 
		grid_footer=grid_cols 
		grid_footer$(grid_cols+1:9999)=""
	end if
	grid_footer_scol=max(grid_scol,grid_scol+grid_cols-grid_footer_len) 
	grid_footer_cols=min(grid_footer_len,grid_cols)
	if trim$(grid_border$)<>"" then 
		gridio_footer_handle=fngethandle 
		open #gridio_footer_handle: "SCOL="&str$(grid_footer_scol)&",SROW="&str$(grid_srow+grid_window_rows)&",COLS="&str$(grid_footer_cols)&",ROWS=1",display,outin  
		print #gridio_footer_handle,fields "1,1,"&str$(grid_footer_cols)&"/C 200"&",[S]": grid_footer$ 
		searching$=rpad$("Searching",grid_footer_len,".")
	end if
	if len(searching$)>grid_footer_len then 
		searching$=rpad$("Search",grid_footer_len,".")
	end if
	if len(searching$)>grid_footer_len then 
		searching$=rpad$("",grid_footer_len,".")
	end if
	if trim$(grid_keys$)="" or trim$(grid_labels$)="" then 
		goto L59880 
	else 
		fnlist(":"&grid_keys$,mat grid_keys$,";") 
		fnlist(":"&grid_labels$,mat grid_labels$,";")
	end if
	fnerase_buttons
	label_fkeys=0 
	if len(grid_labels$)<60 then label_fkeys=1
	fngenerate_buttons(srep$(grid_keys$,";",","),srep$(grid_labels$,";",","),1,0,label_fkeys)
	L59880: !
	grid_handle$="1,1,LIST "&str$(grid_window_rows)&"/"&str$(grid_cols) 
	print #gridio_handle, fields grid_handle$&",HEADERS,[Tabs]": (mat grid_heading$,mat max_field,mat grid_form$)
	grid_updates=grid_cache_data=0
	max_cache_data=int(512000/(81*max_fields)) 
	min_cache_data=max(int(max_body/2),25) 
	max_cache_data=min(min_cache_data,max_cache_data) 
	msg$("Populating Grid")
	mat grid_cache_data$(max_cache_data,max_fields)=("")
	for nrecord=1 to udim(grid_data$)
		line_count=fnlist(":"&grid_data$(nrecord)&"³",mat xml_line$,"³")
		mat xml_line$(max_fields)
		grid_cache_data+=1 
		! MAT GRID_CACHE_DATA$(GRID_CACHE_DATA,MAX_FIELDS) 
		! ** Don't bother - We are just using the Max every time!
		for nline=1 to max_fields 
			grid_cache_data$(grid_cache_data,nline)=xml_line$(nline)
		next nline
		if grid_update=0 and grid_cache_data>grid_rows then 
			mat grid_cache_data$(grid_cache_data,max_fields) 
			print #gridio_handle, fields grid_handle$&",=": mat grid_cache_data$ 
			grid_update+=1 
			grid_cache_data=0 
			mat grid_cache_data$(max_cache_data,max_fields)=("")
		end if
		if grid_update>0 and grid_cache_data>=max_cache_data then 
			mat grid_cache_data$(grid_cache_data,max_fields) 
			print #gridio_handle, fields grid_handle$&",+": mat grid_cache_data$ 
			grid_update+=1 
			grid_cache_data=0 
			mat grid_cache_data$(max_cache_data,max_fields)=("")
		end if
	next nrecord 
	msg$("")
	if grid_cache_data>0 then 
		mat grid_cache_data$(grid_cache_data,max_fields) 
		print #gridio_handle, fields grid_handle$&",+": mat grid_cache_data$ 
		grid_update+=1 
		grid_cache_data=0 
		mat grid_cache_data$(0,max_fields)
	end if
	if grid_update=0 then 
		mat grid_cache_data$(grid_cache_data,max_fields) 
		print #gridio_handle, fields grid_handle$&",+": mat grid_cache_data$ 
		grid_update+=1 
		grid_cache_data=0 
		mat grid_cache_data$(max_cache_data,max_fields)=("")
	end if
	L59975: !
	curfld(1,default_start)
	input #gridio_handle, fields grid_handle$&",ROWSUB,SELONE": default_start 
	if cmdkey=93 then cmdkey(99)
	if cmdkey=2 or cmdkey=10 or cmdkey=20 then 
		gosub GRID_SEARCH 
		goto L59975
	end if
	item_fkey=fkey
	if default_start>udim(grid_data$) or default_start<0 then 
		default_start=0 
		item_fkey=99
	end if
	fngridio=default_start
	if xfkey>0 then fkey(item_fkey)
	if gridio_handle then close #gridio_handle: ioerr L59988
	L59988: !
	if gridio_footer_handle then close #gridio_footer_handle: ioerr ignore
fnend 
! r: ------------------XML Management Functions----------------------------
def library fnopen_xml_writer(filename$*256)
	if ~setup then fn_setup
	dim xml_node_stack$(1)*100
	mat xml_node_stack$(0)
	open #(xmlfh:=fngethandle): "NAME="&filename$&",REPLACE,RECL=4096",display,output 
	! PRINT #XMLFH: '<?xml version="1.0"?>'
	fnopen_xml_writer = xmlfh
fnend 
def fnxml_push_node(name$*100)
	mat xml_node_stack$(udim(mat xml_node_stack$) + 1)
	xml_node_stack$(udim(mat xml_node_stack$)) = name$
fnend 
def fnxml_pop_node$*100
	if udim(mat xml_node_stack$) < 1 then 
		xml_node_stack$ = "" 
		goto L61190
	end if
	fnxml_pop_node$ = xml_node_stack$(udim(mat xml_node_stack$))
	mat xml_node_stack$(udim(mat xml_node_stack$) - 1)
	L61190: !
fnend 
def library fnopen_node(xml_filehandle, name$*100; mat attr$, mat attr_val$)
	fnopen_node = fnopen_node_(xml_filehandle, name$, mat attr$, mat attr_val$)
fnend 
def fnopen_node_(xml_filehandle, name$*100; mat attr$, mat attr_val$)
	print #xml_filehandle: "<"&name$;
	if udim(mat attr$) = udim(mat attr_val$) then 
		for iter = 1 to udim(mat attr$)
			print #xml_filehandle: ' '&attr$(iter)&'="'&attr_val$(iter)&'"';
		next iter
	end if 
	print #xml_filehandle: ">";
	fnxml_push_node(name$)
fnend 
def library fnclose_node(xml_filehandle; name$*100)
	fnclose_node = fnclose_node_(xml_filehandle, name$)
fnend 
def fnclose_node_(xml_filehandle; name$*100)
	if len(name$) = 0 then 
		name$ = xml_node_stack$(udim(mat xml_node_stack$))
	end if
	if srch(xml_node_stack$, name$) < 1 then goto L61480
	dim popped$*100
	do 
		popped$ = fnxml_pop_node$
		print #xml_filehandle: "</"&popped$&">"
	loop while popped$ <> name$
	L61480: !
fnend 
def fnclose_all_nodes(xml_filehandle)
	if udim(xml_node_stack$) > 0 then 
		fnclose_node_(xml_filehandle, xml_node_stack$(1))
	end if
fnend 
def library fnclose_xml_writer(xml_filehandle)
	fnclose_all_nodes(xml_filehandle)
	close #xml_filehandle: 
fnend 
def library fnadd_data(xml_filehandle, data$*10000)
	fnadd_data = fnadd_data_(xml_filehandle, data$)
fnend 
def fnadd_data_(xml_filehandle, data$*10000)
	print #xml_filehandle: fnhtml$(data$);
fnend 
def library fnadd_node(xml_filehandle, name$*100, data$*10000; mat attr$, mat attr_val$)
	fnadd_node = fnadd_node_(xml_filehandle, name$, data$, mat attr$, mat attr_val$)
fnend 
def fnadd_node_(xml_filehandle, name$*100, data$*10000; mat attr$, mat attr_val$)
	fnopen_node_(xml_filehandle, name$, mat attr$, mat attr_val$)
	fnadd_data_(xml_filehandle, data$)
	fnclose_node_(xml_filehandle, name$)
fnend 
! /r ------------------XML Management Functions-End------------------------
!
def library fnmulti_select(mat ms_selected$,mat ms_unselected$; cap$*80,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
	fnmulti_select=fn_multi_select(mat ms_selected$,mat ms_unselected$, cap$,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
fnend 
def fn_multi_select(mat ms_selected$,mat ms_unselected$; cap$*80,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)

	if ~setup_fn_multi_select then 
		if ~setup then fn_setup
		dim ms_gof_grid_heading$*1024
		dim ms_unselected_2d$(1,1)*80,ms_selected_2d$(1,1)*80
		dim button_image$(10)*128
		dim button_tooltip$(10)*1024
		dim s_search$*80
		!   MS_Side
		ms_side_unselected=1001
		ms_side_selected=1002
		!   MS_FKey
		ms_fkey_add$=str$(ms_fkey_add=1)
		ms_fkey_add_all$=str$(ms_fkey_add_all=3)
		ms_fkey_remove$=str$(ms_fkey_remove=8)
		ms_fkey_remove_all$=str$(ms_fkey_remove_all=7)
		ms_fkey_side_switch$=str$(ms_fkey_side_switch=4)
		ms_fkey_rotate$=str$(ms_fkey_rotate=5)
		ms_fkey_complete$=str$(ms_fkey_complete=6)
		ms_fkey_cancel$=str$(ms_fkey_cancel=fkey_escape)
		ms_fkey_search$=str$(ms_fkey_search=10)
		ms_fkey_search_reverse$=str$(ms_fkey_search_reverse=20)
		ms_fkey_search_1$=str$(ms_fkey_search_1=2)
		ms_fkey_search_2$=str$(ms_fkey_search_2=12)
		ms_fkey_search_3$=str$(ms_fkey_search_3=10)
		ms_fkey_search_4$=str$(ms_fkey_search_4=20)
		ms_fkey_enter_action=ms_fkey_add
		!   MS_Rotation
		ms_rotation_horizontal=1
		ms_rotation_vertical=2
		ms_rotation_locked=false ! If True than Set the Rotation with MS_Rotation_Default ! This should possilby be passed to the function
		if ~ms_rotation_default then ms_rotation_default=ms_rotation_horizontal
		mat empty_1$(0) : mat empty_2$(0) : mat empty_3$(0)
		setup_fn_multi_select=1
	end if  ! ~Setup_fn_multi_select
	! backup arrays for in case of cancel
	dim ms_before_unselected$(1)*256
	dim ms_before_selected$(1)*256
	mat ms_before_unselected$(udim(mat ms_unselected$))=ms_unselected$
	mat ms_before_selected$(udim(mat ms_selected$))=ms_selected$

	! Remove any duplicates from Mat MS_Unselected$
	for ms_selected_item=1 to udim(mat ms_selected$)
		is_there=srch(mat ms_unselected$,ms_selected$(ms_selected_item))
		if is_there>0 then fnremove_arrayitem$(mat ms_unselected$,is_there)
	next ms_selected_item
	if udim(mat ms_selected$)+udim(mat ms_unselected$)=0 then 
		fnmessagebox("There is nothing to select.", mb_stop+mb_okonly,cap$)
		fkey(99)
		goto Xit_FN_MULTI_SELECT
	end if  ! UDIM(MAT MS_SELECTED$)+UDim(MAT MS_UNSELECTED$)=0

	ms_did_change=0

	ms_column_count=udim(mat ms_grid_heading$)
	gosub MS_2D_BUILD
	! fn_Menu_Backup
	if uprc$(env$("GUIMode"))="ON" then 
		gosub MULTISELECT_GON
	else 
		gosub MULTISELECT_GUI_OFF
	end if  ! UprC$(Env$("GUIMode"))="ON"   /   else 
	display menu : mat empty_1$, mat empty_2$, mat empty_3$ ! or do  fn_Menu_Restore  if fnMenu_Backup was performed above.
	goto Xit_FN_MULTI_SELECT
	! ~*~ ~*~ ~*~ ~*~ ~*~
	MS_2D_BUILD: ! 
	ms_unselected_count=udim(mat ms_unselected$)
	if ms_unselected_count=0 then ms_unselected_enabled=false else ms_unselected_enabled=true
	ms_selected_count=udim(mat ms_selected$)
	if ms_selected_count=0 then ms_selected_enabled=false else ms_selected_enabled=true
	fnmat_1d_to_2d(mat ms_unselected$, mat ms_unselected_2d$, ms_column_count)
	fnmat_1d_to_2d(mat ms_selected$, mat ms_selected_2d$, ms_column_count)
	return  ! MS_2D_BUILD
	MS_ROTATE: ! 
	scr_freeze
	if ms_rotation=ms_rotation_horizontal then 
		ms_rotation=ms_rotation_vertical
	else if ms_rotation=ms_rotation_vertical then 
		ms_rotation=ms_rotation_horizontal
	end if  ! MS_Rotation=MS_Rotation_Horizontal   /   MS_Rotation=MS_Rotation_Vertical
	close #ms_win_unselected_handle: 
	close #ms_win_selected_handle: 
	close #ms_tool_handle: 
	gosub MS_GON_WIN_OPEN
	return  ! MS_ROTATE
	MS_GON_WIN_OPEN: ! 
	scr_freeze
	if ms_rotation<=0 or ms_rotation_locked=true then ms_rotation=ms_rotation_default
	if ms_rotation=ms_rotation_horizontal then ! =
		ms_win_unselected_srow=2 : ms_win_unselected_srow$=str$(ms_win_unselected_srow)
		ms_win_unselected_scol=2 : ms_win_unselected_scol$=str$(ms_win_unselected_scol)
		ms_win_unselected_width=ms_parent_width-2 : ms_win_unselected_width$=str$(ms_win_unselected_width)
		ms_win_unselected_height=int(ms_parent_height/2)-2 : ms_win_unselected_height$=str$(ms_win_unselected_height)

		ms_tool_line=ms_win_unselected_height+3 : ms_tool_line$=str$(ms_tool_line)
		ms_tool_col=-1 : ms_tool_col$=str$(ms_tool_col)
		ms_tool_col_width=-1 : ms_tool_col_width$=str$(ms_tool_col_width)

		ms_win_selected_srow=ms_tool_line+2 : ms_win_selected_srow$=str$(ms_win_selected_srow)
		ms_win_selected_scol=2 : ms_win_selected_scol$=str$(ms_win_selected_scol)
		ms_win_selected_width=ms_parent_width-2 : ms_win_selected_width$=str$(ms_win_selected_width)
		ms_win_selected_height=ms_parent_height-ms_tool_line-2 : ms_win_selected_height$=str$(ms_win_selected_height)
	else if ms_rotation=ms_rotation_vertical then ! ||
		ms_win_unselected_width=int(ms_parent_width/2)-3: ms_win_unselected_width$=str$(ms_win_unselected_width)
		ms_win_unselected_height=ms_parent_height-2 : ms_win_unselected_height$=str$(ms_win_unselected_height)
		ms_win_unselected_scol=2 : ms_win_unselected_scol$=str$(ms_win_unselected_scol)
		ms_win_unselected_srow=2 : ms_win_unselected_srow$=str$(ms_win_unselected_srow)

		ms_tool_line=-1 : ms_tool_line$=str$(ms_tool_line)
		ms_tool_col=ms_win_unselected_width+1 : ms_tool_col$=str$(ms_tool_col)
		ms_tool_col_width=2 : ms_tool_col_width$=str$(ms_tool_col_width)

		ms_win_selected_srow=2 : ms_win_selected_srow$=str$(ms_win_selected_srow)
		ms_win_selected_scol=ms_tool_col+ms_tool_col_width+3 : ms_win_selected_scol$=str$(ms_win_selected_scol)
		ms_win_selected_width=ms_parent_width-ms_tool_col-ms_tool_col_width-3 : ms_win_selected_width$=str$(ms_win_selected_width)
		ms_win_selected_height=ms_parent_height-2 : ms_win_selected_height$=str$(ms_win_selected_height)

		ms_tool_col+=2 : ms_tool_col$=str$(ms_tool_col)
	end if  ! MS_Rotation=MS_Rotation_Horizontal   /   MS_Rotation=MS_Rotation_Vertical
	open #ms_win_unselected_handle:=fngethandle: "SRow="&ms_win_unselected_srow$&",SCol="&ms_win_unselected_scol$&",Rows="&ms_win_unselected_height$&",Cols="&ms_win_unselected_width$&",Border=S,Caption=Unselected,Parent="&ms_parent_handle$,display,output 
	open #ms_win_selected_handle:=fngethandle: "SRow="&ms_win_selected_srow$&",SCol="&ms_win_selected_scol$&",Rows="&ms_win_selected_height$&",Cols="&ms_win_selected_width$&",Border=S,Caption=Selected,Parent="&ms_parent_handle$,display,output 
	print #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",Headers,[Tabs]": (mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$)
	print #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",Headers,[Tabs]": (mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$)
	return  ! MS_GON_WIN_OPEN
	MS_GON_GRID_DISPLAY: ! 
	if ms_unselected_enabled=false and ms_selected_enabled=false then 
		print "Nothing to choose" : pause 
	else if ms_unselected_enabled=false then 
		ms_side_active=ms_side_selected
	else if ms_selected_enabled=false then 
		ms_side_active=ms_side_unselected
	end if  ! MS_Unselected_Enabled=False and MS_Selected_Enabled=False   /   MS_Unselected_Enabled=False   /   MS_Selected_Enabled=False

	if ms_side_active=ms_side_unselected then !     SOURCE=Unselected   DESTINATION=Selected
		print #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",=R,-1": mat ms_unselected_2d$ ! ,B"&Str$(MS_Side_Unselected): Mat MS_Unselected$
		x_end(1)=(udim(mat ms_unselected$)) : print #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&',Attr,-1': (mat one_sub_one, mat x_end, mat attr_t$)
		print #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",=R,"&ms_fkey_side_switch$: mat ms_selected_2d$ ! ,B"&Str$(MS_Side_Selected): Mat MS_Selected$
		x_end(1)=(udim(mat ms_selected$)) : print #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&',Attr,'&ms_fkey_side_switch$: (mat one_sub_one, mat x_end, mat attr_inactive$)
	else if ms_side_active=ms_side_selected then 
		print #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",=R,"&ms_fkey_side_switch$: mat ms_unselected_2d$ ! ,B"&Str$(MS_Side_Unselected): Mat MS_Unselected$
		x_end(1)=(udim(mat ms_unselected$)) : print #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&',Attr,'&ms_fkey_side_switch$: (mat one_sub_one, mat x_end, mat attr_inactive$)
		print #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",=R,-1": mat ms_selected_2d$ ! ,B"&Str$(MS_Side_Selected): Mat MS_Selected$
		x_end(1)=(udim(mat ms_selected$)) : print #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&',Attr,-1': (mat one_sub_one, mat x_end, mat attr_t$)
	end if  ! MS_SIDE_ACTIVE=MS_SIDE_UnselectED   /   MS_SIDE_ACTIVE=MS_SIDE_SELECTED

	if ms_gon_selected_focus_update then gosub MS_GON_SELECTED_FOCUS_TO_END
	if ms_gon_unselected_focus_update then gosub MS_GON_UNSELECTED_FOCUS_TO_END
	return  ! MS_GON_GRID_DISPLAY
	MS_GON_TOOL_DISPLAY: !r: requires MS_Side_Active  (and several other things)
	if ms_rotation<>ms_rotation_hold then 
		if ms_rotation=ms_rotation_horizontal then ! =
			open #ms_tool_handle:=fngethandle: "SRow="&ms_tool_line$&",SCol=1,Rows=1,Cols="&ms_parent_width$&",Picture=icons\ToolBar.gif,Parent="&ms_parent_handle$,display,output  ! VBAR.gif  is up and down       and toolbar.gif is left and right"
			fn_array_quick$(mat button_pos$,'34','37','41','44','70')
			button_image$(1)="Icons\A-Dn.png" ! Add
			button_image$(2)="Icons\R-Up.png" ! Remove
			button_image$(3)="Icons\AA-Dn.png" ! Add All
			button_image$(4)="Icons\RA-Up.png" ! Remove All
			!       BUTTON_IMAGE$(5)="Icons\Rotate.png"
			!       BUTTON_IMAGE$(6)="Icons\Switch.png"
			button_image$(5)="Icons\Complete.png" ! Complete
			for button_item=1 to button_count
				print #ms_tool_handle,fields "1,"&button_pos$(button_item)&",P 1/2,,-1": ""
				print #ms_tool_handle,fields "1,"&button_pos$(button_item)&",P 1/2,,"&button_fkey$(button_item), help "1a;"&button_tooltip$(button_item)&";": button_image$(button_item)&":Isotropic"
			next button_item
		else if ms_rotation=ms_rotation_vertical then ! ||
			open #ms_tool_handle:=fngethandle: "SRow=1,SCol="&ms_tool_col$&",Rows="&ms_parent_height$&",Cols="&ms_tool_col_width$&",Parent="&ms_parent_handle$,display,output  ! VBAR.gif  is up and down       and toolbar.gif is left and right"
			fn_array_quick$(mat button_line$,'10','11','13','14','20')
			button_image$(1)="Icons\A-Right.png" ! Add
			button_image$(2)="Icons\R-Left.png" ! Remove
			button_image$(3)="Icons\AA-Right.png" ! Add All
			button_image$(4)="Icons\RA-Left.png" ! Remove All
			!       BUTTON_IMAGE$(5)="Icons\Rotate.png"
			!       BUTTON_IMAGE$(6)="Icons\Switch.png"
			button_image$(5)="Icons\Complete.png" ! Complete
			!        FOR Button_Item=1 TO Button_Count
			!          PRINT #Ms_Tool_Handle,FIELDS Button_Line$(Button_Item)&",1,P 1/2,,-1": ""
			!          PRINT #Ms_Tool_Handle,FIELDS Button_Line$(Button_Item)&",1,P 1/2,,"&Button_Fkey$(Button_Item), HELP "1a;"&Button_Tooltip$(Button_Item)&';': Button_Image$(Button_Item)&":Isotropic"
			!        NEXT Button_Item
			print #ms_tool_handle,fields button_line$(1)&",1,CC 2,[Toolbar],B"&button_fkey$(1), help "4a;"&button_tooltip$(1)&';': ">"
			print #ms_tool_handle,fields button_line$(2)&",1,CC 2,[Toolbar],B"&button_fkey$(2), help "4a;"&button_tooltip$(2)&';': "<"
			print #ms_tool_handle,fields button_line$(3)&",1,CC 2,[Toolbar],B"&button_fkey$(3), help "4a;"&button_tooltip$(3)&';': ">>"
			print #ms_tool_handle,fields button_line$(4)&",1,CC 2,[Toolbar],B"&button_fkey$(4), help "4a;"&button_tooltip$(4)&';': "<<"
		end if  ! MS_Rotation=MS_Rotation_Horizontal   /   MS_Rotation=MS_Rotation_Vertical
		ms_rotation_hold=ms_rotation
	end if  ! MS_ROTATION<>MS_ROTATION_HOLD
	return  ! /r MS_GON_TOOL_DISPLAY
	MULTISELECT_GON: ! 
	dim ms_parent_caption$*89
	dim ms_gons_field$(4)*40 ! ,MS_GONS_FIELD_DISABLED$(4)*40
	ms_rotation_hold=0
	if trim$(cap$)="" then ms_parent_caption$="" else ms_parent_caption$=",Caption="&cap$
	open #ms_parent_handle:=fngethandle: fnwindowthis_size$(ms_parent_height,ms_parent_width,"Full Screen","S")&ms_parent_caption$&",Parent=0",display,output 
	ms_parent_height$=str$(ms_parent_height)
	ms_parent_width$=str$(ms_parent_width)
	ms_parent_handle$=str$(ms_parent_handle)
	if ms_side_active=0 then ms_side_active=ms_side_unselected
	if ms_rotation=0 then ms_rotation=ms_rotation_default
	gosub MS_GON_WIN_OPEN

	mat one_sub_one(1)=(1) : mat attr_inactive$(1)=('[INACTIVE]') : mat attr_t$(1)=('[T]') : mat x_end(1) ! these are all used only in the GUI on grid display routine.           ! [INACTIVE]

	mat msgo_menu_text$(0) : mat msgo_menu_status$(0) : mat msgo_menu_fkey$(0)
	fnmsgo_menu_item_add('&Navigate')
	fnmsgo_menu_item_add(' Add',ms_fkey_add)
	fnmsgo_menu_item_add(' Add All',ms_fkey_add_all)
	fnmsgo_menu_item_add(' -')
	fnmsgo_menu_item_add(' Remove',ms_fkey_remove)
	fnmsgo_menu_item_add(' Remove All',ms_fkey_remove_all)
	fnmsgo_menu_item_add(' -')
	fnmsgo_menu_item_add(' Rotate',ms_fkey_rotate)
	fnmsgo_menu_item_add(' Switch Sides',ms_fkey_side_switch)
	!   FNMSGO_MENU_ITEM_ADD(' Search',MS_FKEY_SEARCH)
	!   FNMSGO_MENU_ITEM_ADD('  Search 1',MS_FKEY_SEARCH_1)
	!   FNMSGO_MENU_ITEM_ADD('  Search 2',MS_FKEY_SEARCH_2)
	!   FNMSGO_MENU_ITEM_ADD('  Search 3',MS_FKEY_SEARCH_3)
	!   FNMSGO_MENU_ITEM_ADD('  Search 4',MS_FKEY_SEARCH_4)
	fnmsgo_menu_item_add(' -')
	fnmsgo_menu_item_add(' Complete',ms_fkey_complete)
	fnmsgo_menu_item_add(' Cancel',fkey_escape)

	button_count=5
	button_text$(btn_num:=1)='Add' : button_fkey$(btn_num)=ms_fkey_add$ : button_tooltip$(btn_num)="[F"&button_fkey$(btn_num)&"]Select the highlighted item(s)."
	button_text$(btn_num:=2)='Remove' : button_fkey$(btn_num)=ms_fkey_remove$ : button_tooltip$(btn_num)="[F"&button_fkey$(btn_num)&"]\nUnselect the highlighted item(s)."
	button_text$(btn_num:=3)='Add All' : button_fkey$(btn_num)=ms_fkey_add_all$ : button_tooltip$(btn_num)="[F"&button_fkey$(btn_num)&"]\nSelect all items."
	button_text$(btn_num:=4)='Remove All' : button_fkey$(btn_num)=ms_fkey_remove_all$ : button_tooltip$(btn_num)="[F"&button_fkey$(btn_num)&"]\nUnselect all items."
	!   BUTTON_TEXT$(btn_num:=5)="Rotate" : BUTTON_FKEY$(btn_num)=MS_FKEY_ROTATE$ : BUTTON_TOOLTIP$(btn_num)="[F"&BUTTON_FKEY$(btn_num)&"]\nToggle the rotation of the selection grids between horizontal and vertical."
	!   BUTTON_TEXT$(btn_num:=6)="Switch Sides" : BUTTON_FKEY$(btn_num)=MS_FKEY_SIDE_SWITCH$ : BUTTON_TOOLTIP$(btn_num)="[F"&BUTTON_FKEY$(btn_num)&"]\nMove your cursor to the other side (selected or Unselected).  If the other side empty than you will not be able to switch to it."
	button_text$(btn_num:=5)="OK" : button_fkey$(btn_num)=ms_fkey_complete$ : button_tooltip$(btn_num)="[F"&button_fkey$(btn_num)&"]\nProceed.\nComplete.\nDone.\nContinue.\nOK"
	ms_gons_field$(1)="1,10,CU 30/80,[D]S"
	ms_gons_field$(2)="1,41,Check 16/18,[W],1001"
	ms_gons_field$(3)="1,58,Radio 6/6,0,1001"
	ms_gons_field$(4)="1,65,Radio 5/6,0,1001"
	ms_gons_field_count=udim(mat ms_gons_field$)
	mat ms_gons_field$(ms_gons_field_count)
	!   FOR _MS_GONS_FIELD=1 TO MS_GONS_FIELD_COUNT
	!     MS_GONS_FIELD_DISABLED$(_MS_GONS_FIELD)=FN_FIELD_DISABLE$(MS_GONS_FIELD$(_MS_GONS_FIELD))
	!   NEXT _MS_GONS_FIELD

	gosub MS_GON_NAVIGATION_DISPLAY
	!   Input Lists
	MS_INPUT: ! 
	gosub MS_GON_GRID_DISPLAY
	gosub MS_GON_TOOL_DISPLAY
	!   MAT MS_CURRENT_SELECTION(9999)=(0)

	if ms_side_active=ms_side_unselected then 
		ms_fkey_enter_action=ms_fkey_add
		ms_fkey_click_action=ms_fkey_add
		ms_fkey_click_double_action=ms_fkey_add
		input #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",RowCnt,Sel": ms_current_selection_count
		mat ms_current_selection(ms_current_selection_count)
		input #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",RowSub,Sel,NoWait": mat ms_current_selection
		!     INPUT #MS_WIN_UNSELECTED_HANDLE, FIELDS "1,1,List "&MS_WIN_UNSELECTED_HEIGHT$&"/"&MS_WIN_UNSELECTED_WIDTH$&",RowSub,Sel": MAT MS_CURRENT_SELECTION
		gosub MS_FKEY_CONSOLODATE
		if fkey=ms_fkey_add then 
			gosub MS_GON_ADD
			goto MS_INPUT
		else if fkey=105 or fkey=106 then 
			goto MS_INPUT
		else if fkey=ms_fkey_remove then 
			gosub MS_GON_REMOVE
			goto MS_INPUT
		else if fkey=ms_fkey_add_all then 
			gosub MS_GON_ADD_ALL
			goto MS_INPUT
		else if fkey=ms_fkey_remove_all then 
			gosub MS_GON_REMOVE_ALL
			goto MS_INPUT
		else if fkey=ms_fkey_side_switch then 
			ms_side_active=ms_side_selected
			generic_grid_line_current=1
			goto MS_INPUT
		else if fkey=ms_fkey_complete then 
			fkey(0) ! and it fall through
		else if fkey=fkey_exit or fkey=fkey_escape then 
			if ~ms_did_change or fn_confirm('cancel',cap$,'','multi_select_cancel') then ! fn_confirm('cancel',cap$,'You will lose any selections!\nSelected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$)))
				mat ms_unselected$(udim(mat ms_before_unselected$))=ms_before_unselected$
				mat ms_selected$(udim(mat ms_before_selected$))=ms_before_selected$
				!         it fall through
			else ! they did not confirm to complete
				goto MS_INPUT
			end if  ! FN_CONFIRM('complete'...
		else if fkey=ms_fkey_rotate then 
			gosub MS_ROTATE
			goto MS_INPUT
		else if fkey=ms_fkey_search then 
			gosub MS_GON_GRID_SEARCH
			goto MS_INPUT
		else 
			goto MS_INPUT
		end if  ! FKey=MS_FKey_Add   /   FKey=MS_FKey_Add_All   /   FKey=MS_FKey_Side_Switch    /   FKey=MS_FKey_Complete   /   FKey=FKey_Exit   /   FKey=FKey_Escape   /   FKey=MS_FKey_Rotate   /   else 

	else if ms_side_active=ms_side_selected then 
		ms_fkey_enter_action=ms_fkey_remove
		ms_fkey_click_action=ms_fkey_remove
		ms_fkey_click_double_action=ms_fkey_remove
		input #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",RowCnt,Sel": ms_current_selection_count
		mat ms_current_selection(ms_current_selection_count)
		input #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",RowSub,Sel,NoWait": mat ms_current_selection
		!     INPUT #MS_WIN_SELECTED_HANDLE, FIELDS "1,1,List "&MS_WIN_SELECTED_HEIGHT$&"/"&MS_WIN_SELECTED_WIDTH$&",RowSub,Sel": MAT MS_CURRENT_SELECTION
		gosub MS_FKEY_CONSOLODATE
		if fkey=ms_fkey_add then 
			gosub MS_GON_ADD
			goto MS_INPUT
		else if fkey=105 or fkey=106 then 
			goto MS_INPUT
		else if fkey=ms_fkey_remove then 
			gosub MS_GON_REMOVE
			goto MS_INPUT
		else if fkey=ms_fkey_add_all then 
			gosub MS_GON_ADD_ALL
			goto MS_INPUT
		else if fkey=ms_fkey_remove_all then 
			gosub MS_GON_REMOVE_ALL
			goto MS_INPUT
		else if fkey=ms_fkey_side_switch then 
			ms_side_active=ms_side_unselected
			generic_grid_line_current=1
			goto MS_INPUT
		else if fkey=ms_fkey_complete then 
			fkey(0) ! and it fall through
		else if fkey=fkey_exit or fkey=fkey_escape then 
			if fn_confirm('cancel',cap$,'You will lose any selections!\nSelected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$))) then 
				mat ms_unselected$(udim(mat ms_before_unselected$))=ms_before_unselected$
				mat ms_selected$(udim(mat ms_before_selected$))=ms_before_selected$
				!         it fall through
			else ! they did not confirm to complete
				goto MS_INPUT
			end if  ! FN_CONFIRM('complete'...
		else if fkey=ms_fkey_rotate then 
			gosub MS_ROTATE
			goto MS_INPUT
		else if fkey=ms_fkey_search then 
			gosub MS_GON_GRID_SEARCH
			goto MS_INPUT
		else 
			goto MS_INPUT
		end if  ! FKey=MS_FKey_Add   /   FKey=MS_FKey_Add_All   /   FKey=MS_FKey_Side_Switch    /   FKey=MS_FKey_Complete   /   FKey=FKey_Exit   /   FKey=FKey_Escape   /   FKey=MS_FKey_Rotate   /   else 
	else 
		print 'no valid MS_Side_Active ('&str$(ms_side_active)&")" : pause 
	end if  ! MS_Side_Active=MS_Side_Unselected   /   MS_Side_Active=MS_Side_Unselected   /   else 
	close #ms_parent_handle: 
	fnerase_buttons
	return  ! MULTISELECT_GON
	MS_GON_NAVIGATION_DISPLAY: ! r:
	fngenerate_buttons(ms_fkey_side_switch$&','&ms_fkey_search$&','&ms_fkey_search_reverse$&','&ms_fkey_rotate$, 'Switch Sides,Search,^ Search,Rotate', 1, 0, 1)
	fngenerate_buttons(ms_fkey_complete$&','&ms_fkey_cancel$, 'OK,Cancel', 2, 0, 1)

	display menu : mat msgo_menu_text$, mat msgo_menu_fkey$, mat msgo_menu_status$
	return  ! /r MS_GON_NAVIGATION_DISPLAY
	ms_did_change=1
	MS_GON_ADD: ! r:
	ms_did_change=1
	mat tmp_selection(9999)=(0)
	input #ms_win_unselected_handle, fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",RowSub,Sel,NoWait": mat tmp_selection
	tmp_selection_count=srch(mat tmp_selection,0)-1
	mat tmp_selection(tmp_selection_count)
	fn_array_sort(mat tmp_selection)
	for tmp_selection_item=1 to tmp_selection_count
		fn_array_item_move$(mat ms_unselected$, mat ms_selected$, tmp_selection(tmp_selection_item)-tmp_selection_item+1)
	next tmp_selection_item
	gosub MS_2D_BUILD
	ms_gon_selected_focus_update=true
	return  ! /r MS_GON_ADD
	MS_GON_REMOVE: ! r:
	ms_did_change=1
	mat tmp_selection(9999)=(0)
	input #ms_win_selected_handle, fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",RowSub,Sel,NoWait": mat tmp_selection
	tmp_selection_count=srch(mat tmp_selection,0)-1
	mat tmp_selection(tmp_selection_count)
	fn_array_sort(mat tmp_selection)
	for tmp_selection_item=1 to tmp_selection_count
		fn_array_item_move$(mat ms_selected$, mat ms_unselected$, tmp_selection(tmp_selection_item)-tmp_selection_item+1)
	next tmp_selection_item
	gosub MS_2D_BUILD
	ms_gon_unselected_focus_update=true
	return  ! /r
	MS_GON_SELECTED_FOCUS_TO_END: ! r:   set the focus to the bottom of selected
	input #ms_win_selected_handle,fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",RowCnt,All,NoWait": gui_base ! ioerr ...
	curfld(1,gui_base)
	input #ms_win_selected_handle,fields "1,1,List "&ms_win_selected_height$&"/"&ms_win_selected_width$&",Rowsub,SelOne,NoWait": choice
	ms_gon_selected_focus_update=false
	return  ! /r MS_GON_Selected_Focus_to_End
	MS_GON_UNSELECTED_FOCUS_TO_END: ! r: set the focus to the bottom of unselected
	input #ms_win_unselected_handle,fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",RowCnt,All,NoWait": gui_base ! ioerr ...
	curfld(1,gui_base)
	input #ms_win_unselected_handle,fields "1,1,List "&ms_win_unselected_height$&"/"&ms_win_unselected_width$&",Rowsub,SelOne,NoWait": choice
	ms_gon_unselected_focus_update=false
	return  ! /r
	MS_GON_ADD_ALL: ! r: 
	fnarray_add$(mat ms_selected$,mat ms_selected$,mat ms_unselected$) ! FNARRAY_ADD$(MAT ARRAY_COMBINED$,MAT ARRAY_PART_ONE$,MAT ARRAY_PART_TWO$)
	mat ms_unselected$(0)
	gosub MS_2D_BUILD
	ms_gon_selected_focus_update=true
	return  ! /r
	MS_GON_REMOVE_ALL: ! 
	ms_did_change=1
	fnarray_add$(mat ms_unselected$,mat ms_unselected$,mat ms_selected$) ! FNARRAY_Add$(MAT ARRAY_COMBINED$,MAT ARRAY_PART_ONE$,MAT ARRAY_PART_TWO$)
	mat ms_selected$(0)
	gosub MS_2D_BUILD
	ms_gon_unselected_focus_update=true
	return  ! MS_GON_REMOVE_ALL
	MULTISELECT_GUI_OFF: ! 
	for ms_grid_heading_item=1 to udim(mat ms_grid_heading$)
		ms_gof_grid_heading$=ms_gof_grid_heading$&ms_grid_heading$(ms_grid_heading_item)&";"
	next ms_grid_heading_item
	ms_gof_grid_heading$=trim$(ms_gof_grid_heading$,";")
	fndisplay_top(cap$,pk$)
	fnerase_buttons
	ms_fkey_enter_action=ms_fkey_complete
	ms_fkey_click_action=ms_fkey_add
	ms_fkey_click_double_action=ms_fkey_add

	MS_GOF_ASK_SELECTED: ! 
	if udim(mat ms_selected$)=0 then 
		ms_gof_mb_response=fnmessagebox("There is nothing selected.\nWould you like to add selections now?", mb_yesnocancel+mb_question+mb_button1_default,cap$)
		if ms_gof_mb_response=mb_yes then 
			goto MS_GOF_ASK_UNSELECTED
		else if ms_gof_mb_response=mb_no then 
			goto Xit_MS_GO
		else ! MS_GOF_MB_Response=MB_CANCEL
			fkey(99)
			goto Xit_MS_GO
		end if  ! MS_GOF_MB_Response=MB_YES   /   MS_GOF_MB_Response=MB_NO   /   else 
	end if  ! udim(MAT MS_SELECTED$)=0

	ms_gof_as_ws_response=fnwinscroll(mat ms_selected$, udim(mat ms_selected$),"Title=Selected Heading="&ms_gof_grid_heading$,"","Keys="&ms_fkey_complete$&";"&ms_fkey_add$&";"&ms_fkey_add_all$&";"&ms_fkey_side_switch$&";99 Labels=Complete;Remove;Remove All;Unselected;Cancel",scol=3,scroll_srow=0,scroll_cols=0,scroll_rows=0,display_only=0)

	gosub MS_FKEY_CONSOLODATE
	if fkey=ms_fkey_add then 
		fn_array_item_move$(mat ms_selected$, mat ms_unselected$, ms_gof_as_ws_response)
		goto MS_GOF_ASK_SELECTED
	else if fkey=ms_fkey_add_all then 
		fnarray_add$(mat ms_unselected$,mat ms_unselected$,mat ms_selected$)
		mat ms_selected$(0)
		goto MS_GOF_ASK_UNSELECTED
	else if fkey=ms_fkey_side_switch then 
		goto MS_GOF_ASK_UNSELECTED
	else if fkey=ms_fkey_complete then 
		if fn_confirm('complete',cap$,'Selected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$))) then 
			goto Xit_MS_GO
		else ! they did not confirm to complete
			goto MS_GOF_ASK_SELECTED
		end if  ! FN_CONFIRM('complete'...
	else if fkey=fkey_exit or fkey_escape then 
		if fn_confirm('cancel',cap$,'You will lose any selections!\nSelected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$))) then 
			goto Xit_MS_GO
		else ! they did not confirm to complete
			goto MS_GOF_ASK_SELECTED
		end if  ! FN_CONFIRM('complete'...
	end if  ! FKey...

	MS_GOF_ASK_UNSELECTED: ! 
	if udim(mat ms_unselected$)=0 then 
		ms_gof_mb_response=fnmessagebox("Everything is selected.\nWould you like to go to remove items now?", mb_yesnocancel+mb_question+mb_button1_default,cap$)
		if ms_gof_mb_response=mb_yes then 
			goto MS_GOF_ASK_SELECTED
		else if ms_gof_mb_response=mb_no then 
			goto Xit_MS_GO
		else ! MS_GOF_MB_Response=MB_CANCEL
			fkey(fkey_escape)
			goto Xit_MS_GO
		end if  ! MS_GOF_MB_Response=MB_YES   /   MS_GOF_MB_Response=MB_NO   /   else 
	end if  ! udim(MAT MS_SELECTED$)=0
	ms_gof_au_ws_response=fnwinscroll(mat ms_unselected$, ms_gof_au_ws_response,"Title=Unselected Heading="&ms_gof_grid_heading$,"","Keys="&ms_fkey_complete$&";"&ms_fkey_add$&";"&ms_fkey_add_all$&";"&ms_fkey_side_switch$&";99 Labels=Complete;Add;Add All;Selected;Cancel",scol=3,scroll_srow=0,scroll_cols=0,scroll_rows=0,display_only=0)

	gosub MS_FKEY_CONSOLODATE
	if fkey=ms_fkey_add then 
		fn_array_item_move$(mat ms_unselected$, mat ms_selected$, ms_gof_au_ws_response)
		goto MS_GOF_ASK_UNSELECTED
	else if fkey=ms_fkey_add_all then 
		fnarray_add$(mat ms_selected$,mat ms_selected$,mat ms_unselected$)
		mat ms_unselected$(0)
		goto MS_GOF_ASK_SELECTED
	else if fkey=ms_fkey_side_switch then 
		goto MS_GOF_ASK_SELECTED
	else if fkey=ms_fkey_complete then 
		if fn_confirm('complete',cap$,'Selected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$))) then 
			goto Xit_MS_GO
		else ! they did not confirm to complete
			goto MS_GOF_ASK_UNSELECTED
		end if  ! FN_CONFIRM('complete'...
	else if fkey=fkey_exit or fkey_escape then 
		if fn_confirm('cancel',cap$,'You will lose any selections!\nSelected Items:'&tab$&str$(udim(ms_selected$))&'\nUnselected Items:'&tab$&str$(udim(ms_unselected$))) then 
			goto Xit_MS_GO
		else ! they did not confirm to complete
			goto MS_GOF_ASK_UNSELECTED
		end if  ! FN_CONFIRM('complete'...
	end if  ! FKey...
	XIT_MS_GO: ! 
	return  ! MULTISELECT_GUI_OFF
	MS_FKEY_CONSOLODATE: ! 
	if fkey=fkey_menu then fkey(val(msgo_menu_fkey$(menu)))
	if fkey=fkey_click then fkey(ms_fkey_click_action)
	if fkey=fkey_click_double then fkey(ms_fkey_click_double_action)
	if fkey=0 then fkey(ms_fkey_enter_action)
	if fkey=ms_fkey_search_1 or fkey=ms_fkey_search_2 or fkey=ms_fkey_search_3 or fkey=ms_fkey_search_4 then 
		if fkey=ms_fkey_search_1 then ! fkey=2
			ms_gon_search_type=2
		else if fkey=ms_fkey_search_2 then ! fkey=12
			ms_gon_search_type=12
		else if fkey=ms_fkey_search_3 then ! fkey=10
			ms_gon_search_type=10
		else if fkey=ms_fkey_search_4 then ! fkey=20
			ms_gon_search_type=20
		end if  ! FKey=MS_FKEY_Search_1   /   MS_FKEY_Search_2   /   MS_FKEY_Search_3   /   MS_FKEY_Search_4
		fkey(ms_fkey_search)
	end if  ! FKey=MS_FKEY_Search_1 or FKey=MS_FKEY_Search_2 or FKey=MS_FKEY_Search_3 or fkey=MS_FKEY_Search_4
	!   if MS_SIDE_ACTIVE=MS_Side_Unselected then
	!    else ! MS_SIDE_ACTIVE=MS_Side_Selected
	! pause
	!     if fkey=MS_FKEY_ADD then
	!       fkey(MS_FKEY_REMOVE)
	!      else if fkey=MS_FKEY_ADD_ALL then
	!       fkey(MS_FKEY_REMOVE_ALL)
	!      else if fkey=MS_FKEY_REMOVE then
	!       fkey(MS_FKEY_ADD)
	!     end if ! fkey=...
	!   end if ! MS_SIDE_ACTIVE=MS_Side_Unselected   /   else 
	return  ! MS_FKey_Consolodate
	MS_GON_GRID_SEARCH: ! Search MultiSelect GUI On Grid
	!   uses: MS_GON_Search_Type and
	!     GOSUB RecNumSel_REFRESH ! grid must be in origional sort order
	if ms_side_active=ms_side_unselected then row_count=udim(mat ms_unselected$) else row_count=udim(mat ms_selected$)
	generic_search_return=fn_generic_search(ms_gon_search_type,seek_col=1,row_count)
	if generic_search_return>0 then curfld(1,generic_search_return)
	gosub MS_GON_NAVIGATION_DISPLAY
	!     FKEY(-1) ! set FKey so loop will continue back to the input
	return  ! MS_GON_GRID_SEARCH
	XIT_FN_MULTI_SELECT: ! 
fnend  ! fn_multi_select
def fnmsgo_menu_item_add(msgo_menu_text$*256; msgo_menu_fkey)
	msgo_menu_status$='E'
	if msgo_menu_fkey=99 then 
		msgo_menu_fkey$='[Esc]'
	else if msgo_menu_fkey>0 then 
		msgo_menu_fkey$='[F'&str$(msgo_menu_fkey)&']'
	else 
		msgo_menu_fkey$=''
		msgo_menu_status$=''
	end if  ! MSGO_Menu_FKey=...
	if msgo_menu_fkey$<>'' then msgo_menu_text$=msgo_menu_text$&tab$&msgo_menu_fkey$
	fnadd_one$(mat msgo_menu_text$,msgo_menu_text$)
	fnadd_one$(mat msgo_menu_status$,msgo_menu_status$)
	fnadd_one$(mat msgo_menu_fkey$,str$(msgo_menu_fkey))
fnend  ! fnMSGO_Menu_Item_Add
def library fnwindow_this_size$*40(&win_height,&win_width; position$*80,border$*1)
	fnwindow_this_size$=fnwindowthis_size$(win_height,win_width, position$, border$)
fnend 
def fnwindowthis_size$*100(&win_height,&win_width; position$*80, border$*1)
	if ~setup then fn_setup
	dim wts_border$*9
	position$=uprc$(trim$(position$))
	if position$="" then position$="CENTER"
	if position$="FULL" then position$="FULL SCREEN"
	if trim$(border$)='' then border=false else border=true
	WTS_DIMENSIONS_SET: ! 
	if position$="FULL SCREEN" and border then 
		win_srow=2
		win_scol=2
		win_height=session_rows-2
		win_width=session_cols-2
	else if position$="FULL SCREEN" and ~border then 
		win_srow=1
		win_scol=1
		win_height=session_rows
		win_width=session_cols
	else if position$="CENTER" then 
		win_srow=int((session_rows-win_height)/2)+1
		win_scol=int((session_cols-win_width)/2)
	end if  ! Position$= full screen or center (+- border)
	if fn_find_and_remove(position$,'TOP')>0 then 
		if border then 
			win_srow=2
		else ! ~border
			win_srow=1
		end if  ! border   /   else 
		if win_height=0 then print 'sorry, Win_Height=0 and position TOP not compatable.' : pause 
	end if  ! fn_find_and_remove(Position$,'TOP')>0
	if fn_find_and_remove(position$,'BOTTOM')>0 then 
		if win_height=0 then print 'sorry, Win_Height=0 and position BOTTOM not compatable.' : pause 
		win_srow=session_rows-win_height-1
	end if  ! fn_find_and_remove(Position$,'BOTTOM')>0
	if fn_find_and_remove(position$,'LEFT')>0 then 
		if win_width=0 then print 'sorry, Win_Width=0 and position LEFT not compatable, or are they type go and find out... if it works - delete this line.' : pause 
		if border then 
			win_scol=2
		else ! ~border
			win_scol=1
		end if  ! border   /   else 
	end if  ! fn_find_and_remove(Position$,'LEFT')>0
	if fn_find_and_remove(position$,'RIGHT')>0 then 
		if win_width=0 then print 'sorry, Win_Width=0 and position RIGHT not compatable.' : pause 
		if border then 
			win_scol=session_cols-win_width-2
		else ! ~border
			win_scol=session_cols-win_width
		end if  ! border   /   else 
	end if  ! fn_find_and_remove(Position$,'RIGHT')
	if win_width<=0 or win_height<=0 or win_srow<=0 or win_scol<=0 then 
		print "Somethin unhandled occured in fnWindowThis_Size$." : pause 
		if uprc$(env$("Debug"))="YES" then pause 
		position$="CENTER"
		goto WTS_DIMENSIONS_SET
	end if  ! Win_Width<=0 or Win_Height<=0 or Win_SRow<=0 or Win_SCol<=0

	if border$<>'' then wts_border$=',Border='&border$ else wts_border$=''

	fnwindowthis_size$="SRow="&str$(win_srow)&",SCol="&str$(win_scol)&",Rows="&str$(win_height)&",Cols="&str$(win_width)&wts_border$
fnend 
!
IGNORE: continue 
!
def library fnconfirm(verb$*40,cap$*80; text_addition$*2048,confirm_dont_ask_again_key$*28)
	fnconfirm=fn_confirm(verb$,cap$, text_addition$,confirm_dont_ask_again_key$)
fnend 
def fn_confirm(verb$*40,cap$*80; text_addition$*2048,confirm_dont_ask_again_key$*28)
	! Verb$ - something like "confirm" or "cancel" or "delete" or "complete"
	if ~setup then fn_setup
	confirm_fkey_origional=fkey
	if lwrc$(text_addition$(1:2))='\n' then text_addition$=text_addition$(3:len(text_addition$))
	if lwrc$(text_addition$(len(text_addition$)-1:len(text_addition$)))='\n' then text_addition$=text_addition$(1:len(text_addition$)-2)
	if text_addition$<>'' then text_addition$='\n\n'&text_addition$
	if verb$='delete' then confirm_button_add=mb_button2_default else confirm_button_add=mb_button1_default
	mb_repsonse=fnmessagebox('Do you really want to '&lwrc$(verb$)&'?'&text_addition$,mb_question+mb_yesno+confirm_button_add,cap$,confirm_dont_ask_again_key$)
	fkey(confirm_fkey_origional)
	if mb_repsonse=mb_yes then fn_confirm=1 else fn_confirm=0
fnend 
def library fnconfirm_delete(what_you_deleting$*60,cap$*80;pk$*80,confirm_dont_ask_again_key$*28)
	fnconfirm_delete=fn_confirm('delete',cap$,what_you_deleting$,confirm_dont_ask_again_key$)
fnend 
def library fnarray_quick$(mat array$,a$*128; b$*128,c$*128,d$*128,e$*128,f$*128,g$*128,h$*128,i$*128,j$*128,k$*128,l$*128,m$*128)
	fnarray_quick$=fn_array_quick$(mat array$,a$, b$,c$,d$,e$,f$,g$,h$,i$,j$,k$,l$,m$)
fnend  ! FN_ARRAY_QUICK$
def fn_array_quick$(mat array$,a$*128; b$*128,c$*128,d$*128,e$*128,f$*128,g$*128,h$*128,i$*128,j$*128,k$*128,l$*128,m$*128)
	if ~setup then fn_setup
	mat array$(0)
	fnadd_one$(mat array$,a$,no_add_blanks=1)
	fnadd_one$(mat array$,b$,no_add_blanks)
	fnadd_one$(mat array$,c$,no_add_blanks)
	fnadd_one$(mat array$,d$,no_add_blanks)
	fnadd_one$(mat array$,e$,no_add_blanks)
	fnadd_one$(mat array$,f$,no_add_blanks)
	fnadd_one$(mat array$,g$,no_add_blanks)
	fnadd_one$(mat array$,h$,no_add_blanks)
	fnadd_one$(mat array$,i$,no_add_blanks)
	fnadd_one$(mat array$,j$,no_add_blanks)
	fnadd_one$(mat array$,k$,no_add_blanks)
	fnadd_one$(mat array$,l$,no_add_blanks)
	fnadd_one$(mat array$,m$,no_add_blanks)
fnend  ! fnArray_Quick$
def library fnarray_quick(mat array,an; bn,cn,dn,en,ffn,gn,n_08,n_09,n_10, n_11,n_12,n_13,n_14,n_15,n_16,n_17,n_18,n_19,n_20)
	fnarray_quick=fn_array_quick(mat array,an, bn,cn,dn,en,ffn,gn,n_08,n_09,n_10, n_11,n_12,n_13,n_14,n_15,n_16,n_17,n_18,n_19,n_20)
fnend  ! FN_ARRAY_QUICK$
def fn_array_quick(mat array,an; bn,cn,dn,en,ffn,gn,n_08,n_09,n_10, n_11,n_12,n_13,n_14,n_15,n_16,n_17,n_18,n_19,n_20)
	if ~setup then fn_setup
	mat array(0)
	fnadd_one(mat array,an,no_add_blanks=1)
	fnadd_one(mat array,bn,no_add_blanks)
	fnadd_one(mat array,cn,no_add_blanks)
	fnadd_one(mat array,dn,no_add_blanks)
	fnadd_one(mat array,en,no_add_blanks)
	fnadd_one(mat array,ffn,no_add_blanks)
	fnadd_one(mat array,gn,no_add_blanks)
	fnadd_one(mat array,n_08,no_add_blanks)
	fnadd_one(mat array,n_09,no_add_blanks)
	fnadd_one(mat array,n_10,no_add_blanks)
	fnadd_one(mat array,n_11,no_add_blanks)
	fnadd_one(mat array,n_12,no_add_blanks)
	fnadd_one(mat array,n_13,no_add_blanks)
	fnadd_one(mat array,n_14,no_add_blanks)
	fnadd_one(mat array,n_15,no_add_blanks)
	fnadd_one(mat array,n_16,no_add_blanks)
	fnadd_one(mat array,n_17,no_add_blanks)
	fnadd_one(mat array,n_18,no_add_blanks)
	fnadd_one(mat array,n_19,no_add_blanks)
	fnadd_one(mat array,n_20,no_add_blanks)
fnend  ! fnArray_Quick
def library fnarray_append$(mat array$,a$*128; b$*128,c$*128,d$*128,e$*128,f$*128,g$*128)
	fnarray_append$=fn_array_append$(mat array$,a$, b$,c$,d$,e$,f$,g$)
fnend  ! fnArray_Append$
def fn_array_append$(mat array$,a$*128; b$*128,c$*128,d$*128,e$*128,f$*128,g$*128)
	if ~setup then fn_setup
	fnadd_one$(mat array$,a$,no_add_blanks=1)
	fnadd_one$(mat array$,b$,no_add_blanks)
	fnadd_one$(mat array$,c$,no_add_blanks)
	fnadd_one$(mat array$,d$,no_add_blanks)
	fnadd_one$(mat array$,e$,no_add_blanks)
	fnadd_one$(mat array$,f$,no_add_blanks)
	fnadd_one$(mat array$,g$,no_add_blanks)
fnend  ! fnArray_Append$
def library fnarray_append(mat array,an; bn,cn,dn,en,ffn,gn)
	fnarray_append=fn_array_append(mat array,an, bn,cn,dn,en,ffn,gn)
fnend  ! fnArray_Append
def fn_array_append(mat array,an; bn,cn,dn,en,ffn,gn)
	if ~setup then fn_setup
	fnadd_one(mat array,an,no_add_blanks=1)
	fnadd_one(mat array,bn,no_add_blanks)
	fnadd_one(mat array,cn,no_add_blanks)
	fnadd_one(mat array,dn,no_add_blanks)
	fnadd_one(mat array,en,no_add_blanks)
	fnadd_one(mat array,ffn,no_add_blanks)
	fnadd_one(mat array,gn,no_add_blanks)
fnend  ! fnArray_Append
def library fnarray_item_move$(mat from_array$, mat to_array$, array_item)
	fnarray_item_move$=fn_array_item_move$(mat from_array$, mat to_array$, array_item)
fnend  ! FNARRAY_ITEM_MOVE$
def fn_array_item_move$(mat from_array$, mat to_array$, array_item)
	if ~setup then fn_setup
	fnadd_one$(mat to_array$, from_array$(array_item))
	fnremove_arrayitem$(mat from_array$, array_item)
fnend 
def library fnarray_item_move(mat from_array, mat to_array, array_item)
	fnarray_item_move=fn_array_item_move(mat from_array, mat to_array, array_item)
fnend  ! FNARRAY_ITEM_MOVE
def fn_array_item_move(mat from_array, mat to_array, array_item)
	if ~setup then fn_setup
	fnadd_one(mat to_array, from_array(array_item))
	fnremove_arrayitem(mat from_array, array_item)
fnend 
def library fnarray_sort$(mat as$)
	fnarray_sort$=fn_array_sort$(mat as$)
fnend  ! fnArray_Sort$
def fn_array_sort$(mat as$)
	dim as_before$(1)*128,as_before_u$(1)*128
	mat as_before$(udim(mat as$))=as$ 
	mat as_before_u$(udim(mat as$))
	for _as_before=1 to udim(as_before$) 
		as_before_u$(_as_before)=uprc$(as$(_as_before)) 
	next _as_before
	mat as_sequence(udim(mat as_before$))=aidx(as_before_u$)
	for as_sequence_item=1 to udim(as_sequence)
		as$(as_sequence_item)=as_before$(as_sequence(as_sequence_item))
	next as_sequence_item
fnend  ! FN_ARRAY_Sort$
def library fnarray_sort(mat as)
	fnarray_sort=fn_array_sort(mat as)
fnend  ! FNArray_Sort
def fn_array_sort(mat as)
	dim as_before(1)
	mat as_before(udim(mat as))
	mat as_before=as
	mat as_sequence(udim(mat as_before))=aidx(as_before)
	for as_sequence_item=1 to udim(as_sequence)
		as(as_sequence_item)=as_before(as_sequence(as_sequence_item))
	next as_sequence_item
fnend  ! FN_Array_Sort
def library fnfind_and_remove(&far_find_in$,far_remove$*128)
	fnfind_and_remove=fn_find_and_remove(far_find_in$,far_remove$)
fnend  ! fnfind_and_remove
def fn_find_and_remove(&far_find_in$,far_remove$*128)
	set_fn_find_and_remove=0
	far_pos_start=pos(far_find_in$,far_remove$)
	if far_pos_start>0 then 
		far_pos_end=far_pos_start+len(far_remove$)
		far_find_in$(far_pos_start:far_pos_end)=''
		set_fn_find_and_remove=1
	end if  ! FAR_Pos_Start>0
	fn_find_and_remove=set_fn_find_and_remove
fnend  ! fn_find_and_remove
def library fnfield_disable$*1024(fd_field$*1024)
	fnfield_disable$=fn_field_disable$(fd_field$)
fnend  ! fnField_Disable
def fn_field_disable$*1024(fd_field$*1024)
	dim set_fnfield_disable$*1024
	set_fnfield_disable$=fd_field$
	set_fnfield_disable$=srep$(set_fnfield_disable$,"[D]ST","[P]SP")
	set_fnfield_disable$=srep$(set_fnfield_disable$,"[D]S","[P]SP")
	pos_comma_last=pos(set_fnfield_disable$,",",-1)
	hotkey_value=val(set_fnfield_disable$(pos_comma_last+1:9999)) conv XIT_FN_FIELD_DISABLE
	if hotkey_value>0 then 
		set_fnfield_disable$(pos_comma_last:9999)=''
	end if  ! hotkey_value>0
	XIT_FN_FIELD_DISABLE: ! 
	fn_field_disable$=set_fnfield_disable$
fnend  ! fn_Field_Disable
def library fngeneric_search_ask(&s_search$,&search_seek,&search_find; skey)
	if ~setup_fn_grid_search then 
		setup_fn_grid_search=1
		if ~setup then fn_setup
		dim search_type$*20,search$*80,search_direction$(2)*30,search_seek$*30
		dim list_data$(1)*80,search_match$*256,prior_search$*80,searching$*80
		mat empty$(0)
		dim search_form$(4)*40
		search_form$(1)="1,10,CU 30/80,[D]S" 
		search_form$(2)="1,41,Check 16/18,[W],1001" 
		search_form$(3)="1,58,Radio 6/6,0,1001" 
		search_form$(4)="1,65,Radio 5/6,0,1001"
	end if  ! ~SETUP_FN_GRID_SEARCH
	if skey=02 then 
		search_seek=1 : search_find=01 : search_type$="Seek" 
	else if skey=10 then 
		search_seek=0 : search_find=01 : search_type$="Find" 
	else if skey=12 then 
		search_seek=1 : search_find=-1 : search_type$="Rev.Seek" 
	else if skey=20 then 
		search_seek=0 : search_find=-1 : search_type$="Rev.Find"
	end if
	open #grid_search_handle:=fngethandle: "SCol=1,SRow=24,Cols=80,Rows=1",display,outin  
	print #grid_search_handle: newpage
	display menu : mat empty$, mat empty$, mat empty$
	! FNERASE_BUTTONS 
	! ** Note: This Erase_buttons will cause the buttons from the calling program to be erased 
	! As this function cannot restore the buttons, the calling program should handle this
	GRID_SEARCH_ASK_AGAIN: ! 
	if search_seek=1 then 
		search_seek$="Extended search" 
	else 
		search_seek$="^Extended search"
	end if
	if search_find=1 then 
		search_direction$(1)="^Down" 
		search_direction$(2)="Up" 
	else 
		search_direction$(1)="Down" 
		search_direction$(2)="^Up"
	end if
	if env$("GUIMODE")="ON" then 
		print #grid_search_handle,fields "1,1,Cr 9/15,[W];1,71,CC 7,[Button],B0": search_type$&":","Search" 
	else 
		print #grid_search_handle,fields "1,1,Cr 9/15,[W];1,71,CC 7,[Button_CUI],B0": search_type$&":","Search"
	end if
	if env$("GUIMODE")="ON" then 
		rinput #grid_search_handle,fields mat search_form$,attr '[A]': s_search$,search_seek$,mat search_direction$ 
	else 
		rinput #grid_search_handle, fields "1,10,CU 50/80,[D]AE",attr '[A]': s_search$
	end if
	if search_seek$(1:1)<>"^" then 
		search_seek=1 
	else 
		search_seek=0
	end if
	if search_direction$(1)(1:1)="^" then  search_find=1 else search_find=-1
	if fkey=1001 then goto GRID_SEARCH_ASK_AGAIN             ! ** After user selects a choice, allow them to type more
	close #grid_search_handle: 
	s_search$=uprc$(trim$(s_search$))
fnend  ! fnGeneric_Search_Ask

! Updateable Region . Generic Grid Search . Top
! This region was last updated on 2008.06.04 at 1:13 pm
def fn_generic_search(generic_fkey, generic_key_column, generic_grid_line_last)
	! This function returns for Cancel a -1 and a FKey=93 or FKey=99
	if ~setup_generic then 
		setup_generic=1
		dim generic_line$*1024
		dim generic_search_criteria$*80,generic_search_criteria_prior$*80
		generic_grid_line_current=1
	end if  ! ~SETUP_Generic
	if generic_fkey<>2 and generic_fkey<>10 and generic_fkey<>12 and generic_fkey<>20 then print 'Invalid Generic_FKey='&str$(fkey) : pause 
	if generic_key_column=0 then generic_key_column=1
	set_fn_generic_search=-1
	fngeneric_search_ask(generic_search_criteria$,generic_one_column_only,generic_line_increment,generic_fkey) ! ask criteria and options
	if fkey<>93 and fkey<>99 then 
		generic_found_it=0
		if generic_search_criteria$<>generic_search_criteria_prior$ then generic_grid_line_current=1 : generic_search_criteria_prior$=generic_search_criteria$
		generic_grid_line_first_test=generic_grid_line_current
		do 
			generic_line$=fn_generic_grid_to_line$(generic_grid_line_current,generic_one_column_only*generic_key_column)
			if generic_one_column_only and lwrc$(generic_line$(1:len(generic_search_criteria$)))=lwrc$(generic_search_criteria$) or ~generic_one_column_only and pos(lwrc$(generic_line$),lwrc$(generic_search_criteria$))>0 then generic_found_it=generic_grid_line_current
			generic_grid_line_current=generic_grid_line_current+=generic_line_increment ! Search next line first
			if generic_grid_line_current>generic_grid_line_last then generic_grid_line_current=1
			if generic_grid_line_current<0 then generic_grid_line_current=generic_grid_line_last
		loop until generic_found_it>0 or generic_grid_line_first_test=generic_grid_line_current
		set_fn_generic_search=generic_found_it
	end if  ! FKey<>93 and FKey<>99
	fn_generic_search=set_fn_generic_search
fnend  ! fn_Generic_Search
! Updateable Region . Generic Grid Search . End

def fn_generic_grid_to_line$*1024(array_line,single_column_only)
	! This function is custom for every different type of grid.  It pulls from local variables and returns The data from that line of grid for fn_Generic_Search
	column_count=6
	if ms_side_active=ms_side_unselected then 
		row_count=udim(mat ms_unselected$)
	else ! MS_SIDE_ACTIVE=MS_SIDE_SELECTED
		row_count=udim(mat ms_selected$)
	end if  ! MS_SIDE_ACTIVE=MS_SIDE_UNSELECTED   /   else 
	if array_line>0 and array_line<=row_count then 
		if single_column_only and single_column_only<=column_count then 
			if ms_side_active=ms_side_unselected then 
				keep_len=pos(ms_unselected$(array_line),"³")-1
				if keep_len<=0 then keep_len=len(ms_unselected$(array_line))
				fn_generic_grid_to_line$=ms_unselected$(array_line)(1:keep_len)
			else 
				keep_len=pos(ms_selected$(array_line),"³")-1
				if keep_len<=0 then keep_len=len(ms_selected$(array_line))
				fn_generic_grid_to_line$=ms_selected$(array_line)
			end if  ! MS_SIDE_ACTIVE=MS_SIDE_UNSELECTED   /   else 
		else ! ~Single_Column_Only or Single_Column_Only>Column_Count
			if ms_side_active=ms_side_unselected then 
				fn_generic_grid_to_line$=ms_unselected$(array_line)
			else 
				fn_generic_grid_to_line$=ms_selected$(array_line)
			end if  ! MS_SIDE_ACTIVE=MS_SIDE_UNSELECTED   /   else 
		end if  ! Single_Column_Only   /   else 
	else 
		fn_generic_grid_to_line$=''
	end if  ! Array_Line>0 and Array_Line<=UDim(Mat Col_1)
fnend  ! fn_Generic_Grid_To_Line$
def library fnlistboxae_process$(laep_fkey,&tcurfld,max_rows)
	action$="Error"
	if laep_fkey=93 then 
		action$="Exit"
	else if laep_fkey=99 then 
		action$="Cancel"
	else if laep_fkey=91 or laep_fkey=102 or laep_fkey=111 then 
		action$="Loop"
		tcurfld-=1
	else if laep_fkey=104 or laep_fkey=110 or laep_fkey=114 then 
		action$="Loop"
		tcurfld+=1
	else if (laep_fkey>=100 and laep_fkey<=116) then 
		action$="Loop"
	else if laep_fkey=120 then ! Shift+End or Ctrl+End
		action$="Loop"
	else if (laep_fkey>=126 and laep_fkey<=127) then ! Ctrl+Up   Ctrl+Down
		action$="Loop"
	else if (laep_fkey>=133 and laep_fkey<=136) then !  Shift+Left   Shift+Right   Shift+Up   Shift+Down
		action$="Loop"
	else if laep_fkey=200 or laep_fkey=201 or laep_fkey=206 then 
		action$="Zoom"
	else 
		action$="OK" ! happens for any unrecognized FKey
	end if  ! LAEP_FKey=...
	tcurfld=min(max_rows,tcurfld)
	tcurfld=max(1,tcurfld)
	fnlistboxae_process$=action$
fnend  ! fnListBoxAE_Process
def library fnarray_range_insert$(mat ari_array$,ari_insert_after_item,ari_insert_item_count)
	fnarray_range_insert$=fn_array_range_insert$(mat ari_array$,ari_insert_after_item,ari_insert_item_count)
fnend  ! fnArray_Range_Insert$
def fn_array_range_insert$(mat ari_array$,ari_insert_after_item,ari_insert_item_count)
	if ari_insert_after_item>udim(mat ari_array$) then ari_insert_after_item=udim(mat ari_array$)
	if ari_insert_item_count>0 then 
		ari_count_origional=udim(mat ari_array$)
		ari_count_new=ari_count_origional+ari_insert_item_count
		mat ari_array$(ari_count_new)
		if ari_insert_after_item=0 then ! insert at beginning
			mat ari_array$(ari_insert_item_count+1:ari_count_new)=ari_array$(1:ari_count_origional)
			mat ari_array$(1:ari_insert_item_count)=("")
		else if ari_insert_after_item=ari_count_origional then ! insert at ending
			mat ari_array$(ari_count_origional+1:ari_count_new)=("")
		else ! insert in middle
			mat ari_array$(ari_insert_after_item+ari_insert_item_count+1:ari_count_new)=ari_array$(ari_insert_after_item+1:ari_count_origional)
			mat ari_array$(ari_insert_after_item+1:ari_insert_after_item+ari_insert_item_count)=("")
		end if  ! ARI_Insert_After_Item=...
	end if  ! ARI_Insert_Item_Count>0
fnend  ! fn_Array_Range_Insert$
def library fnarray_range_insert(mat ari_array,ari_insert_after_item,ari_insert_item_count)
	fnarray_range_insert=fn_array_range_insert(mat ari_array,ari_insert_after_item,ari_insert_item_count)
fnend  ! fnArray_Range_Insert
def fn_array_range_insert(mat ari_array,ari_insert_after_item,ari_insert_item_count)
	if ari_insert_after_item>udim(mat ari_array) then ari_insert_after_item=udim(mat ari_array)
	if ari_insert_item_count>0 then 
		ari_count_origional=udim(mat ari_array)
		ari_count_new=ari_count_origional+ari_insert_item_count
		mat ari_array(ari_count_new)
		if ari_insert_after_item=0 then ! insert at beginning
			mat ari_array(ari_insert_item_count+1:ari_count_new)=ari_array(1:ari_count_origional)
			mat ari_array(1:ari_insert_item_count)=(0)
		else if ari_insert_after_item=ari_count_origional then ! insert at ending
			mat ari_array(ari_count_origional+1:ari_count_new)=(0)
		else ! insert in middle
			mat ari_array(ari_insert_after_item+ari_insert_item_count+1:ari_count_new)=ari_array(ari_insert_after_item+1:ari_count_origional)
			mat ari_array(ari_insert_after_item+1:ari_insert_after_item+ari_insert_item_count)=(0)
		end if  ! ARI_Insert_After_Item=...
	end if  ! ARI_Insert_Item_Count>0
fnend  ! fn_Array_Range_Insert
def library fnmenu2(m2_title$*80,m2_cap$*80,&m2_pk$,mat m2_option$; m2_default_selection$*256, m2_trust$*80,m2_timeout,m2_footer$*80,m2_preserve_backgroud)
	if ~setup then fn_setup
	fnmenu2=fn_menu2(m2_title$,m2_cap$,m2_pk$,mat m2_option$, m2_default_selection$, m2_trust$,m2_timeout,m2_footer$,m2_preserve_backgroud)
fnend  ! Fnmenu2
def fn_menu2(m2_title$*80,m2_cap$*80,m2_pk$*80,mat m2_option$; m2_default_selection$*256, m2_trust$*80,m2_timeout,m2_footer$*80,m2_preserve_backgroud) ! M2_
	! Mat M2_Option$ - options - each item should delimit it's fields with ³.
	! This function consumes and will not return the following FKey values
	!        8000-8999 reserved for internal use.
	!        182
	!        200, 201, 206
	!        102-107
	! todo: 2-9   F:\SIUL\CLSINC\PROG1\PRINTACT.wb   are having issues program needs update
	! todo: dynamic height needs to push up one line when cute.
	! This function returns the origional row number of the item selected. by the user, or -10093/F93, or -10099/F99, or 0/F[antything not handled explicitly by this function]
	dim m2_field$(2)*80,m2_return,m2_option_text$(1)*80
	dim m2_option_2d$(1,1)*80,m2_heading$(1)*80
	dim m2_big_empty$*100,m2_footer_tooltip$*128
	dim m2_option_col1$(1)*80,m2_filter$*80
	m2_big_empty$=rpt$(chr$(0),100)
	if m2_parent_handle>0 and file$(m2_parent_handle)=":CON:" then goto M2_ASK ! XXX EXPIRAMENTAL
	if m2_preserve_backgroud=0 then 
		open #m2_display_top_parent:=fngethandle: 'SRow=1,SCol=1,Cols=80,Rows=24,Border=None',display,output 
	else if m2_preserve_backgroud=2 then 
		dim m2_dtp_data$(1)*128,dumb_tmp$*2048
		dumb_tmp$=fnopen_parent$('Parent=None,SRow=1,SCol=1,Cols=80,Rows=30,Border=None,Name=M2,button.fkey=0;99,button.text=OK;Exit',mat m2_dtp_data$,1)
		if jbowman then pr 'dumb_tmp$=';dumb_tmp$ : pause
		open #m2_display_top_parent:=fngethandle: dumb_tmp$,display,output 
		fngenerate_buttons_for_window(mat m2_dtp_data$,m2_display_top_parent)
	else 
		m2_display_top_parent=0
	end if  ! m2_preserve_backgroud=0   /   2   /   else 

	! if env$("UserName")="JBowman" then pause

	m2_cap$=trim$(srep$(m2_cap$,"["&m2_pk$&"]",""))
	m2_cap$=trim$(srep$(m2_cap$,"["&trim$(m2_pk$,'-')&"]",""))
	m2_pk$=fnpk_cleanup$(m2_pk$,1)(1:80)
	m2_cap$=trim$(srep$(m2_cap$,"["&m2_pk$&"]",""))
	m2_cap$=srep$(m2_cap$,' & ',' and ')
	if m2_title$='' then m2_title$=m2_cap$
	if m2_display_top_parent>0 then fndisplay_top(m2_title$,m2_pk$,0,m2_trust$,m2_display_top_parent)
	m2_option_count=udim(mat m2_option$)
	mat m2_option_text$(m2_option_count)
	m2_help_available=fnsecurity("GUI:MENU.HELP",'',89,fnuser_init$,inquiry_only:=1) : if m2_help_available=-1 then m2_help_available=0
	m2_return=0
	m2_type_and_grow=0
	m2_option_col1_len_max=0
	m2_rinput_process_count=0
	m2_stime_start=fnstime(time$)
	m2_tick_delay=30
	if m2_timeout>0 and m2_timeout<m2_tick_delay then m2_tick_delay=m2_timeout
	if m2_option_count=20 then 
		m2_parent_srow$=str$(m2_parent_srow=2)
	else if m2_option_count=19 or m2_option_count>20 then 
		m2_parent_srow$=str$(m2_parent_srow=3)
	else 
		m2_parent_srow$=str$(m2_parent_srow=4)
	end if  ! M2_Option_Count=>20   /   =19   /   else 
	m2_parent_height$=str$(m2_parent_height=min(session_rows-m2_parent_srow,m2_option_count+3))
	m2_parent_width$=str$(m2_parent_width=72)
	open #m2_parent_handle:=fngethandle: 'Parent='&str$(m2_display_top_parent)&',SRow='&m2_parent_srow$&',SCol=4,Rows='&m2_parent_height$&',Cols='&m2_parent_width$&',Border=S[N],Tab='&m2_cap$,display,output 
	m2_parent_handle$=str$(m2_parent_handle)
	m2_grid_height$=str$(m2_grid_height=m2_parent_height-2)
	m2_grid_width$=str$(m2_grid_width=m2_parent_width-3)
	fn_m2_setup_grid(mat m2_option$, mat m2_option_2d$,mat m2_heading$,mat m2_width,mat m2_form$)
	mat m2_option_col1$(m2_option_count)
	for m2_option_col1_item=1 to m2_option_count
		m2_option_col1$(m2_option_col1_item)=trim$(m2_option_2d$(m2_option_col1_item,1))
		m2_option_col1_len_max=max(len(m2_option_col1$(m2_option_col1_item)),m2_option_col1_len_max)
	next m2_option_col1_item
	m2_pk_len$=str$(m2_pk_len=len(m2_pk$))
	m2_field_1_width=max(2,m2_option_col1_len_max+1)
	m2_toolbar_width=m2_grid_width-m2_field_1_width-m2_pk_len+2

	if m2_default_selection$='' then 
		m2_default_selection=0
	else 
		m2_default_selection$=trim$(m2_default_selection$,"@")
		m2_default_selection$=trim$(m2_default_selection$,"-")
		m2_default_selection=srch(mat m2_option_col1$,m2_default_selection$)
	end if  ! M2_Default_Selection$=''   /   else 
	if m2_default_selection>0 then m2_filter$=m2_default_selection$ else m2_filter$=m2_option_col1$(1)

	open #m2_toolbar_handle:=fngethandle: 'SRow=1,SCol='&str$(m2_field_1_width+m2_pk_len+2)&',Rows=1,Cols='&str$(m2_toolbar_width)&',Border=None,Picture=Icons\Toolbar.gif,Parent='&m2_parent_handle$,display,output 
	rinput #m2_toolbar_handle,select '1,'&str$(m2_toolbar_width-1)&',P 1/2,[Toolbar],B99',wait=0, help "3a;[Esc] Exit;": 'Icons\X.gif:Isotropic' timeout ignore
	print #m2_parent_handle,fields '1,1,Cr '&str$(m2_pk_len+1)&',[Toolbar]S': m2_pk$&'-'
	if m2_help_available then rinput #m2_toolbar_handle,select '1,'&str$(m2_toolbar_width-4)&',P 1/2,[Toolbar],B100',wait=0, help "3a;Help;": 'Icons\Help.png:Isotropic' timeout ignore
	if debug then rinput #m2_toolbar_handle,select '1,'&str$(m2_toolbar_width-32)&',P 1/2,[Toolbar],B8000',wait=0, help "3a;[F8000] Copy Menu Path (*Debug Feature);": 'Icons\PK_Copy.png:Isotropic' timeout ignore ioerr ignore
	gosub M2_TICK_EVENT
	open #m2_vbar_handle:=fngethandle: 'SRow=2,SCol='&str$(m2_parent_width-1)&',Rows='&str$(m2_parent_height-2)&',Cols=2,Parent='&m2_parent_handle$&",picture=icons\VBar.Gif" ,display,outin ioerr ignore
	m2_grid_pos$="2,1"
	m2_grid_field$=m2_grid_pos$&",List "&m2_grid_height$&"/"&m2_grid_width$
	m2_field_1_len=max(1,len(m2_filter$))
	! gosub M2_FIELD_1_SET
	if m2_footer$<>'' then 
		m2_footer_tooltip$=srep$(m2_footer$,'³',chr$(10))
		m2_footer$=srep$(m2_footer$,'³','|')
		for m2_space_count=6 to 2, step -1
			m2_footer$=srep$(m2_footer$,rpt$(' ',m2_space_count),rpt$(' ',m2_space_count-1))
		next m2_space_count
		rinput #m2_parent_handle,select m2_parent_height$&',1,49/C 128,[S];'&m2_parent_height$&',50,23/C 100,[W]',wait=0, help '3a;'&m2_footer_tooltip$&';': m2_footer$,m2_big_empty$ timeout ignore
	end if  ! 
	if m2_preserve_backgroud<>2 then rinput #m2_parent_handle,select m2_parent_height$&',51,Cc 10,[Button],B0',wait=0, help "3a;[Enter] OK;": 'OK' timeout ignore ! M2_Parent_Width-21
	if m2_preserve_backgroud<>2 then rinput #m2_parent_handle,select m2_parent_height$&',62,Cc 10,[Button],B99',wait=0, help "3a;[Esc] Exit;": 'Exit' timeout ignore ! M2_Parent_Width-10
	m2_field$(2)=m2_grid_field$&",RowSub,SelOne"

	print #m2_parent_handle, fields m2_grid_field$&",Headers,S[N]": (mat m2_heading$, mat m2_width, mat m2_form$)
	print #m2_parent_handle, fields m2_grid_field$&",=R,-1": mat m2_option_2d$
	if m2_default_selection>0 then 
		curfld(1,m2_default_selection)
		rinput #m2_parent_handle, fields m2_field$(2)&',nowait': m2_current_selection
		curfld(1)
	end if  ! M2_Default_Selection>0
	! 
	M2_ASK: ! 
	gosub M2_FIELD_1_SET
	rinput #m2_parent_handle,wait=0,fields mat m2_field$: m2_filter$,m2_current_selection timeout ignore
	curfld(1)
	rinput #m2_parent_handle, fields mat m2_field$,wait=m2_tick_delay: m2_filter$,m2_current_selection timeout ignore
	! if env$("UserName")="JBowman" then pause
	m2_type_and_grow=0
	m2_rinput_process_count+=1
	m2_curfld=curfld
	if developer and m2_preserve_backgroud=0 then print #m2_display_top_parent,fields '2,1,Cl 80,[w]': ' FKey='&str$(fkey)&' M2_RInput_Process_Count='&str$(m2_rinput_process_count)&' M2_Curfld='&str$(m2_curfld)&' currow='&str$(currow)
	! if fkey=100 then goto M2_ASK ! Ctrl+Y   ...    used here for HELP
	if fkey=0 then 
		if curfld=1 then 
			m2_return=srch(mat m2_option_col1$,trim$(m2_filter$))
			if m2_return<=0 then 
				print bell;
				goto M2_ASK
			end if  ! M2_Return<=0
		else ! if curfld=2 then
			m2_return=currow
		end if  ! curfld=1   /   else 
	!  else if Fkey=90 then ! PageUp
	!   goto M2_ASK
	!  else if Fkey=91 then ! grid, ctrl+left
	!   goto M2_ASK
	else if fkey=93 then 
		m2_return=-10093
	!   execute 'system' ! Fn_Menu2='Exit' ! todo: hmmm...
	else if fkey=98 then 
		m2_return=-10098
	else if fkey=99 then 
		m2_return=-10099
	else if fkey=100 then 
		if curfld=1 then 
			m2_help_context=srch(mat m2_option_col1$,trim$(m2_filter$))
			if m2_help_context<=0 then 
				print bell;
				fnhelp(m2_pk$)
				goto M2_ASK
			end if  ! M2_Return<=0
		else ! if curfld=2 then
			m2_help_context=currow
		end if 
		fnhelp(m2_pk$&'-'&str$(m2_help_context))
		goto M2_ASK
	else if fkey=101 then ! Timeout
		gosub M2_TICK_EVENT
		m2_stime_progress=fnstime(time$)-m2_stime_start
	!   if Developer then print #M2_Display_Top_Parent,fields '24,1,Cl 80,[w]': ' FKey='&Str$(Fkey)&' M2_Stime_Start='&Str$(M2_Stime_Start)&' M2_Stime_Progress='&Str$(M2_Stime_Progress)&' M2_Timeout='&Str$(M2_Timeout)
		if m2_timeout>0 and m2_stime_progress>m2_timeout then 
			m2_return=-10101
		else 
			goto M2_ASK
		end if 
	else if fkey=103 then ! left
		goto M2_ASK
	else if fkey=90 or fkey=91 or fkey=102 or fkey=104 or fkey=105 or fkey=106 then 
		if fkey=90 then ! Text, PgUp
			m2_nextrow=m2_current_selection-m2_grid_height
		else if fkey=91 then ! Text, PgDn
			m2_nextrow=m2_current_selection+m2_grid_height
		else if fkey=102 then ! Text, Up
			m2_nextrow=m2_current_selection-1
		else if fkey=104 then ! Text, Down
			m2_nextrow=m2_current_selection+1
		else if fkey=105 then 
			m2_nextrow=currow-1
		else if fkey=106 then 
			m2_nextrow=currow+1
		end if 
		m2_nextrow=max(1,m2_nextrow)
		m2_nextrow=min(m2_nextrow,udim(m2_option_col1$))
		m2_filter$=trim$(m2_option_2d$(m2_nextrow,1))
		m2_field_1_len=len(m2_filter$)
		curfld(2,m2_nextrow)
		goto M2_ASK
	else if fkey=107 then 
		m2_type_and_grow=1
		if m2_option_col1_len_max>1 and m2_field_1_len=m2_option_col1_len_max then 
			m2_return=srch(mat m2_option_col1$,trim$(m2_filter$))
			if m2_return<=0 then 
				print bell;
				m2_filter$=m2_filter$(1:m2_field_1_len-1)
				goto M2_ASK
			end if  ! M2_Return<=0 then
		else if m2_option_col1_len_max>1 and m2_field_1_len<m2_option_col1_len_max then 
			m2_option_item=m2_highlighted_first_match=m2_match_count=0
			do 
				m2_option_item+=1
				if m2_filter$=m2_option_col1$(m2_option_item)(1:m2_field_1_len) then 
					m2_match_count+=1
					m2_highlighted_first_match=m2_option_item
				end if  ! 
			loop until m2_option_item=>m2_option_count ! Udim(Mat M2_Option_Col1$)
			if m2_match_count=1 then 
				m2_return=m2_highlighted_first_match
			else 
				m2_field_1_len+=1
				goto M2_ASK
			end if 
		else 
			m2_return=srch(mat m2_option_col1$,trim$(m2_filter$))
			if m2_return<=0 then 
				m2_field_1_len=1
				m2_filter$=''
				print bell;
				goto M2_ASK
			end if  ! M2_Return<=0
		end if  ! M2_Option_Col1_Len_Max>1 And M2_Field_1_Len=M2_Option_Col1_Len_Max   /   M2_Option_Col1_Len_Max>1 And M2_Field_1_Len<M2_Option_Col1_Len_Max   /   else 
	else if fkey=110 then ! Tab
		if m2_curfld=1 then curfld(2) else curfld(1)
		goto M2_ASK
	else if fkey=111 then ! Shift+Tab
		if m2_curfld=1 then curfld(2) else curfld(1)
		goto M2_ASK
	else if fkey=112 then ! Home
		m2_filter$=m2_option_col1$(1)
		m2_field_1_len=len(m2_filter$)
		curfld(2,1)
		goto M2_ASK
	else if fkey=113 then ! End
		m2_filter$=m2_option_col1$(m2_option_count)
		m2_field_1_len=len(m2_filter$)
		curfld(2,m2_option_count)
		goto M2_ASK
	else if fkey=116 then ! right
		goto M2_ASK
	else if fkey=120 then ! Grid, Shift+End
		curfld(2,m2_option_count)
		goto M2_ASK
	else if fkey=126 then ! Ctrl+Up
		goto M2_ASK
	else if fkey=127 then ! Ctrl+Down
		goto M2_ASK
	else if fkey=133 then ! Grid, Shift+Left
		goto M2_ASK
	else if fkey=134 then ! Grid, Shift+Right
		goto M2_ASK
	else if fkey=135 then ! Ctrl+Shift+Up
		goto M2_ASK
	else if fkey=136 then ! Ctrl+Shift+Down
		goto M2_ASK
	else if fkey=182 then ! Grid, Shift+Right
		goto M2_ASK
	else if fkey=201 then ! they double clicked on an option
		if nxtfld=1 then 
			goto M2_ASK
		else 
			fkey(0)
			m2_return=srch(mat m2_option_col1$,trim$(m2_filter$))
		end if  ! nxtfld=1   /   else 
	else if fkey=200 or fkey=206 then ! or fkey=201
		curfld(m2_curfld,fkey)
		m2_filter$=""
		rinput #m2_parent_handle,wait=0,fields mat m2_field$: m2_filter$,m2_current_selection timeout M2_ASK_FIELD2_CLICK_TIMEOUT
		rinput #m2_parent_handle,wait=0,fields mat m2_field$: m2_filter$,m2_current_selection timeout M2_ASK_FIELD2_CLICK_TIMEOUT
	M2_ASK_FIELD2_CLICK_TIMEOUT: ! 
		m2_filter$=m2_option_col1$(m2_current_selection)
		m2_field_1_len=len(m2_filter$)
		goto M2_ASK
	else if fkey=208 then 
		goto M2_ASK
	else if fkey=8000 then ! reqerved for fnMenu2's future internal use
		setenv('ClipBoard',m2_pk$&'-'&m2_filter$)
		goto M2_ASK
	else if fkey=>8001 and fkey<=8999 then ! reqerved for fnMenu2's future internal use
		goto M2_ASK
	else ! any other FKey, should return with the selection
		m2_return=srch(mat m2_option_col1$,trim$(m2_filter$))
	end if  ! Fkey=...
	! ~
	! if fkey=101 then pause
	if m2_return=>-10000 and m2_return<0 then 
		print bell;
		if debug then print '**M2_Return=';m2_return
		if developer then pause 
		m2_filter$=''
		m2_field_1_len=1
		goto M2_ASK
	end if  ! M2_Return<=0 and M2_Return=>-10000 then
	! ~  only succesful result should fall through, all othere should goto M2_ASK
	gosub M2_FINIS
	goto M2_XIT
	! ~
	M2_FIELD_1_SET: ! 
	if m2_type_and_grow then 
		m2_field$(1)='1,'&str$(m2_pk_len+2)&','&str$(m2_field_1_width)&'/Cu '&str$(m2_field_1_len)&',[D]STAEX'&str$(m2_field_1_len)
	else 
		m2_field$(1)='1,'&str$(m2_pk_len+2)&','&str$(m2_field_1_width)&'/Cu '&str$(m2_field_1_len)&',[D]STAEX'
	end if 
	return  ! M2_FIELD_1_SET
	! ~
	M2_TICK_EVENT: ! 
	if env$("DEFAULT_DATE")='' then 
		print #m2_toolbar_handle,fields '1,'&str$(m2_toolbar_width-22)&',09/Cc 10,[Toolbar]S': fndate_dis10$(date$)
	else 
		print #m2_toolbar_handle,fields '1,'&str$(m2_toolbar_width-22)&',10/Cc 10,[S]S': fndate_dis10$(date$)
	end if  ! ENV$("DEFAULT_DATE")=''   /   else 
	print #m2_toolbar_handle,fields '1,'&str$(m2_toolbar_width-11)&',Cc 5,[Toolbar]S': time$(1:5)
	return  ! M2_TICK_EVENT
	! ~
	M2_FINIS: ! 
	if fkey=107 then fkey(0)
	if m2_return<>-10101 then fn_menu2_finis
	return  ! M2_FINIS
	! ~
	M2_XIT: ! 
	! if Debug And Fkey<>0 then print 'on exit of Fn_Menu2, M2_Return='&Str$(M2_Return)&' and fkey='&Str$(Fkey) ! pause
	fn_menu2=m2_return
fnend  ! Fn_Menu2
def library fnmenu2_finis(; m2_transition_text$*78)
	fnmenu2_finis=fn_menu2_finis( m2_transition_text$)
fnend  ! FnMenu2_Finis
def fn_menu2_finis(; m2_transition_text$*78)
	if m2_parent_handle>0 and file$(m2_parent_handle)=":CON:" then 
		!       if M2_Transition_Text$='' then M2_Transition_Text$='Loading...'
		!       print #M2_Parent_Handle: Newpage
		!       print #M2_Parent_Handle,fields '4,1,Cc '&M2_Grid_Width$&',[W]': M2_Transition_Text$
		!       if Debug then print #M2_Parent_Handle,fields M2_Parent_Height$&',20,10/C 60,[P]SP': 'FKey='&Str$(Fkey) ! DEBUG todo: remove
		close #m2_parent_handle: ioerr ignore
		scr_freeze
		! DISPLAY MENU: "","",""
		if m2_display_top_parent>0 then close #m2_display_top_parent: ioerr ignore
		m2_parent_handle=m2_display_top_parent=0
	! SCR_THAW
	end if  ! M2_Display_Top_Parent>0 and file$(M2_Display_Top_Parent)=":CON:"
fnend  ! Fn_Menu2_Finis
def fn_m2_setup_grid(mat msg_option$, mat msg_option_2d$,mat msg_heading$,mat msg_width,mat msg_form$) ! Msg_
	mat msg_width(1)=(0)
	mat msg_form$(1)=("")
	msg_column_count=0
	msg_option_count=udim(mat msg_option$)
	msg_column_count=fnchr_count(msg_option$(1),"³")+1
	if msg_column_count=1 then 
		fnmenu_autonumber(mat msg_option$,"³")
		msg_column_count=2
	end if  ! Msg_Column_Count=1'
	xlen_extra=0
	fnmat_1d_to_2d(mat msg_option$,mat msg_option_2d$,msg_column_count)
	for msg_option_item=1 to msg_option_count
		this_field=0
		msg_option_pos_pipe_prior=0
		MSG_NEXT_FIELD: ! 
		this_field+=1
		msg_option_pos_pipe=pos(msg_option$(msg_option_item),"³",msg_option_pos_pipe_prior+1)
		xmsg_option_pos_pipe=msg_option_pos_pipe
		if msg_option_pos_pipe<=0 then xmsg_option_pos_pipe=9999
		if udim(msg_width)<this_field then mat msg_width(this_field) : mat msg_form$(this_field)
		xlen=max(1,len(rtrm$(msg_option$(msg_option_item)(msg_option_pos_pipe_prior+1:xmsg_option_pos_pipe-1))))
		if xlen>msg_width(this_field) then msg_width(this_field)=xlen

		if msg_form$(this_field)="C" then goto PARA_59760
		if ~fn_is_val(msg_option$(msg_option_item)(msg_option_pos_pipe_prior+1:xmsg_option_pos_pipe-1)) then 
			goto MSG_TYPE_DATE_TEST
		end if 
		if trim$(msg_form$(this_field))="" or trim$(msg_form$(this_field))="N" then 
			msg_form$(this_field)='Cr 128,[M]LX' ! "N"
			goto PARA_59760
		end if 
		MSG_TYPE_DATE_TEST: ! 
		dval$=fndate10$(msg_option$(msg_option_item)(msg_option_pos_pipe_prior+1:xmsg_option_pos_pipe-1)(1:18))
		if trim$(dval$)="" then goto MSG_TYPE_CHARACTER_SET
		if trim$(msg_form$(this_field))="" or trim$(msg_form$(this_field))="D" then 
			msg_form$(this_field)='Cr 128,[M]LX' ! "D"
			goto PARA_59760
		end if 
		MSG_TYPE_CHARACTER_SET: ! 
		msg_form$(this_field)='C 128,[M]LX' ! 'C'
		PARA_59760: ! 
		if msg_option_pos_pipe>0 then 
			msg_option_pos_pipe_prior=msg_option_pos_pipe
			goto MSG_NEXT_FIELD
		end if 
		if this_field>msg_column_count then msg_column_count=this_field
	next msg_option_item
	for msg_width_item=1 to udim(mat msg_width)
		if msg_width(msg_width_item)<=8 then msg_width(msg_width_item)+=1 : xlen_extra+=1
	next msg_width_item
	msg_width(udim(mat msg_width))-=xlen_extra
	mat msg_heading$(msg_column_count)=("")
fnend  ! Fn_M2_Setup_Grid
def library fntable_unique_key_list(stukl_handle,mat stukl_key_list$)
	gosub SETUP_LIBRARY
	gosub SETUP_CONSTANTS
	fntable_unique_key_list=fn_table_unique_key_list(stukl_handle,mat stukl_key_list$)
fnend  ! fntable_unique_key_list
def fn_table_unique_key_list(stukl_handle,mat stukl_key_list$) ! stukl_
	! This function when provided stukl_Handle,mat stukl_data$,mat stukl_data,stukl_formall$ returns an array of all unique keys in the file)
	! This function itself will return the number of items found
	! currently this function only works with single part keys.
	open #stukl_com_handle:=fngethandle: "SRow=12,SCol=32,Rows=1,Cols=14,Border=None",display,output 
	restore #stukl_handle: 
	dim stukl_key$*128,stukl_form$*80
	stukl_key_pos=kps(stukl_handle)
	stukl_key_len=kln(stukl_handle)
	stukl_form$=cform$("Form Pos "&str$(stukl_key_pos)&",C "&str$(stukl_key_len))
	mat stukl_key_list$(0)
	read #stukl_handle,using stukl_form$: stukl_key$ eof STUKL_EOF
	gosub STUKL_ADD
	do 
		read #stukl_handle,using stukl_form$,key>=fnkey_after$(stukl_key_prior$,stukl_handle): stukl_key$ eof STUKL_EOF nokey STUKL_EOF ! 
		fnspin$("1,2,C 1", "1,4,C 8,[W]",time_start$,stukl_com_handle,stukl_com_handle)
		gosub STUKL_ADD
	loop 
	STUKL_ADD: ! 
	fnadd_one$(mat stukl_key_list$,stukl_key$,no_add_blanks=0,no_add_duplicates=1)
	stukl_key_prior$=stukl_key$
	return  ! STUKL_ADD
	STUKL_EOF: ! 
	close #stukl_com_handle: 
	fn_table_unique_key_list=udim(mat stukl_key_list$)
fnend  ! fn_table_unique_key_list
def library fnstring_to_wordlist(stw_source$*1024,mat stw_wordlist$)
	gosub SETUP_LIBRARY
	gosub SETUP_CONSTANTS
	fnstring_to_wordlist=fn_string_to_wordlist(stw_source$,mat stw_wordlist$)
fnend  ! fnstring_to_wordlist
def fn_string_to_wordlist(stw_source$*1024,mat stw_wordlist$) ! stw_
	stw_source$=fnsymbol_xlate$(stw_source$)
	str2mat(srep$(stw_source$," ",tab$),mat stw_wordlist$, tab$)
fnend  ! fn_string_to_wordlist
def library fnm2_string_in_choice(msic_user_input$*1024,msic_option_text$*1024)
	gosub SETUP_LIBRARY
	gosub SETUP_CONSTANTS
	fnm2_string_in_choice=fn_m2_string_in_choice(msic_user_input$,msic_option_text$)
fnend  ! fnm2_string_in_choice
def fn_m2_string_in_choice(msic_user_input$*1024,msic_option_text$*1024) ! msic_
	dim msic_wordlist_a$(1)*128
	dim msic_wordlist_b$(2)*128
	fn_string_to_wordlist(lwrc$(msic_user_input$),mat msic_wordlist_a$)
	fn_string_to_wordlist(lwrc$(msic_option_text$),mat msic_wordlist_b$)
	fn_m2_string_in_choice=fn_wordlist_compare_all_left(mat msic_wordlist_a$,mat msic_wordlist_b$)
fnend  ! fn_m2_string_in_choice
def library fnwordlist_compare_all_left(mat wcap_wordlist_a$,mat wcap_wordlist_b$)
	gosub SETUP_LIBRARY
	gosub SETUP_CONSTANTS
	fnwordlist_compare_all_left=fn_wordlist_compare_all_left(mat wcap_wordlist_a$,mat wcap_wordlist_b$)
fnend  ! FnWordlist_Compare_all_left
def fn_wordlist_compare_all_left(mat wcap2_wordlist_a$,mat wcap2_wordlist_b$) ! wcap2_
	! returns true if all words in wordlist_A are either words or the start of words in wordlist_B
	! this function IS CASE SENSITIVE
	wcap2_return=false
	wcap2_hit_count=0
	wcap2_wordlist_a_count=udim(mat wcap2_wordlist_a$)
	wcap2_wordlist_b_count=udim(mat wcap2_wordlist_b$)
	for wcap2_wa_item=1 to wcap2_wordlist_a_count
		if fn_word_vs_wordlist_left(wcap2_wordlist_a$(wcap2_wa_item),mat wcap2_wordlist_b$) then wcap2_hit_count+=1
	next wcap2_wa_item
	if wcap2_hit_count=wcap2_wordlist_a_count then wcap2_return=true
	fn_wordlist_compare_all_left=wcap2_return
fnend  ! Fn_Wordlist_Compare_all_left
def library fnword_vs_wordlist_left(wvwl_word$*128,mat wvwl_wordlist$)
	gosub SETUP_LIBRARY
	gosub SETUP_CONSTANTS
	fnword_vs_wordlist_left=fn_word_vs_wordlist_left(wvwl_word$,mat wvwl_wordlist$)
fnend  ! fnword_vs_wordlist_left
def fn_word_vs_wordlist_left(wvwl_word$*128,mat wvwl_wordlist$) ! wvwl_
	! this function IS CASE SENSITIVE
	wvwl_return=false
	if srch(mat wvwl_wordlist$,wvwl_word$)>0 then wvwl_return=true : goto WVWL_XIT
	wvwl_word_len=len(wvwl_word$)
	for wvwl_wordlist_item=1 to udim(mat wvwl_wordlist$)
		if fn_is_number(wvwl_word$(1:1)) then 
			if wvwl_word$=wvwl_wordlist$(wvwl_wordlist_item)(1:wvwl_word_len) then wvwl_return=true : goto WVWL_XIT
		else 
			if wvwl_word_len<=2 then ! or env$("username")='JBowman' then
				if wvwl_word$=wvwl_wordlist$(wvwl_wordlist_item)(1:wvwl_word_len) then wvwl_return=true : goto WVWL_XIT
			else 
				if wvwl_word$=wvwl_wordlist$(wvwl_wordlist_item)(1:wvwl_word_len) or fnsoundex$(wvwl_word$)=fnsoundex$(wvwl_wordlist$(wvwl_wordlist_item)(1:wvwl_word_len)) then wvwl_return=true : goto WVWL_XIT
			end if  ! wvwl_word_len<=2   /   else 
		end if  ! fn_is_number(wvwl_word$)   /   else 
	next wvwl_wordlist_item
	WVWL_XIT: ! 
	fn_word_vs_wordlist_left=wvwl_return
fnend  ! fn_word_vs_wordlist_left
def fn_is_number(hn_string$*128)
	hn_return=0
	val(hn_string$) conv HN_FINIS
	hn_return=1
	HN_FINIS: ! 
	fn_is_number=hn_return
fnend  ! fn_is_number
def fn_array_move_braketed_to_top(mat amb_list$)
	dim amb_bracketed$(1)*128
	mat amb_bracketed$(0)
	amb_item=0
	do until amb_item>=udim(mat amb_list$)
		amb_item+=1
		if amb_list$(amb_item)(1:1)='[' then 
			fnadd_one$(mat amb_bracketed$,amb_list$(amb_item))
			fnremove_arrayitem$(mat amb_list$,amb_item)
			amb_item-=1
		end if  ! amb_list$(amb_item)(1:1)='[' then
	loop 
	amb_list_count=udim(mat amb_list$)
	amb_bracketed_count=udim(mat amb_bracketed$)
	if amb_bracketed_count>0 then 
		mat amb_list$(amb_list_count+amb_bracketed_count)
		mat amb_list$(amb_bracketed_count+1:amb_list_count+amb_bracketed_count)=amb_list$(1:amb_list_count)
		mat amb_list$(1:amb_bracketed_count)=amb_bracketed$
	end if  ! amb_bracketed_count>0
fnend  ! fn_array_move_braketed_to_top(mat ambtt_array$)
def library fncombobox$*128(cb_parent,cb_parent_rows,cb_parent_cols,cb_srow,cb_scol,cb_width,mat cb_opt$; cb_col_heading$*80,cb_default_selection$*256,cb_timeout,cb_select_disable)
	if ~setup then fn_setup
	fncombobox$=fn_combobox$(cb_parent,cb_parent_rows,cb_parent_cols,cb_srow,cb_scol,cb_width,mat cb_opt$, cb_col_heading$,cb_default_selection$,cb_timeout,cb_select_disable)
fnend  ! fncombobox
def fn_combobox$*128(cb_parent,cb_parent_rows,cb_parent_cols,cb_srow,cb_scol,cb_width,mat cb_opt$; cb_col_heading$*80,cb_default_selection$*256,cb_timeout,cb_select_disable) ! cb_
	! this function will return fkey 93 (big x), fkey 101 (timeout), or fkey 0 (selection made or canceled)
	dim cb_field$(2)*80,cb_return$*128,cb_filter$(1)*128,cb_filter$*128,last_cb_filter$*128
	dim cb_heading$(1)*80,cb_width(1),cb_form$(1)*20
	dim cb_filter$*128

	cb_return$=''
	cb_rinput_process_count=0
	cb_opt_count=udim(mat cb_opt$) 
	if cb_opt_count=0 then             print bell; : goto CB_XIT
	cb_width$=str$(cb_width)
	fn_array_sort$(mat cb_opt$)
	fn_array_move_braketed_to_top(mat cb_opt$)
	cb_opt_filter=0
	if udim(cb_opt$)=0 then             print bell; : goto CB_XIT
	mat cb_filter$(udim(cb_opt$))=(""): cb_filter$=last_cb_filter$=""
	for cb_opt_item=1 to cb_opt_count
		cb_filter$=rtrm$(cb_opt$(cb_opt_item))
		if uprc$(cb_filter$)<>uprc$(last_cb_filter$) then 
			cb_filter$(cb_opt_filter+=1)=cb_filter$ 
			last_cb_filter$=uprc$(cb_filter$)
		end if
	next cb_opt_item
	cb_opt_count=cb_opt_filter 
	if cb_opt_filter=0 then             print bell; : goto CB_XIT
	mat cb_opt$(cb_opt_count)=cb_filter$(1:cb_opt_count)
	cb_default_selection$=rtrm$(cb_default_selection$)
	! 
	if cb_opt_count=0 then print bell;'ComboBox - no options to select, default returned.' : cb_return$=cb_default_selection$ : goto CB_XIT
	! 
	if cb_default_selection$='' and srch(mat cb_opt$,'')<=0 and ~cb_select_disable then cb_default_selection$=cb_opt$(1)
	cb_default_which=srch(mat cb_opt$,cb_default_selection$)
	if cb_default_which<=0 and ~cb_select_disable then 
		cb_filter$=''
		cb_default_which=1
		print bell;'ComboBox - invalid default selection passed ('&cb_default_selection$&') could not be found in passed options (mat cb_opt$).  cb_opt$(1) ('&cb_opt$(1)&') will be the default instead.'
		if developer then pause 
	end if 
	cb_return$=cb_filter$=cb_default_selection$ ! cb_opt$(cb_default_which)
	cb_current_selection=cb_default_which
	! 
	if cb_parent=0 and (cb_parent_rows=0 or cb_parent_cols=0) then cb_parent_rows=session_rows : cb_parent_cols=session_cols
	if cb_parent<>0 and (cb_parent_rows=0 or cb_parent_cols=0) then print 'error - parent used but parents size was not passed.' : pause 
	! calc above or below
	tmp_lines_above=cb_srow-1
	tmp_lines_below=cb_parent_rows-cb_srow
	if tmp_lines_above>tmp_lines_below then cb_parent_above=1 else cb_parent_above=0
	! calc above or below
	if cb_parent_above then ! combo box apears on and ABOVE cb_srow
		cb_srow_origional=cb_srow
		cb_rows=min(cb_opt_count+2,cb_srow_origional)
		cb_rows=max(cb_rows,4)
		cb_srow=max(cb_srow_origional-cb_rows+1,2)
	else ! combo box apears on and BELOW cb_srow
		cb_rows=min(cb_opt_count+2,cb_parent_rows-cb_srow)
	end if  ! cb_parent_above

	open #cb_canvas_handle:=fngethandle: 'Parent='&str$(cb_parent)&',SRow='&str$(cb_srow)&',SCol='&str$(cb_scol)&',Cols='&str$(cb_width)&',Rows='&str$(cb_rows)&',Border=S,N=[N]',display,output 

	cb_canvas_handle$=str$(cb_canvas_handle)

	cb_grid_height$=str$(cb_grid_height=cb_rows-1)
	cb_heading$(1)=cb_col_heading$
	cb_width(1)=cb_width
	cb_form$(1)='Cl 128,[M]LX^NoSort'

	if cb_parent_above then 
		open #cb_x_handle:=fngethandle: 'SRow='&str$(cb_rows)&',SCol='&str$(cb_width-1)&',Rows=1,Cols=2,Border=None,Parent='&cb_canvas_handle$&',Picture=Icons\Comobo.png:Tile',display,output 
	!     open #cb_x_handle:=fngethandle: 'SRow='&str$(cb_rows)&',SCol='&str$(cb_width-1)&',Rows=1,Cols=2,Border=None,Picture=Icons\Toolbar.gif,Parent='&cb_canvas_handle$,display,output
		cb_grid_field$='1,1,List '&cb_grid_height$&'/'&cb_width$
		cb_field$(1)=str$(cb_rows)&',1,'&str$(cb_width)&'/Search 128,[D]STAEX,1,1'
	else ! combo box apears on and BELOW cb_srow
		open #cb_x_handle:=fngethandle: 'SRow=1,SCol='&str$(cb_width-1)&',Rows=1,Cols=2,Border=None,Parent='&cb_canvas_handle$&',Picture=Icons\Comobo.png:Tile',display,output 
	!     open #cb_x_handle:=fngethandle: 'SRow=1,SCol='&str$(cb_width-1)&',Rows=1,Cols=2,Border=None,Picture=Icons\Toolbar.gif,Parent='&cb_canvas_handle$,display,output
		cb_grid_field$='2,1,List '&cb_grid_height$&'/'&cb_width$
		cb_field$(1)='1,1,'&str$(cb_width)&'/Search 128,[D]STAEX,2,1'
	end if  ! cb_parent_above   /   else 
	print #cb_x_handle,fields '1,1,P 1/2,[Toolbar],B99',help "3a;[Esc] Exit;": 'Icons\Combo_X.PNG'
	cb_field$(2)=cb_grid_field$&',RowSub,SelOne'

	print #cb_canvas_handle, fields cb_grid_field$&",Headers,S[N]": (mat cb_heading$, mat cb_width, mat cb_form$)
	print #cb_canvas_handle, fields cb_grid_field$&",=R,-1": mat cb_opt$
	print #cb_canvas_handle, fields cb_grid_field$&",Sort": 1

	CB_ASK: ! 
	!   rinput #cb_canvas_handle,wait=0,fields cb_field$(2): cb_current_selection timeout IGNORE
	curfld(2,cb_current_selection)
	rinput #cb_canvas_handle, fields cb_field$(2)&',nowait': cb_current_selection
	curfld(1)
	if cb_timeout>0 then 
		rinput #cb_canvas_handle, fields mat cb_field$,attr '[A]',wait=cb_timeout: cb_filter$,cb_current_selection timeout ignore
	else 
		rinput #cb_canvas_handle, fields mat cb_field$: cb_filter$,cb_current_selection
	end if  ! cb_timeout>0   /   else 

	cb_rinput_process_count+=1
	cb_curfld=curfld
	if fkey=93 then 
		cb_return$=cb_default_selection$
	else if fkey=99 then 
		cb_return$=cb_default_selection$
	else if fkey=101 then ! Timeout
		cb_return$=cb_default_selection$
	else if fkey=103 then ! left
		goto CB_ASK
	else if fkey=90 or fkey=91 or fkey=102 or fkey=104 or fkey=105 or fkey=106 then 
		if fkey=90 then ! Text, PgUp
			cb_current_selection=cb_nextrow=max(1,cb_current_selection-cb_grid_height)
		else if fkey=91 then ! Text, PgDn
			cb_current_selection=cb_nextrow=min(cb_opt_count,cb_current_selection+cb_grid_height)
		else if fkey=102 then ! Text, Up
			cb_current_selection=max(1,cb_current_selection-1)
			cb_filter$=cb_opt$(cb_current_selection)
	!       cb_nextrow=cb_current_selection-1
		else if fkey=104 then ! Text, Down
			cb_current_selection=min(cb_opt_count,cb_current_selection+1)
			cb_filter$=cb_opt$(cb_current_selection)
	!       cb_nextrow=cb_current_selection+1
		else if fkey=105 then 
			cb_nextrow=currow-1
		else if fkey=106 then 
			cb_nextrow=currow+1
		end if 
		cb_nextrow=max(1,cb_nextrow)
		cb_nextrow=min(cb_nextrow,cb_opt_count)
		curfld(2,cb_nextrow)
		goto CB_ASK
	else if fkey=112 then ! Home
		cb_filter$=cb_opt$(1)
		curfld(2,1)
		goto CB_ASK
	else if fkey=113 then ! End
		cb_filter$=cb_opt$(cb_opt_count)
		curfld(2,cb_opt_count)
		goto CB_ASK
	else if fkey=116 or fkey=126 or fkey=127 or fkey=135 or fkey=136 then ! Right or Ctrl+Up or Ctrl+Down or Ctrl+Shift+Up or Ctrl+Shift+Down
		goto CB_ASK
	else if fkey=201 then ! they double clicked on an option
		if nxtfld=1 then 
			goto CB_ASK
		else 
			rinput #cb_canvas_handle,wait=0,fields mat cb_field$: cb_filter$,cb_current_selection timeout ignore
			cb_return$=cb_filter$=cb_opt$(cb_current_selection)
			fkey(0)
		end if  ! nxtfld=1   /   else 
	else if fkey=200 or fkey=206 then 
		curfld(cb_curfld,fkey)
		cb_filter$=""
		rinput #cb_canvas_handle,wait=0,fields mat cb_field$: cb_filter$,cb_current_selection timeout ignore
		cb_filter$=cb_opt$(cb_current_selection)
		goto CB_ASK
	else ! any other FKey, should return with the selection
		if cb_select_disable then 
			cb_return$=cb_filter$
		else 
			cb_return$=cb_opt$(cb_current_selection)
		end if  ! cb_select_disable
	end if  ! Fkey=...
	! ~
	! ~  only succesful result should fall through, all othere should goto CB_ASK
	scr_freeze
	close #cb_canvas_handle: ioerr ignore
	cb_canvas_handle=0
	!   if fkey<>93 and fkey<>101 and fkey<>110 and fkey<>111 then fkey(0) ! all FKey values should be returned.
	goto CB_XIT
	! ~
	CB_XIT: ! 
	fn_combobox$=cb_return$
fnend  ! fn_combobox$
include: cm\enum\common