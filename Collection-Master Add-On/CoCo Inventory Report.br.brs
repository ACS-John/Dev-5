on error goto Error_Hanler
fn_setup
fntop(program$)

	! r: open master
	dim masterData$(1)*60,masterDataN(1)
	dim masterFieldsc$(1)*20,masterFieldsN$(1)*20
	dim masterFormC$*1024,masterFormN$*1024
	dim mFormAll$*2048
	fnget_form("Master",mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,masterFormC$,masterFormN$)
	fnunpack$(masterFormC$,masterFormN$)
	mFormAll$=fnget_formall$
	gosub enumMaster
	open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
	! dim masterData$(1)*60,masterDataN(1),masterFieldsc$(1)*20,masterFieldsN$(1)*20,masterFormC$*512,masterFormN$*512,masterFormAll$*512
	! /r
	! r: get mat CoCo
	dim coco(0)
	mat coco(0)
	fnAddOneN(mat coco, 821)
	fnAddOneN(mat coco, 945)
	fnAddOneN(mat coco, 976)
	fnAddOneN(mat coco, 540)
	fnAddOneN(mat coco,1116)
	fnAddOneN(mat coco,1124)
	fnAddOneN(mat coco, 973)
	fnAddOneN(mat coco,6011)
	fnAddOneN(mat coco, 962)
	fnAddOneN(mat coco, 995)
	fnAddOneN(mat coco, 101)
	fnAddOneN(mat coco, 110)
	fnAddOneN(mat coco, 966)
	fnAddOneN(mat coco,1014)
	fnAddOneN(mat coco,1147)
	fnAddOneN(mat coco, 999)
	fnAddOneN(mat coco, 994)
	fnAddOneN(mat coco, 980)
	fnAddOneN(mat coco, 675)
	fnAddOneN(mat coco,1005)
	fnAddOneN(mat coco,1018)
	fnAddOneN(mat coco, 969)
	fnAddOneN(mat coco, 979)
	fnAddOneN(mat coco,1105)
	fnAddOneN(mat coco, 986)
	fnAddOneN(mat coco,1127)
	fnAddOneN(mat coco, 984)
	fnAddOneN(mat coco,   2)
	fnAddOneN(mat coco, 967)
	fnAddOneN(mat coco,1133)
	fnAddOneN(mat coco,1083)
	fnAddOneN(mat coco, 749)
	! /r
! ! r: ask 
! 	fntos
msDelim$=chr$(179) ! ("³") '|' ! '³'    !  chr$(179) works - the other things here do not.
dim coco_selected$(0)*2048
mat coco_selected$(0)
dim coco_unselected$(0)*2048
mat coco_unselected$(udim(mat coco))
for item=1 to udim(mat coco)
	coco_unselected$(item)=str$(coco(item))&msDelim$&fn_cocoData$(coco(item),'name')&msDelim$&fn_cocoData$(coco(item),'email')
nex item
if ~cocoSelectSetup then
	cocoSelectSetup=1
	mat D_Grid_Heading$(3)     	: mat D_Grid_Width(3)	: mat D_Grid_Form$(3)
	D_Grid_Heading$(1)='Key'  	: D_Grid_Width(1)= 5 	: D_Grid_Form$(1)='C 5,[T]L'
	D_Grid_Heading$(2)='Name' 	: D_Grid_Width(2)=60 	: D_Grid_Form$(2)='C 60,[T]L'
	D_Grid_Heading$(3)='Email' 	: D_Grid_Width(3)=60 	: D_Grid_Form$(3)='C 60,[T]L'
end if

setenv('Session_Rows',str$(24))
setenv('Session_Cols',str$(80))

fnmulti_select(mat coco_selected$,mat coco_unselected$,'Select CoCo to include',Mat D_Grid_Heading$,Mat D_Grid_Width,Mat D_Grid_Form$)

! r: main loop
! ! /r
for cocoItem=1 to udim(mat coco_selected$)
	coco$=coco_selected$(cocoItem)(1:pos(coco_selected$(cocoItem),msDelim$,1)-1)
	cocoN=val(coco$)
	dim outFileName$*1024
	outFileName$=fnSpecialFolderPath$('desktop')&'\'&env$('program_caption')&' - '&coco$&' - '&fnsafe_filename$(fn_cocoData$(cocoN,'name'))&' - '&date$('ccyy-mm-dd')&'-'&srep$(time$,':','-')&'.xls'
	open #255: 'name='&env$('at')&outFileName$&',RecL=1024',d,o
		
	! if cocoItem=1 then
		! fnSel(1024, 'Select Output for all '&str$(udim(mat coco_selected$))&' '&env$('cap')&'s' ,255, 'Cancel','HTML',env$('cap'))
		! fnSel(width; printer_prompt$*80,printfile_handle, print_cancel_option$*80,supported_printer_type_list$*80,print_destination_custom$*1024,print_pk$*32)
		! fnSel(80,"Select Report Printer",'Cancel','HTML')
	! else
	! 	fnReopen_last_printer
	! end if
	! print #255: 'As of '&fnDate_rpt10$(Date$)&' for CoCo '&coco$&'.'
	! masterKey$=  "forwarder number here"
	restore #hM: ! ,key=>masterKey$: 
	pr #255: '</pre>'
	pr #255: '<table>'
	gosub PrHeader
	do
		read #hM,using mFormAll$: mat masterData$,mat masterDataN eof NextCoCo
		if masterDataN(master_coco_no)=cocoN then
			pr #255: '<tr> ';
			pr #255: '<td>'&cnvrt$('N 4',masterDataN(master_coco_no))&'</td>';
			pr #255: '<td>'&masterData$(master_fileno)&'</td>';
			pr #255: '<td>'&masterData$(master_d1_name)&'</td>';
			pr #255: '<td>'&masterData$(master_suit_date)&'</td>';
			pr #255: '<td>'&cnvrt$('N 10.2',masterDataN(master_suit_amt))&'</td>';
			pr #255: '<td>'&cnvrt$('N 10.2',masterDataN(master_balance))&'</td>';
			pr #255: '<td>'&masterData$(master_jmt_date)&'</td>';
			pr #255: '<td>'&cnvrt$('N 10.2',masterDataN(master_jmt_amt))&'</td>';
			pr #255: '<td>'&masterData$(master_lpaymnt_date)&'</td>';
			pr #255: '<td>'&cnvrt$('N 10.2',masterDataN(master_lpaymnt_amt))&'</td>';
			pr #255: '<td>'&cnvrt$('N 10.2',masterDataN(master_stored_int))&'</td>';
			pr #255: '<td></td>'
			pr #255: '<td></td>'
			pr #255: '</tr> '
		end if
	loop
	NextCoCo: !
	pr #255: '</table>'
	fnAddOneC(mat fileCreated$,outFileName$)
	fnAddOneC(mat fileEmailAddr$,fn_cocoData$(cocoN,'email'))
	close #255:
nex cocoItem
for cocoItem=1 to udim(mat coco_selected$)
	dim tmpEmail$*256
	dim tmpEmailList$(0)*256
	tmpEmail$=fn_cocoData$(cocoN,'email')
	str2mat(tmpEmail$,mat tmpEmailList$,';')
	for emailItem=1 to udim(mat tmpEmail$)
		EXECUTE "sy -@ -c -M start mailto:"&tmpEmail$(emailItem)&"^&Subject="&Email_Subject$&'^&Attach="'&fileCreated$(cocoItem)
		pause
	nex emailItem
nex cocoItem
goto Finis ! /r
PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:

		pr #255: '<tr>'
		pr #255: '<th> CoCo  </th>'
		pr #255: '<th> FileNo   </th>'
		pr #255: '<th> d1_name </th>'
		pr #255: '<th> suit_date </th>'
		pr #255: '<th> suit_amt </th>'
		pr #255: '<th> suit_amt </th>'
		pr #255: '<th> Balance </th>'
		pr #255: '<th>  Jmt Date </th>'
		pr #255: '<th> Jmt Amount </th>'
		pr #255: '<th> Last Payment Date </th>'
		pr #255: '<th> Last Payment Amount </th>'
		pr #255: '<th> Interest </th>'
		pr #255: '<th> Garn Date </th>'
		pr #255: '</tr>'
return ! /r
Finis: ! r:
goto Xit ! /r
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		library 'library\CLSUtil.wb': fnDate_rpt10$
		
		library 'S:\Core\Library.br': fnsafe_filename$
		library 'S:\Core\Library.br': fnSpecialFolderPath$
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnCountMatchesC
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop
		library 'S:\Core\Library.br': fnXit

		library "library\CLSUtil.wb": fnGetInf$
		library "library\CLSUtil.wb": fncom
		library "library\CLSUtil.wb": fnget_formall$,fnget_formarr
		library "library\CLSUtil.wb": fnreport_path$,fnclaim_path$
		library "library\CLSUtil.wb": fnget_claimfiles,fnclaim_scroll
		library "library\CLSUtil.wb": fnrange_to_array,fnarray_to_range$
		library "library\CLSUtil.wb": fnfix_bh,fnask_payref
		library "library\CLSUtil.wb": fnget_form
		library "library\CLSUtil.wb": fnunpack$
		library "library\CLSUtil.wb": fnStime,fnStime$
		library "library\CLSUtil.wb": fnMessageBox
		library "library\CLSUtil.wb": Fnlist_Print
		library "Prog2\Mast2.wb": fnsql_read

		library "library\CLSUtil.wb": fnfix_bh
		library "Prog2\Mast_SQL.wb": fnmast2_int_cache
		library "library\CLSUtil.wb": fnAsk_file1
		
		
		
		library 'Library\clsUtil': fnmessagebox
		library 'Library\clsUtil': fngrid_setup
		library 'Library\GridIO': fnmulti_select
		library 'Library\GridIO': fnconfirm
		library 'Library\GridIO': fnconfirm_delete
		library 'Prog2\RE': fnrights_effective
		library 'Theme\Theme': fnsection_divider
		library 'Library\SQL': fnopen_sql_file,fnsql_setup$
		
		! gosub SetupSql
		gosub SetupPrint
		
		
	end if
fnend
! SetupSql: ! r: (Ends by Line 14990) - SQL Setup #AutoNumber# 14900,1
! 	if ~setup_sql then 
! 		setup_sql=1
! 		!   printer
! 		dim printer_data$(0)*60,printer_data(0),printer_fieldsc$(0)*20,printer_fieldsn$(0)*20,printer_formall$*512,printer_fc$(1,3)*80,printer_fn$(1,3)*80,printer_desc_c$(0)*80,printer_desc_n$(0)*80,printer_seq$(0)*80,printer_valid$(0)*80
! 		execute "*SubProc "&fnsql_setup$('printer',mat printer_data$,mat printer_data,mat printer_fieldsc$,mat printer_fieldsn$,printer_formall$,mat printer_fc$,mat printer_fn$,mat printer_desc_c$,mat printer_desc_n$,mat printer_seq$,mat printer_valid$)
! 	end if  ! ~Setup_SQL
! return  ! /r SETUP_SQL
OPEN_FILES: ! r: (Ends by Line 20990) - Open_Files #AutoNumber# 20000,10
	fnopen_sql_file
	table_related_count=6
	! 
	h_table_related(tr_stat_msg  =1)=fnopen_sql_file(table_related$(tr_stat_msg    )='Stat_Msg')    : tr_group_pos(tr_stat_msg   )=val(stat_msg_fc$(stat_msg_group,1))
	h_table_related(tr_collfile  =2)=fnopen_sql_file(table_related$(tr_collfile    )='CollFile')    : tr_group_pos(tr_collfile   )=val(collfile_fc$(collfile_group_access_cd,1))
	h_table_related(tr_printer   =3)=fnopen_sql_file(table_related$(tr_printer     )='Printer')     : tr_group_pos(tr_printer    )=val(printer_fc$(printer_group,1))
	h_table_related(tr_masforw   =4)=fnopen_sql_file(table_related$(tr_masforw     )='MasForw')     : tr_group_pos(tr_masforw    )=val(masforw_fc$(masforw_group_access_cd,1))
	h_table_related(tr_diarycdsec=5)=fnopen_sql_file(table_related$(tr_diarycdsec  )='DiaryCdSec') : tr_group_pos(tr_diarycdsec  )=val(stat_msg_fc$(diarycdsec_groupcode,1))
	h_table_related(tr_zone_sec  =6)=fnopen_sql_file(table_related$(tr_zone_sec    )='Zone_Sec')    : tr_group_pos(tr_zone_sec   )=val(zone_sec_fc$(diarycdsec_groupcode,1))
	! 
	table_related_record_count=0
	for table_item=1 to table_related_count
		table_related_record_count+=lrec(h_table_related(table_item))
	next table_item
	open #masterhandle:=64: "Name=PERMISN//8,KFName=PERMISN.IDX//8,USE,RecL=60,KPs=1/4,KLn=3U/20U,Shr",internal,outin,keyed 
	open #65: "Name=PERMISN//8,KFName=PERMISN.COD//8,USE,RecL=60,KPs=4/1,KLn=20U/3U,Shr",internal,outin,keyed 
	open #h_userlist_in:=51: "Name=USERLIST//8,KFName=USERLIST.IN//8,Shr",internal,outin,keyed 
	open #h_userlist_nam_unused:=52: "Name=USERLIST//8,KFName=USERLIST.NAM//8,Shr",internal,outin,keyed 
	open #h_groups:=69: "Name=GROUPS//8,KFName=GROUPS.IDX//8,USE,RecL=43,KPs=1,KLn=3U,Shr",internal,outin,keyed 
	open #groupmem_handle:=70: "Name=GROUPMEM//8,KFName=GROUPMEM.GRP//8,USE,RecL=6,KPs=1/4,KLn=3U/3U,Shr",internal,outin,keyed 
	open #groupmem_usr_handle:=71: "Name=GROUPMEM//8,KFName=GROUPMEM.USR//8,USE,RecL=6,KPs=4/1,KLn=3U/3U,Shr",internal,outin,keyed 
return  ! /r
def fn_cocoData$*60(cocoNo,field$*20; ___,return$*60)
	if ~setupCocoData then
		setupCocoData=1
		dim cocoData$(0)*60
		dim cocoDataN(0)
		dim cocoFieldsc$(0)*20
		dim cocoFieldsN$(0)*20
		dim cocoFormAll$*256
		execute "*SubProc "&fnsql_setup$('masco',mat cocoData$,mat cocoDataN,mat cocoFieldsc$,mat cocoFieldsN$,cocoFormAll$)
	end if
	if ~hCoco then
		open #hCoco:=fngethandle:'name=masco//8,kfname=masco.idx//8,shr',internal,input,keyed
	end if
	field$=trim$(uprc$(field$))
	if cocoNo<>cocoNo_prior then
		read #hCoco,using cocoFormAll$,key=cnvrt$('BH 3',cocoNo): mat cocoData$,mat cocoDataN
		cocoNo_prior=cocoNo
	end if
	whichC=srch(mat cocoFieldsc$,field$)
	whichN=srch(mat cocoFieldsn$,field$)
	if whichC then
		return$=rtrm$(cocoData$(whichC))
	else if whichN then
		return$=str$(cocoDataN(whichN))
	else
		pr 'can not find a field with the name '&field$&' in masco.'
		pause
	end if
	fn_cocoData$=return$
fnend
include: cm\enum\common
include: cm\err
include: cm\print
include: cm\enum\master