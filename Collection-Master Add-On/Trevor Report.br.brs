! questions:  
! 1 - open claims only or closed claims also?
! 2 - is existance of suit date appropriate way to determine if suit is filed?
! 2 - how to determine if paying or paid
! 3 - how to determine:  accounts lost assets (mail return, bankrupt)

! Would you be able to gather a report that would show by month for 2018
! 	Number of placements,
! 	# of suits on those placement accounts .
! 	non sued accounts that are paying or paid,
! 		accounts lost assets (mail return, bankrupt)


on error goto Error_Hanler
fn_setup
fntop(program$)
mat forwarderFilter(0)
fnAddOneN(mat forwarderFilter,4033)
fnAddOneN(mat forwarderFilter,4034)
fnAddOneN(mat forwarderFilter,4035)
	! r: open master
	dim masterData$(1)*60,masterDataN(1)
	dim masterFieldsc$(1)*20,masterFieldsN$(1)*20,masterFormC$*1024,masterFormN$*1024
	dim mFormAll$*2048
	fnget_form("Master",mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,masterFormC$,masterFormN$)
	fnunpack$(masterFormC$,masterFormN$)
	mFormAll$=fnget_formall$
	gosub enumMaster
	open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
	! /r
	fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	
	
	startingDay=days(date$(days(date$)-365,'mm/01/ccyy'),'mm/dd/ccyy')
	
	monthCount=12
	
	dim monthStartDay(12)
	mat monthStartDay(monthCount)
	monthStartDay(1)=startingDay
	for x=2 to udim(mat monthStartDay)
		monthStartDay(x)=days(date$(monthStartDay(x-1)+32,'mm/01/ccyy'),'mm/dd/ccyy')
	nex x
	endingDay=fnEndOfMonth(monthStartDay(monthCount))

	mat placementCount(monthCount)
	mat placementAmounts(monthCount)
	mat suitFiled(monthCount)
! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table>'
	pr #255: '<tr>'
	pr #255: '<td><h1>'&env$('program_caption')&'</h1></td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td>For '&str$(monthCount)&' month period starting As of '&date$(startingDay,'mm/dd/ccyy')&'.</td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: 'As of '&fnDate_rpt10$(Date$)&'.'
	pr #255: '</tr>'
	if udim(mat forwarderFilter)>0 then
		pr #255: '<tr>'
		pr #255: '<td>Forwarder Filter: ';
		For forwarder=1 to udim(mat forwarderFilter)
		pr #255: str$(forwarderFilter(forwarder));
		if forwarder<udim(mat forwarderFilter) then
			pr #255: ', '
		end if
		nex forwarder
		pr #255: '</td>'
	pr #255: '</tr>'
	end if
	pr #255: '</table>'
	! /r
! r: Accumulate that Data
	! masterKey$=  "forwarder number here"
	restore #hM: ! ,key=>masterKey$:
	mat placementCount=(0)
	mat placementAmounts=(0)
	mat suitFiled=(0)
	do
		read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
		if udim(mat forwarderFilter)<=0 or srch(mat forwarderFilter,masterDataN(master_forw_no))>0 then
			month=fn_whichMonth(days(masterData$(master_date_recd),'ccyymmdd'))
			if month>0 then
				placementCount(month)   +=1
				placementAmounts(month) +=masterDataN(master_ORIG_CLAIM)
				if masterData$(master_suit_date)<>'' then
					suitFiled(month)+=1
				end if
			end if
		end if
	loop
	EoMaster: !
	! /r
	! r: Print the Accumulated Data in Excel

	pr #255: '<table>'
	gosub PrHeader
	For month=1 to monthCount
		pr #255: '<tr> ';
		pr #255: '<td>'&date$(monthStartDay(month),'MM/CCYY')&'</td>';
		pr #255: '<td>'&cnvrt$('pic(z,zzz,zzz,zz#)',placementCount(month))&'</td>';
		pr #255: '<td>'&cnvrt$('pic(z,zzz,zzz,zz#.##)',placementAmounts(month))&'</td>';
		pr #255: '<td>'&cnvrt$('pic(z,zzz,zzz,zz#)',suitFiled(month))&'</td>';
		pr #255: '<td></td>'
		pr #255: '</tr> '
	nex month


	pr #255: '</table>'
	fnClose
goto Finis ! /r
PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:
		pr #255: '<tr>'
		pr #255: '<th> Month    </th>'
		pr #255: '<th> Placement Count </th>'
		pr #255: '<th> Placement Amount </th>'
		pr #255: '<th> Suit Filed </th>'
		pr #255: '</tr>'
return ! /r
def fn_whichMonth(aDay; ___,returnN)
	! utilizies local: mat monthStartDay,endingDay
	if aDay<monthStartDay(1) or aDay>endingDay then ! outside month range
		returnN=0
	else if aDay=>monthStartDay(12) and aDay<=endingDay then ! last month
		returnN=12
	else
		for x=1 to udim(mat monthStartDay)-1
			if aDay=>monthStartDay(x) and aDay<monthStartDay(x+1) then
				returnN=x
			end if
		nex x
	end if
	! pr returnN : pause
	fn_whichMonth=returnN
fnend
Finis: ! r:
goto Xit ! /r
Xit: CHAIN "m/prog1"
def fn_setup
	if ~setup then
		setup=1
		library 'library\CLSUtil.wb': fnDate_rpt10$

		library 'S:\Core\Library.br': fnEndOfMonth
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnCountMatchesC
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop

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
		library "library\CLSUtil.wb": fnList_Print
		library "Prog2\Mast2.wb": fnsql_read

		library "library\CLSUtil.wb": fnfix_bh
		library "Prog2\Mast_SQL.wb": fnmast2_int_cache
		library "library\CLSUtil.wb": fnAsk_file1



		library "CLSUtil/Library": fnadd_one$,fngrid_setup,fnget_file,fnget_groups,fnuser_init$,fnsecurity,fnget_form,fnget_formall$,fnget_var$,fngethandle,fnremove_arrayitem$,fnremove_arrayitem,fnadd_one,fncom
		library "CLSUtil/Library": fnmessagebox,fn_encryptdecrypt,fndefault_password$,fnlist_print,fnsetmatcnc,fngui_push_on,fnprogram_top
		library "CLSUtil/Library": fngenerate_buttons,fnerase_buttons,fndisplay_top ! ,fnarray_item_insert$
		library 'GridIO/Library': fnmulti_select,fnconfirm,fnconfirm_delete
		library 'RE/Prog2': fnrights_effective
		library 'Theme/Theme': fnsection_divider
		library 'sql/library': fnopen_sql_file,fnsql_setup$

		gosub SetupSql
		gosub SetupPrint


	end if
fnend
SetupSql: ! r: (Ends by Line 14990) - SQL Setup #AutoNumber# 14900,1
if ~setup_sql then
	setup_sql=1
	!   printer
	dim printer_data$(0)*60,printer_data(0),printer_fieldsc$(0)*20,printer_fieldsn$(0)*20,printer_formall$*512,printer_fc$(1,3)*80,printer_fn$(1,3)*80,printer_desc_c$(0)*80,printer_desc_n$(0)*80,printer_seq$(0)*80,printer_valid$(0)*80
	execute "*SubProc "&fnsql_setup$('printer',mat printer_data$,mat printer_data,mat printer_fieldsc$,mat printer_fieldsn$,printer_formall$,mat printer_fc$,mat printer_fn$,mat printer_desc_c$,mat printer_desc_n$,mat printer_seq$,mat printer_valid$)
end if  ! ~Setup_SQL
return  ! /r SETUP_SQL
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
include: cm\enum
include: cm\err
include: cm\print
include: cm\enumMaster