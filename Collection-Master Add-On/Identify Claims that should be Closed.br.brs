! questions:  

! 3 - how to determine:  accounts lost assets (mail return, bankrupt)

! Would you be able to gather a report that would show by month for 2018
! 	Number of placements,
! 	# of suits on those placement accounts .
! 	non sued accounts that are paying or paid,
! 		accounts lost assets (mail return, bankrupt)
! 					Mail Return - scan all debtors
! 						debtor--RET_MAIL=Y   means returned mail
! 						debtor--BKCY_FILED_DATE
! 
! count of accounts with judgment but no assets

! E or B on debtor screen than they do have assets


on error goto Error_Hanler
fn_setup
fnTop(program$)
mat forwarderFilter(0)
fnAddOneN(mat forwarderFilter,4033)
fnAddOneN(mat forwarderFilter,4034)
fnAddOneN(mat forwarderFilter,4035)


mat closeCode$(0)
fnAddOneC(mat closeCode$,'*cc:C101') ! Close –Bankrupt
fnAddOneC(mat closeCode$,'*cc:C102') ! Close – Client Request
fnAddOneC(mat closeCode$,'*cc:C104') ! Close – Deceased No Estate
fnAddOneC(mat closeCode$,'*cc:C109') ! Close – Paid in Full
fnAddOneC(mat closeCode$,'*cc:C113') ! Close – Skip
fnAddOneC(mat closeCode$,'*cc:C115') ! Close – Debtor Out of Area (This one doesn’t necessarily mean it needs to be closed in our office. Usually we need to forward it to a CA for a different state because the consumer has moved).
fnAddOneC(mat closeCode$,'*cc:C118') ! Close – Settle in Full
fnAddOneC(mat closeCode$,'*cc:C140') ! Close – Soldiers & Sailors
fnAddOneC(mat closeCode$,'*cc:C141') ! Close – Debtor in Jail



fnTos(sn$='ictsbc')
dim resp$(64)*128
fnLbl(2,2,'Forwarder Number(s):',20,1)
fnTxt(2,24,20)
resp$(1)=fnArray_to_range$(mat forwarderFilter)
fnCmdSet(2)
fnAcs(sn$,0,mat resp$,ckey)
if ckey=5 then goto Xit
fnrange_to_array(resp$(1),mat forwarderFilter)
	! r: open master
	! dim masterData$(1)*60,masterDataN(1)
	! dim masterFieldsc$(1)*20,masterFieldsN$(1)*20,masterFormC$*1024,masterFormN$*1024
	! fnget_form("Master",mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,masterFormC$,masterFormN$)
	! fnunpack$(masterFormC$,masterFormN$)
	! mFormAll$=fnget_formall$
	
	dim masterData$(0)*60,masterDataN(0)
	dim masterFieldsc$(0)*20,masterFieldsN$(0)*20
	dim mFormAll$*2048
	! execute "*SubProc "&     <--- not necessary with include:enum\master
	fnsql_setup$('master',mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,mFormAll$)

	
	gosub enumMaster
	open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
	! /r
	! open #hDebtor:=fngethandle: 'name=debtor//6,kfname=debtor.idx//6,shr',internal,input,keyed
	! gosub enumDebtor
	
	fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	if fkey=93 or fkey=99 then goto Xit
	
	
! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table align="Center">'
	pr #255: '<tr>'
	pr #255: '<td align="Center"><h2>'&env$('program_caption')&'</h2></td>'
	pr #255: '</tr>'
	! pr #255: '<tr>'
	! pr #255: '<td align="Center">For '&str$(monthCount)&' month period '&date$(startingDay,'mm/dd/ccyy')&' - '&date$(startingDay,'mm/dd/ccyy')&'.</td>'
	! pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td  align="Center">As of '&fnDate_rpt10$(Date$)&'.</td>'
	pr #255: '</tr>'
	if udim(mat forwarderFilter)>0 then
		pr #255: '<tr>'
		pr #255: '<td align="Center">Forwarder Filter: ';
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
	
	fn_listPrint('Stage 1/2 - gather claims')
	mat listFileno$(0)
	mat listForwarder$(0)
	mat listClosedDate$(0)
	mat listOpenedDate$(0)
	mat listBalance(0)
	mat listDiary$(0)
	
	
	
	do
		read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
		countReadMaster+=1
		dim fileno$*8
		fileno$=masterData$(master_fileno)
		if udim(mat forwarderFilter)<=0 or srch(mat forwarderFilter,masterDataN(master_forw_no))>0 then
			if date(days(masterData$(master_date_recd),'ccyymmdd'),'ccyy')=>2017 then
				fnAddOneC(mat listFileno$,fileno$)
				fnAddOneC(mat listForwarder$,str$(masterDataN(master_forw_no)))
				fnAddOneC(mat listClosedDate$,masterData$(master_closed_date))
				fnAddOneC(mat listOpenedDate$,str$(masterDataN(master_opened_date)))
				fnAddOneN(mat listBalance,masterDataN(master_balance))
				fnAddOneC(mat listDiary$,'')
				! pause
			end if
		end if
	loop
	EoMaster: !
	! /r

	fn_listPrint('Stage 2/3 - scan paperless')
	OPEN #hActive:=fngethandle: "Name=Active.int//6,KFName=Active.idx//6,Shr",internal,input,keyed ! RecL=92,KPs=1,KLn=8,Shr
	do
		read #hActive,using active_FormAll$: mat active_data$,mat active_data eof EoActive
		countReadActive+=1
		whichCode=srch(mat closeCode$,trim$(active_data$(active_code)))
		if whichCode>0 then
			whichListItem=srch(mat listFileno$,active_data$(active_fileno))
			if whichListItem>0 then
				dim tmpDiary$(0)*128
				mat tmpDiary$(0)
				str2mat(listDiary$(whichListItem),mat tmpDiary$,',')
				fnAddOneC(mat tmpDiary$,closeCode$(whichCode), 0,1)
				mat2str(mat tmpDiary$,listDiary$(whichListItem),',')
			end if
		end if
			
	loop
	EoActive: !
	close #hActive:


	! r: Print the Accumulated Data in Excel
	fn_listPrint('Stage 3/3 - produce report')
	pr #255: '<table cellspacing=10>'
	gosub PrHeader
	For item=1 to udim(mat listFileno$)
		pr #255: '<tr> ';
		pr #255: '<td              >'&listFileno$    (item)&'</td>';
		pr #255: '<td align="right">'&listForwarder$ (item)&'</td>';
		pr #255: '<td align="right">'&listClosedDate$(item)&'</td>';
		pr #255: '<td align="right">'&listOpenedDate$(item)&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(---,---,---,--z.zz)',listBalance(item))&'</td>';
		pr #255: '<td>'               &listDiary$     (item)&'</td>';
		pr #255: '</tr> '
	nex item
	pr #255: '</table>'
	fnClose
goto Finis ! /r
PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:
		pr #255: '<tr>'
		pr #255: '<th> FileNo    </th>'
		pr #255: '<th> Forwarder</th>'
		pr #255: '<th> Closed Date </th>'
		pr #255: '<th> Opened Date</th>'
		pr #255: '<th> Balance</th>'
		pr #255: '<th> Diaries of Interest</th>'
		pr #255: '</tr>'
return ! /r

Finis: ! r:
pr #255: '<table>'
pr #255: '  <tr><th>Category</th><th>Total</th></tr>'
pr #255: fn_finisAddRow('Master Read Count',str$(countReadMaster))
pr #255: fn_finisAddRow('Active Read Count',str$(countReadActive))
pr #255: '</table>'
goto Xit ! /r
Xit: fnXit
def fn_finisAddRow(label$*256,value$*256)
	pr #255: '  <tr>'
	pr #255: '    <td>'&label$&'</td>'
	pr #255: '    <td>'&value$&'</td>'
	pr #255: '  </tr>'
fnend
def fn_setup
	if ~setup then
		setup=1
		
		LIBRARY 'Library\OpenFile': Fnopen_Active
		
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		library 'library\clsUtil.wb': fnArray_to_range$,fnRange_to_array
		library 'prog2\intermnt.wb': fnInternal_data


		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs
		library 'S:\Core\Library.br': fnEndOfMonth
		library 'S:\Core\Library.br': fnGetHandle
		! library "CLSUtil/Library": fngethandle
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
		library "library\CLSUtil.wb": fncom
		library "Prog2\Mast2.wb": fnsql_read

		library "library\CLSUtil.wb": fnfix_bh
		library "Prog2\Mast_SQL.wb": fnmast2_int_cache
		library "library\CLSUtil.wb": fnAsk_file1



		library "Library\CLSUtil": fnremove_arrayitem$,fnremove_arrayitem
		library "Library\CLSUtil": fngrid_setup
		library "Library\CLSUtil": fnget_file
		library "Library\CLSUtil": fnget_groups
		library "Library\CLSUtil": fnuser_init$
		library "Library\CLSUtil": fnsecurity
		library "Library\CLSUtil": fnget_form
		library "Library\CLSUtil": fnget_formall$
		library "Library\CLSUtil": fnget_var$
		library "Library\CLSUtil": fnmessagebox,fn_encryptdecrypt,fndefault_password$
		library "Library\CLSUtil": fndisplay_top
		library "Library\CLSUtil": fngenerate_buttons
		library 'Library\GridIO': fnmulti_select,fnconfirm,fnconfirm_delete
		library 'Prog2\RE': fnrights_effective
		library 'Theme\Theme': fnsection_divider
		library 'Library\SQL': fnopen_sql_file
		library 'Library\SQL': fnsql_setup$

		gosub Enum
		gosub SetupPrint


		library "CLSUtil/Library": fnget_form,fnget_formarr

		dim active_data$(0)*60,active_data(0)
		dim active_fieldsc$(0)*20,active_fieldsn$(0)*20
		dim active_formall$*2048
		execute "*SubProc "&fnsql_setup$('active',mat active_data$,mat active_data,mat active_fieldsc$,mat active_fieldsn$,active_formall$)
		! dim active_formc$*2048,active_formn$*2048
		! dim active_des_c$(1)*80,active_des_n$(1)*80,active_seq$(1)*80,active_valid$(1)*80,active_fc$(1,3)*80,active_fn$(1,3)*80
		! fnget_form("active",mat active_data$,mat active_data,mat active_fieldsc$,mat active_fieldsn$,active_formc$,active_formn$)
		! fnget_formarr("active",mat active_data$,mat active_data,mat active_fieldsc$,mat active_fieldsn$,mat active_fc$,mat active_fn$,mat active_des_c$,mat active_des_n$,mat active_seq$,mat active_valid$)
		! active_formall$=fnget_formall$
		! fnget_var$


	end if
fnend
def fn_listPrint(item$*2048)
	if ~listPrintSetup then
		listPrintSetup=1
		if env$('Session_Rows')='' then let setenv('Session_Rows',24)
		if env$('Session_Cols')='' then let setenv('Session_Cols',80)
		
		listPrint_sRow=int(val(env$('Session_Rows'))/2)
		listPrint_Rows=val(env$('Session_Rows'))-listPrint_sRow-2
		
		listPrint_sCol=3
		listPrint_Cols=val(env$('Session_Cols'))-listPrint_sCol-2
		pr 'listPrint_sRow=';listPrint_sRow
		pr 'listPrint_Rows=';listPrint_Rows
		pr 'listPrint_sCol=';listPrint_sCol
		pr 'listPrint_Cols=';listPrint_Cols
		pause
	end if
	
	
	Fnlist_Print(item$, handle,Lp_Cache_Disable, Cap_Etc$,listPrint_sRow,listPrint_sCol,listPrint_Rows,listPrint_Cols,listPrint_Border$)
fnend
include: cm\enum\common
include: cm\err
include: cm\print
include: cm\enum\master
include: cm\enum\debtor
