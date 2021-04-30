on error goto Ertn
fn_setup
! fnTop(program$)
fnTop(program$,'',1)
! r: Set Defaults and Filters
	mat forwarderFilter(0)
	fnAddOneN(mat forwarderFilter,4033)
	fnAddOneN(mat forwarderFilter,4034)
	fnAddOneN(mat forwarderFilter,4035)
	dim forwarderFilter$*40
	forwarderFilter$=fnArray_to_range$(mat forwarderFilter)
	
	mat closeCode$(0)
	fnAddOneC(mat closeCode$,'*CC:C101') ! Close –Bankrupt
	fnAddOneC(mat closeCode$,'*CC:C102') ! Close – Client Request
	fnAddOneC(mat closeCode$,'*CC:C104') ! Close – Deceased No Estate
	fnAddOneC(mat closeCode$,'*CC:C109') ! Close – Paid in Full
	fnAddOneC(mat closeCode$,'*CC:C113') ! Close – Skip
	fnAddOneC(mat closeCode$,'*CC:C115') ! Close – Debtor Out of Area (This one doesn’t necessarily mean it needs to be closed in our office. Usually we need to forward it to a CA for a different state because the consumer has moved).
	fnAddOneC(mat closeCode$,'*CC:C118') ! Close – Settle in Full
	fnAddOneC(mat closeCode$,'*CC:C140') ! Close – Soldiers & Sailors
	fnAddOneC(mat closeCode$,'*CC:C141') ! Close – Debtor in Jail
! /r

! r: Screens
	fnTos(sn$='ictsbc')
	dim resp$(64)*128
	fnLbl(2,2,'Forwarder Number(s):',20,1)
	fnTxt(2,24,40)
	resp$(1)=forwarderFilter$
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	forwarderFilter$=resp$(1)
	fnrange_to_array(forwarderFilter$,mat forwarderFilter)
	
	fn_sel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	if fkey=93 or fkey=99 then goto Xit
	
! /r

! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table align="Center">'
	pr #255: '<tr>'
	pr #255: '<td align="Center"><h2>'&env$('program_caption')&'</h2></td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td  align="Center">As of '&fnDate_rpt10$(Date$)&'.</td>'
	pr #255: '</tr>'
	if forwarderFilter$<>'' then
		pr #255: '<tr>'
		pr #255: '  <td align="Center">Forwarder Filter: '&forwarderFilter$&'</td>'
		pr #255: '</tr>'
	end if
	pr #255: '</table>'
	! /r
! r: Accumulate that Data
	! masterKey$=  "forwarder number here"
	open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
	restore #hM: ! ,key=>masterKey$:
	
	fn_listPrint('Stage 1/2 - gather claims')
	comLine=12
	mat listFileno$(0)
	mat listForwarder$(0)
	mat listOpenedDate$(0)
	mat listBalance(0)
	mat listClosedDate$(0)
	mat listDiary$(0)

	do
		read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
		countReadMaster+=1
		fncom(countReadMaster,lrec(hM), comLine)
		dim fileno$*8
		fileno$=masterData$(master_fileno)
		if udim(mat forwarderFilter)<=0 or srch(mat forwarderFilter,masterDataN(master_forw_no))>0 then
			if date(days(masterData$(master_date_recd),'ccyymmdd'),'ccyy')=>2017 then
				countMatchMaster+=1
				fnAddOneC(mat listFileno$,fileno$)
				fnAddOneC(mat listForwarder$,str$(masterDataN(master_forw_no)))
				fnAddOneC(mat listOpenedDate$,str$(masterDataN(master_opened_date)))
				fnAddOneN(mat listBalance,masterDataN(master_balance))
				fnAddOneC(mat listClosedDate$,masterData$(master_closed_date))
				fnAddOneC(mat listDiary$,'')
				! pause
			end if
		end if
	loop
	EoMaster: !
	close #hM:
	fncom(lrec(hM),lrec(hM), comLine)
	! /r
	!				! r: Stage 2 - scan paperless - SCAN WHOLE FILE ONCE approach - works but is slow
	!				fn_listPrint('Stage 2/3 - scan paperless')
	!				comLine+=1
	!				open #hActive:=fngethandle: "Name=Active.int//6,KFName=Active.idx//6,Shr",internal,input,keyed ! RecL=92,KPs=1,KLn=8,Shr
	!				do
	!					read #hActive,using active_FormAll$: mat active_data$,mat active_data eof EoActive
	!					countReadActive+=1
	!					fncom(countReadActive,lrec(hActive),comLine)
	!					whichCode=srch(mat closeCode$,trim$(active_data$(active_code)))
	!					if whichCode>0 then
	!						whichListItem=srch(mat listFileno$,active_data$(active_fileno))
	!						if whichListItem>0 then
	!							dim tmpDiary$(0)*128
	!							mat tmpDiary$(0)
	!							str2mat(listDiary$(whichListItem),mat tmpDiary$,',')
	!							fnAddOneC(mat tmpDiary$,closeCode$(whichCode), 0,1)
	!							mat2str(mat tmpDiary$,listDiary$(whichListItem),',')
	!						end if
	!					end if
	!						
	!				loop
	!				EoActive: !
	!				close #hActive:
	!				fncom(lrec(hActive),lrec(hActive),comLine)
	!				! /r
		! r: Stage 2 - scan paperless - RESTORE FILE for each claim approach - should be faster
	fn_listPrint('Stage 2/3 - scan paperless')
	comLine+=1
	open #hActive:=fngethandle: "Name=Active.int//6,KFName=Active.idx//6,Shr",internal,input,keyed ! RecL=92,KPs=1,KLn=8,Shr
	for claim=1 to udim(mat listFileno$)
		fileno$=listFileno$(claim)
		restore #hActive,key=>rpad$(fileno$,kln(hActive)): noKey S2b_nextClaim
		do
			read #hActive,using active_FormAll$: mat active_data$,mat active_data eof S2b_nextClaim
			countReadActive+=1
			if fileno$=trim$(active_data$(active_fileno)) then

				whichCode=srch(mat closeCode$,trim$(active_data$(active_code)))
				if whichCode>0 then
					whichListItem=srch(mat listFileno$,active_data$(active_fileno))
					if whichListItem>0 then
						countMatchActive+=1
						dim tmpDiary$(0)*128
						mat tmpDiary$(0)
						str2mat(listDiary$(whichListItem),mat tmpDiary$,',')
						fnAddOneC(mat tmpDiary$,closeCode$(whichCode), 0,1)
						mat2str(mat tmpDiary$,listDiary$(whichListItem),',')
					end if
				end if

			end if
		loop while fileno$=trim$(active_data$(active_fileno))
		S2b_nextClaim: !
		fncom(claim,udim(mat listFileno$), comLine)
	nex claim
	fncom(udim(mat listFileno$),udim(mat listFileno$), comLine)
	close #hActive:
	! /r

	! r: Print the Accumulated Data in Excel
	fn_listPrint('Stage 3/3 - produce report')
	comLine+=1
	pr #255: '<table cellpadding=10 border=1>'
	pr #255: '  <tr><th>Category</th><th>Total</th></tr>'
	fn_finisAddRow('Master Read Count',str$(countReadMaster))
	fn_finisAddRow('Master Match Count',str$(countMatchMaster))
	fn_finisAddRow('Active Read Count',str$(countReadActive))
	fn_finisAddRow('Active Match Count',str$(countMatchActive))
	pr #255: '</table>'
	
	pr #255: '<table cellpadding=10 border=1>'
	gosub PrHeader
	For item=1 to udim(mat listFileno$)
		fncom(item,udim(mat listFileno$),comLine)
		if listBalance(item)<=0 or listDiary$(item)<>'' then
			pr #255: '<tr> ';
			pr #255: '<td              >'&listFileno$    (item)&'</td>';
			pr #255: '<td align="right">'&listForwarder$ (item)&'</td>';
			pr #255: '<td align="right">'&listOpenedDate$(item)&'</td>';
			pr #255: '<td align="right">'&cnvrt$('pic(---,---,---,--z.zz)',listBalance(item))&'</td>';
			pr #255: '<td align="right">'&listClosedDate$(item)&'</td>';
			pr #255: '<td>'               &listDiary$     (item)&'</td>';
			pr #255: '</tr> '
		end if
	nex item
	pr #255: '</table>'

	fnClose
	fncom(udim(mat listFileno$),udim(mat listFileno$),comLine)
goto Xit ! /r
def fn_finisAddRow(label$*256,value$*256)
	pr #255: '  <tr>'
	pr #255: '    <td>'&label$&'</td>'
	pr #255: '    <td align="right">'&value$&'</td>'
	pr #255: '  </tr>'
fnend

PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:
		pr #255: '<tr>'
		pr #255: '  <th>FileNo             </th>'
		pr #255: '  <th>Forwarder          </th>'
		pr #255: '  <th>Opened Date        </th>'
		pr #255: '  <th>Balance            </th>'
		pr #255: '  <th>Closed Date        </th>'
		pr #255: '  <th>Diaries of Interest</th>'
		pr #255: '</tr>'
return ! /r


Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		
		
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnArray_to_range$,fnRange_to_array

		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTos,fnAcs,fnLbl,fnTxt,fnCmdSet
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fnTop

		library 'library\CLSUtil.wb': fncom
		library 'library\CLSUtil.wb': fnList_Print

		library 'Library\SQL.wb': fnSql_setup$

		gosub Enum
		gosub SetupPrint

		dim masterData$(0)*60,masterDataN(0)
		dim masterFieldsc$(0)*20,masterFieldsN$(0)*20
		dim mFormAll$*2048
		! execute "*SubProc "&     <--- not necessary with include:enum\master  and  gosub EnumMaster
		fnsql_setup$('master',mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,mFormAll$)
		gosub EnumMaster

		dim active_data$(0)*60,active_data(0)
		dim active_fieldsc$(0)*20,active_fieldsn$(0)*20
		dim active_formall$*2048
		execute "*SubProc "&fnsql_setup$('active',mat active_data$,mat active_data,mat active_fieldsc$,mat active_fieldsn$,active_formall$)



	end if
fnend
def fn_listPrint(item$*2048)
	if ~listPrintSetup then
		listPrintSetup=1
		if env$('Session_Rows')='' then setenv('Session_Rows',24)
		if env$('Session_Cols')='' then setenv('Session_Cols',80)
		
		listPrint_sRow=int(val(env$('Session_Rows'))/2)
		listPrint_Rows=val(env$('Session_Rows'))-listPrint_sRow-2
		
		listPrint_sCol=3
		listPrint_Cols=val(env$('Session_Cols'))-listPrint_sCol-2
		! pr 'listPrint_sRow=';listPrint_sRow
		! pr 'listPrint_Rows=';listPrint_Rows
		! pr 'listPrint_sCol=';listPrint_sCol
		! pr 'listPrint_Cols=';listPrint_Cols
		! pause
	end if
	
	
	Fnlist_Print(item$, handle,Lp_Cache_Disable, Cap_Etc$,listPrint_sRow,listPrint_sCol,listPrint_Rows,listPrint_Cols,listPrint_Border$)
fnend
include: cm\enum\common
include: cm\enum\master
include: cm\enum\debtor
include: cm\err
include: cm\print
