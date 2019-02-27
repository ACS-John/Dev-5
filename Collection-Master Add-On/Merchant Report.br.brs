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
	open #hDebtor:=fngethandle: 'name=debtor//6,kfname=debtor.idx//6,shr',internal,input,keyed
	gosub enumDebtor
	
	fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	if fkey=93 or fkey=99 then goto Xit
	
	
	startingDay=days(date$(days(date$)-365,'mm/01/ccyy'),'mm/dd/ccyy')
	monthCount=12
	! startingDay=days(date$(days(date$)-(365*2),'mm/01/ccyy'),'mm/dd/ccyy')
	! monthCount=24
	
	dim monthStartDay(0)
	mat monthStartDay(monthCount)
	monthStartDay(1)=startingDay
	for x=2 to udim(mat monthStartDay)
		monthStartDay(x)=days(date$(monthStartDay(x-1)+32,'mm/01/ccyy'),'mm/dd/ccyy')
	nex x
	endingDay=fnEndOfMonth(monthStartDay(monthCount))

	mat placementCount(monthCount)
	mat placementAmounts(monthCount)
	mat suitFiled(monthCount)
	mat paymentScheduled(monthCount)
	mat debtorMailReturn(monthCount)
	mat debtorBankruptyFiled(monthCount)
	mat debtorHasAssets(monthCount)
	mat JmtButNoAssets(monthCount)
! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table align="Center">'
	pr #255: '<tr>'
	pr #255: '<td align="Center"><h2>'&env$('program_caption')&'</h2></td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td align="Center">For '&str$(monthCount)&' month period '&date$(startingDay,'mm/dd/ccyy')&' - '&date$(startingDay,'mm/dd/ccyy')&'.</td>'
	pr #255: '</tr>'
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
	mat placementCount=(0)
	mat placementAmounts=(0)
	mat suitFiled=(0)
	mat paymentScheduled=(0)
	mat debtorMailReturn=(0)
	mat debtorBankruptyFiled=(0)
	mat debtorHasAssets=(0)
	mat JmtButNoAssets=(0)
	
	do
		read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
		dim fileno$*8
		fileno$=masterData$(master_fileno)
		if udim(mat forwarderFilter)<=0 or srch(mat forwarderFilter,masterDataN(master_forw_no))>0 then
			month=fn_whichMonth(days(masterData$(master_date_recd),'ccyymmdd'))
			if month>0 then
				placementCount(month)   +=1
				placementAmounts(month) +=masterDataN(master_ORIG_CLAIM)
				if trim$(masterData$(master_suit_date))<>'' then
					suitFiled(month)+=1
				else if trim$(masterData$((master_lpaymnt_date)))<>'' then
					paymentScheduled(month)+=1
				end if
				dim allDebtor$(0,0)*256,allDebtorN(0,0)
				debtorCount=fnAllDebtors(fileno$,hDebtor,Mat allDebtor$,Mat allDebtorN)
				! if trim$(fileno$)='14893630' then pr 'found it.  this person should have a bankruptcy.' : pause
				! if trim$(fileno$)='14901075' then pr 'found it.  this person should have a D1 Employer.' : pause
				for dno=1 to debtorCount
					! allDebtor$(dno,debtor_*[enum])
					! if trim$(allDebtor$(dno,debtor_ret_mail))<>'' then 
					! 	pr 'allDebtor$(dno,debtor_ret_mail)='&allDebtor$(dno,debtor_ret_mail)
					! 	pause
					! end if
					if allDebtor$(dno,debtor_ret_mail)='Y' then
						debtorMailReturn(month)+=1
					end if
					if allDebtorN(dno,debtor_BKCY_FILED_DATE)>0 then
						debtorBankruptyFiled(month)+=1
						! pause
					end if
					
					! r: debtorHasAssets(month)    does not work yet
							! E or B on debtor screen than they do have assets
							! 		E - Employer has been related to this debtor.
							! 		B - Bank has been related to this debtor.
							
							! recordNumber=1 ! 0 ! i don't *THINK* we need this...   maybe we do
							! Fninternal_Data$(fileno$,dno,recordNumber,relation_bankfile,Sql_Field_Name$)
							! bankExist=Fninternal_Data(fileno$,dno,recordNumber,relation_bankfile,Mat bankfile_data$,Mat bankfile_data)
							bankExist=fn_debtorHasInternalLinked(fileno$,dno,relation_bankfile)
							! employerExist=Fninternal_Data(fileno$,dno,recordNumber,relation_employer,Mat employer_data$,Mat employer_data)
							! if trim$(fileno$)='14901075' and dno=1 then 
							! 	debug=1
							! 	pr 'found it.  this person should have a D1 Employer.'
							! 	pr 'fileno$=';fileno$
							! 	pr 'employerExist=';employerExist
							! 	pr 'dno=';dno
							! 	pause
							! else
							! 	debug=0
							! end if
							employerExist=fn_debtorHasInternalLinked(fileno$,dno,relation_employer)
							if bankExist or employerExist then
								debtorHasAssets(month)+=1
								if masterData$(master_jmt_date)<>'' then
									JmtButNoAssets(month)+=1
								end if
							end if
					! /r
				nex dno
				! pause
			end if
		end if
	loop
	EoMaster: !
	! /r
	! r: Print the Accumulated Data in Excel

	pr #255: '<table cellspacing=10>'
	gosub PrHeader
	For month=1 to monthCount
		pr #255: '<tr> ';
		pr #255: '<td>'&date$(monthStartDay(month),'MM/CCYY')&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',placementCount(month))&'</td>';
		pr #255: '<td>'&cnvrt$('pic(z,zzz,zzz,zz#.##)',placementAmounts(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',suitFiled(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',paymentScheduled(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',debtorMailReturn(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',debtorBankruptyFiled(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',debtorHasAssets(month))&'</td>';
		pr #255: '<td align="right">'&cnvrt$('pic(z,zzz,zzz,zz#)',JmtButNoAssets(month))&'</td>';
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
		pr #255: '<th> Placement<br>Count </th>'
		pr #255: '<th> Placement<br>Amount </th>'
		pr #255: '<th> Suits<br>Filed </th>'
		pr #255: '<th> Non-Sued<br>Payments<br>Scheduled </th>'
		pr #255: '<th> Debtor<br>Mail Returned</th>'
		pr #255: '<th> Debtor<br>Bankrupty Filed</th>'
		pr #255: '<th> Debtor<br>Has Assets </th>'
		pr #255: '<th> Judgment But<br>No Assets </th>'
		
		
		
		pr #255: '</tr>'
return ! /r
def fn_debtorHasInternalLinked(fileno$,dno,relation; ___,returnN)
	if ~dheSetup then
		dheSetup=1
		!  LET Open_Files$(51)="NAME=INTERNAL//6,KFNAME=INTERNAL.REL//6,SHR"
		! IDX = By File # Debt #, ect. 
		! REL = By Internal File Relation-Type then #
		! The Internal File # is a "Regular" Index.
		open #h:=fngethandle: "name=INTERNAL//6,kfname=INTERNAL.IDX//6,shr",internal,input,keyed
	end if
	dim internalKey$*128
	internalKey$=rpad$(fileno$&cnvrt$('n 3',dno)&cnvrt$('n 3',relation),kln(h))
	restore #h,key=>internalKey$: nokey DheNoEmp
	dim iform$*256
	iform$='form pos 1,C 8,n 3,n 2,c 10,BH 3,c 30'
	dim acct_no$*30
	read #h,using iform$: fileno2$,dno2,rel2,type$,no,acct_no$
	pr fileno2$;' ';dno2;' ';rel2type$;' ';no;' ';acct_no$
	if fileno$=fileno2$ and dno=dno2 and rel2=relation then
		returnN=1
	end if
	goto DheFinis
	DheNoEmp: !
		returnN=0
	goto DheFinis
	DheFinis: !
	! if debug then pr 'returnN=';returnN : pause
	fn_debtorHasInternalLinked=returnN
				
fnend
def fn_whichMonth(aDay; ___,returnN)
	! utilizies local: mat monthStartDay,endingDay
	if aDay<monthStartDay(1) or aDay>endingDay then ! outside month range
		returnN=0
	else if aDay=>monthStartDay(monthCount) and aDay<=endingDay then ! last month
		returnN=monthCount
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
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		! library 'prog2\intermnt.wb': fninternal_data

		library 'S:\Core\Library.br': fnEndOfMonth
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fnTop

		library "Library\CLSUtil.wb": fnget_formall$
		library "library\CLSUtil.wb": fnunpack$
		library "Library\CLSUtil.wb": fnget_form
		library "Library\CLSUtil.wb": fnget_formarr
		
		gosub Enum
		gosub SetupPrint

		dim employer_data$(1)*60,employer_data(1),employer_fieldsc$(1)*20,employer_fieldsn$(1)*20,employer_formc$*2048,employer_formn$*2048
		dim employer_des_c$(1)*80,employer_des_n$(1)*80,employer_seq$(1)*80,employer_valid$(1)*80,employer_fc$(1,3)*80,employer_fn$(1,3)*80
		fnget_form("EMPLOYER",mat employer_data$,mat employer_data,mat employer_fieldsc$,mat employer_fieldsn$,employer_formc$,employer_formn$)
		fnget_formarr("EMPLOYER",mat employer_data$,mat employer_data,mat employer_fieldsc$,mat employer_fieldsn$,mat employer_fc$,mat employer_fn$,mat employer_des_c$,mat employer_des_n$,mat employer_seq$,mat employer_valid$)

		dim bankfile_data$(1)*60,bankfile_data(1),bankfile_fieldsc$(1)*20,bankfile_fieldsn$(1)*20,bankfile_formc$*2048,bankfile_formn$*2048
		dim bankfile_des_c$(1)*80,bankfile_des_n$(1)*80,bankfile_seq$(1)*80,bankfile_valid$(1)*80,bankfile_fc$(1,3)*80,bankfile_fn$(1,3)*80
		fnget_form("bankfile",mat bankfile_data$,mat bankfile_data,mat bankfile_fieldsc$,mat bankfile_fieldsn$,bankfile_formc$,bankfile_formn$)
		fnget_formarr("bankfile",mat bankfile_data$,mat bankfile_data,mat bankfile_fieldsc$,mat bankfile_fieldsn$,mat bankfile_fc$,mat bankfile_fn$,mat bankfile_des_c$,mat bankfile_des_n$,mat bankfile_seq$,mat bankfile_valid$)


	end if
fnend

include: cm\enum\common
include: cm\err
include: cm\print
include: cm\enum\master
include: cm\enum\debtor
