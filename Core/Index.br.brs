fn_setup
fn_index('[Q]\UBmstr\UBTransVB.h[cno]', '[Q]\UBmstr\UBTrdt.h[cno]','11/1 8/10')
stop
! r: reindex the *new* add company files
dim syslist$(4)*2
syslist$(1)='GL'
syslist$(2)='UB'
syslist$(3)='PR'
syslist$(4)='CL'
for sysitem=1 to udim(mat syslist$)
	execute 'config SUBSTITUTE "[Q]\'&syslist$(sysitem)&'mstr\" "S:\acs'&syslist$(sysitem)&'\mstr\"'
	fn_index_sys_do_one(99999,'GL')
	execute 'config SUBSTITUTE "[Q]\'&syslist$(sysitem)&'mstr\" "S:\acs'&syslist$(sysitem)&'\mstr\" Clear' ! this clear does not seem to work - just exit BR after running
next sysitem
end
! /r
def fn_setup
	autoLibrary
	if ~setup_index_it then
		setup_index_it=1
		on error goto Ertn
		option retain
	end if
fnend
def library fnIndex(data_file$*256,index_statement_or_file$*512; indexParameters$*256)
	fn_setup
	fnIndex=fn_index(data_file$,index_statement_or_file$, indexParameters$)
fnend
def fn_index(data_file$*256,index_statement_or_file$*512; indexParameters$*256,___,isIndexStatement,fail)
	fn_setup
	data_file$=trim$(data_file$)
	if indexParameters$='' then isIndexStatement=1
	dim dataFile$*256
	dataFile$=data_file$
	data_file$=fnSrepEnv$(data_file$,'[Q]')
	! if dataFile$<>data_file$ then
	! 	pr data_file$
	! 	pr dataFile$
	! 	pause
	! end if
	if exists(data_file$) then
		if isIndexStatement then
			fnStatus(index_statement_or_file$)
			dim indexStatement$*512
			indexStatement$=index_statement_or_file$
			execute indexStatement$ ioerr EXE_INDEX_ERR

			if env$('acsDeveloper')<>'' then
				pr 'developer only pause - you are in fnIndex and youre coming from an old call - please consider fixing it now'
				pause
			end if

		else
			! index_statement_or_file$=fnSrepEnv$(index_statement_or_file$,,'[Q]')
			fnStatus(os_filename$(index_statement_or_file$))
			indexParameters$=lwrc$(indexParameters$)
			indexParameters$=' '&indexParameters$&' '
			indexParameters$=srep$(indexParameters$,',',' ')
			indexParameters$=srep$(indexParameters$,' replace',' ')
			indexParameters$=srep$(indexParameters$,' dupkeys',' ')
			indexParameters$=srep$(indexParameters$,' -n',' ')
			indexParameters$=trim$(indexParameters$)&' Replace DupKeys Shr' ! -N
		!	! r: old way
			if pos(data_file$,' ')>0 then data_file$=fnshortpath$(data_file$)
			if pos(index_statement_or_file$,' ')>0 then index_statement_or_file$=fnshortpath$(index_statement_or_file$)
		!
		!		!       pr 'index '&(data_file$)&' '&(index_statement_or_file$)&' '&indexParameters$ : pause
			indexStatement$='index '&(data_file$)&' '&(index_statement_or_file$)&' '&indexParameters$
		!	! /r old way
		!!! does not work	! r: new high tech major con sub method of dealing with long file names
		!!! does not work		fn_conSub('[dataFile]',data_file$)
		!!! does not work		fn_conSub('[indexFile]',index_statement_or_file$)
		!!! does not work		indexStatement$='index [dataFile] [indexFile] '&indexParameters$
		!!! does not work	! /r

			!       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(1:2)
			!       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(3:len(env$('temp')))
			fnStatus(indexStatement$) ! pr 'indexStatement$='&indexStatement$&' sreped='&fnsrepenv$(indexStatement$,'[Q]') : pause
			execute indexStatement$ ioerr EXE_INDEX_ERR
			!       if env$('ACSDeveloper')='' then execute 'CD S:'
		end if
	else
		fail=1
		fnStatus('Could not find data file:')
		fnStatus('     '&os_filename$(data_file$))
	end if
	goto INDEX_XIT
	EXE_INDEX_ERR: !
	fail=1
	fnStatus('Encountered error '&str$(err)&' executing index:')
	fnStatus('     ('&indexStatement$&')') ! pause
	if err=7600 then
		open #h_tmp=fnH: 'name='&data_file$,internal,input error EIE_7600_XIT
		fnStatus('     (Record Length is '&str$(rln(h_tmp))&')')
		close #h_tmp:
		EIE_7600_XIT: !
	else if err=7605 then
		if env$('acsDeveloper')<>'' then pause
	end if
	INDEX_XIT: !
	if fail then
		! if env$('acsDeveloper')<>'' then pr 'dev: pause on index_xit fail' : pause
		!     fnStatusPause
		index_it_return=0
	else
		index_it_return=1
	end if
	fn_index=index_it_return
fnend
def library fnindex_sys(; only_cno,system_id$*128)
	fn_setup
	fnindex_sys=fn_index_sys( only_cno,system_id$)
fnend
def fn_index_sys(; only_cno,system_id$*128)
	! only_cno=0 means index all company numbers, otherwise index only the company number passed
	! system_id$ of blank means to index the currenet system - otherwise index the system specified.
	if system_id$='' then system_id$=env$('CurSys')
	if only_cno then
		fn_index_sys_do_one(only_cno,system_id$)
	else
		if fnget_company_number_list(mat cno_list, system_id$) then
			for cno_item=1 to udim(mat cno_list)
				fn_index_sys_do_one(cno_list(cno_item),system_id$)
			next cno_item
		else
			fnStatus('no companies found in '&system_id$&' to index')
		end if
	end if
fnend
def fn_index_sys_do_one(cno,system_id$*128)
	if      	system_id$='GL' then ! r:
		fn_index('[Q]\GLmstr\ACGLSCHS.h'&str$(cno),'[Q]\GLmstr\schindex.h'&str$(cno),'1 3')
		fn_index('[Q]\GLmstr\ACGLSCHS.h'&str$(cno),'[Q]\GLmstr\SchIndX2.h'&str$(cno),'3 30')

		fn_index('[Q]\GLmstr\ACTrans.h'&str$(cno),'[Q]\GLmstr\AcTrIdx.h'&str$(cno),'1/71/17/13 12/2/2/4')

		fn_index('[Q]\GLmstr\ACGLfNSf.h'&str$(cno),'[Q]\GLmstr\agfsidx5.h'&str$(cno),',1 5')
		fn_index('[Q]\GLmstr\ACGLfNSg.h'&str$(cno),'[Q]\GLmstr\agfsidx6.h'&str$(cno),',1 5')
		fn_index('[Q]\GLmstr\ACGLFNSi.h'&str$(cno),'[Q]\GLmstr\agfsidx3.h'&str$(cno),',1 5')
		fn_index('[Q]\GLmstr\ACGLFNSj.h'&str$(cno),'[Q]\GLmstr\agfsidx2.h'&str$(cno),',1 5')

		fn_index('[Q]\GLmstr\bankrec.H'&str$(cno),'[Q]\GLmstr\bankrec-idx.h'&str$(cno) ,'79/3/4 12/1/8')

		fn_index('[Q]\GLmstr\BudgetInfo.h'&str$(cno),'[Q]\GLmstr\BudIndx.h'&str$(cno),'1,14')
		fn_index('[Q]\GLmstr\BudInfo.h'&str$(cno),'[Q]\GLmstr\BudInfo_Index.h'&str$(cno),'1,2')

		fn_index('[Q]\GLmstr\GLmstr.h'&str$(cno),'[Q]\GLmstr\GLIndex.h'&str$(cno),'1 12')
		fn_index('[Q]\GLmstr\GLmstr.h'&str$(cno),'[Q]\GLmstr\glIndx2.h'&str$(cno),'13 30')
	! fn_index('[Q]\GLmstr\GLmstr.h'&str$(cno),'[Q]\GLmstr\fsindex.H'&str$(cno),'63 3') ! Secondary
	! fn_index('[Q]\GLmstr\GLmstr.h'&str$(cno),'[Q]\GLmstr\fsindex.H'&str$(cno),'66 3') ! Primary
		fn_index('[Q]\GLmstr\gl1099.h'&str$(cno),'[Q]\GLmstr\gl109Idx.h'&str$(cno),'1 8')
		fn_index('[Q]\GLmstr\gltr1099.H'&str$(cno),'[Q]\GLmstr\gltridx1.H'&str$(cno),'1 8')
		fn_index('[Q]\GLmstr\GLBRec.h'&str$(cno),'[Q]\GLmstr\GLRecIdx.h'&str$(cno),'1 24')
		fn_index('[Q]\GLmstr\glstdad.H'&str$(cno),'[Q]\GLmstr\glstdidx.h'&str$(cno),'1 12')
		fn_index('[Q]\GLmstr\GLTrans.h'&str$(cno),'[Q]\GLmstr\glTrans-IdxAcct.h'&str$(cno),'1 12')

		fn_index('[Q]\GLmstr\payeeglbreakdown.H'&str$(cno),'[Q]\GLmstr\payeeglbkdidx.H'&str$(cno),'1 8')
		fn_index('[Q]\GLmstr\PayMstr.h'&str$(cno),'[Q]\GLmstr\Payidx1.H'&str$(cno),'1 8')
		fn_index('[Q]\GLmstr\PayMstr.h'&str$(cno),'[Q]\GLmstr\Payidx2.H'&str$(cno),'9 38')
		fn_index('[Q]\GLmstr\PRmstr.h'&str$(cno),'[Q]\GLmstr\PRIndex.h'&str$(cno),'1 4')
		fn_index('[Q]\GLmstr\PayeeGLBreakdown.h'&str$(cno),'[Q]\GLmstr\payeeglbkdidx.h'&str$(cno),'1 8')

		fn_index('[Q]\GLmstr\RatioMST.h'&str$(cno),'[Q]\GLmstr\SchIndx2.h'&str$(cno),'3 30')
		fn_index('[Q]\GLmstr\RatioMST.h'&str$(cno),'[Q]\GLmstr\RatioIdx.h'&str$(cno),'1 3')
		fn_index('[Q]\GLmstr\RatioMST.h'&str$(cno),'[Q]\GLmstr\RaNamIdx.h'&str$(cno),'4 28')

		sn=1
		fn_index('[Q]\GLmstr\schedule'&str$(sn)&'.H'&str$(cno),'[Q]\GLmstr\schedule'&str$(sn)&'-idx.h'&str$(cno),'1 12')
		for sn=1 to 8
			fn_index('[Q]\GLmstr\schedule'&str$(sn)&'.H'&str$(cno),'[Q]\GLmstr\schedule_idx'&str$(sn)&'.h'&str$(cno),'1 12')
		next sn
		fn_index('[Q]\GLmstr\TransCodes.h'&str$(cno),'[Q]\GLmstr\transcodes-idx.h'&str$(cno),'1 2')
		fn_index('[Q]\GLmstr\W2Box16.h'&str$(cno),'[Q]\GLmstr\W2Index.h'&str$(cno),'1 8')

		fn_index('[Q]\GLmstr\ACGLFNSB.h'&str$(cno),'[Q]\GLmstr\agfsidx4.h'&str$(cno),',1 5')
		fn_index('[Q]\GLmstr\ACGLFNSc.h'&str$(cno),'[Q]\GLmstr\agfsidx1.h'&str$(cno),',1 5')

		fn_index('[Q]\GLmstr\Year.h'&str$(cno),'[Q]\GLmstr\Year-Idx.h'&str$(cno),',1 1')

		exe 'con sub [FinancialStatementCode] C' ! secondary
		fnReIndex('GL FSDesign')

		exe 'con sub [FinancialStatementCode] B' ! primary
		fnReIndex('GL FSDesign')

		fnReassignTransactionAddresses(cno)
		! pr 'did it' : pause

	! /r
	else if 	system_id$='UB' then ! r:
		fn_index('[Q]\UBmstr\Reads_and_Chgs.h'&str$(cno), '[Q]\UBmstr\Reads_and_Chgs-Key.h'&str$(cno),'1 10')
		fn_ub_index_customer(cno)
		fn_index('[Q]\UBmstr\UBAdrBil.h'&str$(cno), '[Q]\UBmstr\adrIndex.h'&str$(cno),'1 10')
		fn_index('[Q]\UBmstr\UBTransVB.h'&str$(cno), '[Q]\UBmstr\UBTrIndx.h'&str$(cno),'1 19')
		fn_index('[Q]\UBmstr\UBTransVB.h'&str$(cno), '[Q]\UBmstr\UBTrdt.h'&str$(cno),'11/1 8/10')
		! fn_index('[Q]\UBmstr\Note1.h'&str$(cno), '[Q]\UBmstr\NoteIdx1.h'&str$(cno),'1 10')
		! fn_index('[Q]\UBmstr\Deposit1.h'&str$(cno), '[Q]\UBmstr\DepIdx1.h'&str$(cno),'1 10')
		fn_index('[Q]\UBmstr\Meter.h'&str$(cno), '[Q]\UBmstr\Meter_Idx.h'&str$(cno),'1/11 10/2')
		fn_index('[Q]\UBmstr\MeterType.h'&str$(cno), '[Q]\UBmstr\MeterTypeIdx.h'&str$(cno),'1 5u')
		fn_index('[Q]\UBmstr\ubData\RateMst.h'&str$(cno), '[Q]\UBmstr\ubData\RateIdx1.h'&str$(cno),'1 4')
		fn_index('[Q]\UBmstr\ubData\RateMst.h'&str$(cno), '[Q]\UBmstr\ubData\RateIdx2.h'&str$(cno),'5 25')
		fn_index('[Q]\UBmstr\Cass1.h'&str$(cno), '[Q]\UBmstr\Cass1Idx.h'&str$(cno),'1 10')
		fn_index('[Q]\UBmstr\workorder.h'&str$(cno), '[Q]\UBmstr\wkindex.h'&str$(cno),'1/11 10/8')
	! /r
	else if 	system_id$='PR' then ! r:
		fn_index('[Q]\PRmstr\EmpStatus.dat','[Q]\PRmstr\EmpStatus.Idx','1 2')
		fn_index('[Q]\PRmstr\MGLMstr.h'&str$(cno),'[Q]\PRmstr\MGLIdx1.h'&str$(cno),'1 3')
		fn_index('[Q]\PRmstr\PRCkHist.h'&str$(cno),'[Q]\PRmstr\PRCKINDX.h'&str$(cno),'1 14')
		fn_index('[Q]\PRmstr\PRReport.h'&str$(cno),'[Q]\PRmstr\PRRptIdx.h'&str$(cno),'1 2')
		fn_index('[Q]\PRmstr\Employee.h'&str$(cno),'[Q]\PRmstr\EmployeeIdx-no.h'&str$(cno),'1 8')
		fn_index('[Q]\PRmstr\Employee.h'&str$(cno),'[Q]\PRmstr\EmployeeIdx-name.h'&str$(cno),'9 30')
		fn_index('[Q]\PRmstr\dd.h'&str$(cno),'[Q]\PRmstr\DDidx1.h'&str$(cno),'1 10')
		fn_index('[Q]\PRmstr\glmstr.h'&str$(cno),'[Q]\PRmstr\glIndex.h'&str$(cno),'1 12')
		fn_index('[Q]\PRmstr\SCMSTR.h'&str$(cno), '[Q]\PRmstr\SCIndex.h'&str$(cno),'1 3')
		fn_index('[Q]\PRmstr\W2Box16.h'&str$(cno), '[Q]\PRmstr\W2Index.h'&str$(cno),'1 8')
		fn_index('[Q]\PRmstr\Burden.H'&str$(cno), '[Q]\PRmstr\BurdenIdx.H'&str$(cno),'1 8')
		fn_index('[Q]\PRmstr\Category.H'&str$(cno), '[Q]\PRmstr\categoryIDX.H'&str$(cno),'1 5')
		fn_index('[Q]\PRmstr\Department.h'&str$(cno), '[Q]\PRmstr\Deptid4.h'&str$(cno),'12/1/9 12/8/3')
		fn_index('[Q]\PRmstr\Department.h'&str$(cno), '[Q]\PRmstr\DeptIdx4.h'&str$(cno),'50/9/1 2/3/8')
		fn_index('[Q]\PRmstr\Department.h'&str$(cno), '[Q]\PRmstr\DeptIdx.h'&str$(cno),'1/9 8/3')
		fn_index('[Q]\PRmstr\HourBreakdown.H'&str$(cno), '[Q]\PRmstr\HourBreakdown-idx.H'&str$(cno),'1/9/14 8/5/8')
		fn_index('[Q]\PRmstr\PayrollChecks.h'&str$(cno), '[Q]\PRmstr\checkidx.h'&str$(cno),'1 17')
		fn_index('[Q]\PRmstr\PayrollChecks.h'&str$(cno), '[Q]\PRmstr\CheckIdx2.h'&str$(cno),'9/12/1 3/6/8')
		fn_index('[Q]\PRmstr\PayrollChecks.h'&str$(cno), '[Q]\PRmstr\checkidx3.h'&str$(cno),'1/12/9 8/6/3')
		fn_index('[Q]\PRmstr\payrollreports.H'&str$(cno), '[Q]\PRmstr\prrptidx.h'&str$(cno),'1 30')
		fn_index('[Q]\PRmstr\payrollreports.H'&str$(cno), '[Q]\PRmstr\reportidx.H'&str$(cno),'1 30')
		fn_index('[Q]\PRmstr\PRCkHist.h'&str$(cno), '[Q]\PRmstr\PRCKINDX.h'&str$(cno),'1 14')
		fn_index('[Q]\PRmstr\PRReport.h'&str$(cno), '[Q]\PRmstr\prrptidx.h'&str$(cno),'1 2')
		fn_index('[Q]\PRmstr\prTot.h'&str$(cno), '[Q]\PRmstr\PRTotIdx.h'&str$(cno),'1 9')

		! fn_index('[Q]\PRmstr\rpwork[unique_computer_id].h'&str$(cno), '[Q]\PRmstr\rpwork[unique_computer_id]Idx.h'&str$(cno),'1 11')
		! fn_index('[Q]\PRmstr\rpwork[unique_computer_id].h'&str$(cno), '[Q]\PRmstr\rpwork[unique_computer_id]Idx2.h'&str$(cno),'1/27 8/14')
		dim filename$(0)*256
		dim kfname$(2)*256
		fnGetDir2('[Q]\PRmstr\',mat filename$, '','rpwork*.h'&str$(cno))
		for fileItem=1 to udim(mat filename$)
			if pos(lwrc$(filename$(fileItem)),'idx.')<=0 and pos(lwrc$(filename$(fileItem)),'idx2.')<=0 then
				kfname$(1)=srep$(filename$(fileItem),'.','Idx.')
				kfname$(2)=srep$(filename$(fileItem),'.','Idx2.')
				fn_index('[Q]\PRmstr\'&filename$(fileItem), '[Q]\PRmstr\'&kfname$(1),'1 11')
				fn_index('[Q]\PRmstr\'&filename$(fileItem), '[Q]\PRmstr\'&kfname$(2),'1/27 8/14')
			end if
		nex fileItem

		fn_index('[Q]\PRmstr\DeptName.h'&str$(cno),'[Q]\PRmstr\DepNameIdx.h'&str$(cno),'1 3')
	! /r
	else if 	system_id$='CL' then ! r:
		fn_index('[Q]\CLmstr\BankMstr.h'&str$(cno), '[Q]\CLmstr\BankIdx1.h'&str$(cno),'1 2')
		fn_index('[Q]\CLmstr\DPTMSTR.h'&str$(cno), '[Q]\CLmstr\DPTIDX1.h'&str$(cno),'1 5')
		fn_index('[Q]\CLmstr\GLmstr.H'&str$(cno), '[Q]\CLmstr\GLINDEX.H'&str$(cno),'1 12')
		fn_index('[Q]\CLmstr\IvPaid.h'&str$(cno), '[Q]\CLmstr\IVIndex.h'&str$(cno),' 1 20')
		fn_index('[Q]\CLmstr\JCBreakdownS[wsid].h'&str$(cno), '[Q]\CLmstr\jcbrkidx[wsid].H'&str$(cno),'48 20')
		fn_index('[Q]\CLmstr\payeeglbreakdown.H'&str$(cno), '[Q]\CLmstr\Payeeglbkdidx.H'&str$(cno),'1 8')
		fn_index('[Q]\CLmstr\PayMstr.h'&str$(cno), '[Q]\CLmstr\payidx1.H'&str$(cno),'1 8')
		fn_index('[Q]\CLmstr\PayTrans.h'&str$(cno), '[Q]\CLmstr\Unpdidx2.H'&str$(cno),'31/27/1 2/4/26') ! index in year,monthday,reference
		fn_index('[Q]\CLmstr\PayTrans.h'&str$(cno), '[Q]\CLmstr\UNPdIdx1.h'&str$(cno),'1 20')
		fn_index('[Q]\CLmstr\Receiptglbreakdown.h'&str$(cno), '[Q]\CLmstr\receiptglbkdidx.h'&str$(cno),'1 8')
		fn_index('[Q]\CLmstr\Recmstr.h'&str$(cno), '[Q]\CLmstr\Recidx1.h'&str$(cno),' 1 8')
		fn_index('[Q]\CLmstr\Tralloc.h'&str$(cno), '[Q]\CLmstr\Tralloc-idx.h'&str$(cno),' 1 11')
		fn_index('[Q]\CLmstr\TrMstr.h'&str$(cno), '[Q]\CLmstr\TrIdx1.h'&str$(cno),' 1 11')
		fn_index('[Q]\CLmstr\TrMstr.H'&str$(cno), '[Q]\CLmstr\TrIdx2.H'&str$(cno),' 28/1 8/11')
		fn_index('[Q]\CLmstr\TrMstr.H'&str$(cno), '[Q]\CLmstr\Tridx3.H'&str$(cno),' 16/12/4 2/4/8') ! index in year,monthday,reference
		fn_index('[Q]\CLmstr\unpdaloc.H'&str$(cno), '[Q]\CLmstr\Uaidx1.H'&str$(cno),'9,12')
		fn_index('[Q]\CLmstr\unpdaloc.H'&str$(cno), '[Q]\CLmstr\Uaidx2.H'&str$(cno),'1,20')
		! /r
	else if lwrc$(system_id$)=lwrc$('Client Billing') then ! r:
		fnIndex('S:\Core\Data\acsllc\support.h'&str$(cno),'S:\Core\Data\acsllc\support-idx.h'&str$(cno),'1/7,6/2')
		fnIndex('S:\Core\Data\acsllc\EMmstr.h'&str$(cno),'S:\Core\Data\acsllc\EMIndex.h'&str$(cno),'1,9')
		fnIndex('S:\Core\Data\acsllc\IVDesc.h'&str$(cno),'S:\Core\Data\acsllc\IVDIndex.h'&str$(cno),'1,6')
		fnIndex('S:\Core\Data\acsllc\SCMSTR.h'&str$(cno),'S:\Core\Data\acsllc\SCIndex.h'&str$(cno),'1 4')
		! fnIndex('[Temp]\TmSht[session]','[Temp]\TmSht-idx[session]','1,5')
		fnReassignNTA('S:\Core\Data\acsllc\Transactions.h'&str$(cno),'Form pos 1,C 5','Form Pos 58,PD 3')
		! /r
	end if
fnend
def library fnub_index_customer(; cno)
	fn_setup
	if cno=0 then cno=val(env$('cno'))
	fnub_index_customer=fn_ub_index_customer(cno)
fnend
def fn_ub_index_customer(cno)
	fn_index('[Q]\UBmstr\Customer.h'&str$(cno), '[Q]\UBmstr\ubIndex.h'&str$(cno),'1 10')
	fn_index('[Q]\UBmstr\Customer.h'&str$(cno), '[Q]\UBmstr\ubIndx2.h'&str$(cno),'354 7')
	fn_index('[Q]\UBmstr\Customer.h'&str$(cno), '[Q]\UBmstr\ubIndx3.h'&str$(cno),'11 30u')
	fn_index('[Q]\UBmstr\Customer.h'&str$(cno), '[Q]\UBmstr\ubIndx4.h'&str$(cno),'41 30')
	fn_index('[Q]\UBmstr\Customer.h'&str$(cno), '[Q]\UBmstr\ubIndx5.h'&str$(cno),'1741/1743 2/7')
fnend
Xit: fnXit
include: ertn
