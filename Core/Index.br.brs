fn_index_it_setup
fn_index_it("[Q]\UBmstr\UBTransVB.h"&env$('cno'), "[Q]\UBmstr\UBTrdt.h"&env$('cno'),"11/1 8/10")
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
def fn_index_it_setup
	library 'S:\Core\Library': fnxit,fnerror,fnStatus,fnget_company_number_list,fngethandle,fnshortpath$
	if ~setup_index_it then 
		setup_index_it=1
		on error goto ERTN
		option retain 
		!     working_dir_rights=fnrights_test('',"Try Run As Administrator.",'Program','Indexes are unable to process without this access and will be skipped for the remainder of this session.')
	end if 
fnend 
def library fnindex_it(data_file$*256,index_statement_or_file$*512; index_parameters$*256)
	fn_index_it_setup
	fnindex_it=fn_index_it(data_file$,index_statement_or_file$, index_parameters$)
fnend 
def fn_index_it(data_file$*256,index_statement_or_file$*512; index_parameters$*256)
	! r: constants, dims, library, on error, etc
	fn_index_it_setup
	dim cap$*128
	dim index_execute_text$*512
	data_file$=trim$(data_file$)
	cap$='fnindex_it for '&data_file$(1:128) ! data_file$(len(data_file$)-80:len(data_file$)) ! cap$ is just for the error routine anyway
	is_index_statement=1
	is_index_file=2
	!   /r
	fail=0
	if index_parameters$='' then index_statement_or_file=is_index_statement else index_statement_or_file=is_index_file
	if exists(data_file$) then 
		if index_statement_or_file=is_index_statement then 
			fnStatus(index_statement_or_file$)
			index_execute_text$=index_statement_or_file$
			execute index_execute_text$ ioerr EXE_INDEX_ERR
		else ! index_statement_or_file=is_index_file
			fnStatus(os_filename$(index_statement_or_file$))
			index_parameters$=lwrc$(index_parameters$)
			index_parameters$=' '&index_parameters$&' '
			index_parameters$=srep$(index_parameters$,',',' ')
			index_parameters$=srep$(index_parameters$,' replace',' ')
			index_parameters$=srep$(index_parameters$,' dupkeys',' ')
			index_parameters$=srep$(index_parameters$,' -n',' ')
			index_parameters$=trim$(index_parameters$)&' Replace DupKeys Shr' ! -N
			if pos(data_file$,' ')>0 then data_file$=fnshortpath$(data_file$)
			if pos(index_statement_or_file$,' ')>0 then index_statement_or_file$=fnshortpath$(index_statement_or_file$)
			! 
			!       pr 'index '&(data_file$)&' '&(index_statement_or_file$)&' '&index_parameters$ : pause
			index_execute_text$='index '&(data_file$)&' '&(index_statement_or_file$)&' '&index_parameters$
			!       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(1:2)
			!       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(3:len(env$('temp')))
			execute index_execute_text$ ioerr EXE_INDEX_ERR
			!       if env$('ACSDeveloper')='' then execute 'CD S:'
		end if 
	else 
		fail=1
		fnStatus("Could not find data file:")
		fnStatus("     "&os_filename$(data_file$))
	end if 
	goto INDEX_XIT
	EXE_INDEX_ERR: ! 
	fail=1
	fnStatus("Encountered error "&str$(err)&" executing index:")
	fnStatus("     ("&index_execute_text$&")") ! pause
	if err=7600 then 
		open #h_tmp:=fngethandle: 'name='&data_file$,internal,input error EIE_7600_XIT
		fnStatus('     (Record Length is '&str$(rln(h_tmp))&')')
		close #h_tmp: 
		EIE_7600_XIT: ! 
	else if err=7605 then 
		if env$('acsDeveloper')<>'' then pause
	end if 
	INDEX_XIT: ! 
	if fail then 
		!     fnStatusPause
		index_it_return=0
	else 
		index_it_return=1
	end if 
	fn_index_it=index_it_return
fnend
def library fnindex_sys(; only_cno,system_id$*2)
	fn_index_it_setup
	fnindex_sys=fn_index_sys( only_cno,system_id$)
fnend 
def fn_index_sys(; only_cno,system_id$*2)
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
def fn_index_sys_do_one(cno,system_id$*2)
	if system_id$='GL' then ! r:
	! r: A
		fn_index_it("[Q]\GLmstr\ACGLSCHS.h"&str$(cno),"[Q]\GLmstr\schindex.h"&str$(cno),"1 3")
		fn_index_it("[Q]\GLmstr\ACGLSCHS.h"&str$(cno),"[Q]\GLmstr\SchIndX2.h"&str$(cno),"3 30")

		fn_index_it("[Q]\GLmstr\ACTrans.h"&str$(cno),"[Q]\GLmstr\AcTrIdx.h"&str$(cno),"1/71/17/13 12/2/2/4")
		fn_index_it("[Q]\GLmstr\AcTrans.h"&str$(cno),"[Q]\GLmstr\tmp70.h"&str$(cno),"1 70")

		fn_index_it("[Q]\GLmstr\ACGLFNSB.h"&str$(cno),"[Q]\GLmstr\FNSbINDX.h"&str$(cno),",1 5")
		fn_index_it("[Q]\GLmstr\ACGLFNSc.h"&str$(cno),"[Q]\GLmstr\FNScINDX.h"&str$(cno),",1 5")
		fn_index_it("[Q]\GLmstr\ACGLfNSf.h"&str$(cno),"[Q]\GLmstr\FNSfINDX.h"&str$(cno),",1 5")
		fn_index_it("[Q]\GLmstr\ACGLfNSg.h"&str$(cno),"[Q]\GLmstr\FNSGINDX.h"&str$(cno),",1 5")
		fn_index_it("[Q]\GLmstr\ACGLFNSi.h"&str$(cno),"[Q]\GLmstr\FNSiINDX.h"&str$(cno),",1 5")
		fn_index_it("[Q]\GLmstr\ACGLFNSj.h"&str$(cno),"[Q]\GLmstr\FNSjINDX.h"&str$(cno),",1 5")
	! /r
	! r: B
		fn_index_it("[Q]\GLmstr\bankrec.H"&str$(cno),"[Q]\GLmstr\bankrec-idx.h"&str$(cno) ,"79/3/4 12/1/8")

		fn_index_it("[Q]\GLmstr\BudgetInfo.h"&str$(cno),"[Q]\GLmstr\BudIndx.h"&str$(cno),"1,14")
		fn_index_it("[Q]\GLmstr\BudInfo.h"&str$(cno),"[Q]\GLmstr\BudInfo_Index.h"&str$(cno),"1,2")
	! /r
	! r: G
		fn_index_it("[Q]\GLmstr\GLmstr.h"&str$(cno),"[Q]\GLmstr\GLIndex.h"&str$(cno),"1 12")
		fn_index_it("[Q]\GLmstr\GLmstr.h"&str$(cno),"[Q]\GLmstr\glIndx2.h"&str$(cno),"13 30")

	! fn_index_it("[Q]\GLmstr\GLmstr.h"&str$(cno),"[Q]\GLmstr\fsindex.H"&str$(cno),"63 3") ! Secondary
	! fn_index_it("[Q]\GLmstr\GLmstr.h"&str$(cno),"[Q]\GLmstr\fsindex.H"&str$(cno),"66 3") ! Primary

		fn_index_it("[Q]\GLmstr\gl1099.h"&str$(cno),"[Q]\GLmstr\gl109Idx.h"&str$(cno),"1 8")
		fn_index_it("[Q]\GLmstr\GL1099.h"&str$(cno),"[Q]\GLmstr\VNINDX2.h"&str$(cno),"9 25")

		fn_index_it("[Q]\GLmstr\gltr1099.H"&str$(cno),"[Q]\GLmstr\gltridx1.H"&str$(cno),"1 8")

		fn_index_it("[Q]\GLmstr\GLBRec.h"&str$(cno),"[Q]\GLmstr\GLRecIdx.h"&str$(cno),"1 24")

		fn_index_it("[Q]\GLmstr\glstdad.H"&str$(cno),"[Q]\GLmstr\glstdidx.h"&str$(cno),"1 12")

		fn_index_it("[Q]\GLmstr\GLTrans.h"&str$(cno),"[Q]\GLmstr\tmp70.h"&str$(cno),"1 70")
	! /r
	! r: P
		fn_index_it("[Q]\GLmstr\payeeglbreakdown.H"&str$(cno),"[Q]\GLmstr\payeeglbkdidx.H"&str$(cno),"1 8")

		fn_index_it("[Q]\GLmstr\paymstr.H"&str$(cno),"[Q]\GLmstr\Payidx1.H"&str$(cno),"1 8")
		fn_index_it("[Q]\GLmstr\paymstr.H"&str$(cno),"[Q]\GLmstr\Payidx2.H"&str$(cno),"9 38")

		fn_index_it("[Q]\GLmstr\PRmstr.h"&str$(cno),"[Q]\GLmstr\PRIndex.h"&str$(cno),"1 4")

		fn_index_it("[Q]\GLmstr\PayeeGLBreakdown.h"&str$(cno),"[Q]\GLmstr\payeeglbkdidx.h"&str$(cno),"1 8")
	! /r
	! r: R
		fn_index_it("[Q]\GLmstr\RatioMST.h"&str$(cno),"[Q]\GLmstr\SchIndx2.h"&str$(cno),"3 30")
		fn_index_it("[Q]\GLmstr\RatioMST.h"&str$(cno),"[Q]\GLmstr\RatioIdx.h"&str$(cno),"1 3")
		fn_index_it("[Q]\GLmstr\RatioMST.h"&str$(cno),"[Q]\GLmstr\RaNamIdx.h"&str$(cno),"4 28")
	! /r
	! r: S
		sn=1
		fn_index_it("[Q]\GLmstr\schedule"&str$(sn)&".H"&str$(cno),"[Q]\GLmstr\schedule"&str$(sn)&"-idx.h"&str$(cno),"1 12")
		for sn=1 to 8
			fn_index_it("[Q]\GLmstr\schedule"&str$(sn)&".H"&str$(cno),"[Q]\GLmstr\schedule_idx"&str$(sn)&".h"&str$(cno),"1 12")
		next sn
	! /r
	! r: T
		fn_index_it("[Q]\GLmstr\TransCodes.h"&str$(cno),"[Q]\GLmstr\transcodes-idx.h"&str$(cno),"1 2")
	! /r
	! r: W
		fn_index_it("[Q]\GLmstr\W2Box16.h"&str$(cno),"[Q]\GLmstr\W2INDEX.h"&str$(cno),"1 8")
	! /r
	! /r
	else if system_id$='UB' then ! r:
		fn_index_it("[Q]\UBmstr\Reads_and_Chgs.h"&str$(cno), "[Q]\UBmstr\Reads_and_Chgs-Key.h"&str$(cno),"1 10")
		fn_ub_index_customer(cno)
		fn_index_it("[Q]\UBmstr\UBAdrBil.h"&str$(cno), "[Q]\UBmstr\adrIndex.h"&str$(cno),"1 10")
		fn_index_it("[Q]\UBmstr\UBTransVB.h"&str$(cno), "[Q]\UBmstr\UBTrIndx.h"&str$(cno),"1 19")
		fn_index_it("[Q]\UBmstr\UBTransVB.h"&str$(cno), "[Q]\UBmstr\UBTrdt.h"&str$(cno),"11/1 8/10")
		! fn_index_it("[Q]\UBmstr\Note1.h"&str$(cno), "[Q]\UBmstr\NoteIdx1.h"&str$(cno),"1 10")
		! fn_index_it("[Q]\UBmstr\Deposit1.h"&str$(cno), "[Q]\UBmstr\DepIdx1.h"&str$(cno),"1 10")
		fn_index_it("[Q]\UBmstr\Meter.h"&str$(cno), "[Q]\UBmstr\Meter_Idx.h"&str$(cno),"1/11 10/2")
		fn_index_it("[Q]\UBmstr\MeterType.h"&str$(cno), "[Q]\UBmstr\MeterTypeIdx.h"&str$(cno),"1 5")
		fn_index_it("[Q]\UBmstr\ubData\RateMst.h"&str$(cno), "[Q]\UBmstr\ubData\RateIdx1.h"&str$(cno),"1 4")
		fn_index_it("[Q]\UBmstr\ubData\RateMst.h"&str$(cno), "[Q]\UBmstr\ubData\RateIdx2.h"&str$(cno),"5 25")
		fn_index_it("[Q]\UBmstr\Cass1.h"&str$(cno), "[Q]\UBmstr\Cass1Idx.h"&str$(cno),"1 10")
		fn_index_it("[Q]\UBmstr\workorder.h"&str$(cno), "[Q]\UBmstr\wkindex.h"&str$(cno),"1/11 10/8")
	! /r
	else if system_id$='PR' then ! r:
		fn_index_it("[Q]\PRmstr\EmpStatus.dat","[Q]\PRmstr\EmpStatus.Idx","1 2")
		fn_index_it("[Q]\PRmstr\MGLMstr.h"&str$(cno),"[Q]\PRmstr\MGLIdx1.h"&str$(cno),"1 3")
		fn_index_it("[Q]\PRmstr\PRCkHist.h"&str$(cno),"[Q]\PRmstr\PRCKINDX.h"&str$(cno),"1 14")
		fn_index_it("[Q]\PRmstr\PRReport.h"&str$(cno),"[Q]\PRmstr\PRRptIdx.h"&str$(cno),"1 2")
		fn_index_it("[Q]\PRmstr\Employee.h"&str$(cno),"[Q]\PRmstr\EmployeeIdx-no.h"&str$(cno),"1 8")
		fn_index_it("[Q]\PRmstr\Employee.h"&str$(cno),"[Q]\PRmstr\EmployeeIdx-name.h"&str$(cno),"9 30")
		fn_index_it("[Q]\PRmstr\dd.h"&str$(cno),"[Q]\PRmstr\DDidx1.h"&str$(cno),"1 10")
		fn_index_it("[Q]\PRmstr\glmstr.h"&str$(cno),"[Q]\PRmstr\glIndex.h"&str$(cno),"1 12")
		fn_index_it("[Q]\PRmstr\SCMSTR.h"&str$(cno), "[Q]\PRmstr\SCIndex.h"&str$(cno),"1 3")
		fn_index_it("[Q]\PRmstr\W2Box16.h"&str$(cno), "[Q]\PRmstr\W2Index.h"&str$(cno),"1 8")
		fn_index_it("[Q]\PRmstr\Burden.H"&str$(cno), "[Q]\PRmstr\BurdenIdx.H"&str$(cno),"1 8")
		fn_index_it("[Q]\PRmstr\Category.H"&str$(cno), "[Q]\PRmstr\categoryIDX.H"&str$(cno),"1 5")
		fn_index_it("[Q]\PRmstr\Department.h"&str$(cno), "[Q]\PRmstr\Deptid4.h"&str$(cno),"12/1/9 12/8/3")
		fn_index_it("[Q]\PRmstr\Department.h"&str$(cno), "[Q]\PRmstr\DeptIdx4.h"&str$(cno),"50/9/1 2/3/8")
		fn_index_it("[Q]\PRmstr\Department.h"&str$(cno), "[Q]\PRmstr\DeptIdx.h"&str$(cno),"1/9 8/3")
		fn_index_it("[Q]\PRmstr\HourBreakdown.H"&str$(cno), "[Q]\PRmstr\HourBreakdown-idx.H"&str$(cno),"1/9/14 8/5/8")
		fn_index_it("[Q]\PRmstr\PayrollChecks.h"&str$(cno), "[Q]\PRmstr\checkidx.h"&str$(cno),"1 17")
		fn_index_it("[Q]\PRmstr\PayrollChecks.h"&str$(cno), "[Q]\PRmstr\CheckIdx2.h"&str$(cno),"9/12/1 3/6/8")
		fn_index_it("[Q]\PRmstr\PayrollChecks.h"&str$(cno), "[Q]\PRmstr\checkidx3.h"&str$(cno),"1/12/9 8/6/3")
		fn_index_it("[Q]\PRmstr\payrollreports.H"&str$(cno), "[Q]\PRmstr\prrptidx.h"&str$(cno),"1 30")
		fn_index_it("[Q]\PRmstr\payrollreports.H"&str$(cno), "[Q]\PRmstr\reportidx.H"&str$(cno),"1 30")
		fn_index_it("[Q]\PRmstr\PRCkHist.h"&str$(cno), "[Q]\PRmstr\PRCKINDX.h"&str$(cno),"1 14")
		fn_index_it("[Q]\PRmstr\PRReport.h"&str$(cno), "[Q]\PRmstr\prrptidx.h"&str$(cno),"1 2")
		fn_index_it("[Q]\PRmstr\prTot.h"&str$(cno), "[Q]\PRmstr\PRTotIdx.h"&str$(cno),"1 9")
		fn_index_it("[Q]\PRmstr\rpwork"&wsid$&".h"&str$(cno), "[Q]\PRmstr\rpwork"&wsid$&"Idx.h"&str$(cno),"1 11")
		fn_index_it("[Q]\PRmstr\rpwork"&wsid$&".h"&str$(cno), "[Q]\PRmstr\rpwork"&wsid$&"Idx2.h"&str$(cno),"1/27 8/14")
		fn_index_it("[Q]\PRmstr\DeptName.h"&str$(cno),"[Q]\PRmstr\DepNameIdx.h"&str$(cno),"1 3")
	! /r
	else if system_id$='CL' then ! r:
		fn_index_it("[Q]\CLmstr\BankMstr.h"&str$(cno), "[Q]\CLmstr\BankIdx1.h"&str$(cno),"1 2")
		fn_index_it("[Q]\CLmstr\DPTMSTR.h"&str$(cno), "[Q]\CLmstr\DPTIDX1.h"&str$(cno),"1 5")
		fn_index_it("[Q]\CLmstr\GLmstr.H"&str$(cno), "[Q]\CLmstr\GLINDEX.H"&str$(cno),"1 12")
		fn_index_it("[Q]\CLmstr\IvPaid.h"&str$(cno), "[Q]\CLmstr\IVIndex.h"&str$(cno)," 1 20")
		fn_index_it("[Q]\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno), "[Q]\CLmstr\jcbrkidx"&wsid$&".H"&str$(cno),"48 20")
		fn_index_it("[Q]\CLmstr\payeeglbreakdown.H"&str$(cno), "[Q]\CLmstr\Payeeglbkdidx.H"&str$(cno),"1 8")
		fn_index_it("[Q]\CLmstr\paymstr.H"&str$(cno), "[Q]\CLmstr\payidx1.H"&str$(cno),"1 8")
		fn_index_it("[Q]\CLmstr\PayTrans.h"&str$(cno), "[Q]\CLmstr\Unpdidx2.H"&str$(cno),"31/27/1 2/4/26") ! index in year,monthday,reference
		fn_index_it("[Q]\CLmstr\PayTrans.h"&str$(cno), "[Q]\CLmstr\UNPdIdx1.h"&str$(cno),"1 20")
		fn_index_it("[Q]\CLmstr\Receiptglbreakdown.h"&str$(cno), "[Q]\CLmstr\receiptglbkdidx.h"&str$(cno),"1 8")
		fn_index_it("[Q]\CLmstr\Recmstr.h"&str$(cno), "[Q]\CLmstr\Recidx1.h"&str$(cno)," 1 8")
		fn_index_it("[Q]\CLmstr\Tralloc.h"&str$(cno), "[Q]\CLmstr\Tralloc-idx.h"&str$(cno)," 1 11")
		fn_index_it("[Q]\CLmstr\TrMstr.h"&str$(cno), "[Q]\CLmstr\TrIdx1.h"&str$(cno)," 1 11")
		fn_index_it("[Q]\CLmstr\TrMstr.H"&str$(cno), "[Q]\CLmstr\TrIdx2.H"&str$(cno)," 28/1 8/11")
		fn_index_it("[Q]\CLmstr\TrMstr.H"&str$(cno), "[Q]\CLmstr\Tridx3.H"&str$(cno)," 16/12/4 2/4/8") ! index in year,monthday,reference
		fn_index_it("[Q]\CLmstr\unpdaloc.H"&str$(cno), "[Q]\CLmstr\Uaidx1.H"&str$(cno),"9,12")
		fn_index_it("[Q]\CLmstr\unpdaloc.H"&str$(cno), "[Q]\CLmstr\Uaidx2.H"&str$(cno),"1,20")
	! /r
	end if 
fnend 
def library fnub_index_customer(; cno)
	fn_index_it_setup
	if cno=0 then cno=val(env$('cno'))
	fnub_index_customer=fn_ub_index_customer(cno)
fnend 
def fn_ub_index_customer(cno)
	fn_index_it("[Q]\UBmstr\Customer.h"&str$(cno), "[Q]\UBmstr\ubIndex.h"&str$(cno),"1 10")
	fn_index_it("[Q]\UBmstr\Customer.h"&str$(cno), "[Q]\UBmstr\ubIndx2.h"&str$(cno),"354 7")
	fn_index_it("[Q]\UBmstr\Customer.h"&str$(cno), "[Q]\UBmstr\ubIndx3.h"&str$(cno),"11 30u")
	fn_index_it("[Q]\UBmstr\Customer.h"&str$(cno), "[Q]\UBmstr\ubIndx4.h"&str$(cno),"41 30")
	fn_index_it("[Q]\UBmstr\Customer.h"&str$(cno), "[Q]\UBmstr\ubIndx5.h"&str$(cno),"1741/1743 2/7")
fnend
XIT: fnxit
include: ertn