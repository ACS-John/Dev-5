
fn_setup
on error goto Ertn
fnTop(program$)

dim basePath_db1_test$*512
dim basePath_db2_test$*512
dim basePath_db3_test$*512
dim basePath_db1_real$*512
dim basePath_db2_real$*512
dim basePath_db3_real$*512
basePath_db1_test$='\\192.168.111.44\Data1\CLS_LOCAL\CMNJ'
basePath_db2_test$='\\192.168.111.44\Data1\CLS_LOCAL\CMNY'
basePath_db3_test$='\\192.168.111.44\Data1\CLS_LOCAL\CMSS'
basePath_db1_real$='\\192.168.111.44\Data\ROOT	'
basePath_db2_real$='\\192.168.111.44\Data\CLSINC'
basePath_db3_real$='\\192.168.111.44\Data\CLSINC2'

dim sourcePath$*512
sourcePath$=basePath_db3_test$

fn_sel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
if fkey=93 or fkey=99 then goto Xit

dim filenoList$(0)*8
mat filenoList$(0)
fnasci(env$('at')&'claimsToGetFromDB3.txt',mat filenoList$)
! fnAddOneC(mat filenoList$,'firstOne')
! fnAddOneC(mat filenoList$,'secondUn')
mat hitCount(udim(mat filenoList$))
mat hitCount=(0)

dim trustPath$(0)*512
dim trustName$(0)*28
sourceTrustCount=fn_getTrusts(mat trustPath$,mat trustName$, sourcePath$)
for trustPathItem=1 to udim(mat trustPath$)
	trustPath$(trustPathItem)=sourcePath$&trustPath$(trustPathItem)(pos(trustPath$(trustPathItem),'\',-1):inf)
nex trustPathItem

dim fieldsToReportN(0)
mat fieldsToReportN(0)
dim fieldsToReport$(0)
mat fieldsToReport$(0)
fnAddOneN(mat fieldsToReportN,finsum_EOD_AMT_RCV   ) : fnAddOneC(mat fieldsToReport$,'EOD_AMT_RCV   ')
fnAddOneN(mat fieldsToReportN,finsum_EOD_COSTS_DISB) : fnAddOneC(mat fieldsToReport$,'EOD_COSTS_DISB')
fnAddOneN(mat fieldsToReportN,finsum_EOD_COSTS_RET ) : fnAddOneC(mat fieldsToReport$,'EOD_COSTS_RET ')
fnAddOneN(mat fieldsToReportN,finsum_EOD_COLLHOLD  ) : fnAddOneC(mat fieldsToReport$,'EOD_COLLHOLD  ')
fnAddOneN(mat fieldsToReportN,finsum_EOD_REMITHOLD ) : fnAddOneC(mat fieldsToReport$,'EOD_REMITHOLD ')
fnAddOneN(mat fieldsToReportN,finsum_EOD_CHECK_AMT ) : fnAddOneC(mat fieldsToReport$,'EOD_CHECK_AMT ')
ftrCount=udim(mat fieldsToReportN)


dim amt(0,0,0)
mat amt(udim(mat filenoList$),sourceTrustCount,ftrCount)
mat amt=(0)

for trustItem=1 TO sourceTrustCount
	fncom(trustItem,sourceTrustCount,10)

	hFinSum=Fnget_File('Name='&trustPath$(trustItem)&'\FINSUM,KFName='&trustPath$(trustItem)&'\FNSUMTA.IDX,shr','INPUT')
	F_Trust: FORM Pos 1,2*C 8,Pos 263,6*Pd 6.2,8*Pd 6.2,Pos 455,Pd 4,Pos 477,B 2,Pos 517,C 8
	if hFinSum>0 then
		pr file$(hFinSum)
		do
			read #hFinSum,using finsum_formall$: mat finsum_data$,mat finsum_dataN eof EoTrust
			fncom(recCount+=1,lrec(hFinSum),12)
			fileno$=finsum_data$(finsum_fileno)
			readCount+=1
			filenoItem=srch(mat filenoList$,trim$(fileno$))
			if filenoItem>0 then
				for ftrItem=1 to ftrCount
					amt(filenoItem,trustItem,ftrItem)+=finsum_dataN(fieldsToReportN(ftrItem))
				nex ftrItem
				hitCount(filenoItem)+=1
				! translate which trust acocunt it goes to, open it, etc
				! write to new one
			end if
		loop
		EoTrust: !
		close #hFinSum: ioerr ignore
	end if
next trustItem

! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table align="Center">'
	pr #255: '<tr>'
	pr #255: '<td align="Center"><h2>'&env$('program_caption')&'</h2></td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td  align="Center">As of '&fnDate_rpt10$(Date$)&'.</td>'
	pr #255: '</tr>'
	pr #255: '</table>'
	! /r
	pr #255: '<table cellpadding=10 border=1>'
	pr #255: '  <tr><th>Category</th><th>Total</th></tr>'
	fn_finisAddRow('Read Count',str$(readCount))
	fn_finisAddRow('Claims searhed for',str$(udim(mat filenoList$)))
	fn_finisAddRow('Match Count',str$(sum(mat hitCount)))
	for trustItem=1 to sourceTrustCount
		for ftrItem=1 to ftrCount						! amt(Claim,Trust,Field)
			tmpTotal=fn_3dTotalRow(mat amt,trustItem,ftrItem)
			if tmpTotal<>0 then
				fn_finisAddRow('Total '&fieldsToReport$(ftrItem)&' in '&trustName$(trustItem),str$(tmpTotal))
			end if
		nex ftrItem
	nex trustItem
	pr #255: '</table>'
readCount=0
mat hitCount=(0)
! r: report individual amounts
	for ftrItem=1 to ftrCount
		pr #255: '<h2>'&fieldsToReport$(ftrItem)&'</h2>'
		pr #255: '<table cellpadding=10 border=1>'
		gosub PrHeader
		For filenoItem=1 to udim(mat filenoList$)
			! fncom(filenoItem,udim(mat filenoList$))
			! if listBalance(filenoItem)<=0 or listDiary$(filenoItem)<>'' then
			pr #255: '<tr> ';
			pr #255: '<td              >'&filenoList$    (filenoItem)&'</td>';
			for trustItem=1 to sourceTrustCount
				pr #255: '<td align="right">'&str$(amt(filenoItem,trustItem,ftrItem))&'</td>';
			nex trustItem
			pr #255: '</tr> '
			! end if
		nex filenoItem
		pr #255: '</table>'
	nex ftrItem
! /r
fnClose
Xit: !
fnXit
def fn_3dTotalRow(mat d2,row,d3; ___,x,returnN)
	for x=1 to udim(mat d2,2)
		returnN+=d2(row,x,d3)
	nex x
	fn_3dTotalRow=returnN
fnend
PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:
		pr #255: '<tr>'
		pr #255: '  <th>FileNo             </th>'
		for trustItem=1 to sourceTrustCount
			pr #255: '  <th>'&trustName$(trustItem)&'</th>'
		nex trustItem
		pr #255: '</tr>'
return ! /r
def fn_finisAddRow(label$*256,value$*256)
	pr #255: '  <tr>'
	pr #255: '    <td>'&label$&'</td>'
	pr #255: '    <td align="right">'&value$&'</td>'
	pr #255: '  </tr>'
fnend
def fn_getTrusts(mat trustPath$,mat trustName$; basePath$*512,___,path$*40,tName$*28,trustItem)
	mat trustPath$(0)
	mat trustName$(0)
	if ~setup_categ then fn_setup_categ
	if basePath$='' then basePath$=os_filename$('trustact/common/5')(1:pos(os_filename$('trustact/common/5'),'\COMMON')-1)
	open #hFinSum:=fnGetHandle: 'name='&basePath$&'\common\trustact,shr',INTERNAL,OUTIN,RELATIVE IOERR Xitfn_getTrusts
	! open #hFinSum:=fnGetHandle: "name=trustact/common/5,shr",INTERNAL,OUTIN,RELATIVE IOERR Xitfn_getTrusts
	for trustItem=1 TO lrec(hFinSum)
		read #hFinSum,using "form pos 1,c 40,c 28",rec=trustItem,release: path$,tName$
		if exists(path$) and pos(uprc$(path$),"\ACCOUNT")>0 then
			fnAddOneC(mat trustPath$,path$(1:pos(path$,"\ACCOUNT")-1))
			fnAddOneC(mat trustName$,tName$)
		end if
	next trustItem
	close #hFinSum:
	Xitfn_getTrusts: !
	fn_getTrusts=udim(mat trustPath$)
fnend
def fn_setup_categ(; basePath$*512,___,returnN)
	dim categ2$(99)*80
	dim heading$(48)*80
	dim autostatus2$(99)
	dim finlstat$(1)*20
	mat categ$(udim(mat categ2$))
	Categ_Worked=Setup_Categ=1
	if basePath$='' then basePath$=os_filename$('trustact/common/5')(1:pos(os_filename$('trustact/common/5'),'\COMMON')-1)
	! categ2$(1)-categ2$(99) Are No Longer Hard Coded
	! Use CATEG.LST//9 To Modify the List
	! Codes 33-80 Are Defined by 2-S-2 and "Heading//8"
	hCateg=fnget_display("CATEG.LST//9")
	linput #hCateg: mat Categ2$
	close #hCateg:
	hHeading=fnget_file(basePath$&'\COMMON\HEADING')
	if hHeading<=0 then
		fnmessagebox("Warning COULD NOT OPEN FINANCIAL"&lf$&basePath$&'\COMMON\HEADING'&lf$&"Error:"&Str$(Err)&" Line:"&Str$(Line)&" - In "&program$,16,"Error!")
		Categ_Worked=0
	else
		read #hHeading,using 'FORM 30*C 30',rec=1,release: mat heading$(19:48) norec ignore
		read #hHeading,using 'FORM 18*C 30',rec=2,release: mat heading$(1:18) norec ignore
		close #hHeading: ioerr ignore
	end if
	for z=1 to 48
		categ2$(z+32)=ltrm$(heading$(z)(4:99))
	next z
	for z=1 to udim(mat categ2$)
		categ2$(z)=rpad$(categ2$(z),30)
	next z
	returnN=categ_worked
	Autostatus_Worked=fnAsci(basePath$&'common\finlstat.ini',mat Finlstat$)
	if autostatus_worked>0 then
		_finlstat=udim(finlstat$)
		if _finlstat<99 then
			mat finlstat$(99)
			for j=_finlstat+1 to 99
				finlstat$(j)=str$(j)&","
			next j
		end if
		if _finlstat>99 then mat finlstat$(99)
		for j=1 to 99
			Autostatus2$(J)=Finlstat$(J)(Pos(Finlstat$(J),',')+1:Len(Finlstat$(J)))
		next j
	end if
	fn_setup_categ=returnN
fnend
def fn_setup
	if ~setup then
		setup=1

		library 'library\clsUtil.wb': Fnget_Display
		library 'library\clsUtil.wb': Fnget_File
		library 'library\clsUtil.wb': fnmessagebox
		library 'library\clsUtil.wb': fnget_inf_claim
		library 'library\clsUtil.wb': fnAsci
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		! library 'library\clsUtil.wb': fnArray_to_range$
		library 'prog2\intermnt.wb': fnInternal_data


		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fnTop

		library 'library\CLSUtil.wb': fnList_Print
		library 'library\CLSUtil.wb': fncom

		library 'Library\SQL.wb': fnsql_setup$

		gosub SetupPrint

		dim finsum_data$(0)*128,finsum_dataN(0)
		dim finsum_fieldsc$(0)*20,finsum_fieldsn$(0)*20
		dim finsum_formall$*2048
		execute "*SubProc "&fnsql_setup$('finsum',mat finsum_data$,mat finsum_dataN,mat finsum_fieldsc$,mat finsum_fieldsn$,finsum_formall$)

	end if
fnend
include: cm\err
include: cm\print