
fn_setup
on error goto Error_Hanler
fnTop(program$)

fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
if fkey=93 or fkey=99 then goto Xit

dim filenoList$(0)*8
mat filenoList$(0)
fnasci(env$('at')&'claimsToGetFromDB3.txt',mat filenoList$)
! fnAddOneC(mat filenoList$,'firstOne')
! fnAddOneC(mat filenoList$,'secondUn')
mat hitCount(udim(mat filenoList$))
mat hitCount=(0)

	dim trustPath$(0)*40
dim trustName$(0)*28
if ~Setup_Trust then max_trust=fn_getTrusts(mat trustPath$,mat trustName$)

dim amt(0,0)
mat amt(udim(mat filenoList$),max_trust)
mat amt=(0)

for trustItem=1 TO max_trust
	fncom(trustItem,max_trust,10)
	hFinSum=Fnget_File("Name=K:"&trustPath$(trustItem)&"\FINSUM,KFName=K:"&trustPath$(trustItem)&"\FNSUMTA.IDX,SHR","INPUT")
	F_Trust: FORM Pos 1,2*C 8,Pos 263,6*Pd 6.2,8*Pd 6.2,Pos 455,Pd 4,Pos 477,B 2,Pos 517,C 8
	if hFinSum>0 then
		pr file$(hFinSum)
		do
			read #hFinSum,using finsum_formall$: mat finsum_data$,mat finsum_dataN eof EoTrust
			fncom(recCount+=1,lrec(hFinSum),12)
			fileno$=finsum_data$(finsum_fileno)
			readCount+=1
			filenoItem=srch(mat filenoList$,trim$(fileno$))>0
			if srch(mat filenoList$,trim$(fileno$))>0 then
				hitCount(filenoItem)+=1
				amt(filenoItem,trustItem)+=finsum_dataN(finsum_ACC_D1_BAL)
				! translate which trust acocunt it goes to, open it, etc
				! write to new one
			end if
		loop
		EoTrust: !
		close #hFinSum: ioerr ignore
	end if
next trustItem
Xit: !

	pr #255: '<table cellpadding=10 border=1>'
	pr #255: '  <tr><th>Category</th><th>Total</th></tr>'
	fn_finisAddRow('Read Count',str$(readCount))
	fn_finisAddRow('Claims searhed for',str$(udim(mat filenoList$)))
	fn_finisAddRow('Match Count',str$(sum(mat hitCount)))
	pr #255: '</table>'
readCount=0
mat hitCount=(0)

	pr #255: '<table cellpadding=10 border=1>'
	gosub PrHeader
	For filenoItem=1 to udim(mat filenoList$)
		! fncom(filenoItem,udim(mat filenoList$))
		! if listBalance(filenoItem)<=0 or listDiary$(filenoItem)<>'' then
		pr #255: '<tr> ';
		pr #255: '<td              >'&filenoList$    (filenoItem)&'</td>';
		for trustItem=1 to max_trust
			pr #255: '<td align="right">'&str$(amt(filenoItem,trustItem))&'</td>';
		nex trustItem
		pr #255: '</tr> '
		! end if
	nex filenoItem
	pr #255: '</table>'

fnClose
fnXit
PgOf: ! r:
	pr #255: newpage
	! gosub PrHeader
continue ! /r
PrHeader: ! r:
		pr #255: '<tr>'
		pr #255: '  <th>FileNo             </th>'
		for trustItem=1 to max_trust
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
def fn_getTrusts(mat trustPath$,mat trustName$; ___,path$*40,tName$*28,Ntrust,returnN)
	mat trustPath$(0)
	mat trustName$(0)
	dim path$*80
	if ~setup_categ then let fnsetup_categ
	open #hFinSum:=fnGetHandle: "name=trustact/common/5,shr",INTERNAL,OUTIN,RELATIVE IOERR Xitfn_getTrusts
	for Ntrust=1 TO lrec(hFinSum)
		read #hFinSum,using "form pos 1,c 40,c 28",rec=Ntrust,release: Path$,tName$
		if exists(path$) and pos(uprc$(path$),"\ACCOUNT")>0 then
			fnAddOneC(mat trustPath$,path$(1:pos(path$,"\ACCOUNT")-1))
			fnAddOneC(mat trustName$,tName$)
		end if
	next Ntrust
	close #hFinSum:
	returnN=udim(mat trustPath$)
	setup_trust=1
	Xitfn_getTrusts: !
	fn_getTrusts=returnN
fnend
def fnsetup_categ(; ___,returnN)
	dim categ2$(99)*80,heading$(48)*80
	dim autostatus2$(99),finlstat$(1)*20
	mat categ$(udim(mat categ2$))
	Categ_Worked=Setup_Categ=1
	! categ2$(1)-categ2$(99) Are No Longer Hard Coded
	! Use CATEG.LST//9 To Modify the List
	! Codes 33-80 Are Defined by 2-S-2 and "Heading//8"
	hCateg=fnget_display("CATEG.LST//9")
	linput #hCateg: mat Categ2$
	close #hCateg:
	hHeading=fnget_file("HEADING/COMMON/5")
	if hHeading<=0 then
		fnmessagebox("Warning COULD NOT OPEN FINANCIAL"&Chr$(10)&os_filename$("HEADING/COMMON/5")&Chr$(10)&"Error:"&Str$(Err)&" Line:"&Str$(Line)&" - In "&program$,16,"Error!")
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
	Autostatus_Worked=fnAsci("finlstat.ini//8",mat Finlstat$)
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
	fnsetup_categ=returnN
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
		library 'S:\Core\Library.br': fnTos
		library 'S:\Core\Library.br': fnChk
		library 'S:\Core\Library.br': fnLbl,fnTxt
		library 'S:\Core\Library.br': fnCmdSet,fnAcs
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop

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