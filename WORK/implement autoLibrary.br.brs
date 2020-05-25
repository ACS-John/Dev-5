autoLibrary
on error goto Ertn
fnTop(program$)
pr b: env$('program_caption')
tab$=chr$(9)
fn_getList
fileCount=udim(mat fileName$)
countlinesRemoved=countAutoLibraryAdded= _
	countProgramWithLineNoSkipped=countProgramWithLineNoFixed= _
	countLibraryOfferingProgram= 0
for item=1 to fileCount
	fnStatus(str$(item)&'/'&str$(fileCount))
	! fnStatusPause
	ProcessThisFile: !
	if exists(fileName$(item)) then
		fnStatus(fileName$(item))
		open #hIn :=fnGetHandle: 'name='&fileName$(item),d,i
		dim tmpFile$*512
		tmpFile$=env$('temp')&'\acs\tmpSourceFile.br.brs'
		open #hOut:=fnGetHandle: 'name='&tmpFile$&',RecL=1024,Replace',d,o
		encounteredAutoLibrary=lineCount=enableSkipThisFile=0
		do
			dim line$*2048
			linput #hIn: line$ eof EoIn
			dim ln$*2048 ! modified version of line$ for testing purposes
			dim tab$*1
			tab$=chr$(9)
			tabLevel=fn_tabLevel(line$)
	
			ln$=srep$(line$,tab$,'')
			ln$=trim$(ln$)
			fn_stripComment(ln$)
			lineCount+=1
			! r: check for and if necessary remove line numbers and restart
			if lineCount=1 then
				lineVal=0
				lineVal=val(ln$(1:5)) conv ignore
				if lineVal then
					close #hIn:
					close #hOut:
					if fn_removeLineNumbers(fileName$(item)) then
						countProgramWithLineNoSkipped+=1
						fnStatus(' . line numbers removed - restarting this files processing.')
						goto ProcessThisFile
					else 
						countProgramWithLineNoFixed+=1
						fnStatus(' X failed removing line numbers - skipping it.')
						enableSkipThisFile=1
						goto EoIn
					end if
				end if
			en if
			! /r
			
			ln$=lwrc$(ln$)
			
			! pr ln$ : pause
			
			if ln$(1:11)='autolibrary' then
				! existing autoLibrary encountered
				encounteredAutoLibrary=1
				pr #hOut: line$
			else if ln$(1:14)='def library fn' and ln$(15:15)<>'_' then
				! r: library offering program - SKIPPED
				countLibraryOfferingProgram+=1
				fnStatus(' X program offers a library lets just skip these for now and see how many they are')
				enableSkipThisFile=1
				! /r
			else if ln$(1:6)='def fn' and ln$(7:7)<>'_' then 
				! r: illegal local function name
				encounterBadFn=1
				fnStatus(' Y Illegally named function: '&ln$)
				fnStatus(' Y Make your changes to the program and then continue to restart on this program.')
				close #hIn:
				close #hOut:
				execute 'sy ""C:\ACS\Program\Notepad++\notepad++.exe" "'&filename$(item)&'" -n'&str$(lineCount)&'"'
				! pause
				fnStatusPause
				! enableSkipThisFile=1
				goto ProcessThisFile
				! /r
			else if ln$(1:26)="library 's:\core\library':" then
				! r: autoLibrary magic
				if encounteredAutoLibrary then
					countlinesRemoved+=1
					! not even a blank line ! pr #hOut: ''
				else ! if ~encounteredAutoLibrary then
					encounteredAutoLibrary=1
					pr #hOut: rpt$(tab$,tablevel)&'autoLibrary'
					countAutoLibraryAdded+=1
				end if
				! /r
			else
				fn_acsToAcs2(line$)
				pr #hOut: line$
			end if
		loop until enableSkipThisFile
		EoIn: !
		close #hIn:  ioerr ignore
		close #hOut: ioerr ignore
		if ~enableSkipThisFile then
			dim backupFile$*1024
			backupFile$
			if fn_makeBackup(fileName$(item),'.beforeAutoLibrary') then
				fnCopy(tmpFile$,fileName$(item))
			else
				fnStatus(' X Backup failed so new program not put into place.')
				pr bell;
				pause
			end if
		end if
	else 
		fnStatus(fileName$(item)&' : NOT FOUND - SKIPPED.')
	end if

nex item
! r: Totals Screen
fnStatus('*files processed: '&str$(item))
fnStatus('*lines removed: '&str$(countlinesRemoved))
fnStatus('*autoLibrary added: '&str$(countAutoLibraryAdded))
fnStatus('*Library Offering Programs (skipped): '&str$(countLibraryOfferingProgram))
fnStatus('*Programs with Line Numbers (fixed): '&str$(countProgramWithLineNoFixed))
fnStatus('*Programs with Line Numbers (skipped): '&str$(countProgramWithLineNoSkipped))
fnStatusPause
! /r
end
Xit: fnXit
def fn_removeLineNumbers(rFile$*1024; ___,returnN,hIn,hOut,rlnFile$*1024,line$*2048,lineLen)
	!  DO NOT PASS FILES THAT DO NOT CONTAIN LINE NUBERS
	fn_makeBackup(rFile$,'.beforeRemoveLineNumbers')
	
	rlnFile$=env$('temp')&'\acs\tmpSourceFile.br.brs'
	open #hIn :=fnGetHandle: 'name='&rFile$,d,i
	open #hOut:=fnGetHandle: 'name='&rlnFile$&',RecL=1024,Replace',d,o
	do
		linput #hIn: line$ eof R_eoIn
		line$=rtrm$(line$)
		line$=rtrm$(line$)
		lineLen=len(line$)
		if line$(lineLen-1:lineLen)='!:' then line$(lineLen-1:lineLen)=': _'
		line$(1:6)=''
		if trim$(line$)='!' then line$=''
		if lwrc$(trim$(line$)(1:4))='def '   then line$=ltrm$(line$)
		if lwrc$(trim$(line$)(1:5))='fnend'  then line$=ltrm$(line$)
		if lwrc$(trim$(line$)(1:6))='return' then line$=ltrm$(line$)
		line$=rtrm$(line$)
		! r: convert '  ' to '[tab]'
		! tabConvertCount=0
		! pr line$
		for x=10 to 2 step -2
			! pr x
			if line$(1:x)=rpt$(' ',x) then 
				! tabConvertCount+=1
				line$(1:x)=rpt$(chr$(9),x/2)
			end if
		nex x
		! if tabConvertCount>0 then
		! 	pr srep$(line$,chr$(9),'>') : pause
		! end if
		! /r
		pr #hOut: rtrm$(line$)
	loop
	R_eoIn: !
	close #hIn:
	close #hOut:
	returnN=fnCopy(rlnFile$,rFile$)
	! pr 'fn_removeLineNumbers returning ';returnN;' after processing '&rFile$ : pause
	fn_removeLineNumbers=returnN
fnend
def fn_makeBackup(of$*1024,newExt$*128; ___,returnN,backupFile$*1024)	
	backupFile$=of$&newExt$
	if exists(backupFile$) then
		returnN=1
	else
		returnN=fnCopy(of$,backupFile$)
	end if
	fn_makeBackup=returnN
fnend
def fn_tabLevel(&line$; ___,it,returnN)
	it=5 ! highest tested for level
	do until returnN or ~it
		it-=1
		if line$(1:(it))=rpt$(tab$,it) then 
			returnN=it
		end if
	loop 
	fn_tabLevel=returnN
fnend
def fn_stripComment(&ln$; ___,ePos)
	ePos=pos(ln$,'!')
	if ePos>0 then
		ln$(ePos:inf)=''
	en if
fnend
def fn_getList
	dim fileName$(0)*512
	mat fileName$(0)
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\apmstr-cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\glmstr-to-recl62.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\paymstr-v0-to-v1.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\paytrans-v1-to-v2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\tralloc-v1-to-v2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\trmstr-v1-to-v2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\unpdaloc-v1-to-v2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\fnupdatebankbal.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\account_search.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\acglblds_lib.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\acprscr_lib.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\actpd$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\actpd.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\cch$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\cmbbud.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\Conversion\acglbld.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\Conversion\acglcovl-cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\Conversion\acglnote-cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\Conversion\glmstr-338-416.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\Conversion\glpayee_v0_to_v1.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\employee_search.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnacglblds.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnacprscr.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnactpd$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnactpd.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fncch$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnglfs.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\fnglmerge.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsGL\w2box16.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\category_srch.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cat_srch2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cmbcat.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cmbcategory.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cmbemp.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cmbjob.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\cmbsubcat.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\Conversion\v4_cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\Conversion\v4_part2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\employee_srch.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\hourclassification2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\hours_lib.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\newprreg2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\newprrpts1.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\subcat_srch.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsPR\subcmbcat.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsTM\print_invoice.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsTM\tmsclist.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\calk_chatom.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\calk_standard.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\check_balance_breakdowns.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\cmbact.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\Conversion\bld_trans.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\Conversion\note-cnv-c7.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\Conversion\note-cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\Conversion\ubadrbil-cnv.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\Conversion\ubmstr-vb.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\duptr.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsUB\totalbal.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Checkbook\Payee.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Collection-Master Add-On\cpt.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Collection-Master Add-On\fn\claimFolder.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Ace\fnShortPath.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Ace\GetCD.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Ace\Win3B.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\ACS_Component.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Check File Versions.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Client.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\CmbCNo.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\CNo.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Confirm.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\copy.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\dec2hex.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\email.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Favorites.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\File Open and Save.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\askFileName.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\chain.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\error.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\getDir2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\makeSurePathExists.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\wait.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\windowsStart.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fn\xit.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnagl$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnFixWordingOnGrid.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnMsgBox.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnrgl$.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnSnap\fnMsExe.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\fnSnap\rtflib_dll.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Hamster.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\HamsterFio.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\hamster_print.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\inch2twip.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Index.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\ini.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Label\fnaddlabel.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Label\fnlabel.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Menu.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\oldmsgbox.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Parse\csz.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Parse\remove2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Print\w2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Print\w3.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Print.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\print1099.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\PrintAce.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\PrintPdf.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\process.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Programs\1099.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Programs\Preferences.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Programs\PrintAce_Test.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Programs\Update.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\program_properties.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\reg.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\search.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\specialFolderPath.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\Start.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\status.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\twip2inch.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Core\xit.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Dev\xmldiff.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\Change Payroll Dates.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\fn\dedNames.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\fn\deptName.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\Job Cost\fnBurden_srch.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\Job Cost\fnCmbBurden.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Payroll\Payroll Check History.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\TEST\comboa.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\TEST\fnProgressBar.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Time Management\Category.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Time Management\Email Queued Invoices.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Time Management\fn\customerHasEbilling.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Time Management\fn\printInvoice.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Time Management\fn\reassignNTA.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Add An Other Charge.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Collections.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Company.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Customer.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\fn\cmbRt2.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\fn\eftData.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\fn\lastBillingDate.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Hand Held\Create Hand Held File.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Hand Held\Meter Location.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Labels.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Rates.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Type of Service.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Work Order Add.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Work Order List.br.brs')
	fnAddOneC(mat fileName$,'C:\ACS\Dev-5\Utility Billing\Work Order Print.br.brs')
fnend
def fn_acsToAcs2(&line$; ___,posX,removing$*12,adding$*7)
	removing$='fnacs(sn$,0,' ! must be lwrc$
	adding$='fnAcs2('
	removingLen=len(removing$)
	posX=pos(lwrc$(line$),removing$)
	if posX>0 then
		! pr 'before line$: '&line$ ! pause
		line$(posX:posX+removingLen-1)=adding$
		! pr ' after line$: '&line$
		! pause
	end if
fnend
include: Ertn
fn_removeLineNumbers