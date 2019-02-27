on error goto Error_Hanler
fn_setup
! fnsession_size_setup(Session_Rows,Session_Cols)
fntop(program$,'',1)

open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
! r: get mat CoCo
	dim coco(0)
	mat coco(0)
	fnAddOneN(mat coco,   2)
	fnAddOneN(mat coco, 101)
	fnAddOneN(mat coco, 110)
	fnAddOneN(mat coco, 540)
	fnAddOneN(mat coco, 675)
	fnAddOneN(mat coco, 749)
	fnAddOneN(mat coco, 821)
	fnAddOneN(mat coco, 945)
	fnAddOneN(mat coco, 962)
	fnAddOneN(mat coco, 966)
	fnAddOneN(mat coco, 967)
	fnAddOneN(mat coco, 969)
	fnAddOneN(mat coco, 973)
	fnAddOneN(mat coco, 976)
	fnAddOneN(mat coco, 979)
	fnAddOneN(mat coco, 980)
	fnAddOneN(mat coco, 984)
	fnAddOneN(mat coco, 986)
	fnAddOneN(mat coco, 994)
	fnAddOneN(mat coco, 995)
	fnAddOneN(mat coco, 999)
	fnAddOneN(mat coco,1005)
	fnAddOneN(mat coco,1014)
	fnAddOneN(mat coco,1018)
	fnAddOneN(mat coco,1083)
	fnAddOneN(mat coco,1105)
	fnAddOneN(mat coco,1116)
	fnAddOneN(mat coco,1124)
	fnAddOneN(mat coco,1127)
	fnAddOneN(mat coco,1133)
	fnAddOneN(mat coco,1147)
	fnAddOneN(mat coco,6011)
! /r
! r: ask 
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

fnmulti_select(mat coco_selected$,mat coco_unselected$,'Select CoCo to include',Mat D_Grid_Heading$,Mat D_Grid_Width,Mat D_Grid_Form$)
if fkey=93 or fkey=99 then goto Xit
! ! /r

! r: open the files and pr headers
cocoCount=udim(mat coco_selected$)
dim outFileName$(0)*1024
mat outFileName$(cocoCount)
mat coco$(cocoCount)
mat cocoN(cocoCount)
mat hOut(cocoCount)
for cocoItem=1 to cocoCount
	coco$(cocoItem)=coco_selected$(cocoItem)(1:pos(coco_selected$(cocoItem),msDelim$,1)-1)
	cocoN(cocoItem)=val(coco$(cocoItem))
	dim tempFolder$*256 ! a safe place for read/write/delete/etc on the server.
	tempFolder$='F:\CLSINC\TEMP'
	outFileName$(cocoItem)=env$('program_caption')&' - '&coco$(cocoItem)&' - '&fnsafe_filename$(fn_cocoData$(cocoN(cocoItem),'name'))&' - '&date$('ccyy-mm-dd')&'-'&srep$(time$,':','-')&'.xls'
	! outFileName$(cocoItem)=srep$(outFileName$(cocoItem),' ','_')
	outFileName$(cocoItem)(0:0)=tempFolder$&'\'
	open #hOut(cocoItem):=fngethandle: 'name='&outFileName$(cocoItem)&',RecL=1024,replace',d,o
	pr #hOut(cocoItem): '<table>'
	! r: Pr Header
		pr #hOut(cocoItem): '<tr>'
		pr #hOut(cocoItem): '<th> CoCo  </th>'
		pr #hOut(cocoItem): '<th> FileNo   </th>'
		! pr #hOut(cocoItem): '<th> d1_name </th>'
		pr #hOut(cocoItem): '<th> Suit Date </th>'
		pr #hOut(cocoItem): '<th> Suit Amt </th>'
		pr #hOut(cocoItem): '<th> Balance </th>'
		pr #hOut(cocoItem): '<th> Jmt Date </th>'
		pr #hOut(cocoItem): '<th> Jmt Amount </th>'
		pr #hOut(cocoItem): '<th> Last Payment Date </th>'
		pr #hOut(cocoItem): '<th> Last Payment Amount </th>'
		pr #hOut(cocoItem): '<th> Interest </th>'
		pr #hOut(cocoItem): '<th> Garn Date </th>'
		pr #hOut(cocoItem): '</tr>'
	! /r
nex cocoItem
! /r
! r: add the bodys to the files 
open #hDebtor:=fngethandle: 'name=debtor//6,kfname=debtor.idx//6,shr',internal,input,keyed
restore #hM: !
do
	read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
	fncom(readCount+=1,lrec(hM),12)
	cocoWhich=srch(mat cocoN,masterDataN(master_coco_no))
	if cocoWhich>0 then
	
	dim allDebtor$(0,0)*256,allDebtorN(0,0)
	debtorCount=fnAllDebtors(MasterData$(master_fileno),hDebtor,Mat allDebtor$,Mat allDebtorN)
	garn_date$=''
	if debtorCount=>1 then
		garn_date$=str$(allDebtorN(1,debtor_garn_date))
	end if
	
		! r: Pr Row
			pr #hOut(cocoWhich): '<tr> ';
			pr #hOut(cocoWhich): '<td>'&cnvrt$('N 4',masterDataN(master_coco_no))&'</td>';
			pr #hOut(cocoWhich): '<td>'&masterData$(master_fileno)&'</td>';
			! pr #hOut(cocoWhich): '<td>'&masterData$(master_d1_name)&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtDate$(masterData$(master_suit_date))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtCurrency$(masterDataN(master_suit_amt))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtCurrency$(masterDataN(master_balance))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtDate$(masterData$(master_jmt_date))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtCurrency$(masterDataN(master_jmt_amt))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtDate$(masterData$(master_lpaymnt_date))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtCurrency$(masterDataN(master_lpaymnt_amt))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtCurrency$(masterDataN(master_stored_int))&'</td>';
			pr #hOut(cocoWhich): '<td>'&fn_fmtDate$(garn_date$)&'</td>'
			pr #hOut(cocoWhich): '</tr> '
		! /r
	end if
loop
EoMaster: !
! /r
! r: finalize the files, close them, and email them
for cocoItem=1 to cocoCount
	pr #hOut(cocoItem): '</table>'
	pr file$(hOut(cocoItem))
	close #hOut(cocoItem):
nex cocoItem
emailCount=0
for cocoItem=1 to cocoCount
	dim tmpEmail$*256
	tmpEmail$=fn_cocoData$(cocoN(cocoItem),'email')
	dim tmpEmailList$(0)*256
	str2mat(tmpEmail$,mat tmpEmailList$,';')
	for emailItem=1 to udim(mat tmpEmailList$)
		dim emailSubject$*256
		emailSubject$=env$('program_caption')&' as of '&date$('month d, ccyy')&' for '&fn_cocoData$(cocoN(cocoItem),'name')
		dim emailMessage$*1048
		emailMessage$='See attached report.'&chr$(10)&'Should have been sent to '&tmpEmailList$(emailItem)
		! pause
		! fnSendEmail('niceguywinning@gmail.com',emailMessage$, emailSubject$ ,outFileName$(cocoItem))
		! fnSendEmail('jbowman@bqlaw.com',emailMessage$, emailSubject$ ,outFileName$(cocoItem))
		emailCount+=1
		fnSendEmail(tmpEmailList$(emailItem),emailMessage$, emailSubject$ ,outFileName$(cocoItem))
		! sleep(16)
		! pause
	nex emailItem
nex cocoItem
fncom(100,100,12)
fnMessageBox('Completed processing '&str$(emailCount)&' emailCountemail(s).', mb_ok,env$('program_caption'),'cocoInvSuccess')
goto Finis ! /r
Finis: ! r:
goto Xit ! /r
Xit: fnXit
def fn_fmtCurrency$*40(amount; ___,return$*40)
	return$=cnvrt$('pic(---,---,---,---,--#.##)',amount)
	fn_fmtCurrency$=return$
fnend
def fn_fmtDate$*10(theDate$; providedFormat$,___,return$*10)
	if providedFormat$='' then providedFormat$='ccyymmdd'
	theDate$=trim$(theDate$)
	if theDate$='0' or theDate$='' then
		return$=''
	else
		return$=date$(days(theDate$,providedFormat$),'mm/dd/ccyy')
	end if
	fn_fmtDate$=return$
fnend
def fn_setup
	if ~setup then
		setup=1
		library 'library\Gridio.wb': fnmulti_select
		! library 'Collection-Master Add-On\fn\Library.br': fnmulti_select
		library 'Collection-Master Add-On\fn\Library.br': fnsession_size_setup
		library 'library\CLSUtil.wb': fnAllDebtors
		library 'library\CLSUtil.wb': fnDate_rpt10$
		
		library 'S:\Core\Library.br': fnSendEmail
		library 'S:\Core\Library.br': fnsafe_filename$
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnXit

		library "library\CLSUtil.wb": fncom
		library "library\CLSUtil.wb": fnget_form
		library "library\CLSUtil.wb": fnget_formall$
		library "library\CLSUtil.wb": fnunpack$
		library "library\CLSUtil.wb": fnMessageBox
		
		library 'Library\SQL': fnsql_setup$
		
		dim masterData$(1)*60,masterDataN(1)
		dim masterFieldsc$(1)*20,masterFieldsN$(1)*20
		dim masterFormC$*1024,masterFormN$*1024
		dim mFormAll$*2048
		fnget_form("Master",mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,masterFormC$,masterFormN$)
		fnunpack$(masterFormC$,masterFormN$)
		mFormAll$=fnget_formall$
		gosub enumMaster
		gosub enumDebtor
		
		
	end if
fnend
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
include: cm\enum\debtor
include: cm\enum\master