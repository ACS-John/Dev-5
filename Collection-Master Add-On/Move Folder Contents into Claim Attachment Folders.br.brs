fn_setup(table$)
on error goto Ertn
fntop(program$)

! enableReportSuccess=1

Screen1: ! r:
dim resp$(20)*256
dim sourcePath$*256
fnreg_read(env$('cap')&'.Source Path',sourcePath$,'F:\S&S SCANS\')
fnreg_read(env$('cap')&'.Paperless Code',pcode$,'XNEWSCAN')
fntos : lc=0 : rc=0 : col1_width=20 : col2_pos=1+col1_width+1
	fnLbl(lc+=1,1,'Source Path:',col1_width,1)
	fnTxt(lc,col2_pos,42,80,0,'',0,'')
	resp$(resp_sourcePath:=rc+=1)=sourcePath$
	fnLbl(lc+=1,1,'Paperless Code:',col1_width,1)
	fnTxt(lc,col2_pos,8,0,0,'',0,'')
	resp$(resp_pcode:=rc+=1)=pcode$
	fnCmdSet(2)
fnacs2(mat resp$,ckey)
if ckey=5 then goto XIT
sourcePath$=resp$(resp_sourcePath)
pcode$=resp$(resp_pcode)
fnreg_write(env$('cap')&'.Source Path',sourcePath$)
fnreg_write(env$('cap')&'.Paperless Code',pcode$)
! /r
! Screen2
	fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	if fkey=93 or fkey=99 then goto Screen1
!

open   #hMopen:=fngethandle: 'Name=MASTER//6 ,KFName=MASTERX//6 ,shr',INTERNAL,Input,KEYED
open #hMclosed:=fngethandle: 'Name=HISTORY//1,KFName=HISTORYX//1,shr',INTERNAL,Input,KEYED
hActiveOpen=fnOpen_active(mat h_active_open)
hActiveClosed=fnClosed_active(mat h_active_closed)

dim sourceFile$(0)*256
transTime$=time$ 
transDate=Date('CYMD')
fnGetDirClient(sourcePath$,mat sourceFile$) 
moveCount=failCount=0
for sourceItem=1 to udim(mat sourceFile$)
	fileno$=fn_parseFileno$(sourceFile$(sourceItem))
	if fileno$='' then
		fn_reportAdd(sourceFile$(sourceItem),'Failed to parse file number from filename.')
		failCount+=1
	else if ~fnRead_Oc(fileno$,mat m$,mat mN,oc$,hMopen,hMclosed,'', '',mFormAll$) then
		fn_reportAdd(sourceFile$(sourceItem),'Could not find '&fileno$&' in open nor closed claims.')
		failCount+=1
	else
		dim destinationFilePath$*256
		destinationFilePath$=fnClaimFolder$(fileno$)&'\'&(sourceFile$(sourceItem)(pos(sourceFile$(sourceItem),'.')+1:inf))
		! if pos(sourceFile$(sourceItem),'TXP12974')>0 then pr 'TXP12974' : pause
		renameSuccess=fnRename(rtrm$(sourcePath$,'\')&'\'&sourceFile$(sourceItem),destinationFilePath$)
		! if ~renameSuccess and pos(sourceFile$(sourceItem),'TXP12974')>0 then pr 'TXP12974' : pause
		if renameSuccess then
			moveCount+=1
			dim pcmt$*9999
			pcmt$='Attachment Imported: '&destinationFilePath$
			fnPaper_note(fileno$,oc$,transDate,transTime$,pcode$,pcmt$,tag_user,status1_code$,status1_date$,status2_code$,status2_date$,scode,s_fcode)
			if enableReportSuccess then
				fn_reportAdd(sourceFile$(sourceItem),'SUCCESS. MOVED TO:  '&destinationFilePath$)
			end if
		else
			fn_reportAdd(sourceFile$(sourceItem),'Failed to copy.')
			failCount+=1
		end if
	end if
nex sourceItem
XIT: !
for x=1 to udim(mat h_active_closed)
	close #h_active_closed(x):
nex x
for x=1 to udim(mat h_active_open)
	close #h_active_open(x):
nex x
close #hMopen:
close #hMclosed:
fn_reportClose
fnxit
def fn_reportAdd(theFile$*256,theReason$*256; ___,alignOption$)
	if ~initReportReject then
		initReportReject=1
		pr #255: '</pre>'
		pr #255: '<table algin="Center">'
		pr #255: '  <tr><td colspan="2" align="Center"><h2>'&env$('program_caption')&'</h2></td></tr>'
		pr #255: '  <tr><td colspan="2" align="Center"><h3>as of '&date$('month d, ccyy')&' '&time$&'</h3></td></tr>'
	end if
	if theFile$(1:4)='<h4>' then
		alignOption$=' align="Right" '
	end if
	pr #255: '  <tr><td'&alignOption$&'>'&theFile$&'</td><td>'&theReason$&'</td></tr>'
fnend
def fn_reportClose
	fn_reportAdd('<h4>Files Moved:</h4>',str$(moveCount)&'</h4>')
	fn_reportAdd('<h4>Fails:<h4>',str$(failCount)&'</h4>')
	pr #255: '</table>'
	initReportReject=0
	fnClose
fnend
def fn_parseFileno$*8(filename$*256; ___,dpos,return$*8)
	filename$=trim$(filename$)
	dpos=pos(filename$,'.')
	if dpos>9 or dpos<=0 then
		return$=''
	else
		return$=filename$(1:dpos-1)
	end if
	fn_parseFileno$=return$
fnend

def fn_setup(&table$)
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnTop
		library 'S:\Core\Library': fnTos
		library 'S:\Core\Library': fnTxt,fnLbl
		library 'S:\Core\Library': fnCmdSet
		library 'S:\Core\Library': fnAcs2
		library 'S:\Core\Library': fnXit
		library 'S:\Core\Library': fnGetDirClient
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnRename
		! library 'S:\Core\Library': fnOpenPrn,fnClosePrn
		library 'S:\Core\Library': fnGetHandle
		library 'S:\Collection-Master Add-On\fn\Library.br': fnClaimFolder$

		library 'Library\SQL.wb': fnsql_setup$
		library 'Library\CLSUTIL.wb': fnRead_oc
		library 'Library\OPENFILE.wb': fnOpen_active,fnClosed_active
		library 'Library\TagUtil': fnPaper_note

		gosub Enum
		gosub SetupPrint

		dim m$(0)*128,mN(0)
		dim mFieldsC$(0)*20,mFieldsN$(0)*20
		dim mFormAll$*2048
		! execute '*SubProc '&     <--- not necessary with include:enum\forw  and  gosub Enumforw
		fnsql_setup$('master',mat m$,mat mN,mat mFieldsC$,mat mFieldsN$,mFormAll$)
		gosub EnumMaster

	end if
fnend
include: cm\enum\common
include: cm\enum\master
include: fn_open
include: cm\print
include: ertn