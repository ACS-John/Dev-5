fn_setup(table$)
on error goto Ertn
! pr 'table$=';table$
fntop(program$)
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

open   #hMopen:=fngethandle: 'Name=MASTER//6 ,KFName=MASTERX//6 ,shr',INTERNAL,Input,KEYED
open #hMclosed:=fngethandle: 'Name=HISTORY//1,KFName=HISTORYX//1,shr',INTERNAL,Input,KEYED
hActiveOpen=fnOpen_active(mat h_active_open)
hActiveClosed=fnClosed_active(mat h_active_closed)
	
dim sourceFile$(0)*256
transTime$=time$ 
transDate=Date('CYMD')
fnGetDir2(sourcePath$,mat sourceFile$)
for sourceItem=1 to udim(mat sourceFile$)
	fileno$=fn_parseFileno$(sourceFile$(sourceItem))
	if fileno$='' then
		fn_reportRejectAdd(sourceFile$(sourceItem),'Failed to parse file number from filename.')
	else if ~fnRead_Oc(fileno$,mat m$,mat mN,oc$,hMopen,hMclosed,'', '',mFormAll$) then
		fn_reportRejectAdd(sourceFile$(sourceItem),'Could not find '&fileno$&' in open nor closed claims.')
	else
		dim destinationFilePath$*256
		destinationFilePath$=fnClaimFolder$(fileno$)&'\'&(sourceFile$(sourceItem)(pos(sourceFile$(sourceItem),'.')+1:inf))
		if fnCopy(sourcePath$&'\'&sourceFile$(sourceItem),destinationFilePath$) then
			dim pcmt$*9999
			pcmt$
			FnPaper_note(fileno$,oc$,transDate,transTime$,pcode$,pcmt$,tag_user,status1_code$,status1_date$,status2_code$,status2_date$,scode,s_fcode)
		else
			fn_reportRejectAdd(sourceFile$(sourceItem),'Failed to copy.')
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
fnxit
def fn_reportRejectAdd(theFile$*256,theReason$*256)
	if ~initReportReject then
		initReportReject=1
		fnopenprn
		pr #255: '--==headers==--'
	end if
	pr theFile$&'   -   '&theReason$
fnend
def fn_reportRejectClose
	initReportReject=0
	fncloseprn
fnend
def fn_parseFileno$*8(filename$*256; ___,dpos,return$*8)
	dpos=pos(filename$,'.')
	if dpos>8 or dpos<=0 then
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
		library 'S:\Core\Library': fnGetDir2
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnOpenPrn,fnClosePrn
		library 'S:\Core\Library': fnGetHandle
		library 'S:\Collection-Master\fn\Library.br': fnClaimFolder$

		library 'Library\SQL.wb': fnsql_setup$
		library 'Library\CLSUTIL.wb': fnRead_oc
		library 'Library\OPENFILE.wb': fnOpen_active,fnClosed_active
		LIBRARY 'Library\TagUtil': FnPaper_note

		gosub Enum

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
include: ertn