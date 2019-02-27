! make a local test facility


fn_setup
tagTrigger$="NE-EFILE"
fn_testCmRead

goto Xit
def fn_testCmRead
	if ~setup_tag then
		setup_tag=1
		open #h_tags  :=fngethandle: "Name=Tags.int//6,kfname=Tags.idx//6,Shr",internal,outin,keyed
		open #hTagsClm:=fngethandle: "Name=Tags.int//6,kfname=Tags.clm//6,Shr",internal,outin,keyed
		open #hTagsCde:=fngethandle: "Name=Tags.int//6,kfname=Tags.cde//6,Shr",internal,outin,keyed
		open #hTagsDte:=fngethandle: "Name=Tags.int//6,kfname=Tags.dte//6,Shr",internal,outin,keyed
		dim tag$(0)*60,tagN(0),tag_fieldsc$(0)*20,tag_fieldsn$(0)*20,tag_formall$*512
		execute "*SubProc "&fnsql_setup$('Tags',mat tag$,mat tagN,mat tag_fieldsc$,mat tag_fieldsn$,tag_formall$)
		! let h_tags=fnopen_sql_file("tags",openclosed$) ! kps=1,9,13  kln=8,4,4
		open #hClaim:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,outin,keyed
		dim claim$(0)*128,claimN(0),claim_fieldsc$(0)*20,claim_fieldsn$(0)*20,claim_formall$*2048
		execute "*SubProc "&fnsql_setup$('Master',mat claim$,mat claimN,mat claim_fieldsc$,mat claim_fieldsn$,claim_formall$)
	end if

	restore #hTagsCde,key=>rpad$(tagTrigger$,kln(hTagsCde)):
	do
		read #hTagsCde,using tag_formall$: mat tag$,mat tagN eof EoTags
		tagReadCount+=1
		if tagTrigger$=rtrm$(tag$(tags_code)) then
			read #hClaim,using claim_formall$,key=tag$(tags_fileno): mat claim$,mat claimN
			dim allDebtor$(0,0)*256,allDebtorN(0,0)
			debtorCount=fnAllDebtors(MasterData$(master_fileno),hDebtor,Mat allDebtor$,Mat allDebtorN)
			fnlist_print('Processing FileNo '&claim$(master_fileno)&' with '&str$(debtorCount)&' debtors')
		end if
	loop while tagTrigger$=rtrm$(tag$(tags_code))
	EoTags: !
	pr 'tagReadCount=';tagReadCount
	pause
	
	close #h_tags  :
	close #hTagsClm:
	close #hTagsCde:
	close #hTagsDte:
	setup_tag=0
	close #hClaim:
fnend
Xit: end

def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fngethandle
		library "Library\SQL.wb": fnsql_setup
		library "Library\SQL.wb": fnsql_id_parse
		library "Library\SQL.wb": fnopen_sql_file
		library "Library\SQL.wb": fnsql_setup$
		library 'library\CLSUtil.wb': fnAllDebtors
		library 'library\GridIO.wb': fnlist_print
	end if
	on error goto Ertn
fnend
Ertn: ! r:
if err=4340 then 
	pr 'BR! Err 4340 SysErr='&str$(syserr)&' - '&syserr$
	pause
else
	pr 'error '&str$(err)&' on line '&str$(line)&' of '&os_Filename$(program$)
	pause
	retry
end if
! /r

