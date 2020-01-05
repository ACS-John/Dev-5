! formerly S:\acsPR\newjcSCMint

	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fnTos,fnLbl,fnCmdKey,fnAcs
	library 'S:\Core\Library': fnTxt
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fnsubcat_srch
	library 'S:\Core\Library': fncmbsubcat
	on error goto Ertn

	dim iom$(4),scm$(4)*27,resp$(5)*50
	dim cde$*3,des$*30,sc$*3,cnt$*25,message$*40

	fntop(program$)
	open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,outIn,keyed 
	Fprmstr: form pos 1,c 3,c 30

ASKSUBCAT: ! r:
	fnsubcat_srch(cde$,ckey,fixgrid)
	cde$=lpad$(rtrm$(cde$),3)
	if ckey=97 then 
		ti1=addsubcat=1 
		goto ADDREC 
	else if ckey=98 then 
		goto EDITREC 
	else if ckey=3 then 
		read #1,using "Form POS 1,C 3,C 25": cde$,des$ eof ASKSUBCAT
		cde$=uprc$(lpad$(rtrm$(cde$),3))
		holdcde$=cde$
		goto EDITREC
	else if ckey=96 then 
		goto DELETE_RECORD
	else if ckey=94 then 
		gosub SUBCAT_LISTING
	else if ckey=5 then 
		goto XIT
	end if
goto ASKSUBCAT ! /r

ADDREC: ! r:
	cde$="": des$=""
	fnTos(sn$="Ask-sub-cat") 
	respc=0
	fnLbl(1,1,"Sub-category #:",16,right)
	fnTxt(1,19,3,3,0,"30",0,"Assign any number that has not been used before.") 
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Record this sub category record.") 
	fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
	fnAcs(sn$,0,mat resp$,ckey) ! add sub-category #
	if ckey=5 then goto ASKSUBCAT
	cde$=lpad$(rtrm$(resp$(1)),3)
	read #1,using Fprmstr,key=cde$: cde$,des$ nokey L350
	goto EDITREC
	L350: !
	write #1,using Fprmstr: cde$,des$: new1=1
	goto EDITREC
	mat ml$(2) 
	ml$(1)="A record with this number already exists!" 
	ml$(2)="Select a different subcategory number." 
	fnmsgbox(mat ml$,resp$,'',48) 
goto ADDREC ! /r

EDITREC: ! r:
	holdcde$=cde$
	read #1,using Fprmstr,key=cde$: cde$,des$ nokey ignore
	fnTos(sn$="Edit-sub-cat") 
	respc=0
	fnLbl(1,1,"Sub-category #:",16,right)
	fnTxt(1,19,3,3,0,"30",0,"Can be any three digit number.") 
	resp$(respc+=1)=cde$
	fnLbl(2,1,"Description:",16,right)
	fnTxt(2,19,30,30,0,"",0,"") 
	resp$(respc+=1)=des$
	fnCmdKey("&Next"			,1,1,0,"Record any changes & return to main screen.") 
	fnCmdKey("&Add" 			,2,0,0,"Save these changes and then add a new record." ) 
	fnCmdKey("&Delete"		,4,0,0,"Delete this sub-category record." ) 
	fnCmdKey("E&xit"			,5,0,1,"Returns to main screen.")
	fnAcs(sn$,0,mat resp$,ckey) ! edit sub-category
	if ckey=5 then goto ASKSUBCAT
	cde$=lpad$(rtrm$(resp$(1)),3)
	des$=resp$(2)
	if ckey=4 then goto DELETE_RECORD
	if holdcde$<>cde$ then goto L540 else goto Save1
	L540: !
	mat ml$(2) 
	ml$(1)="You are attempting to change the sub-category # from "&holdcde$ 
	ml$(2)="to "&cde$&".  Take OK to continue, else cancel." 
	fnmsgbox(mat ml$,resp$,'',48)
	if resp$="OK" then 
		goto Save1 
	end if
goto EDITREC ! /r
Save1: ! r:
	rewrite #1,using Fprmstr,key=cde$: cde$,des$ nokey L590
	if ckey=2 then goto ADDREC
	goto ASKSUBCAT
	L590: !
	write #1,using Fprmstr: cde$,des$
	if ckey=2 then goto ADDREC
goto ASKSUBCAT ! /r

DELETE_RECORD: ! r:
	delete #1,key=cde$: nokey ignore
goto ASKSUBCAT ! /r

FixScMstr_Unused: ! r:  the following lines are not used and are only there if necessary to fix a file
	close #1: ioerr ignore
	open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno]",internal,output 
	close #1,free: ioerr ignore
	open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],SIZE=0,RecL=33",internal,output 
	close #1: 
	execute "Index [Q]\PRmstr\SCMSTR.h[cno],[Q]\PRmstr\SCIndex.h[cno],1,3,Replace,DupKeys"
goto XIT ! /r



SUBCAT_LISTING: ! r:
	on fkey 5 goto L910
	fnopenprn
	restore #1,key>="   ": nokey ASKSUBCAT
	gosub Header
	do
		read #1,using Fprmstr: cde$,des$ eof L910
		pr #255,using 'form pos 16,c 5,c 30': cde$,des$ pageoflow L870
	loop
	L870: !
	pr #255: newpage
	gosub Header
continue ! /r

SRCHEND: ! r:
	close #win: ioerr ignore
goto ASKSUBCAT ! /r
L910: ! r:
	on fkey 5 ignore 
	fncloseprn
goto ASKSUBCAT ! /r

Header: ! r:
	pr #255,using 'form pos 1,c 8,cc 52': date$,env$('cnam')
	pr #255,using 'form pos 1,c 8,pos 11,cc 50,skip 2': time$,"Sub-Category File Listing"
	pr #255: tab(15);"Code  Description"
	pr #255: tab(15);"____  ______________________________"
return ! /r
XIT: fnxit
include: ertn