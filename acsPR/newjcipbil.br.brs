! Replace S:\acsPR\newjcipbil
! Enter (Job Cost) billings

	library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnmsgbox,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnemployee_srch,fncmbjob,fnflexinit1,fnflexadd1
	on error goto Ertn

	dim cap$*128,em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
	dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,io2b$(2)*20
	dim jn$*6,inp(3),ch2$(5),cm2$(5),d$*30,item2$(5)*30
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
	dim message$*40,ml$(3)*80,resp$(30)*60,fullname$(20)*20,comboname$(21)*23

	fntop(program$,cap$="Enter Billings")

	if exists("jcbillings."&wsid$) >0 then goto L180 else goto L200
	L180: !
	mat ml$(2) 
	ml$(1)="An unposted file appears to exist! " 
	ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries." 
	fnmsgbox(mat ml$,resp$,cap$,52)
	if resp$="Yes" then goto L220 else goto L200
	L200: !
	open #3: "Name=jcbillings."&wsid$&",SIZE=0,RecL=84,Replace",internal,outIn,relative 
	goto L230
	L220: !
	open #3: "Name=jcbillings."&wsid$,internal,outIn,relative 
	L230: !
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed 
	open #14: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,input,keyed 
	open #12: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
	open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed 

	addone=1 ! set code as adding when first entering

TRANSACTION_ENTRY: ! 
	if addone=1 then inp(1)=inp(3)=0
	fnTos(sn$="billinginput") 
	respc=0 : frac=0 
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Job Number:",mylen,1)
	fncmbjob(1,mypos) 
	resp$(respc+=1)=jn$
	fnLbl(2,1,"Amount:",mylen,1)
	fnTxt(2,mypos,10,10,0,"10",0,"Amount to be charged to job.") 
	resp$(respc+=1)=str$(inp(1))
	fnLbl(3,1,"Date:",mylen,1)
	fnTxt(3,mypos,8,8,0,"1",0,"Date of transaction") 
	resp$(respc+=1)=str$(inp(2))
	fnLbl(4,1,"Status:",mylen,1)
	fnTxt(4,mypos,2,2,0,"30",0,"") 
	resp$(respc+=1)=str$(inp(3))
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("Co&rrection",7,0,0,"Make a correction to any entry.")
	fnCmdKey("&LIsting",9,0,0,"Print a listing of all entries.")
	fnCmdKey("De&lete",4,0,0,"Deletes this entry.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnCmdKey("&Post",8,0,1,"Post these entries to the job files.")
	fnAcs(sn$,0,mat resp$,ckey) ! detail job screen     editrec
	if ckey=5 then goto L490 else goto L510
L490: mat ml$(2) 
	ml$(1)="You have chosen to cancel without postng these entries!  " 
	ml$(2)="Take Yes to Exit, else take No to return to the entry screens." 
	fnmsgbox(mat ml$,resp$,cap$,52)
	if resp$="Yes" then goto XIT else goto TRANSACTION_ENTRY
L510: if ckey=7 then goto CORRECTIONS
	if ckey=8 then goto POSTTOJOBS
	if ckey=9 then goto PRINTPROOFLIST
	jn$=lpad$(trim$(resp$(1)(1:6)),6) ! job #
	inp(1)=val(resp$(2)) ! amount
	inp(2)=val(resp$(3)) ! date
	inp(3)=val(resp$(4)) ! status
	if trim$(jn$)="" then goto L590 else goto L610
L590: mat ml$(2) 
	ml$(1)="You failed to enter a job number. Take Yes to continue;" 
	ml$(2)="else take No to return to previous screen and enter the job number." 
	fnmsgbox(mat ml$,resp$,cap$,52)
	if resp$="Yes" then goto L610 else goto TRANSACTION_ENTRY
L610: if addone=1 then goto L620 else goto L650
L620: write #3,using L640: jn$,mat inp
	goto L660
L640: form pos 1,c 6,pd 5.2,pd 4,n 2
L650: rewrite #3,using L640,rec=editrec: jn$,mat inp noRec CORRECTIONS
L660: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS

PRINTPROOFLIST: ! 
	on fkey 5 goto PROOF_LIST_DONE
	fnopenprn
	goto L810

PROOF_LIST_HDR: ! 
	pr #255,using L770: env$('cnam')
	pr #255,using L770: "Billing Proof List"
	pr #255,using L770: "Date: "&date$&"      Time: "&time$
L770: form pos 1,cc 113,skip 1
	pr #255: "Job #      Amount     Date   Status"
return 

L810: gosub PROOF_LIST_HDR
	for j=1 to lrec(3)
		read #3,using L640,rec=j: jn$,mat inp
		form pos 10,c 9,skip 1,pos 10,pic(-----,---.##),skip 1
		t1=0
		pr #255,using L870: jn$,mat inp pageoflow PROOF_LIST_NWPG
L870: form pos 1,c 6,x 1,pic(----,---.##),x 2,pic(zz/zz/zz),x 2,n 2,skip 1
		t1=t1+inp(1)
	next j
	pr #255,using L910: " __________",t1
L910: form pos 8,c 11,skip 1,pos 7,pic(-----,---.##),skip 1
PROOF_LIST_DONE: ! 
	gt1=0
	fncloseprn
goto TRANSACTION_ENTRY

POSTTOJOBS: ! 
	for j=1 to lrec(3)
		read #3,using L640,rec=j: jn$,mat inp noRec L1050
		if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1050
		read #11,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4 nokey L1050
		b3=b3+inp(1)
		if inp(3)><0 then b4=inp(3)
		rewrite #11,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4
L1050: next j
	close #3,free: 
goto XIT

PROOF_LIST_NWPG: ! 
	pr #255: newpage
	gosub PROOF_LIST_HDR
continue 

	if err=61 then goto TRANSACTION_ENTRY

XIT: fnxit

! INPUT FROM DISKETTE FILE    ! took this option out on new system

CORRECTIONS: ! 
	addone=0: editone=0
	fnTos(sn$="EntryCorrection")
	ch2$(1)="Rec #": ch2$(2)="Job #": ch2$(3)="Amount": ch2$(4)="Date #" 
	ch2$(5)="Status" 
	mat ch2$(5) ! : Mat CM2$(5) : Mat ITEM2$(5)
	cm2$(1)="30": cm2$(2)="": cm2$(3)="10" 
	cm2$(4)="1" 
	cm2$(5)="30" 
	cm2$(5): ch2$(5): item2$(5)
	fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
	restore #3: 
READ_FILE: ! 
	read #3,using L640: jn$,mat inp eof L1400
	item2$(1)=str$(rec(3)): item2$(2)=jn$ 
	item2$(3)=str$(inp(1)): item2$(4)=str$(inp(2)) 
	item2$(5)=str$(inp(3))
	fnflexadd1(mat item2$)
	goto READ_FILE
L1400: fnCmdKey("&Add",1,0,0,"Add a new transaction." ) 
	fnCmdKey("E&dit",2,1,0,"Edit the highlited record") 
	fnCmdKey("&Delete",4,0,0,"Deletes the highlited record") 
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information") 
	fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
	fnAcs(sn$,0,mat resp$,ckey) ! review_details  grid of transactions
	if ckey=5 then goto TRANSACTION_ENTRY
	editrec=val(resp$(1))
	if ckey=1 then addone=1: mat inp=(0): jn$="": goto TRANSACTION_ENTRY
	if ckey=2 then read #3,using L640,rec=editrec: jn$,mat inp: editone=1 : goto TRANSACTION_ENTRY
	if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
goto CORRECTIONS
include: Ertn
