! formerly S:\acsPR\newjcInptC
 
	autoLibrary
	on error goto Ertn
 
	dim em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
	dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,io2b$(2)*20
	dim jn2$*6,ji2(3),ch2$(8),cm2$(8),d$*30,item2$(8)*30
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
	dim message$*40,ml$(3)*80,resp$(30)*60,fullname$(20)*20,comboname$(21)*23
 
	fnTop(program$)
 
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	open #5: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed
	if exists("jccharges."&wsid$) >0 then
		mat ml$(2)
		ml$(1)="An unposted file appears to exist! "
		ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries."
		fnmsgbox(mat ml$,resp$,'',52)
		if resp$="Yes" then goto L260
	end if
	L240: !
	open #3: "Name=jccharges."&wsid$&",SIZE=0,RecL=84,Replace",internal,outIn,relative
	goto L270
	L260: !
	open #3: "Name=jccharges."&wsid$,internal,outIn,relative
	L270: !
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #14: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.h[cno],Shr",internal,input,keyed
	open #12: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed
 
	addone=1 ! set code as adding when first entering
 
TRANSACTION_ENTRY: !
	if addone=1 then ji2(3)=0
L360: !
	fnTos
	respc=0 : frac=0
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Reference #:",mylen,1)
	fnTxt(1,mypos,12,12,0,"",0,"Use any reference # that will help you identify the entry later on.")
	resp$(respc+=1)=rn$
	fnLbl(2,1,"Date:",mylen,1)
	fnTxt(2,mypos,8,8,0,"1",0,"Date of transaction")
	resp$(respc+=1)=str$(dat)
	fnLbl(3,1,"Job Number:",mylen,1)
	fncmbjob(3,mypos)
	resp$(respc+=1)=jn2$
	fnLbl(4,1,"Category:",mylen,1)
	fncmbcategory(4,mypos)
	resp$(respc+=1)=str$(ji2(1))
	fnLbl(5,1,"Sub-Category:",mylen,1)
	fncmbsubcat(5,mypos)
	resp$(respc+=1)=str$(ji2(2))
	fnLbl(6,1,"Amount:",mylen,1)
	fnTxt(6,mypos,10,10,0,"10",0,"Amount to be charged to job.")
	resp$(respc+=1)=str$(ji2(3))
	fnLbl(7,1,"Description:",mylen,1)
	fnTxt(7,mypos,30,30,0,"",0,"Use any description you choose.")
	resp$(respc+=1)=d$
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("Co&rrection",7,0,0,"Make a correction to any entry.")
	fnCmdKey("&LIsting",9,0,0,"Print a listing of all entries.")
	fnCmdKey("De&lete",4,0,0,"Deletes this entry.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnCmdKey("&Post",8,0,1,"Post these entries to the job files.")
	fnAcs(mat resp$,ckey) ! detail job screen     editrec
	if ckey=5 then goto L590 else goto L610
L590: mat ml$(2)
	ml$(1)="You have chosen to cancel without postng these entries!  "
	ml$(2)="Take Yes to Exit, else take No to return to the entry screens."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto Xit else goto TRANSACTION_ENTRY
L610: if ckey=7 then goto CORRECTIONS
	if ckey=8 then goto POSTTOJOBS
	if ckey=9 then goto PRINTPROOFLIST
	rn$=resp$(1) ! reference #
	dat=val(resp$(2)) ! date
	jn2$=lpad$(trim$(resp$(3)(1:6)),6) ! job number
	if trim$(jn2$)="" then goto L690 else goto L710
L690: mat ml$(2)
	ml$(1)="You failed to enter a job number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the job number."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto L710 else goto L740
L710: ji2(1)=val(resp$(4)(1:5)) ! category
	if ji2(1)=0 and dontwarnsubcat=0 then goto L730 else goto L750
L730: mat ml$(2)
	ml$(1)="You failed to enter a category number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the category number."
	fnmsgbox(mat ml$,resp$,'',52)
L740: if resp$="Yes" then dontwarnsubcat=1: goto L750 else goto L360
L750: ji2(2)=val(resp$(5)(1:3)) ! sub-category
	if ji2(2)=0 and dontwarnsubcat=0 then
		goto L770
	else
		goto L790
	end if
	L770: !
	mat ml$(2)
	ml$(1)="You failed to enter a sub-category number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the sub-category number."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then dontwarnsubcat=1 : goto L790 else goto L360
	L790: !
	ji2(3)=val(resp$(6)) ! amount
	d$=resp$(7) ! description
	if addone=1 then goto L820 else goto L850
L820: write #3,using L840: rn$,dat,jn2$, mat ji2,d$
	goto L860
L840: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
L850: rewrite #3,using L840,rec=editrec: rn$,dat,jn2$, mat ji2,d$ noRec CORRECTIONS
L860: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS
 
PRINTPROOFLIST: !
	on fkey 5 goto PROOF_LIST_DONE
	fnopenprn
goto L1010
 
PROOF_LIST_HDR: ! r:
	pr #255,using L970: env$('cnam')
	pr #255,using L970: "Charges Proof List"
	pr #255,using L970: "Date: "&date$&"      Time: "&time$
	L970: form pos 1,cc 113,skip 1
	pr #255: "Ref #      Date     Job #  Category  Sub-Cat Amount  Description"
return ! /r
 
L1010: ! r:
	gosub PROOF_LIST_HDR
	for j=1 to lrec(3)
		read #3,using L840,rec=j: rn$,dat,jn2$, mat ji2,d$
		if j<>1 then
			if ji1(1)=en then goto L1120
			pr #255,using L1070: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
			L1070: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
		end if
		en=ji1(1)
		t5=0
		t6=0
		t10=0
		L1120: !
		pr #255,using L1130: rn$,dat,jn2$,mat ji2 pageoflow PROOF_LIST_NWPG
		L1130: form pos 1,c 12,x 1,n 8,x 1,n 5,x 1,pic(---,---.##),x 2,c 30,skip 1
		t5=t5+ji1(5)
		t6=t6+ji1(6)
		t10=t10+ji2(3)
		gt5=gt5+ji1(5)
		gt6=gt6+ji1(6)
		gt10=tg10+ji2(3)
	next j
	pr #255,using L1070: " ________"," ________"," ____________",t5,t6,t10
	pr #255,using L1230: " ________"," ________"," ____________",gt5,gt6,gt10
L1230: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
PROOF_LIST_DONE: !
	gt5=gt6=gt10=0
	fncloseprn
goto TRANSACTION_ENTRY ! /r
 
POSTTOJOBS: ! r:
	close #1:
	close #2:
	close #3:
	close #11:
	close #12:
	close #13:
fnchain("S:\acsPR\NEWJCMRGC") ! /r
 
PROOF_LIST_NWPG: ! r:
	pr #255: newpage
	gosub PROOF_LIST_HDR
continue ! /r
 
Xit: fnXit
 
! INPUT FROM DISKETTE FILE    ! took this option out on new system
 
CORRECTIONS: !
	addone=0: editone=0
	fnTos
	ch2$(1)="Rec #": ch2$(2)="Ref #": ch2$(3)="Date": ch2$(4)="Job #"
	ch2$(5)="Cat"
	ch2$(6)="Sub-Cat": ch2$(7)="Amount": ch2$(8)="Description"
	mat ch2$(8) ! : Mat CM2$(8) : Mat ITEM2$(8)
	cm2$(1)="30": cm2$(2)="": cm2$(3)="1"
	cm2$(4)=""
	cm2$(5)="30": cm2$(6)="30": cm2$(7)="10"
	cm2$(8)=""
	cm2$(8): ch2$(8): item2$(8)
	fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
	restore #3:
	do
		read #3,using L840: rn$,dat,jn2$, mat ji2,d$ eof L1690
		item2$(1)=str$(rec(3)): item2$(2)=rn$
		item2$(3)=str$(dat): item2$(4)=jn2$
		item2$(5)=str$(ji2(1)): item2$(6)=str$(ji2(2))
		item2$(7)=str$(ji2(3)) : item2$(8)=d$
		fnflexadd1(mat item2$)
	loop
	L1690: !
	fnCmdKey("&Add"    ,1,0,0,"Add a new transaction." )
	fnCmdKey("E&dit"   ,2,1,0,"Edit the highlited record")
	fnCmdKey("&Delete" ,4,0,0,"Deletes the highlited record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information")
	fnCmdKey("E&Xit"   ,5,0,1,"Returns to main screen.")
	fnAcs(mat resp$,ckey) ! review_details  grid of transactions
	if ckey=5 then goto TRANSACTION_ENTRY
	editrec=val(resp$(1))
	if ckey=1 then addone=1: mat ji2=(0): jn2$="": goto TRANSACTION_ENTRY
	if ckey=2 then read #3,using L840,rec=editrec: rn$,dat,jn2$, mat ji2,d$: editone=1 : goto TRANSACTION_ENTRY
	if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
goto CORRECTIONS
include: ertn
