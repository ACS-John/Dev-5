! Maintain Department Matching GL Numbers
 
	autoLibrary
	on error goto Ertn
 
	dim label1$(22)*20,io2$(34),mgl$(11)*12
	dim rpnames$(86)*20,label1p$(12)*14,io1$(6),wrd1$(6)*40,err$(2)*65
	dim fullname$(20)*20,abbrevname$(20)*8,calcode(20),dedfed(20),dedfica(20)
	dim dedst(20),deduc(20),gl$(20)*12,ml$(3)*80,resp$(15)*60,dedcode(20)
	dim deptname$*20
	dim cap$*128
 
	fnTop(program$,cap$="Accrued Payroll Tax Information")
	gosub BLDSCR
 
	if exists("[Q]\PRmstr\MGLMstr.h[cno]")=0 then gosub CREATE_FILES
	open #1: "Name=[Q]\PRmstr\MGLMstr.h[cno],KFName=[Q]\PRmstr\MGLIdx1.h[cno],Shr",internal,outIn,keyed
	open #9: "Name=[Q]\PRmstr\DeptName.h[cno],KFName=[Q]\PRmstr\DeptNameIdx.h[cno],Shr",internal,input,keyed
 
MENU1: !
ASKDEPARTMENT: !
	fnTos(sn$="Department-ask") : _
	respc=0
	fnLbl(1,1,"Department #:",15,1)
	fncombof("Dept",1,18,3,"[Q]\PRmstr\mglmstr.h[cno]",1,3,0,0,"[Q]\PRmstr\mglidx1.h[cno]",0,0, "Set the matching g/l numbers for accruing payroll taxes by department. Choose a department.",0,0) : _
	resp$(respc+=1)=""
	fnCmdKey("&Add",1,0,0,"Enter accrual information on a new department." ) : _
	fnCmdKey("E&dit",2,1,0,"Change or review then highlighted record") : _
	fnCmdKey("&Delete",3,0,0,"Deletes the highlited record.") : _
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey) ! ask department #
	if ckey=5 then goto Xit
	dp$=resp$(1)
	dp$=lpad$(uprc$(rtrm$(dp$)),3)
	addrec=editrec=0
	if ckey=1 then addrec=1 else : _
		if ckey=2 then editrec=1 else : _
			if ckey=3 then goto DELETE_RECORD
 
ADD_EDIT_REC: !
	deptname$="": read #9,using "form pos 4,c 20",key=rpad$(ltrm$(dp$),3): deptname$ nokey L370
L370: if addrec=1 then dp$="": mat mgl$=(""): goto L400
	k$=dp$=lpad$(uprc$(rtrm$(dp$)),3)
	read #1,using "Form POS 1,G 3,11*C 12",key=k$,release: dp$,mat mgl$ nokey MENU1
L400: fnTos(sn$="Department-gl") : _
	respc=0
	fnLbl(1,1,deptname$,50,2)
	fnLbl(2,1,"Department #:",15,1)
	fnTxt(2,18,3,3,1,"30",0,"Department # to change or add.") : _
	resp$(respc+=1)=dp$
	fnLbl(3,1,label1$(1),15,1)
	fnqgl(3,18,0,2,0) : _
	resp$(respc+=1)=fnrgl$(mgl$(1)) ! fica
	x=1 : y=3
	for j=1 to 20
		if dedcode(j)=3 then goto L490 else goto L510
L490: fnLbl(y+=1,1,label1$(x+=1),15,1)
		fnqgl(y,18,0,2,0) : _
		resp$(respc+=1)=fnrgl$(mgl$(j+1))
L510: next j
	fnCmdKey("&Next",1,1,0,"Save changes and move to next record" ) : _
	fnCmdKey("&Complete",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey) ! ask gl numbers
	if ckey=5 then goto Xit
	dp$=resp$(1)
	dp$=lpad$(uprc$(rtrm$(dp$)),3)
	mgl$(1)=fnagl$(resp$(2)) ! fica
	x=2
	for j=1 to 20
		if dedcode(j)=3 then goto L610 else goto L620
L610: mgl$(j+1)=fnagl$(resp$(x+=1))
L620: next j
	if addrec=1 then goto ADD_RECORD else goto REWRITE_RECORD
 
DELETE_RECORD: !
	mat ml$(2) : _
	ml$(1)="You have chosen to delete department # "&dp$ ! " : _
	ml$(2)="Take OK to delete; else Cancel to retain the record." : _
	fnmsgbox(mat ml$,resp$,cap$,1)
	if resp$="OK" then goto L680 else goto MENU1
L680: delete #1,key=dp$: nokey MENU1
	goto MENU1
REWRITE_RECORD: !
	rewrite #1,using "Form POS 1,G 3,11*C 12",key=dp$: dp$,mat mgl$ nokey L740
	goto MENU1
ADD_RECORD: !
L740: write #1,using "Form POS 1,G 3,11*C 12": dp$,mat mgl$
	goto MENU1
 
CREATE_FILES: !
	close #1: ioerr L790
L790: close #2: ioerr L800
L800: open #1: "Name=[Q]\PRmstr\MGLMstr.h[cno],RecL=135,Replace",internal,output
	close #1:
	execute "Index [Q]\PRmstr\MGLMstr.h[cno],[Q]\PRmstr\MGLIdx1.h[cno],1,3,Replace,DupKeys"
return
 
	restore #1:
	pg=0
	hp1=66-int(len(rtrm$(env$('cnam')))/2)
	fnopenprn (cp,58,230,process)
	gosub HDR4
L900: read #1,using "Form POS 1,G 3,11*C 12",release: dp$,mat mgl$ eof END4
	pr #255,using L920: dp$,mat mgl$ pageoflow NWPG
L920: form pos 1,c 6,11*c 14,skip 1
	goto L900
 
NWPG: pr #255: newpage: gosub HDR4: continue
 
HDR4: pg+=1
	pr #255,using L990: "Page",pg,env$('cnam')
L990: form pos 1,c 4,n 4,pos hp1,c 40,skip 1
	pr #255: date$;tab(50);"Department GL Number File Listing"
	pr #255:
	pr #255,using L920: mat label1p$
	pr #255: "____  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
return
 
END4: on fkey 5 ignore
	if nw=0 then pr #255: newpage
	fncloseprn
	goto MENU1
 
BLDSCR: !
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
	label1$(1)="Fica Match"
	x=1
	for j=1 to 20
		if dedcode(j)=3 then goto L1170 else goto L1180
L1170: label1$(x+=1)=rtrm$(fullname$(j))
L1180: next j
	mat label1$(x)
return
 
Xit: fnXit
 
include: Ertn
