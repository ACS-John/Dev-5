! Maintain Department Matching GL Numbers
autoLibrary
fnTop(program$)
on error goto Ertn
! r: set/read constants and open files
	dim resp$(40)*128

	dim fullname$(20)*20,abbrevname$(20)*8,dedcode(20),calcode(20),dedfed(20),dedfica(20),dedst(20),deduc(20),gl$(20)*12
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
	
	dim label1$(22)*20
	label1$(1)='Fica Match'
	x=1
	for j=1 to 20
		if dedcode(j)=3 then label1$(x+=1)=rtrm$(fullname$(j))
	next j
	mat label1$(x)
	
	!                                             if ~exists('[Q]\PRmstr\MGLMstr.h[cno]') then  ! r: create files
	!                                             	close #1: ioerr ignore
	!   old should already be created in          	open #1: 'Name=[Q]\PRmstr\MGLMstr.h[cno],RecL=135,Replace',internal,output
	!      S:\Core\Check File Versions            	close #1:
	!                                             	execute 'Index [Q]\PRmstr\MGLMstr.h[cno],[Q]\PRmstr\MGLIdx1.h[cno],1,3,Replace,DupKeys'
	!                                             end if ! /r
	
	open #hMgl=fnH: 'Name=[Q]\PRmstr\MGLMstr.h[cno],KFName=[Q]\PRmstr\MGLIdx1.h[cno],Shr',internal,outIn,keyed
	open #hDept=fnH: 'Name=[Q]\PRmstr\DeptName.h[cno],KFName=[Q]\PRmstr\DeptNameIdx.h[cno],Shr',i,i,k
! /r

Menu1: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'Department:',15,1)
	fncombof('Dept',1,18,3,'[Q]\PRmstr\mglmstr.h[cno]',1,3,0,0,'[Q]\PRmstr\mglidx1.h[cno]',0,0, 'Set the matching g/l numbers for accruing payroll taxes by department. Choose a department.',0,0)
	resp$(respc+=1)=''
	fnCmdKey('&Add',1,0,0,'Enter accrual information on a new department.' )
	fnCmdKey('E&dit',2,1,0,'Change or review then highlighted record')
	fnCmdKey('&Delete',3,0,0,'Deletes the highlited record.')
	fnCmdKey('E&xit',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$) ! ask department #
	if ckey=5 then goto Xit
	dp$=resp$(1)
	dp$=lpad$(uprc$(rtrm$(dp$)),3)
	addrec=editrec=0
	if ckey=1 then addrec=1 else if ckey=2 then editrec=1 else if ckey=3 then goto RecordDelete ! /r

RecordAddEdit: ! r:
	dim deptname$*20
	deptname$=''
	read #hDept,using 'form pos 4,c 20',key=rpad$(ltrm$(dp$),3): deptname$ nokey ignore
	dim mgl$(11)*12
	if addrec=1 then
		dp$='': mat mgl$=('')
	else
		k$=dp$=lpad$(uprc$(rtrm$(dp$)),3)
		read #hMgl,using 'form pos 1,G 3,11*C 12',key=k$,release: dp$,mat mgl$ nokey Menu1
	end if
	fnTos
	respc=0
	fnLbl(1,1,deptname$,50,2)
	fnLbl(2,1,'Department:',15,1)
	fnTxt(2,18,3,3,1,'30',0,'Department # to change or add.')
	resp$(respc+=1)=dp$
	fnLbl(3,1,label1$(1),15,1)
	fnQgl(3,18,0,2)
	resp$(respc+=1)=fnrgl$(mgl$(1)) ! fica
	x=1 : y=3
	for j=1 to 20
		if dedcode(j)=3 then
			fnLbl(y+=1,1,label1$(x+=1),15,1)
			fnQgl(y,18,0,2)
			resp$(respc+=1)=fnrgl$(mgl$(j+1))
		end if
	next j
	fnCmdKey('&Next',1,1,0,'Save changes and move to next record' )
	fnCmdKey('&Complete',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$) ! ask gl numbers
	if ckey=5 then goto Xit
	dp$=resp$(1)
	dp$=lpad$(uprc$(rtrm$(dp$)),3)
	mgl$(1)=fnagl$(resp$(2)) ! fica
	x=2
	for j=1 to 20
		if dedcode(j)=3 then goto L610 else goto L620
L610: mgl$(j+1)=fnagl$(resp$(x+=1))
L620: !
	next j
if addrec=1 then goto RecordAdd else goto RecordRewrite ! /r

RecordDelete: ! r:
	dim ml$(3)*128
	mat ml$(2)
	ml$(1)='You have chosen to delete department '&dp$ ! '
	ml$(2)='Take OK to delete; else Cancel to retain the record.'
	fnmsgbox(mat ml$,resp$,'',1)
	if resp$='OK' then
		delete #hMgl,key=dp$: nokey Menu1
	end if
goto Menu1 ! /r
RecordRewrite: ! r:
	rewrite #hMgl,using 'form pos 1,G 3,11*C 12',key=dp$: dp$,mat mgl$ nokey RecordAdd
goto Menu1 ! /r
RecordAdd: ! r:
	write #hMgl,using 'form pos 1,G 3,11*C 12': dp$,mat mgl$
goto Menu1 ! /r

Xit: fnXit

include: ertn
