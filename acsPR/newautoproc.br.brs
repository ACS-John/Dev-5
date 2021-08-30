! Replace S:\acsPR\newAutoProc
! PR - Begin Atomatic Processing
autoLibrary
fnTop(program$,"Begin Automatic Processing")
on error goto Ertn
 
dim clnum(20),clnam$(20)*40
dim filename$(100)*40,opt$(100)*40
dim sys$*100
dim item$(2)*40
dim resp$(10)*100
dim wk(20),mo(20),qt(20)
goto BUILD_PRCLNT
 
BUILD_PRCLNT: ! r:
	open #prclnt=1: "Name=[Q]\PRmstr\prclnt.dat,Size=0,RecL=48,REPLACE",i,outi,r
	for j=1 to 20
		write #prclnt,using 'Form POS 1,N 5,C 40,3*N 1',rec=j: 0," ",0,0,0
	next j
 
	if trim$(mysys$)='' then sys$=fncursys$&"mstr" else sys$=mysys$&"mstr"
 
	gosub BLD_ACNO
goto Main ! /r
Main: ! r:
	fnTos
	respc=0
	fnLbl(1,20,"Select From the Following Companies:",50,1)
	fnLbl(2,1,"Company:",10,1)
	for j=1 to udim(opt$)
		if trim$(env$('cnam'))=trim$(opt$(j)(1:30)) then resp$(respc+=1)=opt$(j): goto L340
	next j
	resp$(1)=opt$(1) ! default to 1st in none found
	L340: !
	fncomboa('CmbAuto',2,13,mat opt$,'Select the companies that should be included in this automatic processing run. Highlite and press enter or Next to register your selection.',55) ! fnCMBCNO(1, 13)
	fnLbl(4,30,"Selected Companies:")
	mat chdr$(2) : mat cmask$(2) : mat item$(2)
	chdr$(1)='Company #'
	chdr$(2)='Company Name'
	cmask$(1)='30'
	cmask$(2)=''
	fnflexinit1('autoproc',5,25,15,35,mat chdr$,mat cmask$,1,0,frame)
	for j=1 to max(count,1)
		item$(1)=str$(clnum(j))
		item$(2)=clnam$(j)
		fnflexadd1(mat item$)
	next j
	fnFra(22,1,4,30,"Period to Print","Select the type of processing.")
	fnOpt(1,3,"Weekly",0,1)
	if wmq=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(2,3,"Monthly",0,1)
	if wmq=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(3,3,"Quarterly",0,1)
	if wmq=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnCmdKey("&Next",1,1,0,"Selects the highlited company to be included in automatic processing.")
	fnCmdKey("C&omplete",2,0,0,"Finished selecting companies; begin porcessing.")
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=2 and count>0 then goto L550
	clnam$(count+=1)=resp$(1)(1:30)
	clnum(count)=val(resp$(1)(33:37))
	if resp$(3)="True" then wk(count)=1: wmq=1
	if resp$(4)="True" then mo(count)=1: wmq=2
	if resp$(5)="True" then qt(count)=1: wmq=3
	if ckey=1 then goto Main
	L550: !
goto WRITE_EM ! /r
BLD_ACNO: ! r:
	dir$='[Q]\&'&fncursys$&"mstr"
	filter$="Company.*"
	fngetdir(dir$,mat filename$,empty$,filter$)
	mat acno(99999): cav=0
	do
		if trim$(filename$(fx+=1))="" then goto L660
		acno(cav+=1)=val(filename$(fx)(10:14)) conv L650
		end=len(filename$(fx))
		x=115
		open #x: "Name="&sys$&"\Company.h"&filename$(fx)(10:14),internal,input ioerr L650
		dim cnam$*40
		read #x,using "Form pos 1,c 40": cnam$
		close #x:
		opt$(fx)=cnam$(1:30)&" ("&cnvrt$("pic(#####)",val(filename$(fx)(10:14)))&")"(1:40)
		L650: !
	loop
	L660: !
	mat opt$(fx)
return ! /r
WRITE_EM: ! r:
	restore #prclnt:
	for j=1 to 20
		rewrite #prclnt,using 'Form POS 1,N 5,C 40,3*N 1',rec=j: clnum(j),clnam$(j),wk(j),mo(j),qt(j)
	next j
goto BEGIN_AUTO ! /r
BEGIN_AUTO: ! r:
	close #prclnt:
	execute "Load S:\Core\Process.br,RESIDENT"
	fnprocess(1)
	fnputcno(clnum(1))
	fnpgnum(-1) ! resets the last program processed back to 0 befor going to NEWPRauto
goto Xit !  /r
Xit: fnXit
include: ertn
