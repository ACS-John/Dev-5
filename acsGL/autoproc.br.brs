! Replace S:\acsGL\AutoProc
! GL - Begin Atomatic Processing
 
	autoLibrary
	fnTop(program$,cap$="Begin Automatic Processing")
	on error goto Ertn
 
	dim clnum(20),clnam$(20)*40,cnam$*40,a$*40,cap$*128,oldcnam$*40
	dim filename$(100)*40,opt$(100)*40,sys$*100,item$(2)*40,resp$(10)*100
 
	cap$="Begin Automatic Processing"
	fncno(cno,oldcnam$)
	open #glclnt=1: "Name=[Q]\GLmstr\glClnt.dat",internal,outIn,relative ioerr BUILD_GLCLNT
	rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=1: cno,cnam$
	goto L210
 
BUILD_GLCLNT: !
	open #glclnt=1: "Name=[Q]\GLmstr\glClnt.dat,Size=0,RecL=45",internal,outIn,relative
	for j=1 to 20 : _
		write #glclnt,using 'Form POS 1,N 5,C 40',rec=j: 0," " : _
	next j
 
L210: if trim$(mysys$)='' then : _
		sys$=fncursys$&"mstr" else : _
		sys$=mysys$&"mstr"
 
	gosub BLD_ACNO
 
MAIN: !
	fnTos(sn$="autoproc") : _
	respc=0
	fnLbl(1,20,"Select From the Following Companies:",50,1)
	fnLbl(2,1,"Company:",10,1)
	for j=1 to udim(opt$)
		if trim$(oldcnam$)=trim$(opt$(j)(1:30)) then resp$(1)=opt$(j): goto L330
	next j
	resp$(1)=opt$(1) ! default to 1st in none found
L330: fncomboa('CmbAuto',2,13,mat opt$,'Select the companies that should be included in this automatic processing run. Highlite and press enter or Next to register your selection.',55) ! fnCMBCNO(1, 13)
	fnLbl(4,30,"Selected Companies:")
	mat chdr$(2) : mat cmask$(2) : mat item$(2) : _
	chdr$(1)='Company #' : _
	chdr$(2)='Company Name'
	cmask$(1)='30' : _
	cmask$(2)='' : _
	fnflexinit1('autoproc',5,25,15,35,mat chdr$,mat cmask$,1,0,frame)
	for j=1 to max(count,1)
		item$(1)=str$(clnum(j)) : _
		item$(2)=clnam$(j) : _
		fnflexadd1(mat item$)
	next j
	fnCmdKey("&Select",1,1,0,"Selects the highlited company to be included in automatic processing.") : _
	fnCmdKey("C&omplete",2,0,0,"Finished selecting companies; begin porcessing.") : _
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=2 then goto L470
	clnam$(count+=1)=resp$(1)(1:30)
	clnum(count)=val(resp$(1)(33:37))
	if ckey=1 then goto MAIN
L470: goto WRITE_EM
 
BLD_ACNO: !
	dir$=fncursys$&"mstr" : _
	filter$="Company.*" : _
	fngetdir(dir$,mat filename$,empty$,filter$)
	mat acno(99): cav=0
L520: if trim$(filename$(fx+=1))="" then goto L580
	acno(cav+=1)=val(filename$(fx)(10:14)) conv L520
	end=len(filename$(fx))
	x=115: open #x: "Name="&sys$&"\Company.h"&filename$(fx)(10:14),internal,input ioerr L570 : _
	read #x,using "Form pos 1,c 40": cnam$ : _
	close #x:
	opt$(fx)=cnam$(1:30)&" ("&cnvrt$("pic(#####)",val(filename$(fx)(10:14)))&")"(1:40)
L570: goto L520
L580: mat opt$(fx)
return
WRITE_EM: !
	restore #glclnt:
	for j=1 to 20 : _
		rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=j: clnum(j),clnam$(j) : _
	next j
	goto BEGIN_AUTO
 
BEGIN_AUTO: !
	close #glclnt:
	execute "Load S:\Core\Process.br,RESIDENT"
	fnprocess(1)
	fnputcno(clnum(1)) : _
	fnpgnum(-1) : _
	! resets the last program processed back to 0 befor going to acglauto
	fnchain("S:\acsGL\acglAuto")
 
Xit: fnXit
 
include: ertn
