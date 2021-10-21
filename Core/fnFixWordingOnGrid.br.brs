def library fnFixWordingOnGrid(ev$*50,outputfile$*50)
	autoLibrary
	! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
	! to create your own file instead of using this program, store the description,variable name,field length,# of deciaml points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
	! if you create the display file, as just described, create a folder under your program folder called GRID; a subfolder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these subfolders (actually one for each file you are allowing them to access with the grid programs.
	! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
	! you will have to create your folders as described above; this routine will not create the folders
	dim a$(200,3)*40
	dim a(200,6)
	dim abbrev$*30
	dim ln$*80
	if env$('cursys')='PR' then
		dim fullname$(20)*20
		dim abbrevname$(20)*8
		fnDedNames(mat fullname$,mat abbrevname$)
	else if env$('cursys')='UB' then
			dim serviceName$(10)*20
			dim srv$(10)*2
		fnGetServices(mat serviceName$,mat srv$)
	end if
	open #2: 'Name='&ev$,display,input
	open #15: 'Name=[Temp]\Temp.[Session],KFName=[Temp]\TempIdx.[Session],RecL=87,KPs=1,KLn=30,Replace',internal,outIn,keyed
	do
		linput #2: ln$ eof Finis
		ln$=srep$(ln$,chr$(9),'')
		if uprc$(ln$(7:10))<>'DATA' then goto NextEv
		DATALN: !
		j3=1
		p1=11
		p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,'^',p1+1)
		p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,'^',p2+1)
		p4=pos(srep$(ln$,'^','~'),'~',p3+1) ! pos(ln$,'^',p3+1)
		p5=len(rtrm$(ln$))
		a$(j3,1)=ln$(p1+1:p2-1)
		a$(j3,1)=fnbooktitle$(a$(j3,1))
		a$(j3,2)=ln$(p2+1:p3-1)
		a$(j3,3)=ln$(p3+1:p4-1) ! P3+1:P4-1) ! MAX(P4-1,P3+8))  this was modified for ea
		if p4=0 then  ! if layout does not contail abbreviated name, then use first 12 characters of real name
			abbrev$=a$(j3,1)(1:12)
		else
			abbrev$=ln$(p4+1:len(ln$))(1:20)
		end if
		! If RTRM$(A$(J3,3))='' Then Goto 850
		p1=pos(a$(j3,3),' ',1)+1
		p2=pos(a$(j3,3),'.',1)+1
		p3=len(rtrm$(a$(j3,3))) ! was standard
		! p3=pos(srep$(ln$,'^','~'),'~',1)-1 ! POS(A$(J3,3),'^',1)-1 ! for acsea and acscl only  (way John does layouts)
		p4=pos(a$(j3,3),'*',1)
		if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
		l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
		if p2>1 then dp=val(a$(j3,3)(p2:p3)) else dp=0           ! DECIMAL POSITIONS
		if uprc$(a$(j3,3)(1:p1-2))='PD' then al=l*2-1 else al=l           !   ACTUAL FIELD LENGTH
		if uprc$(a$(j3,3)(1:1))='X' then goto NextEv ! skip any formats of 'x'
		l=l*m1 ! TOTAL STORAGE LENGTH
		b=a+l
		a=a+1
		ino=ino+1
		j3=1
		a(j3,1)=ino
		a(j3,2)=al
		a(j3,3)=dp
		a(j3,4)=l
		a(j3,5)=a
		a(j3,6)=b
		a=b
		rl=rl+int(val(a$(j3,3)(p1:p3)))*m1



			if env$('cursys')='PR' then

				! r: PAYROLL - PLACE CORRECT SERVICE NAME ON EACH SERVICE
				if uprc$(a$(j3,1)(1:4))<>'MISC' then goto L840
				x=val(a$(j3,1)(8:9)) conv L840
				if trim$(fullname$(x))='' then goto NextEv ! SERVICE NOT USED
				a$(j3,1)=fullname$(x)
				! If UPRC$(ABBREV$)(1:4)='MISC' Then Pause
				if uprc$(abbrev$)(1:4)<>'MISC' then goto L840
				x=val(abbrev$(5:6)) conv L840
				abbrev$=''
				abbrev$=trim$(abbrevname$(x))(1:9)
				! /r
			else if env$('cursys')='UB' then
				! r: UTILITY BILLING - PLACE CORRECT SERVICE NAME ON EACH SERVICE
				if uprc$(a$(j3,1)(1:7))<>'SERVICE' then goto Pr850
				x=val(a$(j3,1)(9:10)) conv Pr850
				if trim$(serviceName$(x))='' then goto NextEv ! SERVICE NOT USED
				a$(j3,1)(1:9)=''
				if x=3 and trim$(serviceName$(x))<>'Electric' and srv$(3)='EL' then goto Pr840
				if x=4 and trim$(serviceName$(x))<>'Gas' and srv$(4)='GA' then goto Pr840 ! gas or electric used for some some reading other that gas or electric (code must be GA or EL for this to work
				if x=3 and trim$(serviceName$(x))<>'Electric' and a$(j3,1)(2:6)='Prior' then goto NextEv
				if x=3 and trim$(serviceName$(x))<>'Electric' and (a$(j3,1)(2:6)='Multi' or a$(j3,1)(2:6)='Elect' or a$(j3,1)(2:6)='Depos' or a$(j3,1)(2:6)='Readi' or a$(j3,1)(2:6)='Used-' or a$(j3,1)(2:6)='Kwh  ' or a$(j3,1)(2:6)='Deman' or a$(j3,1)(2:6)='Units' or a$(j3,1)(2:6)='Prior') then goto NextEv
				if x=4 and trim$(serviceName$(x))<>'Gas' and (a$(j3,1)(2:6)='Multi' or a$(j3,1)(2:6)='Elect' or a$(j3,1)(2:6)='Depos' or a$(j3,1)(2:6)='Readi' or a$(j3,1)(2:6)='Used-' or a$(j3,1)(2:6)='Kwh  ' or a$(j3,1)(2:6)='Deman' or a$(j3,1)(2:6)='Units' or a$(j3,1)(2:6)='Meter') then goto NextEv
				if x=4 and trim$(serviceName$(x))<>'Gas' and a$(j3,1)(2:6)='Prior' then goto NextEv
				Pr840: !
				a$(j3,1)=trim$(serviceName$(x))&' '&trim$(a$(j3,1))
				Pr850: !
				if uprc$(abbrev$)(1:7)<>'SERVICE' then goto Pr890
				x=val(abbrev$(9:10)) conv Pr890
				abbrev$(1:9)=''
				abbrev$=trim$(serviceName$(x))&' '&trim$(abbrev$)
				Pr890: !
				if rtrm$(a$(j3,1))='' or rtrm$(uprc$(a$(j3,1)))='UNUSED' or rtrm$(uprc$(a$(j3,1)))(2:6)='EXTRA' or trim$(abbrev$)='' then goto NextEv
				! /r
			else
				pr program$&' does not know how to process cursys ('&env$('cursys')&')'
				pause
			end if

		L840: ! store as description,variable name,field length,# of deciaml points, format
		write #15,using L860: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
		L860: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
		NextEv: !
	loop

	Finis: !
	close #2: ioerr ignore
	close #15: ioerr ignore
	gosub MoveItToText
	Xit: !
fnend

MoveItToText: ! r:
	dim miitLine$*87
	open #hMiitIn =fnH: 'Name=[Temp]\Temp.[Session],KFName=[Temp]\TempIdx.[Session],RecL=87,KPs=1,KLn=30,use',internal,outIn,keyed
	open #hMiitOut=fnH: 'Name='&outputfile$&',RecL=87,Replace',d,o
	Fmitt: form pos 1,c 87
	do
		read #hMiitIn,using Fmitt: miitLine$ eof MittFinis
		pr #hMiitOut,using Fmitt: miitLine$
	loop
	MittFinis: !
	close #hMiitIn:
	hMiitIn=0
	close #hMiitOut:
	hMiitOut=0
return ! /r
include: ertn