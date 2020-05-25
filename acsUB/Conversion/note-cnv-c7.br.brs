! Replace S:\acsUB\Conversion\Note-Cnv-c7
! Note1 and Note2 conversion program - converts them up to version 1 with account length of 10 (adds .00)
def library fnub_cnv_note_phase_1
  dim rm$*60,ra(2),newra(2)
  autoLibrary
! 
! r: ** phase 1 **
  open #note1=fnGetHandle: "Name=[Q]\UBmstr\Note1.h[cno]",internal,outIn,relative 
	if version(note1)=0 and rln(note1)=16 then 
		let version(note1,1)
    close #note1: 
    fnStatus("[Q]\UBmstr\Note1.h[cno] was already in version 1 format.  Version number corrected.")
		goto EOPHASE1
	end if
  if version(note1)=>1 then 
    close #note1: 
    fnStatus("[Q]\UBmstr\Note1.h[cno] is already at least version 1")
    goto EOPHASE1
  else
    fnStatus("converting [Q]\UBmstr\Note1.h[cno] to version 1")
  end if
  open #work=fnGetHandle: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=16",internal,output 
  READ_NOTE1_PHASE1: ! 
  read #note1,using 'Form POS 1,C 7,2*PD 3': z$,a1,a2 eof EO1
  write #work,using 'Form POS 1,C 7,C 3,2*PD 3': z$,".00",a1,a2
  goto READ_NOTE1_PHASE1
  !
  EO1: ! 
  close #note1: 
  close #work: 
  execute "Free [Q]\UBmstr\Note1.h[cno] -n"
  execute "Rename "&env$('temp')&"\Work."&session$&' '&"[Q]\UBmstr\Note1.h[cno] -n"
  fnCopy("[Q]\UBmstr\Note2.h[cno]","[Q]\UBmstr\Note2.h[cno]",73) 
  fnindex_it("[Q]\UBmstr\Note1.h[cno]","[Q]\UBmstr\NoteIdx1.h[cno]","1 10")
  open #note1=fnGetHandle: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
  version(note1,1)
  close #note1: ioerr ignore
  fnStatus("[Q]\UBmstr\Note1.h[cno] converted successfully to version 1.")
  goto EOPHASE1
  !
	EOPHASE1: ! /r
	! r: *** Phase 2 ***" 
	open #note1=fngethandle: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
	open #note2=fngethandle: "Name=[Q]\UBmstr\Note2.h[cno]",internal,outIn,relative 
	if version(note2)=>1 then 
		close #note2: ioerr ignore
		fnStatus("[Q]\UBmstr\Note2.h[cno] is already at least version 1")
		goto EOPHASE2
	else
		fnStatus("converting [Q]\UBmstr\Note2.h[cno] to version 1")
	end if
	fnStatus("Initial Record Len of "&file$(note2)&" is "&str$(rln(note2))&".")
	READ_NOTE1_PHASE2: ! 
	read #note1,using 'Form POS 1,C 10,2*PD 3': rk$,mat ra eof EO3
	r32=ra(1)
	do
		if r32<1 then goto READ_NOTE1_PHASE2
		read #note2,using 'Form POS 1,C 07,C 60,PD 3',rec=r32: k32$,rm$,n32 conv READ_NOTE1_PHASE2
		rewrite #note2,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,n32
		r32=n32
	loop
	!
	EO3: ! /r
	version(note1,1)
	version(note2,1)
	close #note1: ioerr ignore
	close #note2: ioerr ignore
	fnStatus("[Q]\UBmstr\Note2.h[cno] converted successfully to version 1")
	goto EOPHASE2
	!
	EOPHASE2: ! 
	! ** Phase 3 **
	! Note conversion program
	if exists("[Q]\UBmstr\Customer.h[cno]") then 
		open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]",internal,input,relative 
	else if exists("[Q]\UBmstr\ubMaster.h[cno]") then
		open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\ubMaster.h[cno]",internal,input,relative 
	end if
	fnindex_it("[Q]\UBmstr\Note1.h[cno]","[Q]\UBmstr\NoteIdx1.h[cno]","1 10")
	open #note1b:=fngethandle: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
	open #note2b:=fngethandle: "Name=[Q]\UBmstr\Note2.h[cno]",internal,outIn,relative 
	L50100: !
	read #hCustomer,using 'Form POS 1,C 10': z$ eof EO4
	if z$(8:10)=".00" then goto L50100 ! skip base records
	x$=z$(1:7)&".00"
	mat newra=(0)
	read #note1b,using 'Form POS 1,C 10,2*PD 3',key=x$: rk$,mat ra nokey L50100
	write #note1b,using 'Form POS 1,C 10,2*PD 3': z$,0,0
	r32=ra(1)
	do
		if r32<1 then goto L50100
		read #note2b,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,r32
		n32=lrec(note2b)+1
		if r32>0 then ntr=n32+1 else ntr=0
		write #note2b,using 'Form POS 1,C 10,C 60,PD 3',rec=n32: z$,rm$,ntr
		if newra(1)=0 then newra(1)=n32
		newra(2)=n32
		rewrite #note1b,using 'form pos 11,2*pd 3',key=z$: mat newra
	loop
	EO4: !
	close #note1:  ioerr ignore
	close #note1b: ioerr ignore
	close #note2b: ioerr ignore
	close #hCustomer: ioerr ignore
	Xit: ! 
fnend 