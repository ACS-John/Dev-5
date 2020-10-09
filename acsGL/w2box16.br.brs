! Replace S:\acsGL\W2Box16
! (allows you to enter miscellaneous information to any w-2)
! r: setup for local
	fn_wesupSetup
	dim  cap$*128,io1$(30)*64,in1$(30)
	fnTop(program$,cap$:="W-2 Supplemental Information")
	io1$(1)="7,11,C 12,[screen]"
	io1$(2)="7,25,G 10.2,[screen]"
	io1$(3)="7,37,G 1,[screen]"
	io1$(4)="7,43,G 1,[screen]"
	io1$(5)="7,49,G 1,[screen]"
	for j=1 to 5
		io1$(j+05)="08"&io1$(j)(2:inf)
		io1$(j+10)="10"&io1$(j)(2:inf)
		io1$(j+15)="11"&io1$(j)(2:inf)
		io1$(j+20)="12"&io1$(j)(2:inf)
		io1$(j+25)="13"&io1$(j)(2:inf)
	next j
	mat m1ColHeading$(1)
	mat m1ColMask$(1)
	m1ColHeading$(1)='Employee'
goto MENU1 ! /r
 
def fn_wesupSetup
	if ~w2supSetup then
		w2supSetup=1
		autoLibrary
		on error goto Ertn
		dim resp$(64)*128
		dim t$*8
		open #hw2box16:=fnH: "Name=[Q]\GLmstr\W2Box16.h[cno],Version=0,KFName=[Q]\GLmstr\W2Index.h[cno],Use,RecL=158,KPs=1,KLn=8,Shr",internal,outIn,keyed
		dim fw2box16$*255
		fw2box16$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
	end if
fnend
MENU1: ! r:
	fnTos
	fnflexinit1(sn$,2,2,10,10,mat m1ColHeading$,mat m1ColMask$)
	restore #hw2box16:
	recCount=0
	do
		read #hw2box16,using fw2box16$: emp$ eof m1EoList
		fnflexadd1(emp$)
		recCount+=1
	loop
	m1EoList: !
	if recCount then let fnCmdSet(14) else let fnCmdSet(15)
	fnAcs(mat resp$,ckey)
	if ckey=1 then ! Add
		fn_w2supEdit
	else if ckey=2 then ! Edit
		fn_w2supEdit(resp$(1))
	else if ckey=4 then ! Print
		goto PrintProofList
	else if ckey=5 then ! Cancel
		close #hw2box16:
		goto Xit
	end if
! /r
 
def library fnW2supEdit(;empNo$)
	fn_wesupSetup
	fnW2supEdit=fn_w2supEdit( empNo$)
fnend
def fn_w2supEdit(; empNo$)
	t$=empNo$
	mat in1$=('')
	if empNo$<>'' then
		t$=rpad$(empNo$,kln(hw2box16))
		read #hw2box16,using fw2box16$,key=t$: t$,mat in1$ noKey ignore
	else
		fnTos(sn$='w2supEdit1')
		mylen=15 : mypos=mylen+2
		fnLbl(1,1,'Employee:',mylen,1)
		fncombof('emp',1,mypos,width,'[Q]\GLmstr\PRmstr.h[cno]',1,4,5,25, '[Q]\GLmstr\PRIndex.h[cno]',1)
		resp$(1)=t$
		fnCmdSet(2)
		fnAcs(mat resp$,ckey)
		if ckey=5 then goto w2eFinis
		empNo$=resp$(1)(1:4)
		t$=uprc$(lpad$(rtrm$(t$),8))
		mat in1$=('')
		read #hw2box16,using fw2box16$,key=t$: t$,mat in1$ nokey ignore
	end if
	! r: RecEdit
	fnTos(sn$='w2supEdit2')
	lenCol1=16
	posCol2=18 ! 30
	posCol3=34 ! 50
	posCol4=46
	posCol5=58
	posCol6=70
	rc=0
	fnLbl(1,2,'Employee Number:',lenCol1)
	fnTxt(1,posCol2,8,0,0,'',1)
	resp$(rc+=1)=t$
	fnLbl(4,posCol2,'Description')
	fnLbl(4,posCol3,'Amount')
	fnLbl(3,posCol4,'Effect on Wages')
	fnLbl(4,posCol4,'Fed')
	fnLbl(4,posCol5,'FICA')
	fnLbl(4,posCol6,'State')
	dim nasOption$(3)*10
	nasOption$(1)='0 None'
	nasOption$(2)='1 Add'
	nasOption$(3)='2 Subtract'
	lc=4
	fnLbl(lc+=1,1,"Box 11:",lenCol1,1)
		fn_add_this_one(mat in1$(01:05))
	fnLbl(lc+=1,1,"Unused:",lenCol1,1)
		fn_add_this_one(mat in1$(06:10))
	fnLbl(lc+=1,1,"Box 12a:",lenCol1,1)
		fn_add_this_one(mat in1$(11:15))
	fnLbl(lc+=1,1,"Box 12b:",lenCol1,1)
		fn_add_this_one(mat in1$(16:20))
	fnLbl(lc+=1,1,"Box 12c:",lenCol1,1)
		fn_add_this_one(mat in1$(21:25))
	fnLbl(lc+=1,1,"Box 12d:",lenCol1,1)
		fn_add_this_one(mat in1$(26:30))
	fnCmdSet(4)
	fnAcs(mat resp$,ckey)
	if cmdkey=5 then goto w2eFinis
	rc=0
	t$=resp$(rc+=1)
	in1Count=0
	for in1Item=1 to 30 ! udim(mat in1$)
		in1$(in1Item)=resp$(rc+=1)
		if in1Item=3 or 4 or 5 then
			nasWhich=srch(mat nasOption$,in1$(in1Item))
			if nasWhich>0 then
				in1$(in1Item)=nasOption$(nasWhich)(1:1)
			end if
		end if
	nex in1Item
	rewrite #hw2box16,using fw2box16$,key=rpad$(t$,kln(hw2box16)): t$,mat in1$ nokey w2eWrite
	goto w2eFinis
	w2eWrite: !
	write #hw2box16,using fw2box16$: t$,mat in1$
	goto w2eFinis
	w2eFinis: ! /r
fnend
 
def fn_add_this_one(mat in1Five$)
	fnTxt(lc,posCol2,12) : resp$(rc+=1)=in1Five$(1) ! desc
	fnTxt(lc,posCol3,11,0,0,'10') : resp$(rc+=1)=in1Five$(2) ! amount
	fncomboa('nas',lc,posCol4,mat nasOption$)
	resp$(rc+=1)=nasOption$(val(in1Five$(3))+1) !  Fed
	fncomboa('nas',lc,posCol5,mat nasOption$)
	resp$(rc+=1)=nasOption$(val(in1Five$(4))+1) ! FICA
	fncomboa('nas',lc,posCol6,mat nasOption$)
	resp$(rc+=1)=nasOption$(val(in1Five$(5))+1) ! State
fnend
PrintProofList: ! r:
	restore #hw2box16:
	pg=0
	fnopenprn
	gosub HEADER
	do
		read #hw2box16,using fw2box16$: t$,mat in1$ eof END_OF_PROOF_LIST
		pr #255:
		pr #255: "Employee Number: ";t$
		pr #255,using L1820: "Box 11",in1$(1),val(in1$(2)),val(in1$(3)),val(in1$(4)),val(in1$(5))
		pr #255,using L1820: "Unused",in1$(6),val(in1$(7)),val(in1$(8)),val(in1$(9)),val(in1$(10))
		pr #255,using L1820: "Box 12a",in1$(11),val(in1$(12)),val(in1$(13)),val(in1$(14)),val(in1$(15))
		pr #255,using L1820: "Box 12b",in1$(16),val(in1$(17)),val(in1$(18)),val(in1$(19)),val(in1$(20))
		pr #255,using L1820: "Box 12c",in1$(21),val(in1$(22)),val(in1$(23)),val(in1$(24)),val(in1$(25))
		pr #255,using L1820: "Box 12d",in1$(26),val(in1$(27)),val(in1$(28)),val(in1$(29)),val(in1$(30))
		L1820: form pos 2,c 9,c 12,n 12.2,3*n 6
	loop
	END_OF_PROOF_LIST: !
	fncloseprn
goto MENU1 ! /r
HEADER: ! r:
	pr #255,using 'pos 1,c 10,cc 51': date$("mm/dd/ccyy"),env$('cnam')
	pg+=1
	pr #255,using L2040: time$,"W-2 Supplemental Information Proof List","Page",pg,date$("Month DD, CCYY")
	L2040: form pos 1,c 8,cc 52,skip 1,pos 1,c 5,n 3,cc 52,skip 1
return ! /r
Xit: fnXit
include: ertn
