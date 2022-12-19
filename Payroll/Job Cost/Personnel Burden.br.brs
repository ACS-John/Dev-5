! formerly  S:\acsPR\burden
! Service Code File

autoLibrary
fnTop(program$)
on error goto Ertn
dim dat$*20,gl(3),sf1$*28,sn$*30,search$(22),resp$(10)*256
dim name$*30
dim holdeno$*8,vcode$*6,de$*30,eno$*8,search$(22),ic$*2,pc$*1
dim df$*256,if$*256
dim code$(2),item2$(4)*30,breakdownde$*30,ml$(3)*80,code2$(3)

fndat(dat$,1)

if exists("[Q]\PRmstr\Burden.h[cno]")=0 then goto SETUP_FILES
L180: open #1: "Name=[Q]\PRmstr\Burden.h[cno],KFName=[Q]\PRmstr\BurdenIdx.h[cno],Shr",i,outIn,k
L190: form pos 1,n 8,c 30,3*n 6.3
ASKEMPLOYEE: !
	mat resp$=("")
	ad1=0 ! add code - used to tell other parts of the program,
	! that I am currently adding a service code record.
	fnTos
	respc=0
	fnLbl(1,1,"Employee Number:",20,right)
	fnCmbBurden(1,23)
	if hact$="" then
		resp$(respc+=1)=""
	else
		resp$(respc+=1)=hact$
	end if
	fnCmdKey("&Add",1,0,0,"Add a new employee burden record." )
	fnCmdKey("E&dit",2,1,0,"Access the highlited record")
	fnCmdKey("&Next",3,0,0,"Access next record in employee burden order")
	fnCmdKey("&Search",6,0,0,"Search for employee burden record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new employee burden information")
	fnCmdKey("&Proof List",8,0,0,"Returns to menu")
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=8 then gosub PRINT_PROOF: goto ASKEMPLOYEE
	if ckey=1 then goto ADD_RECORD
	if ckey=3 then read #1,using L190: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE eof ASKEMPLOYEE: holdeno=eno: goto SCREEN_1
	if ckey=4 then read #1,using L190,key=holdeno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE,ioerr ASKEMPLOYEE : goto SCREEN_1
	eno=val(resp$(1)(1:8)): holdeno=eno
	eno$=lpad$(str$(eno),8)
	if ckey=2 then read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
	if ckey=6 then let fnburden_srch(eno$,fixgrid) : eno$=lpad$(rtrm$(eno$),8) : read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
	if trim$(eno$)="" then goto ASKEMPLOYEE else read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
	if ckey=7 then gosub RECREATE_GRID: goto ASKEMPLOYEE
SCREEN_1: ! maintain personnel burdern screen
	fnTos
	respc=0
	mylen=12: mypos=mylen+3 : right=1
	fnLbl(1,1,"Employee #:",mylen,right)
	fnTxt(1,mypos,8,0,0,"")
	resp$(1)=eno$
	fnLbl(2,1,"Name:",mylen,right)
	fnTxt(2,mypos,30,0,0,"",0,"The name should be pulled from payroll and automatically displayed.")
	resp$(2)=name$
	fnLbl(3,1,"Burden %:",mylen,right)
	fnTxt(3,mypos,6,0,0,"33",0,"Enter the % of wage that you wish to use on this employee to calculate the personnel burden charged to jobs.")
	resp$(3)=str$(burden)
	fnLbl(4,1,"Unused:",mylen,right)
	fnTxt(4,mypos,6,0,0,"33",0,"Unused field at this time.")
	resp$(4)=""
	fnLbl(5,1,"Unused:",mylen,right)
	fnTxt(5,mypos,6,0,0,"33",0,"Unused field at this time.")
	resp$(5)=""
	fnCmdKey("&Save",1,1,0,"Saves any changes and returns to main screen.")
	fnCmdKey("&Delete",4,0,0,"Deletes this record from the personnel burden file.")
	fnCmdKey("&Cancel",5,0,1,"Returns to first screen without saving any changes.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ASKEMPLOYEE
	eno=val(resp$(1)(1:8)) : eno$=lpad$(trim$(resp$(1)),8)
	name$=resp$(2)
	burden=val(resp$(3))
	if ckey<>4 then goto L640
	mat ml$(2)
	ml$(1)="You have chosen to delete employee "&trim$(eno$)&" from the burden file!"
	ml$(2)="Select OK to delete; else Cancel to retain the record."
	fnMsgBox(mat ml$,resp$,'',49)
	if resp$="OK" then goto L600 else goto ASKEMPLOYEE
L600: if ckey=4 then delete #1,key=eno$: : gosub RECREATE_GRID: goto ASKEMPLOYEE
L640: rewrite #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey L650
L650: if ckey=1 then goto ASKEMPLOYEE
	goto ASKEMPLOYEE
!
RECREATE_GRID: !
	fnburden_srch(x$,99)
	df$="[Q]\PRmstr\Burden.h[cno]" : if$="[Q]\PRmstr\Burdenidx.h[cno]"
	fnComboF("CBurden",lyne,mypos,43,df$,1,8,9,30,if$,1)
	fnComboF("CBurdenaLL",lyne,mypos,43,df$,1,8,9,30,if$,2)
	ad1=0 ! set add code back before returning to main screen
	return
ADD_RECORD: !
	if reindex>3 then goto ERTN
	fnTos
	fnLbl(1,5,"New Personnel Burden Information",45,1)
	fnLbl(3,1,"Employee Number:",15,0)
	fncmbemp(3,18)
	resp$(1)=""
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ASKEMPLOYEE
	eno=val(resp$(1)(1:8))
	eno$=lpad$(trim$(resp$(1)(1:8)),8)
	name$=(resp$(1)(10:40))
	if trim$(eno$)="" then goto ADD_RECORD
	read #1,using L190,key=eno$: z$ nokey L870
	mat ml$(2)
	ml$(1)="A record # "&eno$&" already exists!"
	ml$(2)="Choose to review the record."
	fnMsgBox(mat ml$,resp$,'',48)
	goto ADD_RECORD
L870: burden=burden2=burden3=0
	write #1,using L190: eno,name$,burden,burden2,burden3
	holdeno=eno
	gosub RECREATE_GRID
goto SCREEN_1
SETUP_FILES: !
	open #1: "Name=[Q]\PRmstr\Burden.h[cno],RecL=128,replace",internal,outIn
	close #1:
goto REINDEX
REINDEX: ! indexes if needed
	reindex+=1
	close #1: ioerr ignore
	execute "Index [Q]\PRmstr\Burden.h[cno]"&' '&"[Q]\PRmstr\BurdenIdx.h[cno] 1 8 Replace DupKeys -n"
goto L180
PRINT_PROOF: !
	fnOpenPrn
	gosub L1140
	restore #1:
L1050: !
	read #1,using L190,release: eno,name$,burden,burden2,burden3 eof L1090
	pr #255,using L1070: eno,name$,burden pageoflow L1130
	L1070: form pos 1,n 8,x 6,c 30,n 6.3 ,skip 1
goto L1050
L1090: !r:
	if nw=0 then pr #255: newpage
	fnClosePrn
	on fkey 5 ignore
goto ASKEMPLOYEE
L1130: pr #255: newpage : gosub L1140 : continue
L1140: ! r:
	pr #255,using L1150: date$,env$('cnam')
	L1150: form pos 1,c 10,pos 20,cc 40,skip 1
	pr #255,using L1150: time$,"Personnel Burden "
	pr #255:
	pr #255: " Employee #  Name                         Burden %"
	pr #255: " __________  ____________________         ________"
return ! /r

POF1: ! r:
	pr #255: newpage
	pr #255,using L1300: date$('mm/dd/yy'),env$('cnam'),time$,"SERVICE CODE PROOF LIST",dat$
	pcnt=4
continue ! /r

POF2: ! r:
	pr #255: newpage
	pr #255,using L1300: date$('mm/dd/yy'),env$('cnam'),time$,"SERVICE CODE PROOF LIST",dat$
	L1300: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 53,c 30,skip 1,pos dattab,c 20,skip 2
	pr #255,using L1320: pl$(9,1),pl$(9,2)
	L1320: form pos 20,2*c 50,skip 1
	pcnt=5
continue ! /r

Xit: fnXit
include: ertn
