! Replace S:\acsPR\newimporttime
! Capture Time for Time Sheet Entry                                            this program pulls time form any system that can create the following           layout : employee # n 8 : name c 30 : department n 3: reghrs n 7.2 :            othrs n 7.2 : vachrs n 7.2 : sickhrs n 7.2 : holhrs n 7.2
! the file name should be \program files\acs\timeclock.h&cno
! this program places the information in same file that simple time             clock uses in the input time sheets program
 
autoLibrary
on error goto Ertn
 
dim ml$(0)*128
dim name$*30
 
 
fnTop(program$,"Import Time from Time Clock System")
dim pathtotimecard$*200
pathtotimecard$="c:\progra~1\acs\"
 
open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,outIn,k
open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",i,outi,r
 
ASK_PAYROLL_DATE: !
	fnTos
	respc=0
	fnLbl(1,1,"",34,1) ! bigger screen
	fnLbl(2,1,"Payroll Date:",20,1)
	fnTxt(2,23,10,0,1,"3",0,"Always use the calculation date.")
	resp$(respc+=1)=str$(ppd)
	fnCmdKey("&Next",1,1,0,"Proceed with importing time." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$) ! ask employee #
	if ckey=5 then goto Xit
endingdate=val(resp$(1))
 
fnopenprn
! dim filename$*128
! filename$="Payroll"&cnvrt$("Pic(zzzzzzzz)",endingdate)(5:6) &"-"&cnvrt$("Pic(zzzzzzzz)",endingdate)(7:8)&"-" &cnvrt$("Pic(zzzzzzzz)",endingdate)(3:4) &".txt"
! if env$('client')="West Rest Haven" then filename$=cnvrt$("Pic(zzzzzzzz)",endingdate)(5:6) &"-"&cnvrt$("Pic(zzzzzzzz)",endingdate)(7:8)&"-" &cnvrt$("Pic(zzzzzzzz)",endingdate)(3:4) &".txt"
! if env$('client')="West Rest Haven" then execute "Copy c:\Acs\local\wrhPayroll"&filename$&" "&pathtotimecard$&"TimeCard.h[cno]"
gosub HDR
! fnwait("Importing: please wait...",0)
on fkey 5 goto L580
dim simple$*50
simple$=pathtotimecard$&"TimeCard.h[cno]"
open #3: "Name="&pathtotimecard$&"TimeCard\SimpleSummary,KFName="&pathtotimecard$&"TimeCard\SSIndex,Replace,RecL=46,KPs=1,KLn=16",i,outIn,k
open #5: "Name="&simple$&",RecL=76",display,input
L410: !
dim ln$*76
do
	linput #5: ln$ eof L580
	eno=val(ln$(1:8)) conv MESSAGE1
	dep=val(ln$(39:41)) conv ignore
	reghrs=val(ln$(42:48)) conv ignore
	othrs=val(ln$(49:55)) conv ignore
	vachrs=val(ln$(56:62)) conv ignore
	sickhrs=val(ln$(63:69)) conv ignore
	holhrs=val(ln$(70:76)) conv ignore
	name$=ln$(9:38)
	write #3,using L530: eno,dep,val(env$('cno')),reghrs,othrs,vachrs,sickhrs,holhrs,0
	dim prname$*30
	read #1,using L520,key=lpad$(str$(eno),8): prname$ nokey MESSAGE2
	L520: form pos 9,c 30
	L530: form pos 1,n 8,n 3,n 5,6*pd 5.2
	pr #255,using L550: eno,name$(1:27),dep,reghrs,othrs,vachrs,sickhrs,holhrs pageoflow PAGE_O_FLOW
	L550: form pos 1,n 8,x 1,c 27,x 1,n 3,5*n 10.2
loop
L580: !
	close #3: ioerr ignore
	execute "Index "&pathtotimecard$&"TimeCard\SimpleSummary "&pathtotimecard$&"TimeCard\SSIndex 1 16 Replace DupKeys -n"
goto Xit
 
Xit: !
fncloseprn
fnXit
MESSAGE1: ! r: bad data
	mat ml$(4)
	ml$(1)="Cannot read the data! It must be a corrupted"
	ml$(2)="file or in the wrong format.  You must export"
	ml$(3)="the time from your time clock system before"
	ml$(4)="attempting to run this menu option."
	fnmsgbox(mat ml$,resp$,'',48)
goto Xit ! /r
MESSAGE2: ! r: bad employee number
	mat ml$(4)
	ml$(1)="Employee "&str$(eno)&" has time imported"
	ml$(2)="from the time clock, but does not have a matching"
	ml$(3)="employee number in the payroll system.  This"
	ml$(4)="person will be skipped.   "&name$
	fnmsgbox(mat ml$,resp$,'',48)
goto L410 ! /r
HDR: ! r:
	pr #255,using L790: time$,env$('cnam'),date$,"Time Card Summary",cnvrt$("pic(zzzz/zz/zz)",endingdate)
	L790: form skip 2,pos 1,c 20,pos 20,cc 40,skip 1,pos 1,c 20,pos 20,cc 40,skip 1,pos 20,cc 40,skip 2
	pr #255: "Employee  Name                        Dep  Regular  Overtime  Vacation      Sick   Holiday"
return ! /r
PAGE_O_FLOW: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
include: ertn
