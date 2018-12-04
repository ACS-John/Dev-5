! Replace S:\Core\PrtFlex\PullLay
! ______________________________________________________________________
library 'S:\Core\Library': fnerror,fnsetmonth,fnxit,fnget_services,fnbooktitle$
! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
! to create your own file instead of using this program, store the description,variable name,field length,# of deciaml points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
! if you create the display file, as just described, create a folder under your program folder called GRID; a subfolder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these subfolders (actually one for each file you are allowing them to access with the grid programs.
! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
! you will have to create your folders as described above; this routine will not create the folders
dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
dim a(200,6),a$*132,prg$*20,mo$(12),outputfile$*50,ev$*50
dim serviceName$(10)*20,textfile$*87,abbrev$*30
fnget_services(mat serviceName$)
fnsetmonth(mat mo$)

dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",20"&date$(1:2)
io1$(1)="10,34,c 45,UT,N"
io1$(2)="12,34,C 45,UT,N"
outputfile$="S:\acsPR\grid\checks\checkhistory"
ev$="S:\acsPR\Layouts\checkhistory.LAY"
pr newpage
close #101: ioerr ignore
open #101: "SROW=9,SCOL=2,EROW=13,ECOL=79,BORDER=DR,CAPTION=Pull Flex Grid Files",display,outIn
pr #101: newpage
pr f "10,2,Cr 32": "File name to create (no ext):"
pr f "12,2,Cr 32": "Layout file name (with exts):"
pr f "14,35,c 9,B,1": "Next (F1)"
L290: !
rinput fields mat io1$: outputfile$,ev$ conv L290
ev$=trim$(trim$(ev$,chr$(0)))
outputfile$=trim$(trim$(outputfile$,chr$(0)))&".fil"
open #2: "Name="&ev$,display,input
open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,Replace",internal,outIn,keyed
L340: !
linput #2: ln$ eof L870
ln$=srep$(ln$,chr$(9),'')
pr ln$
if uprc$(ln$(7:10))<>"DATA" then goto L340
DATALN: !
j3=1
p1=11
p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
p4=pos(srep$(ln$,'^','~'),'~',p3+1) ! pos(ln$,"^",p3+1)
p5=len(rtrm$(ln$))
a$(j3,1)=ln$(p1+1:p2-1)
a$(j3,1)=fnbooktitle$(a$(j3,1))
a$(j3,2)=ln$(p2+1:p3-1)
a$(j3,3)=ln$(p3+1:p4-1) ! P3+1:P4-1) ! MAX(P4-1,P3+8))  this was modified for ea
if p4=0 then
	abbrev$=a$(j3,1)(1:12)
	goto L490 ! if layout does not contail abbreviated name, then use first                     12 characters of real name
end if
abbrev$=ln$(p4+1:len(ln$))(1:20)
L490: ! If RTRM$(A$(J3,3))="" Then Goto 850
p1=pos(a$(j3,3)," ",1)+1
p2=pos(a$(j3,3),".",1)+1
p3=len(rtrm$(a$(j3,3))) ! was standard
! p3=pos(srep$(a$(j3,3),'^','~'),'~',1)-1 ! for acsea and acscl only  (way John does layouts)
p4=pos(a$(j3,3),"*",1)
if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
pr ln$
if p2>1 then dp=val(a$(j3,3)(p2:p3)) else dp=0           ! DECIMAL POSITIONS
if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l           !   ACTUAL FIELD LENGTH
if uprc$(a$(j3,3)(1:1))="X" then goto L340 ! skip any formats of "x"
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
! SPECIAL ROUTINE TO PLACE CORRECT SERVICE NAME ON EACH SERVICE IN UTILITY BILLING
if uprc$(a$(j3,1)(1:7))<>"SERVICE" then goto L770
x=val(a$(j3,1)(9:10)) conv L770
if trim$(serviceName$(x))="" then goto L850 ! SERVICE NOT USED
a$(j3,1)(1:9)=""
a$(j3,1)=trim$(serviceName$(x))&" "&trim$(a$(j3,1))
L770: !
if uprc$(abbrev$)(1:7)<>"SERVICE" then goto L810
x=val(abbrev$(9:10)) conv L810
abbrev$(1:9)=""
abbrev$=trim$(serviceName$(x))&" "&trim$(abbrev$)
L810: !
if rtrm$(a$(j3,1))="" or rtrm$(uprc$(a$(j3,1)))='UNUSED' or rtrm$(uprc$(a$(j3,1)))(1:5)='EXTRA' or trim$(abbrev$)="" then goto L850
! store as description,variable name,field length,# of deciaml points, format
write #15,using L840: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
pr a$(j3,1)
pause
L840: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
L850: !
goto L340
! ______________________________________________________________________
L870: !
	close #2: ioerr ignore
	close #15: ioerr ignore
	gosub MOVEITTOTEXT
	pr f "24,1,C 7,UT,N": "Done..."
stop
! ______________________________________________________________________
MOVEITTOTEXT: ! r:
	open #10: "Name="&outputfile$&",RecL=87,Replace",display,output
	open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,use",internal,outIn,keyed
	do
		read #15,using L970: textfile$ eof L1000
		L970: form pos 1,c 87
		pr #10,using L970: textfile$
	loop
	L1000: !
return ! /r
include: ertn