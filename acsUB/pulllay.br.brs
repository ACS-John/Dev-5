00010 ! Replace S:\acsUB\PullLay   ! only use for utility billing  (need to send out S:\acsUB\Layouts\UBmstr-vb.lay to any one trying to use this
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnerror,fnsetmonth,fncno,fnxit,fnremove,fnrights_test,fntop,fnbooktitle$
00032 ! msgbox("Reverse Billing Cycle is currently under construction.","Reverse Billing Cycle Unavailable","OK","Inf") : if env$('ACSDeveloper')='' then goto XIT
00033   fntop(program$,cap$="Main menu")
00034   if ~fnrights_test('',"Try Run As Administrator.",'Program','This program must write data into the working (program) directory.') then goto XIT
00040 ! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
00050 ! to create your own file instead of using this program, store the description,variable name,field length,# of decimal points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
00060 ! if you create the display file, as just described, create a folder under your program folder called GRID; a sub-folder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these sub-folders (actually one for each file you are allowing them to access with the grid programs.
00070 ! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
00080 ! you will have to create your folders as described above; this routine will not create the folders
00090   dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
00100   dim a(200,6),a$*132,prg$*20,mo$(12),outputfile$*50,ev$*50,cap$*128
00110   dim servicename$(10)*20,servicecode$(10)*2,textfile$*87,abbrev$*30
00120   fnsetmonth(mat mo$)
00130   fncno(cno)
00160   dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",19"&date$(1:2)
00180   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative ioerr L190
00182   read #20,using "Form POS 1,10*C 20,10*c 2",rec=1: mat servicename$,mat srv$
00184   close #20: 
00190 L190: io1$(1)="10,34,c 45,UT,N"
00192   io1$(2)="12,34,C 45,UT,N"
00200   outputfile$="S:\acsUB\grid\Customer\Customer.fil"
00210   ev$="S:\acsUB\Layouts\UBmstr-vb.LAY"
00220 !  goto L320 ! for utility billing automatically from menu
00230 !   close #101: ioerr ignore
00240 !   open #101: "SROW=9,SCOL=2,EROW=13,ECOL=79,BORDER=DR,CAPTION=Pull Flex Grid Files",display,outin
00250 !   pr #101: newpage
00260 !   pr f "10,2,Cr 32": "File name to create (no ext):"
00270 !   pr f "12,2,Cr 32": "Layout file name (with exts):"
00280 !   pr f "14,35,c 9,B,1": "Next (F1)"
00290 ! L290: rinput fields mat io1$: outputfile$,ev$ conv L290
00300 !   ev$=trim$(trim$(ev$,chr$(0)))
00310 !   outputfile$=trim$(trim$(outputfile$,chr$(0)))&".fil"
00320 ! L320:
00322   open #2: "Name="&ev$,display,input 
00330   open #h_temp:=15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,Replace",internal,outin,keyed 
00333 F_TEMP: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
00340   do 
00342 READ_TEMP: ! 
00344     linput #2: ln$ eof FINIS
00346     ln$=srep$(ln$,'^','~')
00350     fnremove(chr$(9),ln$)
00360     if uprc$(ln$(7:10))<>"DATA" then goto READ_TEMP
00370 DATALN: j3=1
00380     p1=11
00390     p2=pos(ln$,"~",p1+1)
00400     p3=pos(ln$,"~",p2+1)
00410     p4=pos(ln$,"~",p3+1)
00420     p5=len(rtrm$(ln$))
00430     a$(j3,1)=ln$(p1+1:p2-1) ! pr 'line:'&ln$ : pr 'a$(j3,1)='&a$(j3,1) : pause
00440     a$(j3,1)=fnbooktitle$(a$(j3,1))
00450     a$(j3,2)=ln$(p2+1:p3-1)
00460     a$(j3,3)=ln$(p3+1:p4-1) ! P3+1:P4-1) ! MAX(P4-1,P3+8))  this was modified for ea
00470     if p4=0 then ! if layout does not contail abbreviated name, then use first 12 characters of real name
00472       abbrev$=a$(j3,1)(1:12)
00474     else 
00476       abbrev$=ln$(p4+1:len(ln$))(1:20)
00478     end if 
00490 ! If RTRM$(A$(J3,3))="" Then Goto 850
00500     p1=pos(a$(j3,3)," ",1)+1
00510     p2=pos(a$(j3,3),".",1)+1
00520     p3=len(rtrm$(a$(j3,3))) ! was standard
00530 ! p3=POS(A$(J3,3),"~",1)-1 ! for acsea and acscl only  (way John does layouts)
00540     p4=pos(a$(j3,3),"*",1)
00550     if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
00560     l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
00570     if p2>1 then dp=val(a$(j3,3)(p2:p3)) else dp=0 ! DECIMAL POSITIONS
00580     if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !   ACTUAL FIELD LENGTH
00590     if uprc$(a$(j3,3)(1:1))="X" then goto READ_TEMP ! skip any formats of "x"
00600     l=l*m1 ! TOTAL STORAGE LENGTH
00610     b=a+l
00620     a=a+1
00630     ino=ino+1
00640     j3=1
00650     a(j3,1)=ino
00660     a(j3,2)=al
00670     a(j3,3)=dp
00680     a(j3,4)=l
00690     a(j3,5)=a
00700     a(j3,6)=b
00710     a=b
00720     rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
00725 ! pr ln$ : pause
00730 ! SPECIAL ROUTINE TO PLACE CORRECT SERVICE NAMEON EACH SERVICE IN UTILITY BILLING
00740     if uprc$(a$(j3,1)(1:7))<>"SERVICE" then goto L850
00750     x=val(a$(j3,1)(9:10)) conv L850
00760     if trim$(servicename$(x))="" then goto READ_TEMP ! SERVICE NOT USED
00770     a$(j3,1)(1:9)=""
00780     if x=3 and trim$(servicename$(x))<>"Electric" and srv$(3)="EL" then goto L840
00790     if x=4 and trim$(servicename$(x))<>"Gas" and srv$(4)="GA" then goto L840 ! gas or electric used for some some reading other that gas or electric (code must be GA or EL for this to work
00800     if x=3 and trim$(servicename$(x))<>"Electric" and a$(j3,1)(2:6)="Prior" then goto READ_TEMP
00810     if x=3 and trim$(servicename$(x))<>"Electric" and (a$(j3,1)(2:6)="Multi" or a$(j3,1)(2:6)="Elect" or a$(j3,1)(2:6)="Depos" or a$(j3,1)(2:6)="Readi" or a$(j3,1)(2:6)="Used-" or a$(j3,1)(2:6)="Kwh  " or a$(j3,1)(2:6)="Deman" or a$(j3,1)(2:6)="Units" or a$(j3,1)(2:6)="Prior") then goto READ_TEMP
00820     if x=4 and trim$(servicename$(x))<>"Gas" and (a$(j3,1)(2:6)="Multi" or a$(j3,1)(2:6)="Elect" or a$(j3,1)(2:6)="Depos" or a$(j3,1)(2:6)="Readi" or a$(j3,1)(2:6)="Used-" or a$(j3,1)(2:6)="Kwh  " or a$(j3,1)(2:6)="Deman" or a$(j3,1)(2:6)="Units" or a$(j3,1)(2:6)="Meter") then goto READ_TEMP
00830     if x=4 and trim$(servicename$(x))<>"Gas" and a$(j3,1)(2:6)="Prior" then goto READ_TEMP
00840 L840: ! 
00842     a$(j3,1)=trim$(servicename$(x))&" "&trim$(a$(j3,1))
00850 L850: ! 
00852     if uprc$(abbrev$)(1:7)<>"SERVICE" then goto L890
00860     x=val(abbrev$(9:10)) conv L890
00870     abbrev$(1:9)=""
00880     abbrev$=trim$(servicename$(x))&" "&trim$(abbrev$)
00890 L890: ! 
00892     if rtrm$(a$(j3,1))="" or rtrm$(uprc$(a$(j3,1)))='UNUSED' or rtrm$(uprc$(a$(j3,1)))(2:6)='EXTRA' or trim$(abbrev$)="" then goto READ_TEMP
00900 ! store as description,variable name,field length,# of deciaml points, format
00910     write #h_temp,using F_TEMP: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
00932   loop 
00940 ! ______________________________________________________________________
00950 FINIS: ! 
00952   close #2: ioerr ignore
00960   close #h_temp: ioerr ignore
00970   gosub MOVEITTOTEXT
00980 XIT: fnxit
18000 IGNORE: continue 
20000 ! <Updateable Region: ERTN>
20020 ERTN: fnerror(program$,err,line,act$,"xit")
20040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20100 ERTN_EXEC_ACT: execute act$ : goto ERTN
20120 ! /region
22000 MOVEITTOTEXT: ! r:
22020   open #10: "Name="&outputfile$&",RecL=87,Replace",display,output 
22040   open #h_temp:=15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,use",internal,outin,keyed 
22060   do 
22080     read #h_temp,using 'form pos 1,c 87': textfile$ eof L1080
22100     pr #10,using 'form pos 1,c 87': textfile$
22120   loop 
22140 L1080: ! 
22160   return  ! /r
