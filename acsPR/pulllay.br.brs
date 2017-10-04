00010 ! Replace S:\acsPR\PullLay   ! only use for payroll (need to send out S:\acsPR\Layouts\chechhistory.lay and department.lay for any one trying to use this
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnerror,fnsetmonth,fncno,fnxit,fnremove,fnchain,fnDedNames
00040 ! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
00050 ! to create your own file instead of using this program, store the description,variable name,field length,# of deciaml points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
00060 ! if you create the display file, as just described, create a folder under your program folder called GRID; a subfolder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these subfolders (actually one for each file you are allowing them to access with the grid programs.
00070 ! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
00080 ! you will have to create your folders as described above; this routine will not create the folders
00090   dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
00100   dim a(200,6),a$*132,prg$*20,mo$(12),outputfile$*50,ev$*50
00110   dim fullname$(20)*20,servicecode$(10)*2,textfile$*87,abbrev$*30
00120   fnsetmonth(mat mo$)
00130   fncno(cno)
00140 ! 
00150   gosub L1110
00160   let dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",19"&date$(1:2)
00180   fnDedNames(mat fullname$,mat abbrevname$)
00210   let io1$(1)="10,34,c 45,UT,N" !:
        let io1$(2)="12,34,C 45,UT,N"
00220   outputfile$="S:\acsPR\grid\checks\checkhistory.fil"
00230   ev$="S:\acsPR\Layouts\checkhistory.LAY"
00240   goto L340 ! for utility billing automatically from menu
00250   close #101: ioerr L260
00260 L260: open #101: "SROW=9,SCOL=2,EROW=13,ECOL=79,BORDER=DR,CAPTION=Pull Flex Grid Files",display,outin 
00270   pr #101: newpage
00280   pr f "10,2,Cr 32": "File name to create (no ext):"
00290   pr f "12,2,Cr 32": "Layout file name (with exts):"
00300   pr f "14,35,c 9,B,1": "Next (F1)"
00310 L310: rinput fields mat io1$: outputfile$,ev$ conv L310
00320   ev$=trim$(trim$(ev$,chr$(0)))
00330   outputfile$=trim$(trim$(outputfile$,chr$(0)))&".fil"
00340 L340: open #2: "Name="&ev$,display,input 
00350   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,Replace",internal,outin,keyed 
00360 L360: linput #2: ln$ eof L890
00370   fnremove(chr$(9),ln$)
00380   if uprc$(ln$(7:10))<>"DATA" then goto L360
00390 DATALN: let j3=1
00400   let p1=11
00410   let p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
00420   let p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
00430   let p4=pos(srep$(ln$,'^','~'),'~',p3+1) ! pos(ln$,"^",p3+1)
00440   let p5=len(rtrm$(ln$))
00450   a$(j3,1)=ln$(p1+1:p2-1)
00460   a$(j3,1)=fnbooktitle$(a$(j3,1))
00470   a$(j3,2)=ln$(p2+1:p3-1)
00480   a$(j3,3)=ln$(p3+1:p4-1) ! P3+1:P4-1) ! MAX(P4-1,P3+8))  this was modified for ea
00490   if p4=0 then abbrev$=a$(j3,1)(1:12) !:
          goto L510 ! if layout does not contail abbreviated name, then use first                     12 characters of real name
00500   abbrev$=ln$(p4+1:len(ln$))(1:20)
00510 L510: ! If RTRM$(A$(J3,3))="" Then Goto 850
00520   let p1=pos(a$(j3,3)," ",1)+1
00530   let p2=pos(a$(j3,3),".",1)+1
00540   let p3=len(rtrm$(a$(j3,3))) ! was standard
00550 ! Let P3=pos(srep$(ln$,'^','~'),'~',1)-1 ! POS(A$(J3,3),"^",1)-1 ! for acsea and acscl only  (way John does layouts)
00560   let p4=pos(a$(j3,3),"*",1)
00570   if p4=0 then let m1=1 else let m1=val(a$(j3,3)(1:p4-1))
00580   l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
00590   if p2>1 then let dp=val(a$(j3,3)(p2:p3)) else let dp=0 !:
          ! DECIMAL POSITIONS
00600   if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !:
          !   ACTUAL FIELD LENGTH
00610   if uprc$(a$(j3,3)(1:1))="X" then goto L360 ! skip any formats of "x"
00620   l=l*m1 ! TOTAL STORAGE LENGTH
00630   b=a+l
00640   a=a+1
00650   let ino=ino+1
00660   let j3=1
00670   a(j3,1)=ino
00680   a(j3,2)=al
00690   a(j3,3)=dp
00700   a(j3,4)=l
00710   a(j3,5)=a
00720   a(j3,6)=b
00730   a=b
00740   let rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
00750 ! SPECIAL ROUTINE TO PLACE CORRECT SERVICE NAME !:
        ! ON EACH SERVICE IN PAYROLL
00760   if uprc$(a$(j3,1)(1:4))<>"MISC" then goto L840
00770   let x=val(a$(j3,1)(8:9)) conv L840
00780   if trim$(fullname$(x))="" then goto L870 ! SERVICE NOT USED
00790   a$(j3,1)=fullname$(x)
00795 ! If UPRC$(ABBREV$)(1:4)="MISC" Then Pause
00800   if uprc$(abbrev$)(1:4)<>"MISC" then goto L840
00810   let x=val(abbrev$(5:6)) conv L840
00820   abbrev$=""
00830   abbrev$=trim$(abbrevname$(x))(1:9)
00840 L840: ! store as description,variable name,field length,# of deciaml points, format
00850   write #15,using L860: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
00860 L860: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
00870 L870: goto L360
00880 ! ______________________________________________________________________
00890 L890: close #2: ioerr L900
00900 L900: close #15: ioerr L910
00910 L910: gosub MOVEITTOTEXT
00920 XIT: let fnchain("S:\acsPR\PulllayDept")
00930   stop 
00940 ! ______________________________________________________________________
00950 MOVEITTOTEXT: ! 
00960   open #10: "Name="&outputfile$&",RecL=87,Replace",display,output 
00970   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,use",internal,outin,keyed 
00980 L980: read #15,using L990: textfile$ eof L1020
00990 L990: form pos 1,c 87
01000   pr #10,using L990: textfile$
01010   goto L980
01020 L1020: return 
01030 ! ______________________________________________________________________
01040 ! <Updateable Region: ERTN>
01050 ERTN: let fnerror(program$,err,line,act$,"xit")
01060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01070   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01090 ERTN_EXEC_ACT: execute act$ : goto ERTN
01100 ! /region
01110 L1110: ! ______________________________________________________________________
01120   dim nam$*30
01130 ! ______________________________________________________________________
01140   def fnbooktitle$*80(x$*80)
01150     let x$=lwrc$(trim$(x$)) : olda=0
01160     let x$(1:1)=uprc$(x$(1:1))
01170 ! capitalize anthing after a SPACE
01180 L1180: a=pos(x$," ",olda) !:
          if a<>0 then !:
            a+=1 : let x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L1180
01190     a=olda=0
01200 L1200: a=pos(x$,"-",olda) !:
          if a<>0 then !:
            a+=1 : let x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L1200
01210     a=olda=0
01220 L1220: a=pos(x$,"/",olda) !:
          if a<>0 then !:
            a+=1 : let x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L1220
01230     a=olda=0
01240 L1240: a=pos(x$,"\",olda) !:
          if a<>0 then !:
            a+=1 : let x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L1240
01250     a=olda=0
01260 L1260: a=pos(x$,".",olda) !:
          if a<>0 then !:
            a+=1 : let x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L1260
01270     fnbooktitle$=x$
01280   fnend 
01290   return 
01300 ! ______________________________________________________________________
