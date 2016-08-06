00010 ! Replace R:\Core\PrtFlex\PullLay
00020 ! ______________________________________________________________________
00030   library 'R:\Core\Library': fnerror,fnsetmonth,fncno,fnxit,fnremove
00040 ! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
00050 ! to create your own file instead of using this program, store the description,variable name,field length,# of deciaml points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
00060 ! if you create the display file, as just described, create a folder under your program folder called GRID; a subfolder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these subfolders (actually one for each file you are allowing them to access with the grid programs.
00070 ! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
00080 ! you will have to create your folders as described above; this routine will not create the folders
00090   dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
00100   dim a(200,6),a$*132,prg$*20,mo$(12),outputfile$*50,ev$*50
00110   dim servicename$(10)*20,servicecode$(10)*2,textfile$*87,abbrev$*30
00120   let fnsetmonth(mat mo$)
00130   let fncno(cno)
00140 ! 
00150   gosub L1090
00160   let dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",19"&date$(1:2)
00190   let io1$(1)="10,34,c 45,UT,N" !:
        let io1$(2)="12,34,C 45,UT,N"
00200   let outputfile$="R:\acsPR\grid\checks\checkhistory"
00210   let ev$="R:\acsPR\Layouts\checkhistory.LAY"
00220   print newpage
00230   close #101: ioerr L240
00240 L240: open #101: "SROW=9,SCOL=2,EROW=13,ECOL=79,BORDER=DR,CAPTION=Pull Flex Grid Files",display,outin 
00250   print #101: newpage
00260   print fields "10,2,Cr 32": "File name to create (no ext):"
00270   print fields "12,2,Cr 32": "Layout file name (with exts):"
00280   print fields "14,35,c 9,B,1": "Next (F1)"
00290 L290: rinput fields mat io1$: outputfile$,ev$ conv L290
00300   let ev$=trim$(trim$(ev$,chr$(0)))
00310   let outputfile$=trim$(trim$(outputfile$,chr$(0)))&".fil"
00320   open #2: "Name="&ev$,display,input 
00330   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,Replace",internal,outin,keyed 
00340 L340: linput #2: ln$ eof L870
00341   let fnremove(chr$(9),ln$)
00350   print ln$
00360   if uprc$(ln$(7:10))<>"DATA" then goto L340
00370 DATALN: let j3=1
00380   let p1=11
00390   let p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
00400   let p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
00410   let p4=pos(srep$(ln$,'^','~'),'~',p3+1) ! pos(ln$,"^",p3+1)
00420   let p5=len(rtrm$(ln$))
00430   let a$(j3,1)=ln$(p1+1:p2-1)
00440   let a$(j3,1)=fnbooktitle$(a$(j3,1))
00450   let a$(j3,2)=ln$(p2+1:p3-1)
00460   let a$(j3,3)=ln$(p3+1:p4-1) ! P3+1:P4-1) ! MAX(P4-1,P3+8))  this was modified for ea
00470   if p4=0 then let abbrev$=a$(j3,1)(1:12) !:
          goto L490 ! if layout does not contail abbreviated name, then use first                     12 characters of real name
00480   let abbrev$=ln$(p4+1:len(ln$))(1:20)
00490 L490: ! If RTRM$(A$(J3,3))="" Then Goto 850
00500   let p1=pos(a$(j3,3)," ",1)+1
00510   let p2=pos(a$(j3,3),".",1)+1
00520   let p3=len(rtrm$(a$(j3,3))) ! was standard
00521 ! Let P3=pos(srep$(a$(j3,3),'^','~'),'~',1)-1 ! for acsea and acscl only  (way John does layouts)
00530   let p4=pos(a$(j3,3),"*",1)
00540   if p4=0 then let m1=1 else let m1=val(a$(j3,3)(1:p4-1))
00550   let l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
00551   print ln$
00560   if p2>1 then let dp=val(a$(j3,3)(p2:p3)) else let dp=0 !:
          ! DECIMAL POSITIONS
00570   if uprc$(a$(j3,3)(1:p1-2))="PD" then let al=l*2-1 else let al=l !:
          !   ACTUAL FIELD LENGTH
00572   if uprc$(a$(j3,3)(1:1))="X" then goto L340 ! skip any formats of "x"
00580   let l=l*m1 ! TOTAL STORAGE LENGTH
00590   let b=a+l
00600   let a=a+1
00610   let ino=ino+1
00620   let j3=1
00630   let a(j3,1)=ino
00640   let a(j3,2)=al
00650   let a(j3,3)=dp
00660   let a(j3,4)=l
00670   let a(j3,5)=a
00680   let a(j3,6)=b
00690   let a=b
00700   let rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
00710 ! SPECIAL ROUTINE TO PLACE CORRECT SERVICE NAME !:
        ! ON EACH SERVICE IN UTILITY BILLING
00720   if uprc$(a$(j3,1)(1:7))<>"SERVICE" then goto L770
00730   let x=val(a$(j3,1)(9:10)) conv L770
00740   if trim$(servicename$(x))="" then goto L850 ! SERVICE NOT USED
00750   let a$(j3,1)(1:9)=""
00760   let a$(j3,1)=trim$(servicename$(x))&" "&trim$(a$(j3,1))
00770 L770: if uprc$(abbrev$)(1:7)<>"SERVICE" then goto L810
00780   let x=val(abbrev$(9:10)) conv L810
00790   let abbrev$(1:9)=""
00800   let abbrev$=trim$(servicename$(x))&" "&trim$(abbrev$)
00810 L810: if rtrm$(a$(j3,1))="" or rtrm$(uprc$(a$(j3,1)))='UNUSED' or rtrm$(uprc$(a$(j3,1)))(1:5)='EXTRA' or trim$(abbrev$)="" then goto L850
00820 ! store as description,variable name,field length,# of deciaml points, format
00830   write #15,using L840: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
00835   print a$(j3,1)
00836   pause 
00840 L840: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
00850 L850: goto L340
00860 ! ______________________________________________________________________
00870 L870: close #2: ioerr L880
00880 L880: close #15: ioerr L890
00890 L890: gosub MOVEITTOTEXT
00900   print fields "24,1,C 7,UT,N": "Done..."
00910   stop 
00920 ! ______________________________________________________________________
00930 MOVEITTOTEXT: ! 
00940   open #10: "Name="&outputfile$&",RecL=87,Replace",display,output 
00950   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,use",internal,outin,keyed 
00960 L960: read #15,using L970: textfile$ eof L1000
00970 L970: form pos 1,c 87
00980   print #10,using L970: textfile$
00990   goto L960
01000 L1000: return 
01010 ! ______________________________________________________________________
01020 ! <Updateable Region: ERTN>
01030 ERTN: let fnerror(cap$,err,line,act$,"xit")
01040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01050   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01060   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01070 ERTN_EXEC_ACT: execute act$ : goto ERTN
01080 ! /region
01090 L1090: ! ______________________________________________________________________
01100   dim nam$*30
01110 ! ______________________________________________________________________
01120   def fnbooktitle$*80(x$*80)
01130     let x$=lwrc$(trim$(x$)) : let olda=0
01140     let x$(1:1)=uprc$(x$(1:1))
01150 ! capitalize anthing after a SPACE
01160 L1160: let a=pos(x$," ",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L1160
01170     let a=olda=0
01180 L1180: let a=pos(x$,"-",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L1180
01190     let a=olda=0
01200 L1200: let a=pos(x$,"/",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L1200
01210     let a=olda=0
01220 L1220: let a=pos(x$,"\",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L1220
01230     let a=olda=0
01240 L1240: let a=pos(x$,".",olda) !:
          if a<>0 then !:
            let a+=1 : let x$(a:a)=uprc$(x$(a:a)) : let olda=a : goto L1240
01250     let fnbooktitle$=x$
01260   fnend 
01270   return 
01280 ! ______________________________________________________________________
