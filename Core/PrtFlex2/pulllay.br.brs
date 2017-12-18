00010 ! Replace Core\PrtFlex\PullLay
00020 ! ______________________________________________________________________
00030   library 'Core\Library': fnerror,fnsetmonth
00040 ! This program will read a standard ACS layout and pull the data names for use in the user designed grid features of any ACS system
00050 ! to create your own file instead of using this program, store the description,variable name,field length,# of deciaml points, format (example:  Customer Name,Variable Name,30,0,C)   Form POS 1,C 30,C 20,N 4,N 2,C 11
00060 ! if you create the display file, as just described, create a folder under your program folder called GRID; a subfolder such as CUSTOMER which will be referred to in the grid program as the data base you are using.  You can have any number of these subfolders (actually one for each file you are allowing them to access with the grid programs.
00070 ! if you wish to use this program and do not use ACS layout formats, create a text file (any name you choose) and enter your data as follows :  00010 data Customer Name^Name$(1)^C 30   or 00020  data Customer Balance^amount(5)^pd 5.2
00080 ! you will have to create your folders as described above; this routine will not create the folders
00090   dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
00100   dim a(200,6),a$*132,prg$*20,mo$(12),outputfile$*50,ev$*50
00110   dim servicename$(10)*20,servicecode$(10)*2,textfile$*87,abbrev$*30
00120   fnsetmonth(mat mo$)
00140 ! 
00150   dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",19"&date$(1:2)
00170   open #20: "Name=UBData\Service.h"&env$('cno')&",Shr",internal,input,relative ioerr L180 !:
        read #20,using "Form POS 1,10*C 20",rec=1: mat servicename$ !:
        close #20: 
00180 L180: io1$(1)="10,34,c 45,UT,N" !:
        io1$(2)="12,34,C 45,UT,N"
00190   outputfile$="acspb\grid\patient\patient"
00200   ev$="ACSpb\Layouts\pbMSTR-VB.LAY"
00210   pr newpage
00220   close #101: ioerr L230
00230 L230: open #101: "SROW=9,SCOL=2,EROW=13,ECOL=79,BORDER=DR,CAPTION=Pull Flex Grid Files",display,outin 
00240   pr #101: newpage
00250   pr f "10,2,Cr 32": "File name to create (no ext):"
00260   pr f "12,2,Cr 32": "Layout file name (with exts):"
00270   pr f "14,35,c 9,B,1": "Next (F1)"
00280 L280: rinput fields mat io1$: outputfile$,ev$ conv L280
00290   ev$=trim$(trim$(ev$,chr$(0)))
00300   outputfile$=trim$(trim$(outputfile$,chr$(0)))&".fil"
00310   open #2: "Name="&ev$,display,input 
00320   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,Replace",internal,outin,keyed 
00330 L330: linput #2: ln$ eof L830
00340   if uprc$(ln$(7:10))<>"DATA" then goto L330
00350 DATALN: j3=1
00360   p1=11
00370   p2=pos(ln$,"^",p1+1)
00380   p3=pos(ln$,"^",p2+1)
00390   p4=pos(ln$,"^",p3+1)
00400   p5=len(rtrm$(ln$))
00410   a$(j3,1)=ln$(p1+1:p2-1)
00420   a$(j3,2)=ln$(p2+1:p3-1)
00430   a$(j3,3)=ln$(p3+1:p4-1)
00440   abbrev$=ln$(p4+1:len(ln$))(1:20)
00450   if rtrm$(a$(j3,3))="" then goto L810
00460   p1=pos(a$(j3,3)," ",1)+1
00470   p2=pos(a$(j3,3),".",1)+1
00480   p3=len(rtrm$(a$(j3,3)))
00490   p4=pos(a$(j3,3),"*",1)
00500   if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
00510   l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
00520   if p2>1 then dp=val(a$(j3,3)(p2:p3)) else dp=0 !:
          ! DECIMAL POSITIONS
00530   if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !:
          !   ACTUAL FIELD LENGTH
00540   l=l*m1 ! TOTAL STORAGE LENGTH
00550   b=a+l
00560   a=a+1
00570   ino=ino+1
00580   j3=1
00590   a(j3,1)=ino
00600   a(j3,2)=al
00610   a(j3,3)=dp
00620   a(j3,4)=l
00630   a(j3,5)=a
00640   a(j3,6)=b
00650   a=b
00660   rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
00670 ! SPECIAL ROUTINE TO PLACE CORRECT SERVICE NAME !:
        ! ON EACH SERVICE IN UTILITY BILLING
00680   if uprc$(a$(j3,1)(1:7))<>"SERVICE" then goto L730
00690   x=val(a$(j3,1)(9:10)) conv L730
00700   if trim$(servicename$(x))="" then goto L810 ! SERVICE NOT USED
00710   a$(j3,1)(1:9)=""
00720   a$(j3,1)=trim$(servicename$(x))&" "&trim$(a$(j3,1))
00730 L730: if uprc$(abbrev$)(1:7)<>"SERVICE" then goto L770
00740   x=val(abbrev$(9:10)) conv L770
00750   abbrev$(1:9)=""
00760   abbrev$=trim$(servicename$(x))&" "&trim$(abbrev$)
00770 L770: if rtrm$(a$(j3,1))="" or rtrm$(uprc$(a$(j3,1)))='UNUSED' or rtrm$(uprc$(a$(j3,1)))(1:5)='EXTRA' or trim$(abbrev$)="" then goto L810
00780 ! store as description,variable name,field length,# of deciaml points, format
00790   write #15,using L800: trim$(a$(j3,1)(1:30)),a$(j3,2),a(j3,2),a(j3,3),a$(j3,3),abbrev$(1:20)
00800 L800: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
00810 L810: goto L330
00820 ! ______________________________________________________________________
00830 L830: close #2: ioerr L840
00840 L840: close #15: ioerr L850
00850 L850: gosub MOVEITTOTEXT
00860   pr f "24,1,C 7,UT,N": "Done..."
00870   stop 
00880 ! ______________________________________________________________________
00890 MOVEITTOTEXT: ! 
00900   open #10: "Name="&outputfile$&",RecL=87,Replace",display,output 
00910   open #15: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$&",RecL=87,KPs=1,KLn=30,use",internal,outin,keyed 
00920 L920: read #15,using L930: textfile$ eof L960
00930 L930: form pos 1,c 87
00940   pr #10,using L930: textfile$
00950   goto L920
00960 L960: return 
00970 ! ______________________________________________________________________
00980 ! <Updateable Region: ERTN>
00990 ERTN: fnerror(program$,err,line,act$,"xit")
01000   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01010   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01030 ERTN_EXEC_ACT: execute act$ : goto ERTN
01040 ! /region
