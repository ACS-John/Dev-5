00010 ! Replace S:\Core\Programs\PrintLay1
00020 ! pr Several Layouts
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnerror,fnwin3b,fnxit,fnopenprn,fncloseprn,fnconsole,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$(200,3)*80,h1$*55,rm$(4)*100,filename$*50,fil$(50)*80,ln$*120
00072   dim ev$*50
00080   dim a(200,6),a$*132,prg$*30,mo$(12),cap$*128
00090 ! ______________________________________________________________________
00100   fntop("S:\Core\PrintLay",cap$="Print Several Layouts")
00110   data January,February,March,April,May,June,July,August,September,October,November,December
00120   read mat mo$
00130 ! ______________________________________________________________________
00140   fnconsole(on=1)
00150   dat$=mo$(val(date$("MM")))&" "&date$("DD")&", "&date$("CCYY")
00160   fnwin3b(win=101,cap$,4,58,0,2)
00170 ! pr #WIN,Fields "2,2,Cr 41,N": "0.Printer; 1.Screen; 2.Both:"
00180   pr #win,fields "3,2,Cr 41,N": "Ext/VolId to pr all (blank to select):"
00190   io1$(1)="2,44,N 1,U,N"
00200   io1$(2)="3,44,14/c 50,U,N"
00210   ev$="lay/S:\acsTM\Layouts"
00220 L220: rinput #win,fields io1$(2): ev$ conv CONV1 ! pp was on io1$(1)
00230   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00240   if cmdkey>0 then goto L310 else ce=curfld
00250 L250: ce=ce+1: if ce>udim(io1$) then ce=1
00260 L260: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L250
00270   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L220
00280 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00290   ce=cnt+1
00300 ERR1: pr f "24,78,C 1": bell : goto L260
00310 L310: if cmdkey=5 then goto XIT
00320   if pp<0 or pp>2 then ce=1 : goto ERR1
00330   if pp=1 then goto L350
00340   fnopenprn(cp,58,220,process)
00350 L350: ev$=rtrm$(ev$)
00360   if ev$="" then goto L430
00370   execute "DROP DirFile" ioerr L380
00380 L380: execute "DIR *."&ev$&" >DirFile"
00390   open #2: "Name=DirFile",display,input 
00400   p1=pos(ev$,"/",1)
00410   ex$=ev$(1:p1-1)
00420   goto L530
00430 L430: close #101: ioerr L440
00440 L440: open #101: "SROW=2,SCOL=4,EROW=6,ECOL=79,BORDER=DR,CAPTION="&cap$,display,outin 
00450   pr #101: newpage
00460   if f1>0 then pr f "2,5,C 60,H,N": "LAST FILE NAME ENTER WAS "&fil$(f1)
00470   pr f "4,5,C 60,N": "Enter File Name/VolId to pr or Blank to stop"
00480   rinput fields "4,55,C 20,UE,N": fil$(f1+1)
00490   let fil$(f1+1)=rtrm$(fil$(f1+1))
00500   if pp>0 or fil$(f1+1)="" then goto L530
00510   f1=f1+1
00520   goto L430
00530 L530: let f2=f2+1: rl=l=a=b=ino=pg=j3=0: mat a=(0)
00540   if ev$="" then goto L590
00550 L550: linput #2: a$ eof L1600
00560   if uprc$(rtrm$(a$(11:13)))><uprc$(ex$) then goto L550
00570   let fil$(1)=rtrm$(a$(1:8))&"."&ev$
00580   let f2=1: f1=2
00590 L590: if rtrm$(fil$(f2))="" then goto L1600
00600   open #1: "Name="&fil$(f2),display,input ioerr L530
00610   goto L830
00620 ! ______________________________________________________________________
00630 L630: p1=pos(filename$,".",1)
00640   if p1=0 then p1=min(8,len(filename$)) else p1=p1-1
00650   pr newpage
00660   pr f "1,1,C 40,R,N": " Field Descriptions: "&filename$(1:p1)&"/"&volid$
00670   pr f "1,43,C 8,R,N": " Name"
00680   pr f "1,53,C 8,R,N": " Format"
00690   pr f "1,63,C 5,R,N": " From"
00700   pr f "1,70,C 5,R,N": "  To"
00710   sln=1
00720 L720: if pp=0 or j3=0 then goto L830
00730   if sln=0 then goto L630
00740   if sln<23 then sln=sln+1: goto L780
00750   pr f "24,5,C 60,R,N": "  SCREEN FULL  PRESS ENTER TO CONTINUE:"
00760   input fields "24,50,C 1,RE,N": pause$
00770   goto L630
00780 L780: pr f str$(sln)&",1,C 40,U,N": a$(j3,1)
00790   pr f str$(sln)&",43,C 8,U,N": a$(j3,2)(1:10)
00800   pr f str$(sln)&",53,C 8,U,N": a$(j3,3)(1:8)
00810   pr f str$(sln)&",63,PIC(ZZZZZ),U,N": a(j3,5)
00820   pr f str$(sln)&",70,PIC(ZZZZZ),U,N": a(j3,6)
00830 L830: linput #1: ln$ eof L1410
00840   if uprc$(ln$(7:10))=uprc$("LET ") then goto LETLN
00850   if uprc$(ln$(7:10))=uprc$("DATA") then goto DATALN
00860 ! If UPRC$(LN$(7:7))=UPRC$("!") Then pr #255: LN$(9:LEN(LN$))
00870   p1=pos(ln$,"REPLACE",6)
00880   if p1=0 then goto L920
00890   p1=p1+8
00900   p2=pos(ln$,",",p1)
00910   prg$=ln$(p1:p2-1)
00920 L920: goto L720
00930 ! ______________________________________________________________________
00940 LETLN: p2=len(rtrm$(ln$))-1
00950   p1=pos(uprc$(ln$),"H1$",1)
00960   if p1>0 then h1$=ln$(p1+5:p2) : goto L720
00970   p1=pos(uprc$(ln$),"FILETYPE$",1)
00980   if p1>0 then let filetype$=ln$(p1+11:p2): goto L720
00990   p1=pos(uprc$(ln$),"FILENAME$",1)
01000   if p1>0 then let filename$=ln$(p1+11:p2): goto L720
01010   p1=pos(uprc$(ln$),"VOLID$",1)
01020   if p1>0 then volid$=ln$(p1+8:p2): goto L720
01030   p1=pos(uprc$(ln$),"RM$",1)
01040   if p1=0 then goto L1070
01050   rm=val(ln$(p1+4:p1+4))
01060   rm$(rm)=ln$(p1+8:p2)
01070 L1070: goto L720
01080 ! ______________________________________________________________________
01090 DATALN: j3=j3+1
01100   p1=11
01110   p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
01120   p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
01130   p4=len(rtrm$(ln$))
01140   a$(j3,1)=ln$(p1:p2-1)
01150   a$(j3,2)=ln$(p2+1:p3-1)
01160   a$(j3,3)=ln$(p3+1:p4)
01170   form c 9,skip 0
01180   if rtrm$(a$(j3,3))="" then goto L1390
01190   p1=pos(a$(j3,3)," ",1)+1
01200   p2=pos(a$(j3,3),".",1)+1
01210   p3=len(rtrm$(a$(j3,3)))
01220   p4=pos(a$(j3,3),"*",1)
01230   if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
01240   l=int(val(a$(j3,3)(p1:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1)))) ! FIELD STORAGE LENGTH !:
        ! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)         was      P3
01250   if p2>1 then dp=val(a$(j3,3)(p2:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1))) else dp=0 ! DECIMAL POS. !:
          ! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)      was     P3
01260   if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !   ACTUAL FIELD LENGTH
01270   l=l*m1 ! TOTAL STORAGE LENGTH
01280   b=a+l
01290   a=a+1
01300   ino=ino+1
01310   a(j3,1)=ino
01320   a(j3,2)=al
01330   a(j3,3)=dp
01340   a(j3,4)=l
01350   a(j3,5)=a
01360   a(j3,6)=b
01370   a=b
01380   rl=rl+int(val(a$(j3,3)(p1:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1))))*m1 !:
        ! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)      was    P3
01390 L1390: goto L720
01400 ! ______________________________________________________________________
01410 L1410: pgo=ceil(j3/24)
01420   if pp=1 then goto L1580
01430   gosub HDR
01440   for j=1 to j3
01450     p1=pos(a$(j,3)," ",1)
01460     p2=len(a$(j,3))
01470     l=val(a$(j,3)(p1:min(pos(srep$(a$(j,3),'^','~'),'~')-1,pos(a$(j,3),chr$(9))-1))) ! min(pos(srep$(a$(j,3),'^','~'),'~')-1,POS(A$(J,3),chr$(9))-1)   was    P2
01480     if l>0 then goto L1520
01490     pr #255,using L1500: a$(j,1)(1:43) ! Pageoflow NEWPGE
01500 L1500: form pos 13,c 43,skip 2
01510     goto L1550
01520 L1520: if rtrm$(a$(j,1))="" then goto L1550
01530     a$(j,3)=a$(j,3)(1:pos(a$(j,3),'^')-1)
01540     pr #255,using 'Form POS 5,N 5,X 3,C 43,C 21,N 7,N 10,X 5,C 11,N 7,2*N 9': a(j,1),a$(j,1)(1:43),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6) ! Pageoflow NEWPGE
01550 L1550: next j
01560 ! pr #255: NEWPAGE
01570 ! pr #255,Using 1180: HEX$("1B40")
01580 L1580: close #1: 
01590   if f2<f1 then goto L530
01600 L1600: close #2: ioerr L1620
01610   fncloseprn
01620 L1620: goto XIT
01630 ! ______________________________________________________________________
01640 XIT: fnxit
01650 ! ______________________________________________________________________
01660 NEWPGE: if j=j3 then goto L1690
01670 ! pr #255: NEWPAGE
01680   gosub HDR
01690 L1690: continue 
01700 ! ______________________________________________________________________
01710 HDR: ! 
01720   pr #255,using 'Form POS 1,C 80': "{\b{\qc{\fs28 "&h1$&"}}}"
01730 ! pr #255,Using 'Form POS 5,"Record Length: ",C 5,POS 30,C 45': PRG$,DAT$,PG+=1,PGO
01740 ! Form POS 51,C 40,C 20,"Page ",N 3,"  of ",N 3
01750   pr #255,using 'Form POS 5,"Record Length: ",C 5,POS 30,C 100': str$(rl),rm$(1)
01760   if trim$(rm$(2))<>'' then pr #255,using 'Form POS 30,C 100': rm$(2)
01770   if trim$(rm$(3))<>'' then pr #255,using 'Form POS 30,C 100': rm$(3)
01780   pr #255,using 'Form POS 5,"File Type: ",C 10,POS 30,C 100': filetype$,rm$(4)
01790   pr #255,using 'Form POS 5,"File Name: ",C 50,"Directory: ",C 10': filename$,volid$
01800   pr #255: ""
01810   pr #255: "   Item     Field Description                           Name                 Length    Decimals    Format    Storage     From      To  "
01820   pr #255: "   ______   ________________________________________   __________________    ______    ________   _______    _______    _____    _____"
01830   return 
01840 ! ______________________________________________________________________
01850 ! <Updateable Region: ERTN>
01860 ERTN: fnerror(program$,err,line,act$,"xit")
01870   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01890   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01900 ERTN_EXEC_ACT: execute act$ : goto ERTN
01910 ! /region
01920 ! ______________________________________________________________________
