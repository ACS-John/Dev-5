00010 ! Replace Work\PrintLay
00020 ! ______________________________________________________________________
00025   library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn
00026   on error goto ERTN
00027 ! ______________________________________________________________________
00030   dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
00040   dim a(200,6),a$*132,prg$*20,mo$(12)
00050   data JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER
00055 ! ______________________________________________________________________
00060   read mat mo$
00070   let dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",19"&date$(1:2)
00080   let io1$(1)="10,65,N 1,UT,N"
00090   let io1$(2)="12,65,C 14,UT,N"
00100   pr newpage
00110   close #101: ioerr L120
00120 L120: open #101: "SROW=9,SCOL=4,EROW=13,ECOL=79,BORDER=DR,CAPTION=PRINT LAYOUTS",display,outin 
00130   pr #101: newpage
00140   pr fields "10,5,C 60": "Enter 0 for Printer only, 1 for Screen only, or 2 for Both:"
00150   pr fields "12,5,C 60": "Enter Ext/VolId to pr all or blank to select:"
00160 L160: input fields mat io1$: pp,ev$ conv L160
00165   if pp=0 then let fnopenprn
00170   if pp<0 or pp>2 then goto L160
00180   let ev$=rtrm$(ev$)
00190   if ev$="" then goto L260
00200   execute "DROP DIRFILE" ioerr L210
00210 L210: execute "DIR *."&ev$&" >DIRFILE"
00220   open #2: "Name=DIRFILE",display,input 
00230   let p1=pos(ev$,"/",1)
00240   let ex$=ev$(1:p1-1)
00250   goto L360
00260 L260: close #101: ioerr L270
00270 L270: open #101: "SROW=2,SCOL=4,EROW=6,ECOL=79,BORDER=DR,CAPTION=PRINT LAYOUTS",display,outin 
00280   pr #101: newpage
00290   if f1>0 then pr fields "2,5,C 60,H,N": "LAST FILE NAME ENTER WAS "&fil$(f1)
00300   pr fields "4,5,C 60": "Enter File Name/VolId to pr or Blank to stop"
00310   rinput fields "4,55,C 20,UE,N": fil$(f1+1)
00320   let fil$(f1+1)=rtrm$(fil$(f1+1))
00330   if pp>0 or fil$(f1+1)="" then goto L360
00340   let f1=f1+1
00350   goto L260
00360 L360: let f2=f2+1: let rl=l=a=b=ino=pg=j3=0: mat a=(0)
00370   if ev$="" then goto L430
00380 L380: linput #2: a$ eof L1390
00390   a$=uprc$(a$)
00400   if rtrm$(a$(11:13))><ex$ then goto L380
00410   let fil$(1)=rtrm$(a$(1:8))&"."&ev$
00420   let f2=1: let f1=2
00430 L430: if rtrm$(fil$(f2))="" then goto L1390
00440   open #1: "Name="&fil$(f2),display,input ioerr L360
00450   goto L660
00460 L460: let p1=pos(filename$,".",1)
00470   if p1=0 then let p1=min(8,len(filename$)) else let p1=p1-1
00480   pr newpage
00490   pr fields "1,1,C 40,R,N": " FIELD DESCRIPTIONS: "&filename$(1:p1)&"/"&volid$
00500   pr fields "1,43,C 8,R,N": " NAME"
00510   pr fields "1,53,C 8,R,N": " FORMAT"
00520   pr fields "1,63,C 5,R,N": " FROM"
00530   pr fields "1,70,C 5,R,N": "  TO"
00540   let sln=1
00550 L550: if pp=0 or j3=0 then goto L660
00560   if sln=0 then goto L460
00570   if sln<23 then let sln=sln+1: goto L610
00580   pr fields "24,5,C 60,R,N": "  SCREEN FULL  PRESS ENTER TO CONTINUE:"
00590   input fields "24,50,C 1,RE,N": pause$
00600   goto L460
00610 L610: pr fields str$(sln)&",1,C 40,UT,N": a$(j3,1)
00620   pr fields str$(sln)&",43,C 8,UT,N": a$(j3,2)(1:10)
00630   pr fields str$(sln)&",53,C 8,UT,N": a$(j3,3)(1:8)
00640   pr fields str$(sln)&",63,PIC(ZZZZZ),UT,N": a(j3,5)
00650   pr fields str$(sln)&",70,PIC(ZZZZZ),UT,N": a(j3,6)
00660 L660: linput #1: ln$ eof L1200
00670   if ln$(7:10)="LET " then goto LETLN
00680   if ln$(7:10)="DATA" then goto DATALN
00690   let p1=pos(ln$,"Replace",6)
00700   if p1=0 then goto L740
00710   let p1=p1+8
00720   let p2=pos(ln$,",",p1)
00730   let prg$=ln$(p1:p2-1)
00740 L740: goto L550
00750 LETLN: let p2=len(rtrm$(ln$))-1
00760   let p1=pos(ln$,"H1$",1)
00770   if p1>0 then let h1$=ln$(p1+5:p2) : goto L550
00780   let p1=pos(ln$,"FILETYPE$",1)
00790   if p1>0 then let filetype$=ln$(p1+11:p2): goto L550
00800   let p1=pos(ln$,"FILENAME$",1)
00810   if p1>0 then let filename$=ln$(p1+11:p2): goto L550
00820   let p1=pos(ln$,"VOLID$",1)
00830   if p1>0 then let volid$=ln$(p1+8:p2): goto L550
00840   let p1=pos(ln$,"RM$",1)
00850   if p1=0 then goto L880
00860   let rm=val(ln$(p1+4:p1+4))
00870   let rm$(rm)=ln$(p1+8:p2)
00880 L880: goto L550
00890 DATALN: let j3=j3+1
00900   let p1=11
00910   let p2=pos(srep$(ln$,'^','~'),'~',p1+1)
00920   let p3=pos(srep$(ln$,'^','~'),'~',p2+1)
00930   let p4=len(rtrm$(ln$))
00940   a$(j3,1)=ln$(p1:p2-1)
00950   a$(j3,2)=ln$(p2+1:p3-1)
00960   a$(j3,3)=ln$(p3+1:p4)
00970 L970: form c 9,skip 0
00980   if rtrm$(a$(j3,3))="" then goto L1190
00990   let p1=pos(a$(j3,3)," ",1)+1
01000   let p2=pos(a$(j3,3),".",1)+1
01010   let p3=len(rtrm$(a$(j3,3)))
01020   let p4=pos(a$(j3,3),"*",1)
01030   if p4=0 then let m1=1 else let m1=val(a$(j3,3)(1:p4-1))
01040   let l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
01050   if p2>1 then let dp=val(a$(j3,3)(p2:p3)) else let dp=0 ! DECIMAL POSITIONS
01060   if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !   ACTUAL FIELD LENGTH
01070   let l=l*m1 ! TOTAL STORAGE LENGTH
01080   b=a+l
01090   a=a+1
01100   let ino=ino+1
01110   a(j3,1)=ino
01120   a(j3,2)=al
01130   a(j3,3)=dp
01140   a(j3,4)=l
01150   a(j3,5)=a
01160   a(j3,6)=b
01170   a=b
01180   let rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
01190 L1190: goto L550
01200 L1200: let pgo=ceil(j3/24)
01210   if pp=1 then goto L1370
01220   gosub HDR
01230   for j=1 to j3
01240     let p1=pos(a$(j,3)," ",1)
01250     let p2=len(a$(j,3))
01260     let l=val(a$(j,3)(p1:p2))
01270     if l>0 then goto L1310
01280     pr #255,using L1290: a$(j,1) pageoflow NEWPGE
01290 L1290: form pos 13,c 43,skip 2
01300     goto L1340
01310 L1310: if rtrm$(a$(j,1))="" then goto L1340
01320     pr #255,using L1330: a(j,1),a$(j,1),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6) pageoflow NEWPGE
01330 L1330: form pos 5,n 5,x 3,c 43,c 11,n 7,n 10,x 5,c 11,n 7,2*n 9,skip 2
01340 L1340: next j
01350   pr #255: newpage
01360   pr #255,using L970: hex$("1B40")
01365   let fncloseprn
01370 L1370: close #1: 
01380   if f2<f1 then goto L360
01390 L1390: close #2: ioerr L1400
01400 L1400: pr fields "24,1,C 7,UT,N": "Done..."
01410   stop 
01420 NEWPGE: if j=j3 then goto L1450
01430   pr #255: newpage
01440   gosub HDR
01450 L1450: continue 
01460 HDR: ! 
01470   pr #255,using L970: hex$("2B0205000A1042")
01480   pr #255,using L970: hex$("1B471B2D00")
01490   let pg=pg+1
01500   pr #255,using L970: hex$("1B5701")
01510   pr #255,using L1520: h1$
01520 L1520: form pos 3,c 46,skip 2
01530   pr #255,using L970: hex$("1B57001B53000F")
01540   pr #255,using L1550: prg$,dat$,pg,pgo
01550 L1550: form pos 51,c 40,c 20,"PAGE:",n 3,"  OF:",n 3,skip 1
01560   pr #255,using L970: hex$("1B54")
01570   pr #255,using L970: hex$("2B0205000A0000")
01580   pr #255: tab(30);"----------------- REMARKS -------------------"
01590   pr #255,using L1600: str$(rl),rm$(1)
01600 L1600: form pos 5,"RECORD LENGTH: ",c 5,pos 30,c 45,skip 1
01610   pr #255,using L1620: rm$(2)
01620 L1620: form pos 30,c 45,skip 1
01630   pr #255,using L1640: rm$(3)
01640 L1640: form pos 30,c 45,skip 1
01650   pr #255,using L1660: filetype$,rm$(4)
01660 L1660: form pos 5,"FILE TYPE: ",c 10,pos 30,c 45,skip 2
01670   pr #255,using L1680: filename$,volid$
01680 L1680: form pos 5,"FILE NAME: ",c 20,"VOLID: ",c 10,skip 2
01690   pr #255,using L970: hex$("1B0F")
01700   pr #255: "   ITEM #   FIELD DESCRIPTION                           NAME       LENGTH    DECIMALS    FORMAT    STORAGE     FROM      TO  "
01710   pr #255: "   ------   ----------------------------------------   --------    ------    --------   -------    -------    -----    -----"
01720   pr #255,using L970: hex$("1B2D01")
01730   return 
01740 ! ______________________________________________________________________
01750 ! <Updateable Region: ERTN>
01760 ERTN: let fnerror(program$,err,line,act$,"xit")
01770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01800 ERTN_EXEC_ACT: execute act$ : goto ERTN
01810 ! /region
