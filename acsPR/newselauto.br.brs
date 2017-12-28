00010 ! Replace S:\acsPR\newSelAuto
00020 ! Select Automatic Processing Programs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnTos,fnflexinit1,fnflexadd1,fnCmdKey,fnAcs,fnTxt,fnLbl,fnChk
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim m$(200)*80,pgm$(200)*22,hlpg$(200)*40,status$(200),cap$*128,xec$*80
00080   dim mx$(200)*80,pgmx$(200)*22,statusx$(200),pgm(200),mo$(12)*10
00090   dim cnam$*40,dat$*20,s(3),s(11),pedat$*20,autoproc$(200)*80,temp$(20)*80
00100   dim cch$*20,actpd$*6,iom$(2),scm$(2),sendto$*40,xec$*80,rpmsg$*46
00110   dim sf1$(20),l1(20),desc$*35,sf$(20),sd$(20)*35,prog$*8,srq(20)
00120   dim nxtpgm$(20)*20,nxtdesc$(20)*35,ff$(3)*18,wk(20),mo(20),qt(20)
00130   dim ln$*128,ln2$*128,item$(1)*35,resp$(100)*35,prg$(100)*40,nam$(100)*35
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="Select Auto Processing Programs")
00160   fncno(cno)
00170   open #1: "Name="&env$('Q')&"\PRmstr\NewPrPgmn.h"&env$('cno')&",Use,RecL=61",internal,outIn,relative ioerr MAIN
00180   for j=1 to lrec(1)
00190     read #1,using L810: nxtpgm$(j),nxtdesc$(j),wk(j),mo(j),qt(j) noRec L200
00200 L200: next j
00210   form pos 1,c 20,c 35,n 3,3*n 1
00220   close #1: 
00230 MAIN: ! 
00240 L240: fnTos(sn$="PrAuto") !:
        mylen=20: mypos=mylen+3 : right=1
00250   item=0: resp=0
00260   fnLbl(1,1,"Selected Items                       Wk Qtr Anl       Menu Options to Select From",80,0)
00270   for j=1 to 20
00280 ! If TRIM$(NXTDESC$(J))="Employee" Then nXTDESC$(J)="": nXTPGM$(J)=""
00290     fnTxt(j+1,1,35,0,left,"",0,"Items select for automatic processing.",0 ) !:
          resp$(resp+=1)=nxtdesc$(j)
00300     fnChk(j+1,39,"",1)
00310     if wk(j)=1 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
00320     fnChk(j+1,43,"",1)
00330     if mo(j)=1 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
00340     fnChk(j+1,47,"",1)
00350     if qt(j)=1 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
00360   next j
00370   mat chdr$(2) : mat cmask$(2) : mat item$(2) !:
        chdr$(1)='Item #' !:
        chdr$(2)='Menu option'
00380   cmask$(1)='30' !:
        cmask$(2)='' !:
        fnflexinit1('selauto',2,55,20,35,mat chdr$,mat cmask$,1,0,frame) !:
        editrec=0
00390   close #1: ioerr L400
00400 L400: open #1: "Name=PR.mnu",display,input 
00410 L410: linput #1: ln$ eof L540
00420   if ln$(1:1)<>">" then goto L410 ! skip headings
00430   if ln$(1:1)=">" then ln$(1:1)=""
00440   if ln$(1:1)=">" then ln$(1:1)="" ! delete up to two >>
00450   x=pos(srep$(ln$,'^','~'),'~',1) ! pos(ln$,"^",1)
00460   if x=0 then goto L410 ! skip headings
00470   desc$=ln$(1:x-1)(1:35)
00480   item+=1
00490   y=pos(srep$(ln$,'^','~'),'~',x) ! pos(ln$,"^",x)
00500   prg$(item)=ln$(x+1:len(ln$))
00510   nam$(item)=ln$(1:x-1)(1:35)
00520   item$(1)=str$(item) !:
        item$(2)=desc$ !:
        fnflexadd1(mat item$)
00530   goto L410
00540 L540: fnLbl(22,1," ")
00550   fnCmdKey("&Next",1,1,0,"Selects the highlited option for automatic processing.")
00560   fnCmdKey("&Save",2,0,0,"Saves the selections and returns to menu.")
00570   fnCmdKey("&Delete All",4,0,0,"Deletes all selections.")
00580   fnCmdKey("&Cancel",5,0,1,"Returns to main menu without saving the selections.")
00590   fnAcs(sn$,0,mat resp$,ckey)
00600   if ckey=5 then goto XIT
00610   if ckey=2 then goto L630
00620   if ckey=4 then mat nxtdesc$=(""): !:
          mat nxtpgm$=("") !:
          mat wk=(0) !:
          mat mo=(0) !:
          mat qt=(0) !:
          execute "drop "&env$('Q')&"\PRmstr\NewPrPgmn.h"&env$('cno')&" -n" !:
          goto MAIN
00630 L630: sel=val(resp$(81))
00640   x=0
00650   for j=1 to 20
00660     if resp$(j+(x+=1))="True" then wk(j)=1 else wk(j)=0
00670     if resp$(j+(x+=1))="True" then mo(j)=1 else mo(j)=0
00680     if resp$(j+(x+=1))="True" then qt(j)=1 else qt(j)=0
00690   next j
00700   for j=1 to 20
00710     if trim$(nxtdesc$(j))="Employee" then nxtdesc$(j)="": nxtpgm$(j)="": wk(j)=0: mo(j)=0: qt(j)=0: goto L740
00720     if sel=1 then goto L740
00730     if trim$(nxtdesc$(j))="" then nxtdesc$(j)=nam$(sel): nxtpgm$(j)=prg$(sel): goto L750
00740 L740: next j
00750 L750: if ckey=1 then goto L240
00760   execute "drop "&env$('Q')&"\PRmstr\NewPrPgmn.h"&env$('cno')&" -n"
00770   close #1: ioerr L780
00780 L780: open #1: "Name="&env$('Q')&"\PRmstr\NewPRPgmn.h"&env$('cno')&",Use,RecL=61",internal,outIn,relative 
00790   for j1=1 to 20 !:
          if j1>lrec(3) then write #1,using L810: nxtpgm$(j1),nxtdesc$(j1),wk(j1),mo(j1),qt(j1)
00800   next j1
00810 L810: form pos 1,c 20,c 35,3*n 1
00820   close #1: ioerr L830
00830 L830: close #3: ioerr XIT
00840 XIT: fnxit
00850 ! ______________________________________________________________________
00860 ! <Updateable Region: ERTN>
00870 ERTN: fnerror(program$,err,line,act$,"xit")
00880   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00890   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00900   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00910 ERTN_EXEC_ACT: execute act$ : goto ERTN
00920 ! /region
00930 ! ______________________________________________________________________
