00010 ! Replace S:\Core\jbLabel.br
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fndat,fnwin3b,fnerror,fnwait,fnlabel,fnaddlabel
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim l$(5)*70,iom$(10),cnam$*40,dat$*20,pt$(5)*3,cap$*128,message$*40
00080   dim msgline$(2)*60,wrd01$(2)*32,iomc$(5),iomr$(5),wrd_align$(3)
00090   dim wrd_style$(6),iom$(5),lw(5),sw(6),style$(6)
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$)
00120   fndat(dat$,1)
00130 ! ______________________________________________________________________
00140   on fkey 5 goto SCR_MAIN
00150 SCR_MAIN: ! 
00160   cap$="Print Custom Labels"
00170   win=101
00180   ce=0
00190   for j=1 to 5
00200     if lw(j)=0 then lw(j)=34
00210     if pt$(j)="" then pt$(j)="S"
00220     if align(j)<1 or align>3 then align(j)=1
00230     iomc$(j)=str$(j+3)&",2,Cc "&str$(lw(j))&",U,AE,N" !:
          iomr$(j)=str$(j+3)&",2,Cr "&str$(lw(j))&",U,AE,N" !:
          ioml$(j)=str$(j+3)&",2,C "&str$(lw(j))&",U,AE,N"
00240     if align(j)=1 then iom$(j)=ioml$(j)
00250     if align(j)=2 then iom$(j)=iomc$(j)
00260     if align(j)=3 then iom$(j)=iomr$(j)
00270     l$(j)=rtrm$(ltrm$(l$(j)))(1:lw(j))
00280   next j
00290   win_width=50 !:
        for j=1 to udim(l$) !:
          win_width=max(lw(j),win_width) !:
        next j !:
        win_width=win_width+4
00300   fnwin3b(win,cap$,12,win_width,0,0,5,0)
00310   pr #win,fields "3,2,C 20,N": "Enter your label:"
00320   pr #win,fields "10,2,C "&str$(win_width-2)&",B,8": "(F8) "&cnam$(1:win_width-2)
00330   pr #win,fields "11,2,C 25,B,9": "(F9) "&dat$
00340   pr f "19,14,C 10,B,1": "Print (F1)"
00350   pr f "19,25,C 16,B,2": "Align  Line (F2)"
00360   pr f "19,42,C 15,B,3": "Line Style (F3)"
00370   pr f "19,58,C 10,B,99": "Exit (Esc)"
00380 L380: rinput #win,fields mat iom$: mat l$
00390   main_curfld=curfld
00400   if ce>0 then iom$(ce)(ce1:ce2)="U": ce=0
00410   if cmdkey>0 then goto L480 else ce=curfld
00420 L420: ce=ce+1: if ce>udim(iom$) then ce=1
00430 L430: iom$(ce)=rtrm$(iom$(ce)) : ce1=pos(iom$(ce),"U",1) : if ce1=0 then goto L420
00440   ce2=ce1+1 : iom$(ce)(ce1:ce1)="UC" : goto L380
00450 CONV1: if ce>0 then iom$(ce)(ce1:ce2)="U"
00460   ce=cnt+1
00470 ERR1: pr f "24,78,C 1": bell : goto L430
00480 L480: if cmdkey=1 then goto SCR_HOW_MANY
00490   if cmdkey=2 and main_curfld<6 then gosub ALIGNMENT !:
          ce=main_curfld : goto SCR_MAIN
00500   if cmdkey=3 then gosub LINE_STYLE !:
          ce=main_curfld : goto SCR_MAIN
00510   if cmdkey=8 then !:
          l$(main_curfld)=cnam$(1:lw(main_curfld)) : goto L380
00520   if cmdkey=9 then !:
          l$(main_curfld)=dat$(1:lw(main_curfld)) : goto L380
00530   if cmdkey=5 or cmdkey=99 then goto XIT
00540   goto SCR_MAIN
00550 ! ______________________________________________________________________
00560 SCR_HOW_MANY: ! 
00570   win=103
00580   fnwin3b(win,cap$,3,33,0,3,5,0)
00590   pr #win,fields "2,2,C 26,N": "Number of Labels to print:"
00600   if h=0 then h=1
00610   rinput #win,fields "2,29,Nz 3,UT,N" : h
00620   close #win: 
00630   if cmdkey=5 then goto SCR_MAIN
00640 ! ______________________________________________________________________
00650   numberoflines=h*5
00655   fnwait(0,cap$,message$,1)
00660   for j=1 to h !:
          fnaddlabel(mat l$) !:
        next j
00665   close #111: ioerr L670
00670 L670: fnlabel(101,cap$,mat pt$,cp,nw)
00680   goto SCR_MAIN
00690 ! ______________________________________________________________________
00700 XIT: chain "MENU"
00710 ! ______________________________________________________________________
00720 ALIGNMENT: ! 
00730   win2=103
00740   fnwin3b(win2,cap$,5,33,0,1,2,2)
00750   wrd_align$(1)="Left" !:
        wrd_align$(2)="Center" !:
        wrd_align$(3)="Right"
00760   if iom$(main_curfld)=ioml$(main_curfld) then !:
          iowa$(1)="2,26,C 6,CN" else !:
          iowa$(1)="2,26,C 6,N"
00770   if iom$(main_curfld)=iomc$(main_curfld) then !:
          iowa$(2)="3,26,C 6,CN" else !:
          iowa$(2)="3,26,C 6,N"
00780   if iom$(main_curfld)=iomr$(main_curfld) then !:
          iowa$(3)="4,26,C 6,CN" else !:
          iowa$(3)="4,26,C 6,N"
00790   pr #win2,fields "2,2,C 23,N": "Current Line Alignment:"
00800   rinput #win2,select mat iowa$,attr "H": mat wrd_align$
00810   close #win2: 
00820   align(main_curfld)=curfld
00830   if cmdkey=5 then goto L870
00840 ! If ALIGN(MAIN_CURFLD)=1 Then lA$(MAIN_CURFLD)=IOML$(MAIN_CURFLD)
00850 ! If ALIGN(MAIN_CURFLD)=2 Then lA$(MAIN_CURFLD)=IOMC$(MAIN_CURFLD)
00860 ! If ALIGN(MAIN_CURFLD)=3 Then lA$(MAIN_CURFLD)=IOMR$(MAIN_CURFLD)
00870 L870: return 
00880 ! ______________________________________________________________________
00890 LINE_STYLE: ! 
00900   win2=103
00910   fnwin3b(win2,cap$,8,33,0,1,2,2)
00920   wrd_style$(1)="Standard " : style$(1)="S" : sw(1)=34 !:
        wrd_style$(2)="Bold     " : style$(2)="B" : sw(2)=29 !:
        wrd_style$(3)="Elete    " : style$(3)="E" : sw(3)=41 !:
        wrd_style$(4)="Condensed" : style$(4)="C" : sw(4)=59 !:
        wrd_style$(5)="Wide     " : style$(5)="W" : sw(5)=17 !:
        wrd_style$(6)="Bar Code " : style$(6)="BAR" : sw(6)=14
00930   for j=1 to udim(wrd_style$)
00940     if pt$(main_curfld)=style$(j) then !:
            iows$(j)=str$(j+1)&",23,C 9,CN" else !:
            iows$(j)=str$(j+1)&",23,C 9,N"
00950   next j
00960   pr #win2,fields "2,2,C 19,N": "Current Line Style:"
00970   rinput #win2,select mat iows$,attr "H": mat wrd_style$
00980   close #win2: 
00990   style=curfld
01000   if cmdkey=5 then goto L1020
01010   pt$(main_curfld)=style$(style) !:
        lw(main_curfld)=sw(style)
01020 L1020: return 
01030 ! ______________________________________________________________________
01040 ERTN: fnerror(program$,err,line,act$,"xit")
01050   if uprc$(act$)<>"PAUSE" then goto L1080
01060   execute "list -"&str$(line) !:
        pause  !:
        goto L1080
01070   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01080 L1080: execute act$
01090   goto ERTN
