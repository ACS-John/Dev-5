00010 ! Replace S:\acsPR\jcPctCmp
00020 ! Enter Percent Complete
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnoldmsgbox,fncno,fnerror,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,msgline$(2)*60,response$(5)*1
00080   dim jn$*6,jno$*6,n$*40,cn$*11,cnt$*5,k$*25
00090 ! ______________________________________________________________________
00100   let fntop("S:\acsPR\jcPctCmp",cap$="Enter Percent Complete")
00110   let fncno(cno)
00120 ! 
00125   let fnconsole(1)
00130 ! ______________________________________________________________________
00140   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00150   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00160 ! ______________________________________________________________________
00170 L170: print newpage
00180   let fnopenwin(win=101,08,07,16,72,cap$)
00190   print #win,fields "4,2,Cr 25,N": "Job Number:"
00200   print #win,fields "5,2,Cr 25,N": "Cost Category:"
00210   print #win,fields "6,2,Cr 25,N": "Percent Complete (Labor):"
00220   print #win,fields "7,2,Cr 25,N": "Percent Complete (Other):"
00230   print #win,fields "8,2,Cr 25,N": "Total Units Complete:"
00240   let io1$(1)="4,28,C 6,UT,N"
00250   let io1$(2)="5,28,N 5,UT,N"
00260   let io1$(3)="6,28,N 3,UT,N"
00270   let io1$(4)="7,28,N 3,UT,N"
00280   let io1$(5)="8,28,N 7,UT,N"
00290   print fields "17,30,C 09,B,1": "Next (F1)"
00300   print fields "17,41,C 09,B,5": "Exit (F5)"
00310 L310: input #win,fields mat io1$: jn$,cn,l12,l13,l10 conv CONV1
00320   if rtrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto DONE
00330   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00340   if cmdkey>0 then goto L490 else let ce=curfld
00350   if ce<>1 then goto L390
00360   read #1,using L370,key=lpad$(rtrm$(jn$),6): n$ nokey L630
00370 L370: form pos 7,c 40
00380   print #win,fields "4,36,C 30,N": n$(1:30)
00390 L390: if ce<>2 then goto L430
00400   let cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
00410   read #2,using L600,key=cn$: k$,rl10,rl12,rl13 nokey L630
00420   print #win,fields "5,36,C 25,N": k$
00430 L430: let ce=ce+1: if ce>udim(io1$) then let ce=1
00440 L440: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L430
00450   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L310
00460 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00470   let ce=cnt+1
00480 ERR1: print fields "24,78,C 1": bell : goto L440
00490 L490: if cndkey=5 then goto DONE
00500   if rtrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto DONE
00510   let cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
00520   read #2,using L600,key=cn$: k$,rl10,rl12,rl13 nokey L630
00530   if l10=0 then goto L540 else goto L550
00540 L540: let l10=rl10
00550 L550: if l12=0 then goto L560 else goto L570
00560 L560: let l12=rl12
00570 L570: if l13=0 then goto L580 else goto L590
00580 L580: let l13=rl13
00590 L590: rewrite #2,using L600,key=cn$: k$,l10,l12,l13 nokey L630
00600 L600: form pos 12,c 25,pos 100,pd 7.2,pos 114,2*pd 2
00610   goto L170
00620 ! ______________________________________________________________________
00630 L630: let msgline$(1)="Invalid Job Number or Category Number"
00640   let msgline$(2)="Please reselect."
00650   let fnoldmsgbox(mat response$,cap$,mat msgline$,1)
00660   let ce=1
00670   goto ERR1
00680 ! ______________________________________________________________________
00690   goto L310
00700 ! ______________________________________________________________________
00710 DONE: ! 
00720   close #2: 
00730   goto XIT
00740 ! ______________________________________________________________________
00750 ! <Updateable Region: ERTN>
00760 ERTN: let fnerror(program$,err,line,act$,"xit")
00770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00790   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00800 ERTN_EXEC_ACT: execute act$ : goto ERTN
00810 ! /region
00820 ! ______________________________________________________________________
00830 XIT: let fnxit
00840 ! ______________________________________________________________________
