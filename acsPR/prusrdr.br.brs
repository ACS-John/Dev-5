00010 ! Replace S:\acsPR\PrUsrDR
00020 ! Print User-Designed Reports
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnerror,fnconsole
00050 ! ______________________________________________________________________
00060   dim jcs$(40),cap$*128,rn(20),rn$(20)*74
00070 ! ______________________________________________________________________
00075   let fnconsole(1)
00080   let fntop(program$,cap$="User Designed Report (2)")
00090   open #1: "Name=S:\acsPR\JCReport.mst,KFName=S:\acsPR\JCReport.idx,Shr",internal,input,keyed 
00100   for j=1 to 20 !:
          read #1,using 'Form POS 1,N 2,C 74': rn(j),rn$(j) eof L110 !:
        next j
00110 L110: close #1: 
00120 L120: print newpage !:
        let fnopenwin(win=101,3,2,22,79,cap$)
00130   print #win: newpage
00140   for j=1 to 20 !:
          let jcs$(j)=str$(j)&",2,Pic(ZZ),N" !:
          let jcs$(j+20)=str$(j)&",5,C 74,N" !:
        next j
00150   print #win,fields mat jcs$: mat rn,mat rn$
00160   print fields "23,22,C 09,B,5": "Exit (F5)"
00170   print fields "23,32,C 23,R,N": "Report Number to Print:"
00180 L180: rinput fields "23,56,Nz 2,UT,N": rno conv L180
00190   if rno=0 then goto XIT
00200   for j=1 to 20
00210     if rno=rn(j) then goto JCPRNT
00220   next j
00230   goto L120
00240 ! ______________________________________________________________________
00250 JCPRNT: chain "S:\acsPR\jcPrnt"&str$(rno)
00260 ! ______________________________________________________________________
00270 XIT: let fnxit
00280 ! ______________________________________________________________________
00290 ! <Updateable Region: ERTN>
00300 ERTN: let fnerror(program$,err,line,act$,"xit")
00310   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00330   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00340 ERTN_EXEC_ACT: execute act$ : goto ERTN
00350 ! /region
00360 ! ______________________________________________________________________
