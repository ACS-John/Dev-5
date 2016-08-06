00010 ! Replace R:\acsGL\acglBldN
00020 ! this program builds the file   R:\acsGL\acglScrn
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnxit,fntop, fnerror,fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fa$(21),fb$(21),sa$(21)*28,a$(21)*28,cap$*128
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$="Build Screens")
00100   data Reference Number:
00110   data Description:
00120   data Type of Entry:
00130   data Starting Print Position:
00140   data Lines to Skip After Print:
00150   data $ Sign Printed:
00160   data Underline:
00170   data Reverse Sign:
00180   data Balance Sheet Column:
00190   data # of Accumulator to Print:
00200   data Clear Accumulator 1:
00210   data Clear Accumulator 2:
00220   data Clear Accumulator 3:
00230   data Clear Accumulator 4:
00240   data Clear Accumulator 5:
00250   data Clear Accumulator 6:
00260   data Clear Accumulator 7:
00270   data Clear Accumulator 8:
00280   data Clear Accumulator 9:
00290   data Base Ref. # For %:
00300   data Cost Center Code:
00310   read mat a$
00320   for j=1 to 21
00330     let fa$(j)=str$(j+1)&",2,C 28,N"
00340     let sa$(j)=lpad$(rtrm$(a$(j)),27)
00350     on j goto L370,L380,L390,L400,L400,L410,L410,L410,L410,L410,L410,L410,L410,L410 none L360
00360 L360: on j-14 goto L410,L410,L410,L410,L410,L410,L420
00370 L370: let fb$(j)=str$(j+1)&",30,C 5,U  ,N" : goto L430
00380 L380: let fb$(j)=str$(j+1)&",30,C 50,U  ,N" : goto L430
00390 L390: let fb$(j)=str$(j+1)&",30,C 1,U  ,N" : goto L430
00400 L400: let fb$(j)=str$(j+1)&",30,N 2,U  ,N" : goto L430
00410 L410: let fb$(j)=str$(j+1)&",30,N 1,U  ,N" : goto L430
00420 L420: let fb$(j)=str$(j+1)&",30,N 3,U  ,N"
00430 L430: next j
00440   let fb$(20)="21,30,N 5,U  ,N"
00450   open #1: "Name=R:\acsGL\acglScrn,Size=0,RecL=1407,Replace",internal,output  !:
        write #1,using 'Form POS 1,21*C 11,21*C 28,21*C 28': mat fa$,mat fb$,mat sa$ !:
        close #1: 
00460   let fnchain("R:\acsGL\Company")
00470 ! ______________________________________________________________________
00480 XIT: let fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: let fnerror(cap$,err,line,act$,"xit")
00520   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
