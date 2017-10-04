00010 ! Replace S:\acsPR\prFixAdr
00020 ! Reassign Department Addresses
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncno,fnerror,fntop,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim en$*8,ta(2),cap$*128,message$*40
00080 ! ______________________________________________________________________
00090   fntop("S:\acsPR\prFixAdr",cap$="Reassign Department Addresses")
00100   fncno(cno)
00110 ! 
00120 ! ___________________________
00130   fnwait(101,cap$,message$="Reassigning: please wait...",0)
00140 ! ___________________________
00150 ! Sort routine
00160   open #control=1: "Name="&env$('Temp')&"\Control."&session$&",RecL=128,Replace",internal,output 
00170   write #control,using "Form POS 1,C 128": "! Now Sorting Payroll Department Records" !:
        write #control,using "Form POS 1,C 128": "File "&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N" !:
        write #control,using "Form POS 1,C 128": "Mask 1,8,c,a,9,3,c,a"
00180   close #control: 
00190   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L200
00200 L200: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
00210 ! ___________________________
00220   open #addr=3: "Name="&env$('Temp')&"\Addr."&session$&",NoShr",internal,input 
00230   open #rptrail=2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",NoShr",internal,outin,relative 
00240   open #rpmstr=1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",NoShr",internal,outin,keyed 
00250 ! ___________________________
00260 READ_RPMSTR: read #rpmstr,using 'Form POS 1,N 8': eno eof READ_ADDR
00270   if eno=eno1 then delete #rpmstr: : goto READ_RPMSTR
00280   eno1=eno !:
        rewrite #rpmstr,using 'Form POS 173,2*PD 3': 0,0
00290   goto READ_RPMSTR
00300 ! ______________________________________________________________________
00310 READ_ADDR: read #addr,using 'Form POS 1,PD 3': r1 eof EOF_ADDR
00320   read #rptrail,using 'Form POS 1,N 8',rec=r1: eno
00330   if r2=0 then goto ABC
00340   if eno=eno2 then !:
          rewrite #rptrail,using 'Form POS 468,PD 3',rec=r2: r1 else !:
          rewrite #rptrail,using 'Form POS 468,PD 3',rec=r2: 0
00350   if eno=eno2 then goto DEG
00360   if eno2=0 then goto ABC else !:
          en$=lpad$(str$(eno2),8) !:
          rewrite #rpmstr,using 'Form POS 173,2*PD 3',key=en$: mat ta nokey ABC
00370 ABC: let ta(1)=r1
00380 DEG: let ta(2)=r1 : eno2=eno
00390   if lta<r1 then lta=r1
00400   let r2=r1
00410   goto READ_ADDR
00420 ! ______________________________________________________________________
00430 EOF_ADDR: ! 
00440 ! If ENO2=0 Then Goto ABC
00450   en$=lpad$(str$(eno2),8)
00460   rewrite #rptrail,using 'Form POS 468,PD 3',rec=r2: 0
00470   rewrite #rpmstr,using 'Form POS 173,2*PD 3',key=en$: mat ta nokey L490
00480 ! Rewrite #rptrail,Using 'Form POS 468,PD 3',REC=1: LTA
00490 L490: close #addr: 
00500   close #rpmstr: 
00510   close #rptrail: 
00520 XIT: let fnxit
00530 ! ______________________________________________________________________
00540 ! <Updateable Region: ERTN>
00550 ERTN: let fnerror(program$,err,line,act$,"xit")
00560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00570   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00590 ERTN_EXEC_ACT: execute act$ : goto ERTN
00600 ! /region
00610 ! ______________________________________________________________________
