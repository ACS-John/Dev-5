00010 ! replace S:\acsGL\Account_Search.br
00020 ! search for general ledger accounts
00030 ! ______________________________________________________________________
00040   def library fnaccount_search(&x$;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(10)*50,resp$(30)*80,rf(6)
00090 ! ______________________________________________________________________
00100 ! x$=account #     !:
          ! to extract the flexgrid information (master file)
00110     fncno(cno)
00120     open #file_num:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\glIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00130 ! ______________________________________________________________________
00140     restore #file_num: 
00150     fntos(sn$="AccountSrch")
00160     ch$(1)="Account" : ch$(2)="Description" : ch$(3)="Balance" !:
          ch$(4)="B/S Ref" : ch$(5)="B/S Ref2" !:
          ch$(6)="I/C Ref" : ch$(7)="I/C Ref2" !:
          ch$(8)="Fund Ref" : ch$(9)="Fund Ref2" !:
          mat ch$(9) : mat cm$(9) : mat cm$(9) !:
          cm$(1)=cm$(2)="80": cm$(3)="10" !:
          cm$(4)=cm$(5)=cm$(6)=cm$(7)=cm$(8)=cm$(9)="30"
00170     if fixgrid=99 then usefile=0 else usefile=1 !:
            ! set to rebuild grid file only as you exit ubfm and the !:
            ! fixgrid code has been changed to necessary
00180     usefile=fnflexinit1('Acct',1,1,10,70,mat ch$,mat cm$,1,usefile)
00190     if usefile>0 then goto L330 ! file already exists, do not recreate
00200 READ_FILE: ! 
00210     read #file_num,using 'Form POS 1,C 12,c 50,pos 87,pd 6.2,pos 63,6*pd 3': item$(1),item$(2),cb,mat rf eof L330 ioerr ERR_READ
00220     item$(3)=str$(cb)
00230     for j=1 to 6
00240       item$(j+3)=str$(rf(j))
00250     next j
00260     fnflexadd1(mat item$)
00270     goto READ_FILE
00280 ! ______________________________________________________________________
00290 ERR_READ: ! 
00300     if err<>61 then goto ERTN
00310     pr 'Record locked during Account_Search flexgrid creation' !:
          pr 'It was skipped' !:
          read #file_num,release: !:
          goto READ_FILE
00320 ! ______________________________________________________________________
00330 L330: ! If FIXGRID=99 Then Goto XIT ! FIXING NEW GRID FILE without displaying it
00340     fncmdset(2): fnacs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00350     x$=lpad$(resp$(1),12)
00360     if ckey=5 then x$="            " ! no one selected
00370     goto XIT
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: fnerror(program$,err,line,act$,"xit")
00410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
00470 XIT: close #file_num: : fnend 
00480 ! ______________________________________________________________________
