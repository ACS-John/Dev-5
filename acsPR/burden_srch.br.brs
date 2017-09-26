00010 ! replace S:\acsPR\Burden_Srch.br
00020 ! search for a personnel burden record
00030 ! ______________________________________________________________________
00040   def library fnburden_srch(&x$;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle,fncmdkey
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(6)*30,resp$(30)*80
00090 ! ______________________________________________________________________
00100 ! x$=account #     !:
          ! to extract the flexgrid information (personnel burden)
00110     let fncno(cno)
00120     open #file_num:=fngethandle: "Name="&env$('Q')&"\PRmstr\burden.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\burdenidx.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00130 ! ______________________________________________________________________
00140     restore #file_num: 
00150     let fntos(sn$="BurdenSrch")
00160     let ch$(1)="Employee" : let ch$(2)="Name" : let ch$(3)="Burden" !:
          let ch$(4)="Unused" !:
          let ch$(5)="Unused" !:
          mat ch$(5) : mat cm$(5)
00180     let fnflexinit1('BurdenSrch',1,1,10,70,mat ch$,mat cm$,1,usefile)
00190     if usefile>0 then goto L300 ! file already exists, do not recreate
00200 READ_FILE: ! 
00210     read #file_num,using 'Form POS 1,c 8,c 30,n 6.3': item$(1),item$(2),rate eof L300 ioerr ERR_READ
00220     let item$(3)=cnvrt$("pic(zzz.###)",rate)
00230     let fnflexadd1(mat item$)
00240     goto READ_FILE
00250 ! ______________________________________________________________________
00260 ERR_READ: ! 
00270     if err<>61 then goto ERTN
00280     print 'Record locked during burden_search flexgrid creation' !:
          print 'It was skipped' !:
          read #file_num,release: !:
          goto READ_FILE
00290 ! ______________________________________________________________________
00300 L300: if fixgrid=99 then goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00304     let fncmdkey("&Edit",2,1,0,"Allows you to change the highlighted record.")
00305     let fncmdkey("E&xit",5,0,1,"Returns to main screen.")
00310     let fnacs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00320     let x$=lpad$(resp$(1)(1:8),8)
00330     if ckey=5 then let x$="        " ! no one selected
00340     goto XIT
00350 ! ______________________________________________________________________
00360 ! <Updateable Region: ERTN>
00370 ERTN: let fnerror(program$,err,line,act$,"xit")
00380     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00390     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00400     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00410 ERTN_EXEC_ACT: execute act$ : goto ERTN
00420 ! /region
00430 ! ______________________________________________________________________
00440 XIT: close #file_num: : fnend 
00450 ! ______________________________________________________________________
