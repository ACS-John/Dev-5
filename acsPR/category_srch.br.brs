00010 ! replace S:\acsPR\Category_Srch.br
00020 ! search for a Category record
00030 !
00040   def library fncategory_srch(&cn$;fixgrid)
00050     library 'S:\Core\Library': fnTos,fnflexinit1,fnflexadd1,fnAcs,fnCmdSet,fnerror,fncno,fngethandle,fnCmdKey
00060     on error goto Ertn
00070 !
00080     dim item$(2)*30,resp$(30)*80
00090 !
00100 ! cn$=account #     ! to extract the flexgrid information (Cagegory)
00110     fncno(cno)
00120     open #file_num:=fngethandle: "Name=[Q]\PRmstr\Category.h[cno],KFName=[Q]\PRmstr\Categoryidx.h[cno],Shr",internal,input,keyed ioerr ERTN
00130 !
00140     restore #file_num: 
00150     fnTos(sn$="CategorySrch")
00160     ch$(1)="Category" : ch$(2)="Name" 
00162     mat ch$(2) : mat cm$(2) : cm$(1)="30"
00180     fnflexinit1('CategorySrch',1,1,10,70,mat ch$,mat cm$,1,usefile)
00190     if usefile>0 then goto L300 ! file already exists, do not recreate
00200     READ_FILE: ! 
00210     read #file_num,using 'Form POS 1,c 5,c 30': item$(1),item$(2) eof L300 ioerr ERR_READ
00230     fnflexadd1(mat item$)
00240     goto READ_FILE
00250 !
00260 ERR_READ: ! r:
00270     if err<>61 then goto ERTN
00280     pr 'Record locked during Category_search flexgrid creation' 
00282     pr 'It was skipped' 
00284     read #file_num,release: 
00286   goto READ_FILE ! /r
00300   L300: ! r:
00302     if fixgrid=99 then goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00304     fnCmdKey("&Next",2,1,0,"Allows you to select the highlighted record.")
00305     fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
00310     fnAcs(sn$,0,mat resp$,ckey) ! CALL FLEXGRID
00320     cn$=lpad$(resp$(1)(1:5),5)
00330     if ckey=5 then cn$="     " ! no one selected
00340   goto XIT ! /r
00360 ! <Updateable Region: ERTN>
00370 ERTN: fnerror(program$,err,line,act$,"xit")
00380     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00390     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00400     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00410 ERTN_EXEC_ACT: execute act$ : goto ERTN
00420 ! /region
00440 XIT: close #file_num: : fnend 
