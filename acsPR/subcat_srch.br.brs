00010 ! replace S:\acsPR\subCAT_SRCH.br
00020 ! search for an valid sub-caterories for this job
00030 ! ______________________________________________________________________
00040   def library fnsubcat_srch(&cde$,&ckey;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle,fncmdkey
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(2)*40,resp$(30)*80
00090 ! ______________________________________________________________________
00100     cde$=lpad$(rtrm$(cde$),3) ! pass back sub-category #
00110     fncno(cno)
00120     subcat=1 ! # of subcatergory file in calling program
00130 ! ______________________________________________________________________
00140     restore #subcat: 
00150     fntos(sn$="SubCatSrch")
00160     ch$(1)="Sub-Category #" : !:
          ch$(2)="Description" : !:
          mat ch$(2) : mat cm$(2) : mat cm$=("2")
00170     let usefile=fnflexinit1('SubCat',1,1,20,40,mat ch$,mat cm$,1,usefile)
00190 READ_FILE: ! 
00200     read #subcat,using 'Form POS 1,c 3,c 25': mat item$ eof L280 ioerr ERR_READ
00210     fnflexadd1(mat item$)
00220     goto READ_FILE
00230 ! ______________________________________________________________________
00240 ERR_READ: ! 
00250     if err<>61 then goto ERTN
00260     pr 'Record locked during cat_search flexgrid creation' !:
          pr 'It was skipped' !:
          read #subcat,release: !:
          goto READ_FILE
00270 ! ______________________________________________________________________
00280 L280: let fncmdkey("&Add",97,0,0,"Add a new sub-category record." ) !:
          fncmdkey("E&dit",98,1,0,"Access the highlited record") !:
          fncmdkey("&Delete",96,0,0,"Deletes the highlited record") !:
          fncmdkey("&Listing",94,0,0,"Print a list of sub-category records") !:
          fncmdkey("E&xit",5,0,1,"Returns to main menu.")
00290     fnacs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00300     let x$=cde$=lpad$(resp$(1),3)
00310     if ckey=5 then cde$="   " ! no one selected
00320     goto XIT
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: let fnerror(program$,err,line,act$,"xit")
00360     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00370     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
00420 XIT: fnend 
00430 ! ______________________________________________________________________
