00010 ! Replace S:\Core\parse\CSZ.br
00015   dim ml$(2)*80
00020 ! ______________________________________________________________________
00030 FNCSZ: ! extract  CITY$,STATE$,ZIP$ from CSZ$
00040   def library fncsz(&csz$,&city$,&state$,&zip$)
00050     library 'S:\Core\Library': fnerror,fnmsgbox
00060     let csz$=rtrm$(csz$)
00061     do
00062     let csz$=srep$(csz$,'  ',' ')
00063     loop until pos(csz$,'  ')<=0
00070 CSZ_SET_P1: ! 
00080     let p1=pos(csz$,".",1)
00090     if p1>0 then let csz$(p1:p1)="": goto CSZ_SET_P1
00100     let l1=len(csz$) !:
          let p1=pos(csz$,",",1)-1
00110     if p1=-1 then let p1=pos(csz$," ",1)-1
00120     let p2=pos(csz$," ",p1+3) !:
          let city$=uprc$(rtrm$(csz$(1:p1))(1:15))
00130     if p2=0 then let p2=pos(csz$," ",-1)+2 !:
            ! just in case they forgot a space after the state before the zip, !:
            ! but didn't forget the space after the city comma.
00140     if uprc$(city$(1:3))="FT " then let city$(1:3)="Fort " else !:
            if uprc$(city$(1:3))="FT." then let city$(1:3)="Fort "
00150     let state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2)) !:
          let zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1)))) !:
          let zip5$=zip$(1:5) !:
          let zip4$="" !:
          let l2=len(zip$)
00160     if l2<9 then goto XIT
00165     on error goto MESSAGEBOX
00170     let p2=pos(csz$," ",p1+3) !:
          let city$=rtrm$(csz$(1:p1))(1:15) !:
          let state$=rtrm$(csz$(p2-2:p2))(1:2) !:
          let zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1)))) !:
          let zip5$=zip$(1:5) !:
          let zip4$="" !:
          let l2=len(zip$)
00175     goto L180
00176 MESSAGEBOX: ! 
00177     mat ml$(2) !:
          let ml$(1)="You have a bad address: "&csz$ !:
          let ml$(2)="You should fix the address and run this option again." !:
          let fnmsgbox(mat ml$,resp$,cap$,48)
00178     goto XIT
00180 L180: ! Print STATE$ ! XXX
00190     goto XIT
00200 ! ______________________________________________________________________
00210 ! <Updateable Region: ERTN>
00220 ERTN: let fnerror(program$,err,line,act$,"xit")
00230     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00240     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00250     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00260 ERTN_EXEC_ACT: execute act$ : goto ERTN
00270 ! /region
00280 ! ______________________________________________________________________
00290 XIT: fnend 
00300 ! ______________________________________________________________________
