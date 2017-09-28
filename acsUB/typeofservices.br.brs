00010 ! Replace S:\acsUB\TypeOfServices   !   Type of Services (All Companies)
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit,fnlbl,fntos,fnacs,fnerror,fncmdset,fnflexinit1,fnflexadd1,fngetdir2
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim resp$(60)*20
00070   dim cap$*128
00080   dim item$(61)*20,colhdr$(61)
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Type of Services (All Companies)")
00110   fntos(sn$:="TypeOServices")
00120   fnlbl(1,1,sn$&'1',20,2,3)
00130   colhdr$(1)='CNo'
00140   for j=1 to 10
00150     colhdr$(j+01)='Name '&str$(j)
00160     colhdr$(j+11)='Code '&str$(j)
00170     colhdr$(j+21)='Tax  '&str$(j)
00180     colhdr$(j+31)='Pen  '&str$(j)
00190     colhdr$(j+41)='Sub  '&str$(j)
00200     colhdr$(j+51)='Ord  '&str$(j)
00210   next j
00220   mat colmask$(60)
00230   mat colmask$=("")
00240   fnflexinit1(filename$:='toses',2,1,10,72,mat colhdr$,mat colmask$,1)
00241   dim data_dir$*256
00242   data_dir$=env$('Q')&"\UBmstr\ubData\"
00243   fngetdir2(data_dir$,mat service_file$, '/ON','Service.h*')
00244 ! fnpause
00250   for service_file_item=1 to udim(service_file$)
00270     item$(1)=service_file$(service_file_item)(10:len(service_file$(service_file_item)))
00280     open #h_service=15: "Name="&env$('Q')&"\UBmstr\ubData\"&service_file$(service_file_item),internal,outin,relative 
00290     read #h_service,using "form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*G 2,10*G 2",rec=1: mat item$(2:61)
00300     fnflexadd1(mat item$)
00310     close #h_service: 
00330   next service_file_item
00340   fncmdset(4)
00350   fnacs(sn$,win,mat resp$,ckey)
00360   fnxit
00370 ! ______________________________________________________________________
00380 ! <Updateable Region: ERTN>
00390 ERTN: fnerror(program$,err,line,act$,"xit")
00400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00410   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00420   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00430 ERTN_EXEC_ACT: execute act$ : goto ERTN
00440 ! /region
00450 ! ______________________________________________________________________
