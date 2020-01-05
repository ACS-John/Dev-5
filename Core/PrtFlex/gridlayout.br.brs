00010 ! Replace S:\Core\PrtFlex\GridLayout
00020 ! create layout for grid program
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnTos,fnCmdSet,fnerror,fntop
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim resp$(6)*60,text$*50,outputfile$*60,fieldnam$*30,vn$*20,ft$*11,an$*20
00080 ! ______________________________________________________________________
00090   fntop("S:\Core\PrtFlex\GridLayout","Grid Layout")
00100 L100: fnTos(sn$="file_layout") !:
        lablen=15
00110   fnLbl(1,1,"File Name:",lablen,1)
00120   fnTxt(1,lablen+2,60,0,0,"") !:
        resp$(1)="programfolder\grid\data_base_name\filename"
00130   fnCmdSet(2)
00140   fnAcs(sn$,0,mat resp$,ckey)
00150   outputfile$=trim$(resp$(1))&".fil"
00160   open #10: "Name="&outputfile$&",RecL=87,Use",display,output ioerr L100
00170 L170: fnTos(sn$="file_layout") !:
        lablen=30
00180   fnLbl(1,1,"Field name:",lablen,1)
00190   fnTxt(1,lablen+2,30,0,0,"") !:
        resp$(1)=""
00200   fnLbl(2,1,"Variable Name:",lablen,1)
00210   fnTxt(2,lablen+2,20,0,0,"") !:
        resp$(1)=""
00220   fnLbl(3,1,"Field Length:",lablen,1)
00230   fnTxt(3,lablen+2,4,0,0,"20") !:
        resp$(1)=""
00240   fnLbl(4,1,"# of Decimal Positions:",lablen,1)
00250   fnTxt(4,lablen+2,2,0,0,"20") !:
        resp$(1)=""
00260   fnLbl(5,1,"Format (eg. C 30,pd 4.2):",lablen,1)
00270   fnTxt(5,lablen+2,11,0,0,"") !:
        resp$(5)=""
00280   fnLbl(6,1,"Abbreviated Name:",lablen,1)
00290   fnTxt(6,lablen+2,20,0,0,"") !:
        resp$(6)=""
00300   fnCmdSet(11)
00310   fnAcs(sn$,0,mat resp$,ckey)
00320   if ckey=5 then goto XIT
00330   fieldnam$=trim$(resp$(1))
00340   vn$=trim$(resp$(2))
00350   fl=val(resp$(3))
00360   dp=val(resp$(4))
00370   ft$=trim$(resp$(5))
00380   an$=trim$(resp$(6))
00390   pr #10,using 'Form POS 1,C 30,C 20,N 4,N 2,C 11,C 20': fieldnam$,vn$,fl,dp,ft$,an$
00400   goto L170
00410 ! ______________________________________________________________________
00420 XIT: stop 
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
00510 ! ______________________________________________________________________
