00010 ! Replace S:\Core\GridLayout  ! layout for grid program
00020   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fntos,fnerror,fnxit,fncmdset,fntop
00030   dim resp$(6)*60,text$*50,outputfile$*60,fieldnam$*30,vn$*20,ft$*11,an$*20
00040 ! ______________________________________________________________________
00050   on error goto ERTN
00060   fntop(program$,"Grid Layout")
00070 ! ______________________________________________________________________
00080 L80: let fntos(sn$="file_layout")
00090   lablen=15
00100   let text$="File Name:"
00102   fnlbl(1,1,text$,lablen,1)
00110   fntxt(1,lablen+2,60,0,0,"")
00112   let resp$(1)="programfolder\grid\data_base_name\filename"
00120   fncmdset(2)
00130   fnacs(sn$,0,mat resp$,ckey)
00140   outputfile$=trim$(resp$(1))&".fil"
00150   open #10: "Name="&outputfile$&",RecL=87,use",display,output ioerr L80
00160 L160: sn$="file_layout"
00162   fntos(sn$)
00170   lablen=30
00180   fnlbl(1,1,"Field name:",lablen,1)
00190   fntxt(1,lablen+2,30,0,0,"")
00192   let resp$(1)=""
00200   fnlbl(2,1,"Variable Name:",lablen,1)
00210   fntxt(2,lablen+2,20,0,0,"")
00212   let resp$(1)=""
00220   let text$="Field Length:"
00222   fnlbl(3,1,text$,lablen,1)
00230   fntxt(3,lablen+2,4,0,0,"20")
00232   let resp$(1)=""
00240   let text$="# of Decimal Positions:"
00242   fnlbl(4,1,text$,lablen,1)
00250   fntxt(4,lablen+2,2,0,0,"20")
00252   let resp$(1)=""
00260   let text$="Format (eg. C 30,pd 4.2):"
00262   fnlbl(5,1,text$,lablen,1)
00270   fntxt(5,lablen+2,11,0,0,"")
00272   let resp$(5)=""
00280   let text$="Abbreviated Name:"
00282   fnlbl(6,1,text$,lablen,1)
00290   fntxt(6,lablen+2,20,0,0,"")
00292   let resp$(6)=""
00300   fncmdset(11)
00310   fnacs(sn$,0,mat resp$,ckey)
00320   if ckey=5 then goto XIT
00330   let fieldnam$=trim$(resp$(1))
00340   let vn$=trim$(resp$(2))
00350   fl=val(resp$(3))
00360   let dp=val(resp$(4))
00370   let ft$=trim$(resp$(5))
00380   an$=trim$(resp$(6))
00390   pr #10,using L400: fieldnam$,vn$,fl,dp,ft$,an$
00400 L400: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
00410   goto L160
00420 XIT: stop 
00430 ! <Updateable Region: ERTN>
00440 ERTN: let fnerror(program$,err,line,act$,"xit")
00450   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00480 ERTN_EXEC_ACT: execute act$ : goto ERTN
00490 ! /region
