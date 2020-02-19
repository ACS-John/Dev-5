00010 ! Replace S:\acsGL\ASCIIGLm
00020 ! create Ascii oputput file
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnTos,fnLbl,fnCmdKey,fnxit,fnTxt,fnAcs
00050   fntop(program$,cap$="Create ASCII File")
00060   on error goto Ertn
00070 !
00080   dim de$*50,fl$*50
00090 !
00100   fncno(cno,cnam$)
00110 !
00120   crlf$=chr$(13)&chr$(10)
00130   fl$="C:\ASCIIGLM.txt"
00140   fnTos(sn$="GLascii") !:
        mylen=30: mypos=mylen+3 : right=1
00150   fnLbl(1,1,"Path and File Name to Create:",mylen,right)
00160   fnTxt(1,mypos,50,0,0,"",0,"Enter the drive and filename where the file should be stored. Remember this name so you can find the file and access it with some other software.",0 ) !:
        resp$(1)=fl$
00170   fnCmdKey("&Next",1,1,0,"Will create an ascii file of the general ledger accounts.")
00180   fnCmdKey("&Cancel",5,0,1,"Will return to menu without creating a file.")
00190   fnAcs(sn$,0,mat resp$,ckey)
00200   if ckey=5 then goto XIT
00210   fl$=trim$(resp$(1))
00220   open #2: "Name="&fl$&",RecL=79,EOL=CRLF,Replace",external,output 
00230   open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,input,keyed 
00240 L240: read #1,using 'Form POS 1,C 12,C 50,POS 87,PD 6.2': gl$,de$,cb eof XIT
00250   write #2,using 'Form POS 1,C 12,X 2,C 50,N 12.2,c 2': gl$,de$,cb,crlf$
00260   goto L240
00270 XIT: fnxit
00280 ! _______
00290 ! <Updateable Region: ERTN>
00300 ERTN: fnerror(program$,err,line,act$,"xit")
00310   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00330   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00340 ERTN_EXEC_ACT: execute act$ : goto ERTN
00350 ! /region
