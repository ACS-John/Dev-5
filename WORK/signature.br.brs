00010 ! Replace work\signature
00020   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fntos,fndat,fnerror,fnopenprn,fncloseprn,fncomboa,fnmsgbox,fncmdset
00030   dim docname$*30,comboa$(99)*30,tt$*200,msgline$(5)*200,cap$*128,cddrive$*1,response$(5)*30
00040   open #1: "Name=S:\Core\Docnames,KFName=S:\Core\DocIndex,USE,RecL=39,KPS=1,KLN=30",internal,outin,keyed 
00050 ! read #1,using 60:docname$,docline,docposition,docsigchoice,cddrive$
00060 L60: form pos 1,c 30,n 3,n 3,n 2,c 1
00065   pr newpage
00070 MAINSCREEN: ! 
00075   sn$ = "SignatureMain"
00076   mat comboa$(99)
00080   fntos(sn$)
00082   mylen = 17: myalign = 1
00085   mytext$ = "Document Name:" : fnlbl(1,1,mytext$,mylen,myalign)
00088   a = 0
00090   restore #1: 
00094 L94: read #1,using L60: docname$,docline,docposition,docsigchoice,cddrive$ eof L105
00096   a+=1: comboa$(a) = docname$
00100   goto L94
00105 L105: a=max(1,a)
00106   mat comboa$(a) : filename$="DocNames" !:
        fncomboa(filename$,1,18,mat comboa$,ttt$,25)
00110   mytext$="Number of Copies:": fnlbl(2,1,mytext$,mylen,myalign)
00120   response$(2)=str$(1) !:
        fntxt(2,18,2)
00130   fncmdset(14): fnacs(sn$,0,mat response$,ckey)
00140   docname$=response$(1)
00150   copies=max(1,val(response$(2)))
00160   if ckey=5 then goto XIT
00165   if ckey=10 then goto XIT
00175   docline=docposition=0 !:
        docsigchoice=1
00176   if ckey=1 then docname$="": goto EDITSCREEN
00180   if ckey=2 then read #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey EDITSCREEN !:
          goto EDITSCREEN
00190   if ckey=4 then read #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey EDITSCREEN !:
          goto PRINTSIGNATURE
00200 ! 
00210 ! 
00220 ! 
00230 ! 
00240 ! 
00250 EDITSCREEN: ! 
00260   sn$ = "SignatureEdit": fntos(sn$)
00265   mylen = 17: myalign = 1
00270   mytext$ = "Document Name:" : fnlbl(1,1,mytext$,mylen,myalign)
00275   response$(1)=docname$ !:
        tt$="Choose a name that you will remember, such as Payroll Check" !:
        mask$="" !:
        fntxt(1,mylen+1,30,30,0,mask$,0,tt$)
00280   mytext$ = "Line on Form:" : fnlbl(2,1,mytext$,mylen,myalign)
00285   response$(2)=str$(docline) !:
        tt$="This is the distance from the top of the form where the signature shold print. (Normally about 6 lines per inch)" !:
        mask$="30" !:
        fntxt(2,mylen+1,4,3,0,mask$,0,tt$)
00290   mytext$="Position on Form:": fnlbl(3,1,mytext$,mylen,myalign)
00295   response$(3)=str$(docposition) !:
        tt$="This is the number of characters from the left side of the form.       (Normally about 10 characters per inch)" !:
        mask$="30" !:
        fntxt(3,mylen+1,4,3,0,mask$,0,tt$)
00300   mytext$="Signature Choice:": fnlbl(4,1,mytext$,mylen,myalign)
00305   response$(4)=str$(docsigchoice) !:
        tt$="You can have up to 10 different signatures.  Choose the one you want printed on this document." !:
        mask$="30" !:
        fntxt(4,mylen+1,3,2,0,mask$,0,tt$)
00310   mytext$="CD Drive:": fnlbl(5,1,mytext$,mylen,myalign)
00315   response$(5)=cddrive$ !:
        tt$="Your signature is stored on a cd.  What is the drive designation used on this computer for the cd drive?" !:
        mask$="" !:
        fntxt(5,mylen+1,1,1,0,mask$,0,tt$)
00320   fncmdset(4): fnacs(sn$,0,mat response$,ckey)
00330   if ckey=5 then goto MAINSCREEN
00340   docname$=response$(1)(1:30)
00350   docline=val(response$(2)) conv BADLINE
00360   docposition=val(response$(3)) conv BADPOSITION
00370   docsigchoice=val(response$(4)) conv BADSIGCHOICE
00380   cddrive$=uprc$(response$(5))
00390   if cddrive$<"A" or cddrive$>"Z" then mat msgline$(2): msgline$(1)="The drive designation you used is invalid!" !:
          msgline$(2) =" Normal designations would be D or E, but can be other letters" !:
          cap$="Bad CD Drive designation" : mtype=48: gosub MSGBOX : goto EDITSCREEN
00400   rewrite #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey L415
00410   goto MAINSCREEN
00415 L415: write #1,using L60: docname$,docline,docposition,docsigchoice,cddrive$
00420   goto MAINSCREEN
00500 BADLINE: ! 
00510   mat msgline$(1): msgline$(1)="You have entered an invalid line #. Must be a number from 1 to 200!" !:
        cap$="Bad line number" : mtype=48: gosub MSGBOX : goto EDITSCREEN
00520 BADPOSITION: ! 
00530   mat msgline$(1): msgline$(1)="You have entered an invalid position. Your answer must be from 1 to 200!" !:
        cap$="Bad position" : mtype=48: gosub MSGBOX : goto EDITSCREEN
00535 BADSIGCHOICE: ! 
00540   mat msgline$(1): msgline$(1)="You can have up to 10 different signatures.  You may just one.  Choose an answer from 1 to 10" !:
        cap$="Bad position" : mtype=48: gosub MSGBOX : goto EDITSCREEN
01000 PRINTSIGNATURE: ! 
01020   fnopenprn
01025   if docline = 0 then docline = 1
01026   if docposition = 0 then docposition = 1
01030   for j=1 to copies
01040     pr #255,using L1050: " "
01050 L1050: form skip docline,pos docposition,c 50,skip 1
01060     pr #255: "*Insert File:Z:\Signature1.acs"
01070     pr #255: newpage
01080   next j
01085   fncloseprn
01088   pr newpage
01090   goto MAINSCREEN
07000 MSGBOX: ! 
07010   fnmsgbox(mat msgline$,response$,cap$,mtype)
07020   return 
08000 XIT: ! chain "menu"
