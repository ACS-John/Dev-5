00010 ! Replace S:\acsGL\fnacglBldS
00020 ! this -library function- builds the file    [Q]\GLmstr\ACGLScr.h
00030   def library fnacglblds
00040     library 'S:\Core\Library': fntop,fnxit,fnerror,fncno, fnacprscr,fnFree
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim flo$(31),fli$(65),scr$(30)*20,otd$(65)*30,d(2)
00080 ! ______________________________________________________________________
00090     fncno(cno)
00100     fntop(program$,"CHANGE_ME")
00110     open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  !:
          read #20,using 'Form POS 150,2*N 1': mat d !:
          close #20: 
00120 ! ______________________________________________________________________
00130     flo$(1)="1,5,C 60,R,N"
00140     if d(1)=1 then !:
            scr$(1)="Department #:" : scr$(2)="Account #:" !:
          else !:
            scr$(1)=" " : flo$(2)="3,2,C 20,N"
00150     flo$(2)="3,2,C 20,N" : flo$(3)="3,20,C 20,N"
00160     if d(2)=1 then !:
            scr$(3)="Sub-Account:" : flo$(4)="3,41,C 20,N" !:
          else !:
            scr$(3)=" " : flo$(4)="3,41,C 20,N"
00170     scr$(4)="Description:" !:
          scr$(5)="Beginning Balance:" !:
          scr$(6)="Current Balance:" !:
          scr$(7)="Balance Sheet Ref #:" !:
          scr$(8)="2nd Balance Sheet:" !:
          scr$(9)="Income Stmt Ref #:"
00180     flo$(5)="04,02,C 20,N" : flo$(6)="05,02,C 20,N" !:
          flo$(7)="05,36,C 20,N" : flo$(8)="06,02,C 20,N" !:
          flo$(9)="06,41,C 20,N"
00190     flo$(10)="07,02,C 20,N" : flo$(11)="07,41,C 20,N" !:
          flo$(12)="08,02,C 20,N" : flo$(13)="08,41,C 20,N" !:
          flo$(14)="10,08,C 20,N" : flo$(15)="10,26,C 20,N" !:
          flo$(16)="10,46,C 20,N" : flo$(17)="10,64,C 20,N"
00200     scr$(10)="2nd Income Stmt:" !:
          scr$(11)="Fund/Cash Flow Ref#:" !:
          scr$(12)="2nd Fund/Cash Flow:" !:
          scr$(13)="Balance This Yr:" !:
          scr$(14)="Balance Last Yr:" !:
          scr$(15)="Original Budget:" !:
          scr$(16)="Revised Budget:"
00210     for j=17 to 29 !:
            scr$(j)="Period "&str$(j-16)&":" !:
            flo$(j+1)=str$(j-6)&",2,C 20,N" !:
          next j
00220     scr$(30)="EOY Bal 2 Yrs Ago:"
00230     flo$(31)="09,02,C 20,N"
00240     if d(1)=1 then !:
            fli$(1)="3,15,N 3,UT,N" : otd$(1)=fli$(1) !:
          else !:
            fli$(1)="3,15,N 3,PA,N" : otd$(1)="3,15,PIC(ZZZ),N"
00250     otd$(2)="3,30,PIC(ZZZZZZ),UT,N"
00260     if d(2)=1 then !:
            fli$(3)="3,55,N 3,UET,N" : otd$(3)=fli$(3) !:
          else !:
            fli$(3)="3,55,N 3,PA,N" : otd$(3)="3,55,PIC(ZZZ),N"
00270     fli$(04)="4,15,C 50,UT  ,N" : otd$(04)=fli$(04)
00280     otd$(05)="5,21,PIC(---------.##),UT,N" !:
          otd$(06)="5,54,PIC(---------.##),UT,N" !:
          otd$(07)="6,23,PIC(ZZZZZ),UT,N" !:
          otd$(08)="6,62,PIC(ZZZZZ),UT,N" !:
          otd$(09)="7,23,PIC(ZZZZZ),UT,N" !:
          otd$(10)="7,62,PIC(ZZZZZ),UT,N" !:
          otd$(11)="8,23,PIC(ZZZZZ),UT,N" !:
          otd$(12)="8,62,PIC(ZZZZZ),UT,N" !:
          otd$(13)="9,23,PIC(---------.##),UT,N"
00290     fli$(05)="5,21,N 12.2,UT,N" !:
          fli$(06)="5,54,N 12.2,UT,N" !:
          fli$(07)="6,23,N 5,QUT,51" !:
          fli$(08)="6,62,N 5,QUT,52" !:
          fli$(09)="7,23,N 5,QUT,53" !:
          fli$(10)="7,62,N 5,QUT,54" !:
          fli$(11)="8,23,N 5,QUT,55" !:
          fli$(12)="8,62,N 5,QUT,56" !:
          fli$(13)="9,23,N 12.2,UT,N"
00300     for j=0 to 12
00310       fli$(j*4+14)=str$(j+11)&",12,N 12.2,UT,N" !:
            fli$(j*4+15)=str$(j+11)&",30,N 12.2,UT,N" !:
            fli$(j*4+16)=str$(j+11)&",50,N 12.2,UT,N" !:
            fli$(j*4+17)=str$(j+11)&",67,N 12.2,UT,N"
00320       otd$(j*4+14)=str$(j+11)&",12,PIC(---------.##),UT,N" !:
            otd$(j*4+15)=str$(j+11)&",30,PIC(---------.##),UT,N" !:
            otd$(j*4+16)=str$(j+11)&",50,PIC(---------.##),UT,N" !:
            otd$(j*4+17)=str$(j+11)&",67,PIC(---------.##),UT,N"
00330     next j
00340     fli$(2)="3,30,N 6,UT  ,N"
00350     if d(1)=0 then fli$(2)(11:11)="C"
00360     if d(2)=0 then fli$(2)(12:12)="E"
00370     fnFree("[Q]\GLmstr\ACGLScr.h[cno]")
00380 L380: open #20: "Name=[Q]\GLmstr\ACGLScr.h[cno],Size=0,RecL=4281",internal,output: write #20,using 'Form POS 1,31*C 15,30*C 20,65*C 18,65*C 30': mat flo$,mat scr$,mat fli$,mat otd$ !:
          close #20: 
00390     fnacprscr
00400     goto XIT
00410 ! ______________________________________________________________________
00420 ERTN: ! <Updateable Region: ERTN>
00430     fnerror(program$,err,line,act$,"xit")
00440     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00450     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
00500 XIT: fnend 
00510 ! ______________________________________________________________________
