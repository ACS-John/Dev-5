! Replace S:\acsGL\fnacglBldS
! this -library function- builds the file    [Q]\GLmstr\ACGLScr.h
def library fnacglblds
		autoLibrary
		on error goto Ertn
 
		dim flo$(31),fli$(65),scr$(30)*20,otd$(65)*30,d(2)
 
		fncno(cno)
		fnTop(program$)
		open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
		read #20,using 'Form POS 150,2*N 1': mat d : _
		close #20:
 
		flo$(1)="1,5,C 60,R,N"
		if d(1)=1 then : _
			scr$(1)="Department #:" : scr$(2)="Account #:" : _
		else : _
			scr$(1)=" " : flo$(2)="3,2,C 20,N"
		flo$(2)="3,2,C 20,N" : flo$(3)="3,20,C 20,N"
		if d(2)=1 then : _
			scr$(3)="Sub-Account:" : flo$(4)="3,41,C 20,N" : _
		else : _
			scr$(3)=" " : flo$(4)="3,41,C 20,N"
		scr$(4)="Description:" : _
		scr$(5)="Beginning Balance:" : _
		scr$(6)="Current Balance:" : _
		scr$(7)="Balance Sheet Ref #:" : _
		scr$(8)="2nd Balance Sheet:" : _
		scr$(9)="Income Stmt Ref #:"
		flo$(5)="04,02,C 20,N" : flo$(6)="05,02,C 20,N" : _
		flo$(7)="05,36,C 20,N" : flo$(8)="06,02,C 20,N" : _
		flo$(9)="06,41,C 20,N"
		flo$(10)="07,02,C 20,N" : flo$(11)="07,41,C 20,N" : _
		flo$(12)="08,02,C 20,N" : flo$(13)="08,41,C 20,N" : _
		flo$(14)="10,08,C 20,N" : flo$(15)="10,26,C 20,N" : _
		flo$(16)="10,46,C 20,N" : flo$(17)="10,64,C 20,N"
		scr$(10)="2nd Income Stmt:" : _
		scr$(11)="Fund/Cash Flow Ref#:" : _
		scr$(12)="2nd Fund/Cash Flow:" : _
		scr$(13)="Balance This Yr:" : _
		scr$(14)="Balance Last Yr:" : _
		scr$(15)="Original Budget:" : _
		scr$(16)="Revised Budget:"
		for j=17 to 29 : _
			scr$(j)="Period "&str$(j-16)&":" : _
			flo$(j+1)=str$(j-6)&",2,C 20,N" : _
		next j
		scr$(30)="EOY Bal 2 Yrs Ago:"
		flo$(31)="09,02,C 20,N"
		if d(1)=1 then : _
			fli$(1)="3,15,N 3,UT,N" : otd$(1)=fli$(1) : _
		else : _
			fli$(1)="3,15,N 3,PA,N" : otd$(1)="3,15,PIC(ZZZ),N"
		otd$(2)="3,30,PIC(ZZZZZZ),UT,N"
		if d(2)=1 then : _
			fli$(3)="3,55,N 3,UET,N" : otd$(3)=fli$(3) : _
		else : _
			fli$(3)="3,55,N 3,PA,N" : otd$(3)="3,55,PIC(ZZZ),N"
		fli$(04)="4,15,C 50,UT  ,N" : otd$(04)=fli$(04)
		otd$(05)="5,21,PIC(---------.##),UT,N" : _
		otd$(06)="5,54,PIC(---------.##),UT,N" : _
		otd$(07)="6,23,PIC(ZZZZZ),UT,N" : _
		otd$(08)="6,62,PIC(ZZZZZ),UT,N" : _
		otd$(09)="7,23,PIC(ZZZZZ),UT,N" : _
		otd$(10)="7,62,PIC(ZZZZZ),UT,N" : _
		otd$(11)="8,23,PIC(ZZZZZ),UT,N" : _
		otd$(12)="8,62,PIC(ZZZZZ),UT,N" : _
		otd$(13)="9,23,PIC(---------.##),UT,N"
		fli$(05)="5,21,N 12.2,UT,N" : _
		fli$(06)="5,54,N 12.2,UT,N" : _
		fli$(07)="6,23,N 5,QUT,51" : _
		fli$(08)="6,62,N 5,QUT,52" : _
		fli$(09)="7,23,N 5,QUT,53" : _
		fli$(10)="7,62,N 5,QUT,54" : _
		fli$(11)="8,23,N 5,QUT,55" : _
		fli$(12)="8,62,N 5,QUT,56" : _
		fli$(13)="9,23,N 12.2,UT,N"
		for j=0 to 12
			fli$(j*4+14)=str$(j+11)&",12,N 12.2,UT,N" : _
			fli$(j*4+15)=str$(j+11)&",30,N 12.2,UT,N" : _
			fli$(j*4+16)=str$(j+11)&",50,N 12.2,UT,N" : _
			fli$(j*4+17)=str$(j+11)&",67,N 12.2,UT,N"
			otd$(j*4+14)=str$(j+11)&",12,PIC(---------.##),UT,N" : _
			otd$(j*4+15)=str$(j+11)&",30,PIC(---------.##),UT,N" : _
			otd$(j*4+16)=str$(j+11)&",50,PIC(---------.##),UT,N" : _
			otd$(j*4+17)=str$(j+11)&",67,PIC(---------.##),UT,N"
		next j
		fli$(2)="3,30,N 6,UT  ,N"
		if d(1)=0 then fli$(2)(11:11)="C"
		if d(2)=0 then fli$(2)(12:12)="E"
		fnFree("[Q]\GLmstr\ACGLScr.h[cno]")
L380: open #20: "Name=[Q]\GLmstr\ACGLScr.h[cno],Size=0,RecL=4281",internal,output: write #20,using 'Form POS 1,31*C 15,30*C 20,65*C 18,65*C 30': mat flo$,mat scr$,mat fli$,mat otd$ : _
		close #20:
goto Xit

Xit: fnend
include: Ertn
