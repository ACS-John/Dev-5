! formerly S:\acsCL\UnPdInv2
! Unpaid Invoice Summary
 
	autoLibrary
	on error goto Ertn
 
	dim dat$*20,vnam$*30,de$*31,gl(3),ade$*50
	dim gl$*12,gd$*50
 
	fnTop(program$)
	fndat(dat$)
	fnwait
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\unpdidx1.h[cno],Shr",internal,input,keyed
	open #glmstr=7: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLINDEX.H[cno],Shr",internal,input,keyed
	open #unpdaloc=8: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr",internal,input,keyed
	open #work=9: "Name="&env$('temp')&'\work.tmp,KFName='&env$('temp')&'\addr.tmp,KPS=1,KLN=12,RecL=68,Replace',internal,outIn,keyed
	open #paymstr=13: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed
	gosub HDR
READ_PAYTRANS: !
	read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof END1
	read #paymstr,using 'Form POS 9,C 30',key=vn$: vnam$ nokey L230
L230: pr #255,using 'Form POS 1,2*C 31,N 10.2': vnam$,de$,upa : _
	! this line would love to be a skip 0 one day
	t1+=upa
	restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
READ_UNPDALOC: !
	read #unpdaloc,using 'Form Pos 1,C 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': unvn$,univ$,mat gl,amt,ade$ eof READ_PAYTRANS : _
	if vn$<>unvn$ or iv$<>univ$ then goto READ_PAYTRANS
	if amt=0 then goto READ_UNPDALOC
	pr #255,using 'FORM POS 74,N 10.2,N 4,N 6,N 3': amt,mat gl pageoflow NEWPGE
	gl$=cnvrt$("N 3",gl(1))&cnvrt$("N 6",gl(2))&cnvrt$("N 3",gl(3))
	read #work,using 'FORM POS 63,PD 6.2',key=gl$: ga nokey L360
	ga+=amt
	rewrite #work,using 'FORM POS 63,PD 6.2',key=gl$: ga
	goto READ_UNPDALOC
 
L360: gd$="" : _
	read #glmstr,using 'Form Pos 13,C 50',key=gl$: gd$ nokey L370
L370: write #work,using 'FORM POS 1,C 12,C 50,PD 6.2': gl$,gd$,amt
	goto READ_UNPDALOC
! _______________________________________________________________
NEWPGE: pr #255: newpage: gosub HDR : continue
HDR: !
	fnopenprn
	pr #255,using 'FORM POS 1,C 8,CC 86': date$,env$('cnam')
	pr #255,using 'FORM POS 1,C 8,POS 40,C 40': time$,"Unpaid Invoice Summary"
	pr #255,using 'Form POS 1,C 4,N 4,CC 86': "Page",pg+=1,dat$
	pr #255: "Payee Name                      Description                    Ck-Amount  GL-Amount    GL-Number"
L470: pr #255: "______________________________ ______________________________ __________ __________ ____________"
return
 
END1: !
	gosub L470
	pr #255,using 'FORM POS 50,C 10,N 12.2': "Total",t1 : _
	pr #255: ""
	pr #255: " GL-Number    Description                                            Amount  "
	pr #255: "____________  __________________________________________________  __________"
	restore #work,search>="": nokey L600
READ_WORK: !
	read #work,using 'FORM POS 1,C 12,C 50,PD 6.2': gl$,gd$,ga eof L600
	pr #255,using 'FORM POS 1,C 14,C 50,N 12.2': gl$,gd$,ga
	goto READ_WORK
L600: fncloseprn
	goto Xit
 
Xit: fnXit
 
include: Ertn
 
