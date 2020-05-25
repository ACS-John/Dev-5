open #1: "Name=S:\Core\Data\acsllc\CLmstr.H[cno],KFName=S:\Core\Data\acsllc\CLIndex.H[cno],Shr",internal,outIn,keyed 
do
	dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10)
	dim ca(10),ph2$*12,ss2$*11,ar(5)
	dim arta(2),cm$*70,app(20),ma(20)
	dim ap2(20),ma2(20)
	read #1,using L740: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc, _
			mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma, _
			mat ap2,mat ma2 eof END1
	L740: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
	if app(4)=0 then goto L730
	pr #255: z$;" ";a$(1),ph$
	L730: !
loop
close #1:
END1: stop 
