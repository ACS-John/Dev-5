! Replace S:\acsGL\Conversion\FNS-Fix
! financial statement file
	autoLibrary
	pr newpage
	fnTop(program$)
	pr f "10,10,C 60": "ENTER THE COMPANY NUMBER OR 0 TO STOP:"
L70: input fields "10,50,N 2,UE,N": cno conv L70
	if cno=0 or cmdkey=5 or cmdkey=99 then goto Xit
	dim rno$*5,recno$*5,d$*50,te$*1,fil$(6)*30,idx$(6)*30,ac(9)
	fil$(1)="[Q]\GLmstr\ACGLFNSB.h[cno]": idx$(1)="[Q]\GLmstr\agfsidx4.h[cno]"
	fil$(2)="[Q]\GLmstr\ACGLFNSI.h[cno]": idx$(2)="[Q]\GLmstr\agfsidx3.h[cno]"
	fil$(3)="[Q]\GLmstr\ACGLFNSF.h[cno]": idx$(3)="[Q]\GLmstr\agfsidx5.h[cno]"
	fil$(4)="[Q]\GLmstr\AcGLFnSc.h[cno]": idx$(4)="[Q]\GLmstr\agfsidx1.h[cno]"
	fil$(5)="[Q]\GLmstr\ACGLFNSJ.h[cno]": idx$(5)="[Q]\GLmstr\agfsidx2.h[cno]"
	fil$(6)="[Q]\GLmstr\ACGLFNSG.h[cno]": idx$(6)="[Q]\GLmstr\agfsidx6.h[cno]"
	open #3: "Name=PROC."&wsid$,display,output ioerr L180
	close #3,free:
L180: open #3: "Name=PROC."&wsid$&",SIZE=0",display,output
	for f1=1 to 6
		open #1: "Name="&fil$(f1),internal,input,relative ioerr L330
		open #2: "Name=X",internal,output ioerr L230
		close #2,free:
L230: open #2: "Name=X,SIZE=0,RecL=83",internal,output ioerr L230
		for j=1 to lrec(1)
			read #1,using L260,rec=j: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec L280,conv L280
L260: form pos 1,c 5,c 50,c 1,2*n 2,15*n 1,n 3,n 5
			write #2,using L260: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
L280: next j
		close #1:
		close #2:
		execute "COPY X,"&fil$(f1)
		pr #3: "INDEX "&fil$(f1)&" "&idx$(f1)&" 1 5 REPLACE DupKeys"
L330: next f1
	close #3:
	chain "PROC=PROC."&wsid$
Xit: stop
